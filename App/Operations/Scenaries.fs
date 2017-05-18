module Dak.Operations.Scenaries

open System

open Thread2
open Dak
open Dak.Operations.PartyWorks
open Dak.Operations.ProductWorks
open Dak.Operations.Helpers

[<AutoOpen>]
module private Helpers = 
    type P = ViewModel.ProductViewModel
    let party = AppData.party
    let appCfg = Config.App.config
    let viewCfg = appCfg.View

module ModalMessage = 
    let onShow = Ref.Initializable<_>(sprintf "ModalMessage.onShow %s:%s" __LINE__ __SOURCE_FILE__ )
    let getIsVivisble = Ref.Initializable<_>(sprintf "ModalMessage.getIsVivisble %s:%s" __LINE__ __SOURCE_FILE__ )
    let onClose = Ref.Initializable<unit -> unit>(sprintf "ModalMessage.onClose %s:%s" __LINE__ __SOURCE_FILE__ )
    
    let show (level:Logging.Level) (title:string) (text:string) = 
        onShow.Value title level text
        while isKeepRunning() && getIsVivisble.Value() do
            Threading.Thread.Sleep 50
        onClose.Value()    

let switchPneumo gas = maybeErr{
    let code, title, msg = 
        match gas with
        | Some (gas :ScalePt) -> 
            gas.Code, "Продувка " + gas.What, sprintf "газ %s подан" gas.What
        | _ -> 0uy, "Выключить пневмоблок", "пневмоблок закрыт"
    do! Hardware.Pneumo.switch code        
    Logging.info "%s" msg }

let blow (delay:DelayContext) gas what = 
    let title = "Продувка " + ScalePt.what gas
    newDelayOp what delay <| fun gettime -> maybeErr{        
        do! switchPneumo <| Some gas
        do! Delay.perform title true gettime  }

let setupTermo tempValue = 
    maybeErr{    
        do! switchPneumo None            
        Logging.info "Установка температуры %M\"C" tempValue
        do! Hardware.setupTermo tempValue Thread2.isKeepRunning party.Interrogate  
    }
    
let adjust scaleEdgePt = 
    let cmd, wht =
        match scaleEdgePt with
        | ScaleEdgeBeg -> Cmd.adj0, "начало" 
        | ScaleEdgeEnd -> Cmd.adjE, "конец"
    let whatOperation = sprintf "Калибровка %s шкалы" wht
    newDelayOp whatOperation AdjustDelay <| fun gettime -> maybeErr{
        let gas = scaleEdgePt.ScalePt
        let pgs = party.GetPgs gas
        Logging.info  "Калибровка %s шкалы, %M" wht  pgs
        do! switchPneumo <| Some gas
        do! Delay.perform (sprintf  "Продувка перед калибровкой %A" gas.What) true gettime 
        do! party.WriteModbus( cmd, pgs ) 
        do! sleep (TimeSpan.FromSeconds 10.)
        do! party.DoForEachOnProduct(fun p -> p.FixTestAdjust scaleEdgePt )
        }

let stopTermo = 
    "Остановка термокамеры" <|> (Hardware.Termo.stop >> Result.someErr)

let switchOffPneumo = 
    "Закрыть пневмоблок" <|> fun () -> switchPneumo None

let blowAir  = 
    "Продувка воздухом" <||> [   
        blow BlowDelay ScaleBeg "Продуть воздух"
        switchOffPneumo
    ]

let setupTermperaturePt = 
    party.GetTermoTemperature 
    >> setupTermo

type TemperaturePt with
    member x.Setup() = 
        setupTermperaturePt x
        
let texprogonTermo() =
    
    let whatTFix = function
        | TermoNormRet -> "Повторное снятие на НКУ"
        | TermoLow -> "Снятие на низкой температуре"
        | TermoNorm -> "Снятие на НКУ"
        | TermoHigh -> "Снятие на высокой температуре"
        | Termo90 -> "Снятие на +90 ⁰C"

    let whatTSetpoint = 
        function
            | TNorm -> "НКУ"
            | TLow -> "пониженной температуры"
            | THigh -> "повышенной температуры"
            | T90 -> "+90 ⁰C"
        >> (+) "Установка "

    let whatTWarm = 
        function
            | TNorm -> "НКУ"
            | TLow -> "пониженной температуре"
            | THigh -> "повышенной температуре"
            | T90 -> "при +90 ⁰C"
        >> (+) "Выдержка при "

    let termoPoints = 
        [   yield TermoNorm
            yield TermoLow
            yield TermoHigh
            if party.Party.PartyInfo.ProductType.Code = 37 then
                yield Termo90
            yield TermoNormRet]

    "Техпрогон в диапазоне рабочих температур" <||> [   
        for t in termoPoints ->
            (whatTFix t) <||> [
                yield (whatTSetpoint t.Temperature) <|> t.Temperature.Setup
                yield newDelayOp (whatTWarm t.Temperature)  WarmDelay <| 
                    Delay.perform (whatTWarm t.Temperature) true 
                if t = TermoNorm then 
                    yield adjust ScaleEdgeBeg
                    yield adjust ScaleEdgeEnd
                for gas in ScalePt.values do
                    yield sprintf "Снятие на %s" gas.What <||> [
                        blow BlowDelay gas "Продувка"
                        "Считывание" <|> fun () ->                     
                            party.DoForEachOnProduct(fun p -> p.FixMilVarsValues gas t ) 
                            |> Result.someErr                                
                        ]
                yield stopTermo
                yield blowAir
            ]
    ]
            
let adjustCurrent  =
    let cmd_set_k_336 = ( 0x80 <<< 8 ) + 336
    let cmd_set_k_335 = ( 0x80 <<< 8 ) + 335
    let sleep() = 
        Logging.info "задержка %A " appCfg.Main.AdjustCurrentDelay
        sleep appCfg.Main.AdjustCurrentDelay

    let doWrite whatOp cmdValueList =
        whatOp <|> fun () -> 
            maybeErr{
                Seq.toStr "," (fun (cmd,value) -> sprintf "(%A, %M)" (Cmd.what cmd) value) cmdValueList  
                |> Logging.info "Отправка команд %s" 
                for cmdValue in cmdValueList do
                    do! party.WriteModbus(cmdValue)
                do! sleep()
            }  
    let upd whatOp f =
        whatOp <|> fun () -> 
            maybeErr{
                do! party.DoForEachOnProduct( fun p -> ignore(f p) )
                do! sleep()
            }  
    "Калибровка тока" <||> [ 
        doWrite "Установка к-тов К335=0 К336=1" [ cmd_set_k_335,0m; cmd_set_k_336, 1m]
        doWrite "Установка тока 4 мА" [ Cmd.setCurrent, 4m ]
        upd "Считывание I1"  <| fun p ->            
            maybeErr{
                let d = p.AdjustCurrentData
                let! value,_,_ = p.ReadStend6026()
                d.I1 <- value
                Logging.info "%s, калибровка тока, I1 = %M" p.What d.I1                 
            }
        doWrite "Установка тока 20 мА" [ Cmd.setCurrent, 20m ]
        upd "Считывание I2, установка К336 и 4 мА"  <| fun p ->            
            maybeErr{
                let! value,_,_ = p.ReadStend6026()
                let d = p.AdjustCurrentData
                d.I2 <- value
                d.K336 <- 16m/(d.I2 - d.I1)
                do! p.WriteCmd cmd_set_k_336 d.K336
                do! p.WriteCmd Cmd.setCurrent 4m
                Logging.info "%s, калибровка тока, I2 = %M" p.What d.I2                
                Logging.info "%s, калибровка тока, K336 = 16 / (I2 - I1) = %M" p.What d.K336
            }
        upd "Считывание I3, установка К335"  <| fun p ->            
            maybeErr{
                let! value,_,_ = p.ReadStend6026()
                let d = p.AdjustCurrentData
                d.I3 <- value
                d.K335 <- 4m - d.I3
                do! p.WriteCmd cmd_set_k_335 d.K335
                Logging.info "%s, калибровка тока, I3 = %M" p.What d.I3                
                Logging.info "%s, калибровка тока, K335 = 4 - I3 = %M" p.What d.K335
            }
        "Установка test_watch_dog1" <|> fun () -> maybeErr{
            do! party.DoForEachOnProduct ( fun p ->  
                p.WriteCmd Cmd.testWatchdog 0m 
                |> ignore )
            }            
        ]

let setPorogsProduction =
    "Установка порогов на выпуск в эксплуатацию" <|> fun () ->
        party.GetProductType().Scale.Porogs
        |> party.SetPorogs

let testConc7rele  =
    
    let processTestPt (testPt : TestPt) = 
        testPt.What <||> [
            blow Blow1Delay testPt.ScalePt "Продувка для снят. сост. конт. реле"
            "Сброс сигнализации" <|> party.ResetAlert
            "Cнят. сост. конт. реле" <|> fun () ->                     
                party.DoForEachOnProduct(fun p ->  p.FixPorogs testPt ) 
                |> Result.someErr                
            newDelayOp "Продувка для снятия концентрации" Blow2Delay <| 
                Delay.perform 
                    (sprintf "%s, продувка для снятия конц." testPt.ScalePt.What) true                          
            "Cнятие концентрации" <|> fun () ->                     
                party.DoForEachOnProduct(fun p ->  p.FixTestConc testPt ) 
                |> Result.someErr    ]

    "Проверка диапазона измерений" <||> [
        yield "Установка порогов 90%" <|> fun () ->
            party.SetPorogsByPgsK 0.9m
        yield! List.map processTestPt [Test11; Test22; Test33; Test24; Test15]
        yield "Установка порогов 10%" <|> fun () ->
            party.SetPorogsByPgsK 0.1m
        yield processTestPt Test36
        yield Test17.What <||> [
            blow Blow1Delay Test17.ScalePt "Продувка для снят. сост. конт. реле"
            "Сброс сигнализации" <|> party.ResetAlert
            "Cнят. сост. конт. реле" <|> fun () ->                     
                party.DoForEachOnProduct(fun p ->  p.FixPorogs Test17 ) 
                |> Result.someErr                
            "Cнятие концентрации" <|> fun () ->                     
                party.DoForEachOnProduct(fun p ->  p.FixTestConc Test17 ) 
                |> Result.someErr
            switchOffPneumo ]
        yield setPorogsProduction ]

let testConc7  =
    
    let processTestPt delaycontext (testPt : TestPt) = 
        testPt.What <||> [
            newDelayOp "Продувка для снятия концентрации" delaycontext <| 
                Delay.perform 
                    (sprintf "%s, продувка для снятия конц." testPt.ScalePt.What) true                          
            "Cнятие концентрации" <|> fun () ->                     
            party.DoForEachOnProduct(fun p ->  p.FixTestConc testPt ) 
            |> Result.someErr ]

    "Проверка диапазона измерений" <||> [
        yield! List.map (processTestPt BlowDelay) [Test11; Test22; Test33; Test24; Test15; Test36] 
        yield processTestPt Blow1Delay Test17 
        yield switchOffPneumo ]

   


let main() = 
    let productType = party.Party.PartyInfo.ProductType
    "Настройка ДАК" <||>
        [   yield "Установка к-тов исп." <|> party.WriteKefsInitValues
            yield setPorogsProduction
            yield "Выбор измеряемого компонента" <|> party.SelectGas
            yield adjustCurrent
            yield adjust ScaleEdgeBeg
            yield adjust ScaleEdgeEnd
            if productType.HasHart then
                yield  "Проверка HART протокола" <|> fun () ->                     
                    party.DoForEachOnProduct(fun p ->  p.TestHart() |> ignore ) 
                    |> Result.someErr    
            yield 
                if productType.HasRele then
                    testConc7rele
                else
                    testConc7            
            yield texprogonTermo() ]
    |> Operation.ApplyRootConfig
    
let getAllAsList() =     
    Op.MapReduce Some scenary.Value

let getByDelayContext ctx = 
    getAllAsList() 
        |> List.choose ( function 
        | (Op.Timed (_, ({DelayContext = ctx} as delay),_) as op) -> 
            Some (op,delay)
        | _ -> None)

