module Dak.Operations.Run

open Dak
open Thread2
open Dak.Operations.Helpers
open Dak.Operations.PartyWorks
open Dak.Operations.ProductWorks
open System


[<AutoOpen>]
module private Helpers3 =
    let ( -->> ) s f =
        s <|> f
        |> run false        
    type P = ViewModel.ProductViewModel
    let party = AppData.party
    let appCfg = Config.App.config
    let viewCfg = appCfg.View
        
let runInterrogate() = "Опрос" -->> fun () -> maybeErr{ 
    do! Comport.testPort appCfg.Main.ComportProducts
    while isKeepRunning() do
        do! party.Interrogate() }

let setAddr addr = sprintf "Установка адреса %A" addr -->> fun () -> 
    maybeErr{     
        do! Mdbs.write appCfg.Main.ComportProducts 0uy Cmd.setAddy "установка адреса" addr
        do! sleep (TimeSpan.FromSeconds 1.)
        let! _ =  Mdbs.read3decimal appCfg.Main.ComportProducts (byte addr) 0 "проверка установки адреса"
        () }

let sendCommand (cmd,value as x) = 
    sprintf "%s <- %M" (Cmd.what cmd) value -->> fun () -> 
        party.WriteModbus x
        |> Result.someErr

module Pneumoblock =
    let switch gas = 
        ScalePt.what gas -->> fun () -> 
            Hardware.Pneumo.switch gas.Code |> Result.someErr
    let close() = 
        "Выкл." -->> fun () ->
            Hardware.Pneumo.switch 0uy |> Result.someErr

module Kefs = 
    
    let private run s f = 
        s -->> fun () ->
            let x = appCfg.View
            let kefs = 
                IntRanges.parseSet x.SelectedCoefs
                |> Set.toList
                
            if kefs.IsEmpty then Some "не выбрано ни одного коэффициента" else f kefs

    let write() = run "Запись к-тов" ( List.map(fun x -> x, None) >> party.WriteKefs  )
    let read() = run "Считывание к-тов"  party.ReadKefs

module TermoChamber =
    let termocicling (count,  tempMin, tempMax, warmTime)  =         
        "Термоциклирование" -->> fun () -> maybeErr {
            let warmTime _ = warmTime
            let mutable n = 0
            for n = 1 to count do
                let what = sprintf "Термоцикл %d из %d" n count
                Logging.info "%s начат" what
                for temp in [tempMin; tempMax] do
                    do! Scenaries.setupTermo temp
                    do! Delay.perform 
                            (sprintf "%s, выдержка при %A" what temp)
                            true warmTime  
                Logging.info "%s завершён" what
            Logging.info "Термоциклирование : перевод термокамеры в НКУ"
            do! Scenaries.setupTermperaturePt TNorm 
            do! Delay.perform "выдержка НКУ после термоциклирования" true warmTime                  
        }

    let read () = "Считывание температуры" -->> fun () -> maybeErr{        
        let! (t,stp) = Hardware.Termo.read ()
        Scenaries.ModalMessage.show Logging.Info 
            (sprintf "Температура %M\"C\nУставка %M\"C" t stp)
            "Температура термокамеры" 
        return! None }

    let readTermoLoop()  = "Опрос температуры" -->> fun () ->  
        maybeErr{
            while isKeepRunning() do
                let! (t,stp) = Hardware.Termo.read ()
                Logging.info "Температура %M\"C\nУставка %M\"C" t stp
            return ()
        } 

    let private (-->>) s f = 
        s -->> fun () ->
            f () |> Result.someErr

    let start() = "Старт" -->> Hardware.Termo.start
    
    let stop() = "Стоп" -->> Hardware.Termo.stop
    
    let setSetpoint value = "Уставка" -->> fun () -> 
        Hardware.Termo.setSetpoint value

let testConnect _ = 
    "Проверка связи" <|> fun () -> 
        let oks, errs =
            [   yield "Пневмоблок", Hardware.Pneumo.switch 0xFFuy
                yield "Термокамера", Hardware.Termo.stop() 
                for p in party.Products do
                    yield p.What + ": цифровой канал", Result.map ignore <| p.ReadMil 0
                    yield p.What + ": стенд 6026", Result.map ignore <| p.ReadStend6026() ]
            |> List.partition (snd >> Result.isOk)
        if errs.IsEmpty then None else
        errs 
        |> Seq.toStr "\n" (fun (what, err) -> sprintf "%s : %s" what (Result.Unwrap.err err) )
        |> Some
    |> run false