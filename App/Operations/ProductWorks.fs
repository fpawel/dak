module Dak.Operations.ProductWorks

open System

open Thread2
open Dak
open Dak.Coef

[<AutoOpen>]
module private Helpers = 
    type P = ViewModel.ProductViewModel
    let party = AppData.party
    let appCfg = Config.App.config
    let viewCfg = appCfg.View




let checkedProducts() = 
    party.Products
    |> Seq.filter( fun p -> p.On )
let hasNotCheckedProduct() = checkedProducts() |> Seq.isEmpty 
let hasCheckedProduct() = not <| hasNotCheckedProduct()


let doWithProducts f = 
    checkedProducts() |> Seq.iter ( fun p ->       
        if isKeepRunning() && p.On then 
            f p ) 

type Dak.ViewModel.ProductViewModel with
    
    member x.WriteKefs kefsValues  = maybeErr {
        for kef,value in kefsValues do
            let r = x.WriteKef kef value
            if r = Mdbs.Error.NotResponse then 
                return! Err "не отвечает" }

    member x.SetKefsSerial() =
         let value = (decimal DateTime.Now.Year - 2000m) * 10000m //+ x.Product.ProductSerial
         ()
            
    member x.WriteKefsInitValues() = 
        [   yield! 
                Alchemy.initKefsValues x.Serial party.GetPgs party.Party.PartyInfo.ProductType 
                |> List.map( fun (coef,value) -> coef, Some value )
        ]
        |> List.sortBy fst
        |> x.WriteKefs
        |> ignore

    member x.ReadKefs kefs = maybeErr {
        for kef in kefs do
            let! _ = x.ReadKef kef
            () }
    
    member x.Interrogate() = 
        let milvars = 
            let milvars = appCfg.View.InterrogateMilVars
            if Set.isEmpty milvars then Set.ofList [MilVar.conc] else milvars
        for milvar in MilVar.values do
            if Set.contains milvar milvars && isKeepRunning() then 
                x.ReadMil milvar 
                |> ignore            
        if appCfg.View.InterrogateStend6026 && isKeepRunning() then
            x.ReadStend6026() 
            |> ignore

    member x.SelectGas() = 
        maybeErr{
            do! x.WriteCmd Cmd.setGas (party.Party.PartyInfo.ProductType.Gas.ComponentCode) 
            do! x.WriteCmd Cmd.resetAlert 0m                        
        }
        |> x.WithLogError

    member x.WithLogError f = 
        match f with
        | Some error -> Logging.error "%s: %s" x.What error
        | _ -> ()

    member x.FixMilVarsValues gas t = 
        maybeErr{
            for var in MilVar.values do
                let! readedValue = x.ReadMil var
                let value = (var, gas,t), readedValue
                x.Product <-
                    { x.Product with 
                        Var = x.Product.Var.Add value }
                Logging.info "%s : %s(%s,%s) = %s" 
                    x.What (MilVar.name var) gas.What t.What
                        (Decimal.toStr6 readedValue) 
        } |> x.WithLogError

    member x.ResetAlert() = 
        x.WriteCmd Cmd.resetAlert 0m
        |> Result.someErr
        |> x.WithLogError

    member x.FixPorogs testPt =         
        maybeErr{
            let! (_,p1,p2) = x.ReadStend6026()
            let value = p1,p2
            x.Product <-
                { x.Product with 
                    TestPorog1 = x.Product.TestPorog1.Add (testPt, p1) 
                    TestPorog2 = x.Product.TestPorog2.Add (testPt, p2) 
                    
                    }
            x.LogTest (Alchemy.Test.TestPorog1 testPt)
            x.LogTest (Alchemy.Test.TestPorog2 testPt)
        } |> x.WithLogError

    member x.FixTestAdjust scaleEdgePt = 
        maybeErr{
            let! v = x.ReadMil 0                
            x.Product <- 
                { x.Product with
                    TestAdjust = x.Product.TestAdjust.Add (scaleEdgePt,v)
                } 
            x.LogTest (Alchemy.TestAdjust scaleEdgePt)
        } |> x.WithLogError

    member x.FixTestConc testPt =         
        maybeErr{
            let! readedValue = x.ReadMil 0   
            x.Product <- 
                { x.Product with
                    TestConc = x.Product.TestConc.Add (testPt,readedValue)
                }  
            x.LogTest (Alchemy.TestConc testPt)

            let! (readedValue,_,_) = x.ReadStend6026()
            x.Product <- 
                { x.Product with
                    TestCurr = x.Product.TestCurr.Add (testPt,readedValue)
                }  
            x.LogTest (Alchemy.TestCurr testPt)  
        } |> x.WithLogError

    member x.SetPorogs(porog1,porog2) = 
        maybeErr{
            do! x.WriteCmd Cmd.setPorog1 porog1
            do! x.WriteCmd Cmd.setPorog2 porog2
        } |> x.WithLogError


    member x.TestHartActual () = maybeErr{

        match x.WriteCmd Cmd.hart 1000m with
        | Ok () -> ()
        | Err error -> 
            Logging.warn "%s: сбой при переключениии на HART, %s. Возможно, HART протокол был активирован ранее" x.What error
        do! x.Stend6026Switch()
        do! Thread2.sleep (TimeSpan.FromSeconds 2.)
        
        let! hartToken = Hardware.Hart.on ()
        Logging.info "%s, HART: on - %A" x.What hartToken
        for n = 1 to 10 do
            let startTime = DateTime.Now
            let! conc = Hardware.Hart.readConc hartToken
            Logging.info "%s, HART: концентрация %d = %f" x.What n conc
            do! sleep (DateTime.Now - startTime)
        do! Hardware.Hart.off hartToken
        do! x.WriteKef Coef.Year <| Some 0m
        let year = decimal DateTime.Now.Year
        do! x.WriteKef Coef.Year <| Some year
        let! value = x.ReadKef Coef.Year
        if value <> year then 
            return! 
                sprintf "%s, HART: коэффициент %d - записано %M, считано %M" 
                    x.What Coef.Year year value
                |> Err
        }

    member x.TestHart() =      
        let result = x.TestHartActual()
        let level, text = 
            match result with
            | Some s -> Logging.Error,s
            | _ -> Logging.Info, "OK"
        Logging.write level "%s, результат проверки HART протокола: %s" x.What text        
        x.Product <- {x.Product with TestHart = Some {Date = DateTime.Now; Result = result}}        
    
        