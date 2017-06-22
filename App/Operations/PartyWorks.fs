module Dak.Operations.PartyWorks

open System

open Thread2
open Dak
open Dak.Operations.ProductWorks

[<AutoOpen>]
module private Helpers = 
    type P = ViewModel.ProductViewModel
    let party = AppData.party
    let appCfg = Config.App.config
    let viewCfg = appCfg.View


let getProductsOn() = 
    party.Products
    |> Seq.filter( fun p -> p.On )
let isAllProductOff() = getProductsOn() |> Seq.isEmpty 
let isNotAllPtoductsOff() = not <| isAllProductOff()


let doForEachOnProduct() f = 
    getProductsOn() |> Seq.iter ( fun p ->       
        if isKeepRunning() && p.On then 
            f p ) 

type Dak.ViewModel.Party with
    member x.DoForEachOnProduct f = 
        let xs = x.Products |> Seq.filter(fun p -> p.On)
        if Seq.isEmpty xs then
            Err "приборы не отмечены"
        else
            for p in xs do 
                if isKeepRunning() && p.On then 
                    f p
            Ok ()

    member x.Interrogate() = Option.toResult <| maybeErr {
        let xs = x.Products |> Seq.filter(fun p -> p.On)
        if Seq.isEmpty xs then
            return "приборы не отмечены"
        else
            do! Comport.testPort appCfg.Main.ComportProducts
            for p in xs do 
                if isKeepRunning() && p.On then
                    p.Interrogate()
        }

    member x.WriteModbus(cmd,value) = 
        x.DoForEachOnProduct (fun p -> p.WriteCmd cmd value |> ignore ) 

    member x.WriteKefs(kefs) = maybeErr{
        do! Comport.testPort appCfg.Main.ComportProducts
        do! x.DoForEachOnProduct (fun p -> 
            p.WriteKefs kefs |> ignore ) }

    member x.WriteMilTermoCompensation scalePt = maybeErr{
        do! Comport.testPort appCfg.Main.ComportProducts
        do! x.DoForEachOnProduct (fun p -> 
            Alchemy.MilTermoCompensation.info scalePt
            |> fst
            |> List.map(fun coef -> coef, p.Product.Coef.TryFind coef)
            |> p.WriteKefs 
            |> ignore ) 
        }

    member x.ReadKefs(kefs) = maybeErr{
        do! Comport.testPort appCfg.Main.ComportProducts
        do! x.DoForEachOnProduct (fun p -> 
            p.ReadKefs kefs |> ignore ) }
    
        

    member x.SetPorogs(porog1,porog2) =
        party.DoForEachOnProduct ( fun p ->  p.SetPorogs(porog1,porog2) |> ignore ) 
        |> Result.someErr

    member x.SetPorogsByPgsK k =
        x.SetPorogs(x.GetPgs ScaleMid * k,  x.GetPgs ScaleEnd * k)

    member x.ResetAlert() = maybeErr{
        do! party.DoForEachOnProduct ( fun p ->  p.ResetAlert() ) 
        do! sleep (TimeSpan.FromSeconds 1.)
    }
        
        