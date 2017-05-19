module Dak.AppData
open System


[<AutoOpen>]
module private Helpers =
    let mutable parties = Repository.readPartiesHeaders()
    let isChanged = Ref.Observable(false) 
    let setChanged () =
        isChanged.Value <- true
    let config = Config.App.config
    

let subscribeOnChanged f = isChanged.AddChanged f

open Alchemy
open System.Windows.Forms

let party =    

    let party = 
        Repository.Party.openById config.View.PartyId parties
        |> Result.map ViewModel.Party
        |> Result.withDefault(fun err ->
            Logging.error "не удалось открыть ранее сохранённую партию : %s" err
            ViewModel.Party Party.NewEmpty )

    MainWindow.form.Text <- sprintf "Партия %s"  party.What
    Runtime.PropertyChanged.add party ( fun e -> 
        if e.PropertyName = "What" then 
            MainWindow.form.Text <- sprintf "Партия %s"  party.What )
    
    MainWindow.TextBlockPartyInfo.DataBindings.Add( new Binding("Text", party, "What" ) )
    MainWindow.TextBlockPartyInfo.Refresh()

    let addChangedListener (x:obj) =
        Runtime.PropertyChanged.add x ( fun _ -> 
            setChanged() )

    party.Products |> Seq.iter addChangedListener

    party.Products.AddingNew.Add(fun x -> 
        addChangedListener x.NewObject 
        setChanged() )


    party.Products.ListChanged.Add(fun _ -> 
        setChanged() )

    addChangedListener party
    let i = party.Party.PartyInfo
    parties <- parties.Add (i.ID, i)

    
    party

[<AutoOpen>]
module private Helpers1 =
    
    let setSaved () =   
        isChanged.Value <- false
        let i = party.Party.PartyInfo
        parties <- Map.add i.ID i parties
        config.View.PartyId <- i.ID
        
    type PSr = Chart.ProductSeriesInfo
    

let save _ =
    
    PhysVarValues.save()
    if isChanged.Value then
        match Repository.Party.save party.Party with
        | Err e -> Logging.error "не удалось сохранить партию : %s" e
        | Ok () -> setSaved ()

let updateChartSeriesList () =
    Chart.clear()        
    let i = party.Party.PartyInfo
    let partyPath = Repository.PartyPath.fromPartyHead i
    party.Products 
    |> Seq.filter(fun p -> p.On)
    |> Seq.iter ( fun p -> 
        Chart.addProductSeries
            {   PSr.Product = p.Product.ID
                PSr.Party = partyPath
                PSr.Name = p.What}  )

let load partyId = 
    let r = Repository.Party.openById partyId parties
    match r with
    | Err e -> Some e
    | Ok p -> 
        party.Party <- p
        setSaved ()
        None
    

let getParties prodType serial month year =
    parties |> Map.toList |> List.map snd
    |> List.filter( fun p ->
        match prodType with 
        | Some prodType -> p.ProductType = prodType
        | _ -> true  )
    |> List.filter( fun p ->
        match serial with 
        | Some serial -> p.Serials |> List.exists( (=) serial)
        | _ -> true  )    
    |> Repository.partiesHeadersDateTree 