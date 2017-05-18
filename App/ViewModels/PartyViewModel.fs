namespace Dak.ViewModel

open System
open System.ComponentModel
open Dak
open Alchemy

[<AutoOpen>]
module private PartyViewModelHelpers =
    type PartyPath = Repository.PartyPath
    type P = ProductViewModel
    type Col = System.Windows.Forms.DataGridViewTextBoxColumn
    type Cols = System.Windows.Forms.DataGridViewColumnCollection

type Party ( party : Dak.Party ) =
    inherit ViewModelBase() 

    let mutable party = party

    let getPgs gas = 
        party.GetPgs gas

    let getTermoTemperature t =
        party.GetTemperature t
    
    let products, setProductsList = 
        let x = BindingList<P>()
        let setProductsList xs = 
            x.Clear()
            xs 
            |> List.map ( fun p -> ProductViewModel.New(p, fun () -> party) )
            |> List.iter x.Add        
        setProductsList party.PartyData.Products
        x, setProductsList

    let getNewValidAddr() = 
        products 
        |> Seq.map(fun x -> x.Addr)  
        |> newValidModbusAddr

    let getProducts() = products |> Seq.map(fun x -> x.Product) |> Seq.toList
    
    let getChecked() =
        let xs = getProducts()
        if xs |> List.forall( fun x -> x.On ) then Nullable<bool>(true) else
        if xs |> List.forall( fun x -> not x.On ) then Nullable<bool>(false) else
        Nullable<bool>()
    let mutable productsChecked = Nullable<bool>()

    let setMainWindowTitle() = 
        let i = party.PartyInfo
        MainWindow.form.Text <- 
            sprintf "Партия %s %s %A" 
                (DateTime.format "dd/MM/yy" i.Date)
                i.ProductType.What  
                i.Name
  
    do
        setMainWindowTitle()

    let addLoggingEvent = new Event<_>()

    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName
    member __.NewValidAddr = getNewValidAddr()
    member private __.AddLoggingEvent = addLoggingEvent
    member __.OnAddLogging = addLoggingEvent.Publish            
    member __.Products = products
    member x.Party 

        with get() : Dak.Party = 
            products 
            |> Seq.map (fun x -> x.Product) 
            |> Seq.toList
            |> party.WithProducts
            
        and set (otherParty : Dak.Party) = 
            
            party <- otherParty

            products
            |> Seq.toList
            |> List.iter (fun p -> 
                products.Remove( p ) |> ignore
                p.Delete() )
            
            otherParty.PartyData.Products 
            |> List.map ( fun p -> ProductViewModel.New(p, fun () -> party) )
            |> List.iter products.Add

            x.RaisePropertyChanged "ProductType"
            x.RaisePropertyChanged "Name"
            setMainWindowTitle()
            Config.App.config.View.PartyId <- otherParty.PartyInfo.ID

    member x.CreateNewProduct() = 
        let product = Product.New getPgs party.PartyInfo.ProductType (getNewValidAddr())
        ProductViewModel.New(product, fun () -> party)
        |> products.Add  
        
        
    member x.DeleteProduct(product) = 
        let r = products.Remove( product )
        product.Delete()
        if not r then            
            failwith "Party.DeleteProduct : missing element"
        
        
        
    member __.HasOneCheckedProduct() =
        products
        |> Seq.exists( fun p -> p.On )
    
    member __.HasNotOneCheckedProduct() =
        products
        |> Seq.exists( fun p -> p.On )
        |> not

    member __.GetProductType() = party.PartyInfo.ProductType
    
    member x.ProductType 
        with get() = party.PartyInfo.ProductType.What
        and set v = 
            if v <> x.ProductType then
                let t = 
                    ProductType.values 
                    |> List.tryFind( ProductType.GetWhat >> (=) v)
                    |> Option.withDefault ProductType.values.Head
                party <- 
                    Party.NewWith
                        { party.PartyInfo with ProductType = t}
                        party.PartyData
                x.RaisePropertyChanged "ProductType"
                setMainWindowTitle()

    member x.Name 
        with get() = party.PartyInfo.Name
        and set v = 
            if v <> x.Name then
                party <- 
                    Party.NewWith
                        { party.PartyInfo with Name = v}
                        party.PartyData
                x.RaisePropertyChanged "Name"
                setMainWindowTitle()

    member x.ProdLog 
        with get() = party.PartyData.PerformingJournal
        and set value =
            if value <> party.PartyData.PerformingJournal then
                party <- 
                    Party.NewWith party.PartyInfo
                        { party.PartyData with PerformingJournal =  value }
                x.RaisePropertyChanged "ProdLog"

    member __.GetPgs pgs = getPgs pgs

    member __.GetTermoTemperature = getTermoTemperature        

    member x.SetTermoTemperature t value =
        if Some value <>  party.PartyData.TermoTemperature.TryFind t then
            x.Party <- 
                Party.NewWith party.PartyInfo
                    { party.PartyData with 
                        TermoTemperature =  party.PartyData.TermoTemperature.Add (t,value) }
    member x.SetPgs gas value =
        if Some value <> party.PartyData.BallonConc.TryFind gas then
            x.Party <- 
                Party.NewWith party.PartyInfo
                    { party.PartyData with 
                        BallonConc =  party.PartyData.BallonConc.Add (gas,value) }
            //x.RaisePropertyChanged <| ScalePt.name gas
            setMainWindowTitle()
            for p in products do
                p.ForceUpdateErrors()
        
                
    
    
[<AutoOpen>]
module private RunInfoHelpers =
    let private getHash (x:string) = x.GetHashCode()
    let now() = Some DateTime.Now
    let upd op y (x:Party) = 
        x.ProdLog <- Map.add (getHash op) y x.ProdLog
    let tryGetOp op (x:Party) = x.ProdLog.TryFind (getHash op)
    let getOp x party  = 
        tryGetOp x party 
        |> Option.getWithDefault PerformingOperation.createNew

type Party with
    
    member x.TryGetLogOperation operation  = tryGetOp operation x
    member x.GetLogOperation operation  = getOp operation x

    member x.LogStartOperation operation  = 
        upd operation  { RunStart = now(); RunEnd = None; LogLines = []} x

    member x.LogStopOperation operation =
        upd operation { getOp operation x  with RunEnd = now() } x
    
    member x.WriteJournal operation level text = 
        let perfOp = getOp operation x
        let logLines = (DateTime.Now,level,text)::perfOp.LogLines
        upd operation { perfOp with LogLines = logLines } x
        x.AddLoggingEvent.Trigger (operation,level,text)