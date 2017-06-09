namespace  Dak.ViewModel

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.ComponentModel
open Dak
open Alchemy

[<AutoOpen>]
module private ViewModelProductHelpers =
    let appCfg = Config.App.config

    let formatPorogSt = function
        | Some true -> "вкл." 
        | Some false -> "выкл." 
        | _ -> ""

    let fmtDecOpt = Option.map Decimal.toStr6 >> Option.withDefault ""

    let setKef k v =  state{                
        let! p = getState 
        let m = 
            match v with
            | None -> Map.remove k
            | Some v -> Map.add k v               
        do! setState { p with Coef = m p.Coef } }

    let setVar k v =  state{                
        let! p = getState 
        let m = 
            match v with
            | None -> Map.remove k
            | Some v -> Map.add k v        
        do! setState { p with Var = m p.Var  }        
    }

    
    type Column = Windows.Forms.DataGridViewTextBoxColumn
    let updKef  (col:Column) kefValue k = 
        Coef.values |> List.tryFindIndex( (=) k )
        |> Option.iter( fun kefRow ->
            let row = MainWindow.gridCoefs.Rows.[kefRow]
            let cell = row.Cells.[col.Index]
            let value = 
                kefValue
                |> Option.map Decimal.toStr6
                |> Option.withDefault ""
            if cell.Value=null && value <> "" || (string) cell.Value <> value then
                cell.Value <- value )

    let getDakTestData (p : Product) = function
        | DakTestConcData a -> p.TestConc.TryFind a
        | DakTestCurrData a -> p.TestCurr.TryFind a
        | DakTestPorog1Data a -> 
            p.TestPorog1.TryFind a 
            |> Option.map (function false -> 0m |  _ -> 1m)
        | DakTestPorog2Data a -> 
            p.TestPorog2.TryFind a 
            |> Option.map (function false -> 0m |  _ -> 1m)
        | DakAdjustData s ->
            p.TestAdjust.TryFind s

    let setDakTestData testvar value = 
        state{                
            let! p = getState 
            match testvar with
            | DakTestConcData a -> 
                let m = 
                    match value with
                    | None -> Map.remove a
                    | Some v -> Map.add a v
                do! setState { p with TestConc = m p.TestConc  }
            | DakTestCurrData a -> 
                let m = 
                    match value with
                    | None -> Map.remove a
                    | Some v -> Map.add a v
                do! setState { p with TestCurr = m p.TestCurr  }
            | DakTestPorog1Data a -> 
                let m = 
                    match value with
                    | None -> Map.remove a
                    | Some v -> Map.add a (v <> 0m)
                do! setState { p with TestPorog1 = m p.TestPorog1  }
            | DakTestPorog2Data a -> 
                let m = 
                    match value with
                    | None -> Map.remove a
                    | Some v -> Map.add a (v <> 0m)
                do! setState { p with TestPorog2 = m p.TestPorog2  }
            | DakAdjustData s ->
                let m = 
                    match value with
                    | None -> Map.remove s
                    | Some v -> Map.add s v
                do! setState { p with TestAdjust = m p.TestAdjust  }
        }
    

type AdjustCurrentData = 
    {   mutable I1 : decimal
        mutable I2 : decimal
        mutable I3 : decimal
        mutable K335 : decimal
        mutable K336 : decimal
    }


type ProductViewModel(product : Product, getParty : unit -> Party) =
    inherit ViewModelBase()    
    let mutable product = product
    let mutable connection : Result<string,string> option = None        
    let mutable milVar = Map.empty
    let mutable current_porogs = None

    let adjustCurrentData =
        {   I1 = 0m
            I2 = 0m
            I3 = 0m
            K335 = 0m
            K336 = 0m
        }

    let coefCol = 
        let col = new Column(HeaderText = product.What, Visible = product.On)
        MainWindow.gridCoefs.Columns.Add( col ) |> ignore
        //ProductViewModel.CoefColumns.[col] <- this
        col

    let getMilVarValueUi var =
        Map.tryFind var milVar
        |> Option.map Decimal.toStr6
        |> Option.withDefault ""

    let updateWhat() = 
        MainWindow.form.PerformThreadSafeAction ( fun () -> 
            Chart.setProductLegend product.ID product.What
            coefCol.HeaderText <- product.What
        )

    let getPgs gas =
        getParty().GetPgs gas
    let getProductType() =
        getParty().PartyInfo.ProductType

    let getTestResult test product =
        let i = ProductInfo.New product getPgs (getProductType()) 
        i.TestResult test

    let getPartyPath() =
        Repository.PartyPath.fromPartyHead (getParty().PartyInfo)

    do 
        for k in Coef.values do
            updKef coefCol (product.Coef.TryFind k) k

    override x.RaisePropertyChanged propertyName = 
        ViewModelBase.raisePropertyChanged x propertyName

    member private x.SetMilVarValue var value =
        let k : PhysVarValues.Key = 
            {   Party = getPartyPath()
                Product = product.ID
                Var = var }
        PhysVarValues.addValue k value
        MainWindow.form.PerformThreadSafeAction <| fun () ->
            Chart.addProductValue product.ID var value
        if Map.tryFind var milVar <> Some value then
            milVar <- Map.add var value milVar
            x.RaisePropertyChanged <| MilVar.prop var

    member x.MilConc = getMilVarValueUi MilVar.conc
    member x.MilCurrent = getMilVarValueUi MilVar.curr
    member x.MilVar1 = getMilVarValueUi MilVar.var1
    member x.MilRefK = getMilVarValueUi MilVar.refk
    member x.MilWorkK = getMilVarValueUi MilVar.workk
    member x.MilTemperature = getMilVarValueUi MilVar.temp

    member x.Porog1 = 
        current_porogs 
        |> Option.map(fun (_,p,_) -> p )
        
    member x.Porog2 = 
        current_porogs 
        |> Option.map(fun (_,_,p) -> p )

    member x.Current = 
        current_porogs |> Option.map(fun (p,_,_) -> p )

    member x.Porog1Str = 
        x.Porog1
        |> Option.map Alchemy.formatRele
        |> Option.withDefault ""
        
    member x.Porog2Str = 
        x.Porog2
        |> Option.map Alchemy.formatRele
        |> Option.withDefault ""

    member x.CurrentStr = 
        x.Current
        |> Option.map Decimal.toStr6
        |> Option.withDefault ""
        
            
    member x.Connection
        with get () = connection
        and set v = 
            if v <> connection then
                connection <- v
                x.RaisePropertyChanged "Connection"
    
    member x.On 
        with get () = product.On
        and set v = 
            if v <> product.On then
                x.Product <- { x.Product with On = v}
                
    member x.What = product.What

    member x.Addr
        with get () = product.Addr          
        and set v = 
            if v <> product.Addr then
                x.Product <- { x.Product with Addr = v}
                
    member x.Serial
        with get () = product.Serial
        and set v = 
            if v <> product.Serial then
                x.Product <- 
                    runState (setKef Coef.Serial (Some v)) x.Product   
                    |> snd                

    member x.ForceUpdateErrors() =        
        for test in Alchemy.Test.values do
            x.RaisePropertyChanged test.Property

    member x.Product 
        with get () = product
        and set other =
            if product = other then () else
            let prev  = product
            product <- other
            let (~%%) = x.RaisePropertyChanged

            for var in DakTestData.values do
                let v = getDakTestData product var
                if v <>  getDakTestData prev var  then
                    %% var.Property
            
            for test in Alchemy.Test.values do
                let v = getTestResult test product
                if v <>  getTestResult test prev then
                    %% test.Property

            for scalePt in ScalePt.values do
                for t in TermoPt.values do
                    for var in MilVar.values do
                        let k = var,scalePt,t
                        let v = product.Var.TryFind k 
                        if v <> prev.Var.TryFind k then
                            %% termoVarProperty k

            for k in Coef.values do
                let v = product.Coef.TryFind k
                if v <> prev.Coef.TryFind k then                    
                    updKef coefCol v k

            let updateWhat =
                let mutable already = false
                fun () -> 
                    if already then () else
                        already <- true
                        updateWhat()

            if product.What <> prev.What then
                x.RaisePropertyChanged "What"
                updateWhat()

            if product.Addr <> prev.Addr then
                x.RaisePropertyChanged "Addr"
                updateWhat()

            if product.Coef.TryFind Coef.Serial <> prev.Coef.TryFind Coef.Serial then
                updateWhat()
                x.RaisePropertyChanged "Serial"

            if product.On <> prev.On then
                x.RaisePropertyChanged "On"
                coefCol.Visible <- product.On
                if product.On then
                    Chart.addProductSeries
                        {   Product = product.ID
                            Party = getPartyPath()
                            Name = product.What }
                else 
                    Chart.removeProductSeries product.ID |> ignore
            x.RaisePropertyChanged "Product"

    member x.ReadMil var = 
        let result = 
            Mdbs.read3decimal 
                appCfg.Main.ComportProducts x.Addr var 
                (sprintf "цифровой канал: %s" <| MilVar.descr var)
        match result with
        | Ok value -> 
            x.SetMilVarValue var value
            x.Connection <-
                sprintf "%s = %M" (MilVar.name var) value
                |> Ok
                |> Some
        | Err err ->  x.Connection <- Some (Err err)
        result

    member x.ReadKef kef = 
        let result = 
            Mdbs.read3decimal 
                appCfg.Main.ComportProducts x.Addr (Coef.reg kef) 
                (sprintf "цифровой канал: коэф.%d"  kef)
        match result with
        | Ok value -> 
            x.Product <- 
                {product with Coef = product.Coef.Add (kef,value) }
            Logging.info "%s.коэф.%d = %M" x.What kef value
            x.Connection <-
                sprintf "К%d = %M" kef value
                |> Ok
                |> Some
        | Err err ->  
            Logging.error "%s.коэф.%d : %s" x.What kef err 
            x.Connection <- Some (Err err)
        result

    member x.Stend6026Switch () = 
        let r = Stend6026.switch <| int product.Addr
        x.Connection <-
            r 
            |> Result.map( sprintf "стенд: %A" )
            |> Some
        r

    member x.ReadStend6026() = 
        let r = Stend6026.read <| int product.Addr
        x.Connection <-
            r 
            |> Result.map( fun a -> 
                current_porogs <- Some a
                x.RaisePropertyChanged "Porog1"
                x.RaisePropertyChanged "Porog2"
                x.RaisePropertyChanged "Current"
                x.RaisePropertyChanged "Porog1Str"
                x.RaisePropertyChanged "Porog2Str"
                x.RaisePropertyChanged "CurrentStr"
                sprintf "стенд: %A" a  )
            |> Some
        r
                
    member x.WriteCmd cmd value = 
        let what = Cmd.what cmd
        let r = Mdbs.write appCfg.Main.ComportProducts x.Addr cmd what value
        x.Connection <- 
            r 
            |> Result.map(fun v -> sprintf "%s <-- %s" what (Decimal.toStr6 value))
            |> Some 
        r

    member x.WriteKef kef value =
        let value =
            match value, product.Coef.TryFind kef with
            | Some value, _ -> Some value
            | _, Some value -> Some  value            
            | _ ->  None
        match value with
        | None -> 
            Logging.warn "%s, нет значения записываемого к-та %d, %s" 
                x.What kef (Coef.what kef)
            Ok()
        | Some value -> x.WriteCmd ( Coef.cmd kef) value 

    member __.CoefColumn = coefCol        

    member __.AdjustCurrentData = adjustCurrentData

    member __.GetTestResult test = getTestResult test product

    member __.GetTestResultStr test = 
        getTestResult test product
        |> Option.map string 
        |> Option.withDefault ""

    member x.SetCoefStr kef value = 
        let value = String.tryParseDecimal value
        if product.Coef.TryFind kef = value then () else
        let m = 
            match value with
            | Some value -> product.Coef.Add (kef, value)
            | _ -> product.Coef.Remove kef
        x.Product <- {x.Product with Coef = m}

    member x.LogTest (test : Alchemy.Test)=
        let what = sprintf  "%s,%s" x.What test.What 
        match  x.GetTestResult test with
        | None -> 
            Logging.error "%s : не удалось получить результат проверки. Произошла ошибка программы." what
        | Some tr ->
            Logging.write  
                (if tr.IsFailed then Logging.Error else Logging.Info)
                "%s : %A" what tr   

    member x.GetTermoMilVar var = 
        product.Var.TryFind var
        |> Option.map Decimal.toStr6
        |> Option.withDefault ""

    member x.GetTermoMilVarStr var = 
        product.Var.TryFind var
        |> Option.map Decimal.toStr6
        |> Option.withDefault ""

    member x.SetTermoMilVarStr var value = 
        String.tryParseDecimal value
        |> x.SetTermoMilVar var

    member x.SetTermoMilVar var value =        
        let upd = 
            value 
            |> Option.map(fun value -> x.Product.Var.Add (var,value) )
            |> Option.withDefault ( x.Product.Var.Remove var )
        x.Product <-  {   x.Product with Var = upd }

    member x.GetDakTestData var =
        getDakTestData product var 

    member x.SetDakTestData var value =
        x.Product <- 
            runState (setDakTestData var value) x.Product   
            |> snd       

    member __.GetDakTestDataStr var =
        getDakTestData product var 
        |> Option.map Decimal.toStr6
        |> Option.withDefault ""

    member x.SetDakTestDataStr var value =
        let value = String.tryParseDecimal value
        x.Product <- 
            runState (setDakTestData var value) x.Product   
            |> snd    

    member x.ComputeMilTermoCompensation scalePt = 
        let party = getParty()
        let pgs4Conc = party.GetPgs ScaleEnd
        let st = Alchemy.MilTermoCompensation.compute scalePt pgs4Conc
        let _,nextP =  runState st product
        x.Product <- nextP
        

    
 [<AutoOpen>]
module private ViewModelProductHelpers1 =    
    let coefColumns = Dictionary<Column, ProductViewModel>()
    
type ProductViewModel with 
    static member New (p, partyData ) =
        let x = ProductViewModel (p, partyData )
        coefColumns.[x.CoefColumn] <- x
        x

    static member OfCoefColumn column =
        coefColumns.[column] 

    member x.Delete () = 
        MainWindow.gridCoefs.Columns.Remove x.CoefColumn |> ignore
        coefColumns.Remove x.CoefColumn |> ignore

type MyBrowseableAttribute = MyWinForms.Utils.MyBrowseableAttribute
///<Generated>

type ProductViewModel with
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleBeg_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleBeg, TermoNorm)
        and set value = 
            if value <> x.TermoVar_conc_ScaleBeg_TermoNorm then
                x.SetTermoMilVarStr (MilVar.conc, ScaleBeg, TermoNorm) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleBeg_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleBeg, TermoNorm)
        and set value = 
            if value <> x.TermoVar_temp_ScaleBeg_TermoNorm then
                x.SetTermoMilVarStr (MilVar.temp, ScaleBeg, TermoNorm) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleBeg_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleBeg, TermoNorm)
        and set value = 
            if value <> x.TermoVar_curr_ScaleBeg_TermoNorm then
                x.SetTermoMilVarStr (MilVar.curr, ScaleBeg, TermoNorm) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleBeg_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleBeg, TermoNorm)
        and set value = 
            if value <> x.TermoVar_workk_ScaleBeg_TermoNorm then
                x.SetTermoMilVarStr (MilVar.workk, ScaleBeg, TermoNorm) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleBeg_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleBeg, TermoNorm)
        and set value = 
            if value <> x.TermoVar_refk_ScaleBeg_TermoNorm then
                x.SetTermoMilVarStr (MilVar.refk, ScaleBeg, TermoNorm) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleBeg_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleBeg, TermoNorm)
        and set value = 
            if value <> x.TermoVar_var1_ScaleBeg_TermoNorm then
                x.SetTermoMilVarStr (MilVar.var1, ScaleBeg, TermoNorm) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleMid_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleMid, TermoNorm)
        and set value = 
            if value <> x.TermoVar_conc_ScaleMid_TermoNorm then
                x.SetTermoMilVarStr (MilVar.conc, ScaleMid, TermoNorm) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleMid_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleMid, TermoNorm)
        and set value = 
            if value <> x.TermoVar_temp_ScaleMid_TermoNorm then
                x.SetTermoMilVarStr (MilVar.temp, ScaleMid, TermoNorm) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleMid_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleMid, TermoNorm)
        and set value = 
            if value <> x.TermoVar_curr_ScaleMid_TermoNorm then
                x.SetTermoMilVarStr (MilVar.curr, ScaleMid, TermoNorm) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleMid_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleMid, TermoNorm)
        and set value = 
            if value <> x.TermoVar_workk_ScaleMid_TermoNorm then
                x.SetTermoMilVarStr (MilVar.workk, ScaleMid, TermoNorm) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleMid_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleMid, TermoNorm)
        and set value = 
            if value <> x.TermoVar_refk_ScaleMid_TermoNorm then
                x.SetTermoMilVarStr (MilVar.refk, ScaleMid, TermoNorm) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleMid_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleMid, TermoNorm)
        and set value = 
            if value <> x.TermoVar_var1_ScaleMid_TermoNorm then
                x.SetTermoMilVarStr (MilVar.var1, ScaleMid, TermoNorm) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleEnd_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleEnd, TermoNorm)
        and set value = 
            if value <> x.TermoVar_conc_ScaleEnd_TermoNorm then
                x.SetTermoMilVarStr (MilVar.conc, ScaleEnd, TermoNorm) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleEnd_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleEnd, TermoNorm)
        and set value = 
            if value <> x.TermoVar_temp_ScaleEnd_TermoNorm then
                x.SetTermoMilVarStr (MilVar.temp, ScaleEnd, TermoNorm) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleEnd_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleEnd, TermoNorm)
        and set value = 
            if value <> x.TermoVar_curr_ScaleEnd_TermoNorm then
                x.SetTermoMilVarStr (MilVar.curr, ScaleEnd, TermoNorm) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleEnd_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleEnd, TermoNorm)
        and set value = 
            if value <> x.TermoVar_workk_ScaleEnd_TermoNorm then
                x.SetTermoMilVarStr (MilVar.workk, ScaleEnd, TermoNorm) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleEnd_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleEnd, TermoNorm)
        and set value = 
            if value <> x.TermoVar_refk_ScaleEnd_TermoNorm then
                x.SetTermoMilVarStr (MilVar.refk, ScaleEnd, TermoNorm) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleEnd_TermoNorm 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleEnd, TermoNorm)
        and set value = 
            if value <> x.TermoVar_var1_ScaleEnd_TermoNorm then
                x.SetTermoMilVarStr (MilVar.var1, ScaleEnd, TermoNorm) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("«минус» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleBeg_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleBeg, TermoLow)
        and set value = 
            if value <> x.TermoVar_conc_ScaleBeg_TermoLow then
                x.SetTermoMilVarStr (MilVar.conc, ScaleBeg, TermoLow) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("«минус» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleBeg_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleBeg, TermoLow)
        and set value = 
            if value <> x.TermoVar_temp_ScaleBeg_TermoLow then
                x.SetTermoMilVarStr (MilVar.temp, ScaleBeg, TermoLow) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("«минус» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleBeg_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleBeg, TermoLow)
        and set value = 
            if value <> x.TermoVar_curr_ScaleBeg_TermoLow then
                x.SetTermoMilVarStr (MilVar.curr, ScaleBeg, TermoLow) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("«минус» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleBeg_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleBeg, TermoLow)
        and set value = 
            if value <> x.TermoVar_workk_ScaleBeg_TermoLow then
                x.SetTermoMilVarStr (MilVar.workk, ScaleBeg, TermoLow) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("«минус» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleBeg_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleBeg, TermoLow)
        and set value = 
            if value <> x.TermoVar_refk_ScaleBeg_TermoLow then
                x.SetTermoMilVarStr (MilVar.refk, ScaleBeg, TermoLow) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("«минус» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleBeg_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleBeg, TermoLow)
        and set value = 
            if value <> x.TermoVar_var1_ScaleBeg_TermoLow then
                x.SetTermoMilVarStr (MilVar.var1, ScaleBeg, TermoLow) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("«минус» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleMid_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleMid, TermoLow)
        and set value = 
            if value <> x.TermoVar_conc_ScaleMid_TermoLow then
                x.SetTermoMilVarStr (MilVar.conc, ScaleMid, TermoLow) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("«минус» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleMid_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleMid, TermoLow)
        and set value = 
            if value <> x.TermoVar_temp_ScaleMid_TermoLow then
                x.SetTermoMilVarStr (MilVar.temp, ScaleMid, TermoLow) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("«минус» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleMid_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleMid, TermoLow)
        and set value = 
            if value <> x.TermoVar_curr_ScaleMid_TermoLow then
                x.SetTermoMilVarStr (MilVar.curr, ScaleMid, TermoLow) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("«минус» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleMid_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleMid, TermoLow)
        and set value = 
            if value <> x.TermoVar_workk_ScaleMid_TermoLow then
                x.SetTermoMilVarStr (MilVar.workk, ScaleMid, TermoLow) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("«минус» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleMid_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleMid, TermoLow)
        and set value = 
            if value <> x.TermoVar_refk_ScaleMid_TermoLow then
                x.SetTermoMilVarStr (MilVar.refk, ScaleMid, TermoLow) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("«минус» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleMid_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleMid, TermoLow)
        and set value = 
            if value <> x.TermoVar_var1_ScaleMid_TermoLow then
                x.SetTermoMilVarStr (MilVar.var1, ScaleMid, TermoLow) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("«минус» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleEnd_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleEnd, TermoLow)
        and set value = 
            if value <> x.TermoVar_conc_ScaleEnd_TermoLow then
                x.SetTermoMilVarStr (MilVar.conc, ScaleEnd, TermoLow) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("«минус» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleEnd_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleEnd, TermoLow)
        and set value = 
            if value <> x.TermoVar_temp_ScaleEnd_TermoLow then
                x.SetTermoMilVarStr (MilVar.temp, ScaleEnd, TermoLow) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("«минус» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleEnd_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleEnd, TermoLow)
        and set value = 
            if value <> x.TermoVar_curr_ScaleEnd_TermoLow then
                x.SetTermoMilVarStr (MilVar.curr, ScaleEnd, TermoLow) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("«минус» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleEnd_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleEnd, TermoLow)
        and set value = 
            if value <> x.TermoVar_workk_ScaleEnd_TermoLow then
                x.SetTermoMilVarStr (MilVar.workk, ScaleEnd, TermoLow) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("«минус» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleEnd_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleEnd, TermoLow)
        and set value = 
            if value <> x.TermoVar_refk_ScaleEnd_TermoLow then
                x.SetTermoMilVarStr (MilVar.refk, ScaleEnd, TermoLow) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("«минус» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleEnd_TermoLow 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleEnd, TermoLow)
        and set value = 
            if value <> x.TermoVar_var1_ScaleEnd_TermoLow then
                x.SetTermoMilVarStr (MilVar.var1, ScaleEnd, TermoLow) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("«плюс» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleBeg_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleBeg, TermoHigh)
        and set value = 
            if value <> x.TermoVar_conc_ScaleBeg_TermoHigh then
                x.SetTermoMilVarStr (MilVar.conc, ScaleBeg, TermoHigh) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("«плюс» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleBeg_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleBeg, TermoHigh)
        and set value = 
            if value <> x.TermoVar_temp_ScaleBeg_TermoHigh then
                x.SetTermoMilVarStr (MilVar.temp, ScaleBeg, TermoHigh) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("«плюс» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleBeg_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleBeg, TermoHigh)
        and set value = 
            if value <> x.TermoVar_curr_ScaleBeg_TermoHigh then
                x.SetTermoMilVarStr (MilVar.curr, ScaleBeg, TermoHigh) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("«плюс» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleBeg_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleBeg, TermoHigh)
        and set value = 
            if value <> x.TermoVar_workk_ScaleBeg_TermoHigh then
                x.SetTermoMilVarStr (MilVar.workk, ScaleBeg, TermoHigh) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("«плюс» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleBeg_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleBeg, TermoHigh)
        and set value = 
            if value <> x.TermoVar_refk_ScaleBeg_TermoHigh then
                x.SetTermoMilVarStr (MilVar.refk, ScaleBeg, TermoHigh) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("«плюс» ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleBeg_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleBeg, TermoHigh)
        and set value = 
            if value <> x.TermoVar_var1_ScaleBeg_TermoHigh then
                x.SetTermoMilVarStr (MilVar.var1, ScaleBeg, TermoHigh) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("«плюс» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleMid_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleMid, TermoHigh)
        and set value = 
            if value <> x.TermoVar_conc_ScaleMid_TermoHigh then
                x.SetTermoMilVarStr (MilVar.conc, ScaleMid, TermoHigh) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("«плюс» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleMid_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleMid, TermoHigh)
        and set value = 
            if value <> x.TermoVar_temp_ScaleMid_TermoHigh then
                x.SetTermoMilVarStr (MilVar.temp, ScaleMid, TermoHigh) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("«плюс» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleMid_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleMid, TermoHigh)
        and set value = 
            if value <> x.TermoVar_curr_ScaleMid_TermoHigh then
                x.SetTermoMilVarStr (MilVar.curr, ScaleMid, TermoHigh) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("«плюс» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleMid_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleMid, TermoHigh)
        and set value = 
            if value <> x.TermoVar_workk_ScaleMid_TermoHigh then
                x.SetTermoMilVarStr (MilVar.workk, ScaleMid, TermoHigh) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("«плюс» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleMid_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleMid, TermoHigh)
        and set value = 
            if value <> x.TermoVar_refk_ScaleMid_TermoHigh then
                x.SetTermoMilVarStr (MilVar.refk, ScaleMid, TermoHigh) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("«плюс» ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleMid_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleMid, TermoHigh)
        and set value = 
            if value <> x.TermoVar_var1_ScaleMid_TermoHigh then
                x.SetTermoMilVarStr (MilVar.var1, ScaleMid, TermoHigh) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("«плюс» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleEnd_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleEnd, TermoHigh)
        and set value = 
            if value <> x.TermoVar_conc_ScaleEnd_TermoHigh then
                x.SetTermoMilVarStr (MilVar.conc, ScaleEnd, TermoHigh) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("«плюс» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleEnd_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleEnd, TermoHigh)
        and set value = 
            if value <> x.TermoVar_temp_ScaleEnd_TermoHigh then
                x.SetTermoMilVarStr (MilVar.temp, ScaleEnd, TermoHigh) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("«плюс» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleEnd_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleEnd, TermoHigh)
        and set value = 
            if value <> x.TermoVar_curr_ScaleEnd_TermoHigh then
                x.SetTermoMilVarStr (MilVar.curr, ScaleEnd, TermoHigh) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("«плюс» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleEnd_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleEnd, TermoHigh)
        and set value = 
            if value <> x.TermoVar_workk_ScaleEnd_TermoHigh then
                x.SetTermoMilVarStr (MilVar.workk, ScaleEnd, TermoHigh) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("«плюс» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleEnd_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleEnd, TermoHigh)
        and set value = 
            if value <> x.TermoVar_refk_ScaleEnd_TermoHigh then
                x.SetTermoMilVarStr (MilVar.refk, ScaleEnd, TermoHigh) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("«плюс» ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleEnd_TermoHigh 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleEnd, TermoHigh)
        and set value = 
            if value <> x.TermoVar_var1_ScaleEnd_TermoHigh then
                x.SetTermoMilVarStr (MilVar.var1, ScaleEnd, TermoHigh) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("+90 ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleBeg_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleBeg, Termo90)
        and set value = 
            if value <> x.TermoVar_conc_ScaleBeg_Termo90 then
                x.SetTermoMilVarStr (MilVar.conc, ScaleBeg, Termo90) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("+90 ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleBeg_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleBeg, Termo90)
        and set value = 
            if value <> x.TermoVar_temp_ScaleBeg_Termo90 then
                x.SetTermoMilVarStr (MilVar.temp, ScaleBeg, Termo90) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("+90 ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleBeg_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleBeg, Termo90)
        and set value = 
            if value <> x.TermoVar_curr_ScaleBeg_Termo90 then
                x.SetTermoMilVarStr (MilVar.curr, ScaleBeg, Termo90) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("+90 ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleBeg_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleBeg, Termo90)
        and set value = 
            if value <> x.TermoVar_workk_ScaleBeg_Termo90 then
                x.SetTermoMilVarStr (MilVar.workk, ScaleBeg, Termo90) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("+90 ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleBeg_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleBeg, Termo90)
        and set value = 
            if value <> x.TermoVar_refk_ScaleBeg_Termo90 then
                x.SetTermoMilVarStr (MilVar.refk, ScaleBeg, Termo90) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("+90 ⁰C, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleBeg_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleBeg, Termo90)
        and set value = 
            if value <> x.TermoVar_var1_ScaleBeg_Termo90 then
                x.SetTermoMilVarStr (MilVar.var1, ScaleBeg, Termo90) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("+90 ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleMid_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleMid, Termo90)
        and set value = 
            if value <> x.TermoVar_conc_ScaleMid_Termo90 then
                x.SetTermoMilVarStr (MilVar.conc, ScaleMid, Termo90) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("+90 ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleMid_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleMid, Termo90)
        and set value = 
            if value <> x.TermoVar_temp_ScaleMid_Termo90 then
                x.SetTermoMilVarStr (MilVar.temp, ScaleMid, Termo90) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("+90 ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleMid_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleMid, Termo90)
        and set value = 
            if value <> x.TermoVar_curr_ScaleMid_Termo90 then
                x.SetTermoMilVarStr (MilVar.curr, ScaleMid, Termo90) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("+90 ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleMid_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleMid, Termo90)
        and set value = 
            if value <> x.TermoVar_workk_ScaleMid_Termo90 then
                x.SetTermoMilVarStr (MilVar.workk, ScaleMid, Termo90) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("+90 ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleMid_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleMid, Termo90)
        and set value = 
            if value <> x.TermoVar_refk_ScaleMid_Termo90 then
                x.SetTermoMilVarStr (MilVar.refk, ScaleMid, Termo90) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("+90 ⁰C, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleMid_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleMid, Termo90)
        and set value = 
            if value <> x.TermoVar_var1_ScaleMid_Termo90 then
                x.SetTermoMilVarStr (MilVar.var1, ScaleMid, Termo90) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("+90 ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleEnd_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleEnd, Termo90)
        and set value = 
            if value <> x.TermoVar_conc_ScaleEnd_Termo90 then
                x.SetTermoMilVarStr (MilVar.conc, ScaleEnd, Termo90) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("+90 ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleEnd_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleEnd, Termo90)
        and set value = 
            if value <> x.TermoVar_temp_ScaleEnd_Termo90 then
                x.SetTermoMilVarStr (MilVar.temp, ScaleEnd, Termo90) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("+90 ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleEnd_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleEnd, Termo90)
        and set value = 
            if value <> x.TermoVar_curr_ScaleEnd_Termo90 then
                x.SetTermoMilVarStr (MilVar.curr, ScaleEnd, Termo90) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("+90 ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleEnd_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleEnd, Termo90)
        and set value = 
            if value <> x.TermoVar_workk_ScaleEnd_Termo90 then
                x.SetTermoMilVarStr (MilVar.workk, ScaleEnd, Termo90) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("+90 ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleEnd_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleEnd, Termo90)
        and set value = 
            if value <> x.TermoVar_refk_ScaleEnd_Termo90 then
                x.SetTermoMilVarStr (MilVar.refk, ScaleEnd, Termo90) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("+90 ⁰C, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleEnd_Termo90 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleEnd, Termo90)
        and set value = 
            if value <> x.TermoVar_var1_ScaleEnd_Termo90 then
                x.SetTermoMilVarStr (MilVar.var1, ScaleEnd, Termo90) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("Возврат НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleBeg_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleBeg, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_conc_ScaleBeg_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.conc, ScaleBeg, TermoNormRet) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("Возврат НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleBeg_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleBeg, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_temp_ScaleBeg_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.temp, ScaleBeg, TermoNormRet) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("Возврат НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleBeg_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleBeg, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_curr_ScaleBeg_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.curr, ScaleBeg, TermoNormRet) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("Возврат НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleBeg_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleBeg, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_workk_ScaleBeg_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.workk, ScaleBeg, TermoNormRet) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("Возврат НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleBeg_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleBeg, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_refk_ScaleBeg_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.refk, ScaleBeg, TermoNormRet) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("Возврат НКУ, ПГС1")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleBeg_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleBeg, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_var1_ScaleBeg_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.var1, ScaleBeg, TermoNormRet) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("Возврат НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleMid_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleMid, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_conc_ScaleMid_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.conc, ScaleMid, TermoNormRet) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("Возврат НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleMid_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleMid, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_temp_ScaleMid_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.temp, ScaleMid, TermoNormRet) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("Возврат НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleMid_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleMid, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_curr_ScaleMid_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.curr, ScaleMid, TermoNormRet) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("Возврат НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleMid_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleMid, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_workk_ScaleMid_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.workk, ScaleMid, TermoNormRet) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("Возврат НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleMid_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleMid, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_refk_ScaleMid_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.refk, ScaleMid, TermoNormRet) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("Возврат НКУ, ПГС3")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleMid_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleMid, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_var1_ScaleMid_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.var1, ScaleMid, TermoNormRet) value
   
    [<Category("МИЛ: Концентрация")>]
    [<DisplayName("Возврат НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_conc_ScaleEnd_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.conc, ScaleEnd, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_conc_ScaleEnd_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.conc, ScaleEnd, TermoNormRet) value
   
    [<Category("МИЛ: Температура")>]
    [<DisplayName("Возврат НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_temp_ScaleEnd_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.temp, ScaleEnd, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_temp_ScaleEnd_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.temp, ScaleEnd, TermoNormRet) value
   
    [<Category("МИЛ: Ток излучателя")>]
    [<DisplayName("Возврат НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_curr_ScaleEnd_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.curr, ScaleEnd, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_curr_ScaleEnd_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.curr, ScaleEnd, TermoNormRet) value
   
    [<Category("МИЛ: Рабочий канал")>]
    [<DisplayName("Возврат НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_workk_ScaleEnd_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.workk, ScaleEnd, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_workk_ScaleEnd_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.workk, ScaleEnd, TermoNormRet) value
   
    [<Category("МИЛ: Сравнительный канал")>]
    [<DisplayName("Возврат НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_refk_ScaleEnd_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.refk, ScaleEnd, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_refk_ScaleEnd_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.refk, ScaleEnd, TermoNormRet) value
   
    [<Category("МИЛ: Var1")>]
    [<DisplayName("Возврат НКУ, ПГС4")>]
    [<MyBrowseableAttribute>]
    member x.TermoVar_var1_ScaleEnd_TermoNormRet 
        with get () = 
            x.GetTermoMilVarStr (MilVar.var1, ScaleEnd, TermoNormRet)
        and set value = 
            if value <> x.TermoVar_var1_ScaleEnd_TermoNormRet then
                x.SetTermoMilVarStr (MilVar.var1, ScaleEnd, TermoNormRet) value
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("1-ПГС1")>]
    member x.TestConc_Test11 =  
        x.GetTestResultStr (TestConc(Test11))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("1-ПГС1")>]
    member x.TestCurr_Test11 =  
        x.GetTestResultStr (TestCurr(Test11))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("1-ПГС1")>]
    member x.TestPorog1_Test11 =  
        x.GetTestResultStr (TestPorog1(Test11))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("1-ПГС1")>]
    member x.TestPorog2_Test11 =  
        x.GetTestResultStr (TestPorog2(Test11))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("2-ПГС3")>]
    member x.TestConc_Test22 =  
        x.GetTestResultStr (TestConc(Test22))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("2-ПГС3")>]
    member x.TestCurr_Test22 =  
        x.GetTestResultStr (TestCurr(Test22))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("2-ПГС3")>]
    member x.TestPorog1_Test22 =  
        x.GetTestResultStr (TestPorog1(Test22))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("2-ПГС3")>]
    member x.TestPorog2_Test22 =  
        x.GetTestResultStr (TestPorog2(Test22))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("3-ПГС4")>]
    member x.TestConc_Test33 =  
        x.GetTestResultStr (TestConc(Test33))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("3-ПГС4")>]
    member x.TestCurr_Test33 =  
        x.GetTestResultStr (TestCurr(Test33))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("3-ПГС4")>]
    member x.TestPorog1_Test33 =  
        x.GetTestResultStr (TestPorog1(Test33))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("3-ПГС4")>]
    member x.TestPorog2_Test33 =  
        x.GetTestResultStr (TestPorog2(Test33))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("4-ПГС3")>]
    member x.TestConc_Test24 =  
        x.GetTestResultStr (TestConc(Test24))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("4-ПГС3")>]
    member x.TestCurr_Test24 =  
        x.GetTestResultStr (TestCurr(Test24))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("4-ПГС3")>]
    member x.TestPorog1_Test24 =  
        x.GetTestResultStr (TestPorog1(Test24))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("4-ПГС3")>]
    member x.TestPorog2_Test24 =  
        x.GetTestResultStr (TestPorog2(Test24))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("5-ПГС1")>]
    member x.TestConc_Test15 =  
        x.GetTestResultStr (TestConc(Test15))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("5-ПГС1")>]
    member x.TestCurr_Test15 =  
        x.GetTestResultStr (TestCurr(Test15))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("5-ПГС1")>]
    member x.TestPorog1_Test15 =  
        x.GetTestResultStr (TestPorog1(Test15))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("5-ПГС1")>]
    member x.TestPorog2_Test15 =  
        x.GetTestResultStr (TestPorog2(Test15))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("6-ПГС4")>]
    member x.TestConc_Test36 =  
        x.GetTestResultStr (TestConc(Test36))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("6-ПГС4")>]
    member x.TestCurr_Test36 =  
        x.GetTestResultStr (TestCurr(Test36))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("6-ПГС4")>]
    member x.TestPorog1_Test36 =  
        x.GetTestResultStr (TestPorog1(Test36))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("6-ПГС4")>]
    member x.TestPorog2_Test36 =  
        x.GetTestResultStr (TestPorog2(Test36))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("7-ПГС1")>]
    member x.TestConc_Test17 =  
        x.GetTestResultStr (TestConc(Test17))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("7-ПГС1")>]
    member x.TestCurr_Test17 =  
        x.GetTestResultStr (TestCurr(Test17))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("7-ПГС1")>]
    member x.TestPorog1_Test17 =  
        x.GetTestResultStr (TestPorog1(Test17))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("7-ПГС1")>]
    member x.TestPorog2_Test17 =  
        x.GetTestResultStr (TestPorog2(Test17))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка погрешности концентрации после калибровки")>]
    [<DisplayName("Начало шкалы")>]
    member x.TestAdjust_ScaleEdgeBeg =  
        x.GetTestResultStr (TestAdjust(ScaleEdgeBeg))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка погрешности концентрации после калибровки")>]
    [<DisplayName("Конец шкалы")>]
    member x.TestAdjust_ScaleEdgeEnd =  
        x.GetTestResultStr (TestAdjust(ScaleEdgeEnd))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС1, НКУ")>]
    member x.TestTermo_TermoNorm_ScaleBeg =  
        x.GetTestResultStr (TestTermo(TermoNorm,ScaleBeg))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС1, «минус» ⁰C")>]
    member x.TestTermo_TermoLow_ScaleBeg =  
        x.GetTestResultStr (TestTermo(TermoLow,ScaleBeg))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС1, «плюс» ⁰C")>]
    member x.TestTermo_TermoHigh_ScaleBeg =  
        x.GetTestResultStr (TestTermo(TermoHigh,ScaleBeg))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС1, +90 ⁰C")>]
    member x.TestTermo_Termo90_ScaleBeg =  
        x.GetTestResultStr (TestTermo(Termo90,ScaleBeg))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС1, Возврат НКУ")>]
    member x.TestTermo_TermoNormRet_ScaleBeg =  
        x.GetTestResultStr (TestTermo(TermoNormRet,ScaleBeg))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС3, НКУ")>]
    member x.TestTermo_TermoNorm_ScaleMid =  
        x.GetTestResultStr (TestTermo(TermoNorm,ScaleMid))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС3, «минус» ⁰C")>]
    member x.TestTermo_TermoLow_ScaleMid =  
        x.GetTestResultStr (TestTermo(TermoLow,ScaleMid))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС3, «плюс» ⁰C")>]
    member x.TestTermo_TermoHigh_ScaleMid =  
        x.GetTestResultStr (TestTermo(TermoHigh,ScaleMid))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС3, +90 ⁰C")>]
    member x.TestTermo_Termo90_ScaleMid =  
        x.GetTestResultStr (TestTermo(Termo90,ScaleMid))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС3, Возврат НКУ")>]
    member x.TestTermo_TermoNormRet_ScaleMid =  
        x.GetTestResultStr (TestTermo(TermoNormRet,ScaleMid))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС4, НКУ")>]
    member x.TestTermo_TermoNorm_ScaleEnd =  
        x.GetTestResultStr (TestTermo(TermoNorm,ScaleEnd))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС4, «минус» ⁰C")>]
    member x.TestTermo_TermoLow_ScaleEnd =  
        x.GetTestResultStr (TestTermo(TermoLow,ScaleEnd))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС4, «плюс» ⁰C")>]
    member x.TestTermo_TermoHigh_ScaleEnd =  
        x.GetTestResultStr (TestTermo(TermoHigh,ScaleEnd))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС4, +90 ⁰C")>]
    member x.TestTermo_Termo90_ScaleEnd =  
        x.GetTestResultStr (TestTermo(Termo90,ScaleEnd))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка температурной погрешности")>]
    [<DisplayName("ПГС4, Возврат НКУ")>]
    member x.TestTermo_TermoNormRet_ScaleEnd =  
        x.GetTestResultStr (TestTermo(TermoNormRet,ScaleEnd))
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("1-ПГС1: концентрация")>]
    member x.TestConcData_Test11  
        with get() = x.GetDakTestDataStr (DakTestConcData(Test11))
        and set value = 
            if x.TestConcData_Test11 <> value then
                x.SetDakTestDataStr (DakTestConcData(Test11)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("1-ПГС1: ток")>]
    member x.TestCurrData_Test11  
        with get() = x.GetDakTestDataStr (DakTestCurrData(Test11))
        and set value = 
            if x.TestCurrData_Test11 <> value then
                x.SetDakTestDataStr (DakTestCurrData(Test11)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("1-ПГС1: порог 1")>]
    member x.TestPorog1Data_Test11  
        with get() = x.GetDakTestDataStr (DakTestPorog1Data(Test11))
        and set value = 
            if x.TestPorog1Data_Test11 <> value then
                x.SetDakTestDataStr (DakTestPorog1Data(Test11)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("1-ПГС1: порог 2")>]
    member x.TestPorog2Data_Test11  
        with get() = x.GetDakTestDataStr (DakTestPorog2Data(Test11))
        and set value = 
            if x.TestPorog2Data_Test11 <> value then
                x.SetDakTestDataStr (DakTestPorog2Data(Test11)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("2-ПГС3: концентрация")>]
    member x.TestConcData_Test22  
        with get() = x.GetDakTestDataStr (DakTestConcData(Test22))
        and set value = 
            if x.TestConcData_Test22 <> value then
                x.SetDakTestDataStr (DakTestConcData(Test22)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("2-ПГС3: ток")>]
    member x.TestCurrData_Test22  
        with get() = x.GetDakTestDataStr (DakTestCurrData(Test22))
        and set value = 
            if x.TestCurrData_Test22 <> value then
                x.SetDakTestDataStr (DakTestCurrData(Test22)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("2-ПГС3: порог 1")>]
    member x.TestPorog1Data_Test22  
        with get() = x.GetDakTestDataStr (DakTestPorog1Data(Test22))
        and set value = 
            if x.TestPorog1Data_Test22 <> value then
                x.SetDakTestDataStr (DakTestPorog1Data(Test22)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("2-ПГС3: порог 2")>]
    member x.TestPorog2Data_Test22  
        with get() = x.GetDakTestDataStr (DakTestPorog2Data(Test22))
        and set value = 
            if x.TestPorog2Data_Test22 <> value then
                x.SetDakTestDataStr (DakTestPorog2Data(Test22)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("3-ПГС4: концентрация")>]
    member x.TestConcData_Test33  
        with get() = x.GetDakTestDataStr (DakTestConcData(Test33))
        and set value = 
            if x.TestConcData_Test33 <> value then
                x.SetDakTestDataStr (DakTestConcData(Test33)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("3-ПГС4: ток")>]
    member x.TestCurrData_Test33  
        with get() = x.GetDakTestDataStr (DakTestCurrData(Test33))
        and set value = 
            if x.TestCurrData_Test33 <> value then
                x.SetDakTestDataStr (DakTestCurrData(Test33)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("3-ПГС4: порог 1")>]
    member x.TestPorog1Data_Test33  
        with get() = x.GetDakTestDataStr (DakTestPorog1Data(Test33))
        and set value = 
            if x.TestPorog1Data_Test33 <> value then
                x.SetDakTestDataStr (DakTestPorog1Data(Test33)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("3-ПГС4: порог 2")>]
    member x.TestPorog2Data_Test33  
        with get() = x.GetDakTestDataStr (DakTestPorog2Data(Test33))
        and set value = 
            if x.TestPorog2Data_Test33 <> value then
                x.SetDakTestDataStr (DakTestPorog2Data(Test33)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("4-ПГС3: концентрация")>]
    member x.TestConcData_Test24  
        with get() = x.GetDakTestDataStr (DakTestConcData(Test24))
        and set value = 
            if x.TestConcData_Test24 <> value then
                x.SetDakTestDataStr (DakTestConcData(Test24)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("4-ПГС3: ток")>]
    member x.TestCurrData_Test24  
        with get() = x.GetDakTestDataStr (DakTestCurrData(Test24))
        and set value = 
            if x.TestCurrData_Test24 <> value then
                x.SetDakTestDataStr (DakTestCurrData(Test24)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("4-ПГС3: порог 1")>]
    member x.TestPorog1Data_Test24  
        with get() = x.GetDakTestDataStr (DakTestPorog1Data(Test24))
        and set value = 
            if x.TestPorog1Data_Test24 <> value then
                x.SetDakTestDataStr (DakTestPorog1Data(Test24)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("4-ПГС3: порог 2")>]
    member x.TestPorog2Data_Test24  
        with get() = x.GetDakTestDataStr (DakTestPorog2Data(Test24))
        and set value = 
            if x.TestPorog2Data_Test24 <> value then
                x.SetDakTestDataStr (DakTestPorog2Data(Test24)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("5-ПГС1: концентрация")>]
    member x.TestConcData_Test15  
        with get() = x.GetDakTestDataStr (DakTestConcData(Test15))
        and set value = 
            if x.TestConcData_Test15 <> value then
                x.SetDakTestDataStr (DakTestConcData(Test15)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("5-ПГС1: ток")>]
    member x.TestCurrData_Test15  
        with get() = x.GetDakTestDataStr (DakTestCurrData(Test15))
        and set value = 
            if x.TestCurrData_Test15 <> value then
                x.SetDakTestDataStr (DakTestCurrData(Test15)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("5-ПГС1: порог 1")>]
    member x.TestPorog1Data_Test15  
        with get() = x.GetDakTestDataStr (DakTestPorog1Data(Test15))
        and set value = 
            if x.TestPorog1Data_Test15 <> value then
                x.SetDakTestDataStr (DakTestPorog1Data(Test15)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("5-ПГС1: порог 2")>]
    member x.TestPorog2Data_Test15  
        with get() = x.GetDakTestDataStr (DakTestPorog2Data(Test15))
        and set value = 
            if x.TestPorog2Data_Test15 <> value then
                x.SetDakTestDataStr (DakTestPorog2Data(Test15)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("6-ПГС4: концентрация")>]
    member x.TestConcData_Test36  
        with get() = x.GetDakTestDataStr (DakTestConcData(Test36))
        and set value = 
            if x.TestConcData_Test36 <> value then
                x.SetDakTestDataStr (DakTestConcData(Test36)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("6-ПГС4: ток")>]
    member x.TestCurrData_Test36  
        with get() = x.GetDakTestDataStr (DakTestCurrData(Test36))
        and set value = 
            if x.TestCurrData_Test36 <> value then
                x.SetDakTestDataStr (DakTestCurrData(Test36)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("6-ПГС4: порог 1")>]
    member x.TestPorog1Data_Test36  
        with get() = x.GetDakTestDataStr (DakTestPorog1Data(Test36))
        and set value = 
            if x.TestPorog1Data_Test36 <> value then
                x.SetDakTestDataStr (DakTestPorog1Data(Test36)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("6-ПГС4: порог 2")>]
    member x.TestPorog2Data_Test36  
        with get() = x.GetDakTestDataStr (DakTestPorog2Data(Test36))
        and set value = 
            if x.TestPorog2Data_Test36 <> value then
                x.SetDakTestDataStr (DakTestPorog2Data(Test36)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации цифр. канала")>]
    [<DisplayName("7-ПГС1: концентрация")>]
    member x.TestConcData_Test17  
        with get() = x.GetDakTestDataStr (DakTestConcData(Test17))
        and set value = 
            if x.TestConcData_Test17 <> value then
                x.SetDakTestDataStr (DakTestConcData(Test17)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка концентрации ток.вых.")>]
    [<DisplayName("7-ПГС1: ток")>]
    member x.TestCurrData_Test17  
        with get() = x.GetDakTestDataStr (DakTestCurrData(Test17))
        and set value = 
            if x.TestCurrData_Test17 <> value then
                x.SetDakTestDataStr (DakTestCurrData(Test17)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 1")>]
    [<DisplayName("7-ПГС1: порог 1")>]
    member x.TestPorog1Data_Test17  
        with get() = x.GetDakTestDataStr (DakTestPorog1Data(Test17))
        and set value = 
            if x.TestPorog1Data_Test17 <> value then
                x.SetDakTestDataStr (DakTestPorog1Data(Test17)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка срабатывания порога 2")>]
    [<DisplayName("7-ПГС1: порог 2")>]
    member x.TestPorog2Data_Test17  
        with get() = x.GetDakTestDataStr (DakTestPorog2Data(Test17))
        and set value = 
            if x.TestPorog2Data_Test17 <> value then
                x.SetDakTestDataStr (DakTestPorog2Data(Test17)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка погрешности концентрации после калибровки")>]
    [<DisplayName("Начало шкалы: концентрация")>]
    member x.TestAdjustData_ScaleEdgeBeg  
        with get() = x.GetDakTestDataStr (DakAdjustData(ScaleEdgeBeg))
        and set value = 
            if x.TestAdjustData_ScaleEdgeBeg <> value then
                x.SetDakTestDataStr (DakAdjustData(ScaleEdgeBeg)) value 
   
    [<MyBrowseableAttribute>]
    [<Category("Проверка погрешности концентрации после калибровки")>]
    [<DisplayName("Конец шкалы: концентрация")>]
    member x.TestAdjustData_ScaleEdgeEnd  
        with get() = x.GetDakTestDataStr (DakAdjustData(ScaleEdgeEnd))
        and set value = 
            if x.TestAdjustData_ScaleEdgeEnd <> value then
                x.SetDakTestDataStr (DakAdjustData(ScaleEdgeEnd)) value 
///</Generated>
