namespace Dak

open System 

[<AutoOpen>]
module Helpers1 =
    let currentToConc scale current  = (current-4m)/( 16m/scale)
    let concToCurrent scale conc  = (conc * 16m / scale) + 4m

    let newID() = String.getUniqueKey 12

    let newValidModbusAddr addrs = 
        let rec loop n = 
            if addrs |> Seq.exists( (=) n ) then
                if n=127uy then 1uy else loop (n+1uy)
            else n
        loop 1uy


type TemperaturePt =
    | TLow
    | TNorm        
    | THigh
    | T90
    static member what = function
        | TLow -> "«минус» ⁰C"
        | TNorm -> "НКУ"
        | THigh -> "«плюс» ⁰C"
        | T90 -> "+90 ⁰C"    
    member x.DefaultTemperature = 
        match x with
        | TNorm -> 20m
        | TLow -> -60m        
        | THigh -> 80m
        | T90 -> 90m
    static member values = [ 
        TLow
        TNorm
        THigh
        T90 ] 

    member x.What = TemperaturePt.what x

    override x.ToString() = x.What

type TermoPt =
    | TermoNorm
    | TermoLow    
    | TermoHigh
    | Termo90
    | TermoNormRet
    
    static member GetTemperature = function
        | TermoLow -> TLow
        | TermoNorm -> TNorm
        | TermoHigh -> THigh
        | Termo90 -> T90
        | TermoNormRet -> TNorm
    
    member x.Temperature = 
        TermoPt.GetTemperature x

    static member values = [ TermoNorm; TermoLow;  TermoHigh; Termo90; TermoNormRet ] 
    

    member x.What = 
        match  x with 
        | TermoNormRet -> "Возврат НКУ"
        | t -> t.Temperature.What

    member x.Property = 
        FSharpValue.unionCaseName x

type ScalePt = 
    | ScaleBeg
    | ScaleMid
    | ScaleEnd
    member x.What = ScalePt.what x
    
    static member values = FSharpType.unionCasesList<ScalePt>
    static member what = function
        | ScaleBeg -> "ПГС1"
        | ScaleMid -> "ПГС3"
        | ScaleEnd -> "ПГС4"
    static member whatScale = function
        | ScaleBeg -> "начало шкалы"
        | ScaleMid -> "середина шкалы"
        | ScaleEnd -> "конец шкалы"
    
    static member defaultBallonConc scale = function
        | ScaleBeg -> 0m
        | ScaleMid -> scale / 2m
        | ScaleEnd -> scale
    static member name (x:ScalePt) = FSharpValue.unionCaseName x

    member x.ValidPorogs = 
        match x with
        | ScaleBeg -> false, false
        | ScaleMid -> true, false
        | ScaleEnd -> true, true

    member x.Code = 
        match x with
        | ScaleBeg -> 1uy
        | ScaleMid -> 2uy
        | ScaleEnd -> 3uy

    member x.Property = 
        FSharpValue.unionCaseName x

type ScaleEdgePt = 
    | ScaleEdgeBeg
    | ScaleEdgeEnd
    member x.ScalePt = 
        match x with
        | ScaleEdgeBeg -> ScaleBeg
        | ScaleEdgeEnd -> ScaleEnd
    member x.What = 
        match x with
        | ScaleEdgeBeg -> "Начало шкалы"
        | ScaleEdgeEnd -> "Конец шкалы"

    member x.Property = 
        FSharpValue.unionCaseName x


type TestPt =     
    | Test11 
    | Test22 
    | Test33 
    | Test24 
    | Test15 
    | Test36 
    | Test17    
    
    member x.ScalePt = 
        match x with
        | Test11 | Test15 | Test17 -> ScaleBeg
        | Test22 | Test24 -> ScaleMid
        | Test33 | Test36 -> ScaleEnd
    member x.What = 
        sprintf "%d-%s" (FSharpType.unionCasesOrder x + 1) x.ScalePt.What
    member x.Property =  
        FSharpValue.unionCaseName x

    static member values = FSharpType.unionCasesList<TestPt>

module MilVar =
    let conc = 0
    let temp = 2
    let var1 = 16
    let curr = 4
    let workk = 12
    let refk = 14
    

    let private info = 
        [   conc,   ("C", "Концентрация", "MilConc", "conc")
            temp,   ("Т",  "Температура", "MilTemperature", "temp")
            var1,   ("Var1",  "Var1", "MilVar1", "var1")
            curr,   ("I",  "Ток излучателя", "MilCurrent", "curr")
            workk,  ("Work",  "Рабочий канал", "MilWorkK", "workk")
            refk,  ("Ref",  "Сравнительный канал", "MilRefK", "refk")            
        ]

    let private m = Map.ofList info

    let private getv f var = 
        m.TryFind var 
        |> Option.map f
        |> Option.withDefault ""

    let values = List.map (fun (v,_) -> v) info
    let name  = getv (fun (x,_,_,_) -> x)
    let descr = getv (fun (_,x,_,_) -> x)
    let prop  = getv (fun (_,_,x,_) -> x)
    let symb  = getv (fun (_,_,_,x) -> x)

type TermoVar = int * ScalePt * TermoPt


type DakTestData =
    | DakTestConcData of TestPt 
    | DakTestCurrData of TestPt 
    | DakTestPorog1Data of TestPt 
    | DakTestPorog2Data of TestPt 
    | DakAdjustData of ScaleEdgePt
    member x.Property = 
        match x with
        | DakTestConcData a -> sprintf "TestConcData_%s" a.Property
        | DakTestCurrData a -> sprintf "TestCurrData_%s" a.Property
        | DakTestPorog1Data a -> sprintf "TestPorog1Data_%s" a.Property
        | DakTestPorog2Data a -> sprintf "TestPorog2Data_%s" a.Property
        | DakAdjustData a -> sprintf "TestAdjustData_%s" a.Property

    static member values = 
        [   for test in TestPt.values do
                yield DakTestConcData test
                yield DakTestCurrData test
                yield DakTestPorog1Data test
                yield DakTestPorog2Data test
            yield DakAdjustData ScaleEdgeBeg
            yield DakAdjustData ScaleEdgeEnd
        ]

     member x.Category : string = 
        match x with 
        | DakTestConcData _ -> "Проверка концентрации цифр. канала" 
        | DakTestCurrData _ -> "Проверка концентрации ток.вых."
        | DakTestPorog1Data _ -> "Проверка срабатывания порога 1"
        | DakTestPorog2Data _ -> "Проверка срабатывания порога 2"
        | DakAdjustData _ -> "Проверка погрешности концентрации после калибровки" 

    member x.DisplayName : string = 
        match x with 
        | DakTestConcData a -> a.What + ": концентрация"
        | DakTestCurrData a -> a.What  + ": ток"
        | DakTestPorog1Data a -> a.What + ": порог 1"
        | DakTestPorog2Data a -> a.What + ": порог 2"
        | DakAdjustData a -> a.What + ": концентрация"

    member x.Symb : string = 
        match x with 
        | DakTestConcData a -> sprintf "DakTestConcData(%s)" a.Property
        | DakTestCurrData a -> sprintf "DakTestCurrData(%s)" a.Property
        | DakTestPorog1Data a -> sprintf "DakTestPorog1Data(%s)" a.Property
        | DakTestPorog2Data a -> sprintf "DakTestPorog2Data(%s)" a.Property
        | DakAdjustData a -> sprintf "DakAdjustData(%s)" a.Property
        

[<AutoOpen>]
module Helpers2 =
    let termoVars : TermoVar list = listOf{
        let! t = TermoPt.values
        let! g = ScalePt.values 
        let! v = MilVar.values 
        return (v,g,t)
    }

    let termoVarProperty( (v,g,t) :TermoVar)  =
        sprintf "TermoVar_%s_%s_%s" (MilVar.symb v) g.Property t.Property

    let testDataProperty prop ( test : TestPt)  =
        sprintf "Test%sData_%s" prop test.Property

    let testAdjustDataProperty ( scalePt : ScaleEdgePt)  =
        sprintf "TestAdjustData_%s" scalePt.Property


type TestHartResult = 
    {   Date : DateTime
        Result : string option }

type ID = string
type Product = 
    {   ID      : ID
        On      : bool        
        Addr    : byte
        TestConc: Map<TestPt,decimal>
        TestCurr: Map<TestPt,decimal>
        TestPorog1: Map<TestPt,bool>
        TestPorog2: Map<TestPt,bool>
        TestAdjust : Map<ScaleEdgePt,decimal>
        TestHart : TestHartResult option
        Var     : Map<int * ScalePt * TermoPt, decimal>
        Coef    : Map<int, decimal> }

    member x.What = Product.what x
    member x.Serial = 
        x.Coef.TryFind Coef.Serial
        |> Option.withDefault 0m
    static member GetID x = x.ID
    static member GetSerial (x : Product) = x.Serial
    static member what x = sprintf "№%M.#%d" x.Serial x.Addr 
    static member GetVar k p =p.Var.TryFind k 
    static member GetCoef k p = p.Coef.TryFind k 
    static member NewEmpty = 
        {   ID      = newID()
            On      = true
            Addr    = 0uy
            TestConc = Map.empty
            TestCurr = Map.empty
            TestPorog1 = Map.empty
            TestPorog2 = Map.empty
            Var     = Map.empty
            Coef    = Map.empty 
            TestHart = None
            TestAdjust = Map.empty  }
type LogLines = DateTime * Logging.Level * string

type PerformingOperation =
    {   RunStart : DateTime option 
        RunEnd : DateTime option
        LogLines : LogLines list }
    static member createNew() = 
        {   RunStart = None
            RunEnd = None
            LogLines = [] }

type PerformingJournal = Map<int, PerformingOperation >


type ProductInfo = 
    {   Product : Product
        GetPgs : ScalePt -> decimal
        ProductType : ProductType }

    static member New p f t =
        {   Product = p
            GetPgs  = f
            ProductType = t }

type PartyInfo = 
    {   ID : ID
        Date : DateTime
        Name : string
        Serials : decimal list   
        ProductType  : ProductType }
    static member GetID x = x.ID 
    static member NewEmpty = 
        {   ID = newID()
            Serials = []
            Date=DateTime.Now 
            Name = ""
            ProductType = ProductType.values.[0] }
    member x.WithProducts products =
        { x with Serials = List.map Product.GetSerial products }

type PartyData = 
    {   Products : Product list
        BallonConc : Map<ScalePt,decimal>
        TermoTemperature : Map<TemperaturePt,decimal>
        PerformingJournal : PerformingJournal }
    static member NewEmpty = 
        {   Products = []
            BallonConc = Map.empty
            TermoTemperature = Map.empty
            PerformingJournal = Map.empty}

type Party =
    {   PartyInfo : PartyInfo
        PartyData : PartyData
    }

    member x.GetPgs gas =     
        match Map.tryFind gas x.PartyData.BallonConc with
        | Some x -> x
        | _ -> ScalePt.defaultBallonConc x.PartyInfo.ProductType.Scale.Value gas

    member x.ProductInfo product = 
        ProductInfo.New product x.GetPgs x.PartyInfo.ProductType

    member x.GetTemperature t =
        x.PartyData.TermoTemperature
            |> Map.tryFind t
            |> Option.withDefault t.DefaultTemperature

    

    static member NewEmpty =
        {   PartyInfo = PartyInfo.NewEmpty
            PartyData = PartyData.NewEmpty }

    static member NewWith partyInfo partyData = 
        {   PartyInfo = partyInfo
            PartyData = partyData }

    member x.WithProducts products = 
        Party.NewWith 
            (x.PartyInfo.WithProducts products)
            { x.PartyData with Products = products }

module Cmd = 
    let adj0                = 1
    let adjE                = 2

    let setPorog1           = 3
    let setPorog2           = 4
    let setGas              = 0x10
    let testGuardTimer      = 0x51
    let setCurrent          = 0x53
    let resetAlert          = 0x20
    let testWatchdog        = 96
    let setAddy             = 7
    let hart                = 0x80

    let private info = 
        [   adj0,           "корректировка нулевых показаний"
            adjE,           "корректировка чувствительности"
            setPorog1,      "установка значения ПОРОГ1"
            setPorog2,      "установка значения ПОРОГ2"
            setGas,         "выбор определяемого компонента"
            testGuardTimer, "проверка сторожевого таймера"
            setCurrent,     "калибровка тока"
            resetAlert,     "сброс сигнализации"
            testWatchdog,   "проверка watchdog"
            setAddy,        "установка адреса modbus"
            hart,           "Выбор HART протокола"
        ]

    let what = 
        let whatcoef cmd = 
            Coef.values 
            |> List.tryFind(fun kef-> Coef.cmd kef = cmd )
            |> Option.map(fun kef -> 
                sprintf "запись к-та %d" kef )                
            |> Option.withDefault( sprintf "%d" cmd )

        let m = Map.ofList info
        fun cmd ->
            m.TryFind cmd
            |> Option.withDefault (whatcoef cmd)
            
                
            

    let values = List.map fst info

type DelayContext = 
    | BlowDelay 
    | Blow1Delay 
    | Blow2Delay 
    | WarmDelay 
    | TexprogonDelay
    | AdjustDelay 

    member x.DefaultValue = 
        DelayContext.GetDefaultValue x

    static member values = 
        [   BlowDelay  
            Blow1Delay
            Blow2Delay
            WarmDelay 
            TexprogonDelay
            AdjustDelay  ]

    static member GetDefaultValue = function
        | BlowDelay ->   TimeSpan.FromMinutes 3.
        | Blow1Delay ->   TimeSpan.FromMinutes 1.
        | Blow2Delay -> TimeSpan.FromMinutes 2.
        | WarmDelay  ->    TimeSpan.FromHours 1.
        | TexprogonDelay -> TimeSpan.FromHours 1.
        | AdjustDelay -> TimeSpan.FromMinutes 3.

    static member what = function
        | BlowDelay -> "Продувка"
        | Blow1Delay -> "Продувка 1"
        | Blow2Delay -> "Продувка 2"
        | WarmDelay -> "Прогрев"
        | TexprogonDelay -> "Выдержка, техпрогон"
        | AdjustDelay -> "Продувка на калибровку"

    member x.What = DelayContext.what x
    