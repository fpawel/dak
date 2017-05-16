module Dak.Alchemy  

open System
open System.ComponentModel

type private P = Product
type private T = ProductType



let encodeDate (date : DateTime) = 
    let year = decimal date.Year - 2000m
    let month = decimal date.Month 
    let day = decimal date.Day
    year * 10000m + month * 100m + day

let initKefsValues serial pgs (t : ProductType)= 
    let now = DateTime.Now
    [   Coef.SoftVer, 12m
        Coef.ProductType, 11m
        Coef.Serial, serial
        Coef.Year, decimal now.Year
        Coef.Gas, t.Gas.Code
        Coef.Units, t.Gas.UnitsCode
        Coef.Scale, t.Scale.Code
        Coef.Scale0, t.Scale.Null
        Coef.ScaleEnd, t.Scale.Value
        Coef.Range, t.Scale.Value - t.Scale.Null
        Coef.ProductTypeCode, decimal t.Code
        Coef.PGS1, pgs ScaleBeg
        Coef.PGS3, pgs ScaleEnd  ]

    
let concErrorlimit (t:ProductType) concValue =        
    if t.Gas.IsCH then 2.5m+0.05m * concValue else
        match t.Scale with
        | Scale4 -> 0.2m + 0.05m * concValue
        | Scale10 -> 0.5m
        | _ -> 1.0m
        

let termoErrorlimit (prodType:ProductType) pgs (gas,termoPt) (product:Product) =
    let getvar var t = 
        product.Var.TryFind (var, gas, t)
    if not prodType.Gas.IsCH then 
        let t0 = 
            match termoPt with
            | Termo90 -> 80m
            | _ -> 20m
        (getvar MilVar.conc termoPt, getvar MilVar.temp termoPt) 
        |> Option.map2(fun(c,t) ->             
            let dt = t - t0     
            let maxc = concErrorlimit prodType pgs
            0.5m * abs( maxc*dt ) / 10.0m )
    else
        match gas with
        | ScaleBeg -> Some 5m
        | _ ->
            getvar MilVar.conc TermoNorm
            |> Option.map(fun conc20 -> conc20 * 0.15m |> abs  |> decimal )
 
let formatRele = function
    | true -> "ВКЛ"
    | false -> "ВЫКЛ"

let errorPercent (value,nominal,limit) = 
    if limit = 0m then None else
    Some( 100M * (value - nominal) / limit)

let formatErrorPercent = 
    Option.map ( fun v -> sprintf "%2.0f%s" v "%")
    >> Option.withDefault ""

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type TestResult = 
    | TestPorog of 
        value : bool *
        valid : bool 
    | TestValueError of 
        value : decimal * 
        nominal : decimal * 
        limit : decimal

    static member IsTestFailed (x:TestResult) =  
        x.IsFailed
    
    [<DisplayName ("Результат")>]
    member x.IsFailed =
        match x with
        | TestPorog (a,b) -> a <> b
        | TestValueError(value = v; nominal = n; limit = l ) ->
            abs (v - n) > l

    override x.ToString() =
        match x with
        | TestPorog ( a, _) -> 
            formatRele a
        
        | TestValueError(value = value; nominal = nominal; limit = limit ) ->  
            errorPercent (value,nominal,limit)
            |> formatErrorPercent
            |> sprintf "%M, %s" value 
       
type Test = 
    | TestConc of TestPt
    | TestCurr of TestPt
    | TestPorog1 of TestPt
    | TestPorog2 of TestPt
    | TestTermo of TermoPt * ScalePt
    | TestAdjust of ScaleEdgePt

    member x.DisplayName = 
        match x with 
        | TestConc a -> a.What 
        | TestCurr a -> a.What 
        | TestPorog1 a -> a.What 
        | TestPorog2 a -> a.What 
        | TestTermo (t,gas) -> sprintf  "%s, %s" gas.What t.What 
        | TestAdjust a -> a.What

    member x.What = 
        let s = x.Category
        (s.Substring(0,1).ToLower() + s.Substring(1))
        |> sprintf "%s: %s" x.DisplayName 

    member x.Category : string = 
        match x with 
        | TestConc a -> "Проверка концентрации цифр. канала" 
        | TestCurr a -> "Проверка концентрации ток.вых."
        | TestPorog1 a -> "Проверка срабатывания порога 1"
        | TestPorog2 a -> "Проверка срабатывания порога 2"
        | TestTermo (t,gas) -> "Проверка температурной погрешности" 
        | TestAdjust a -> "Проверка погрешности концентрации после калибровки" 

    member x.Property = 
        match x with 
        | TestConc a -> sprintf  "TestConc_%s" a.Property
        | TestCurr a -> sprintf  "TestCurr_%s" a.Property
        | TestPorog1 a -> sprintf  "TestPorog1_%s" a.Property
        | TestPorog2 a -> sprintf  "TestPorog2_%s" a.Property
        | TestTermo (t,gas) -> sprintf  "TestTermo_%s_%s" t.Property gas.Property 
        | TestAdjust a -> sprintf  "TestAdjust_%s" a.Property

    member x.Symbol = 
        match x with 
        | TestConc a -> sprintf  "TestConc(%s)" a.Property
        | TestCurr a -> sprintf  "TestCurr(%s)" a.Property
        | TestPorog1 a -> sprintf  "TestPorog1(%s)" a.Property
        | TestPorog2 a -> sprintf  "TestPorog2(%s)" a.Property
        | TestTermo (t,gas) -> sprintf  "TestTermo(%s,%s)" t.Property gas.Property 
        | TestAdjust a -> sprintf  "TestAdjust(%s)" a.Property

    static member values = 
        [   for testPt in TestPt.values do
                yield TestConc testPt
                yield TestCurr testPt
                yield TestPorog1 testPt
                yield TestPorog2 testPt
            yield TestAdjust ScaleEdgeBeg
            yield TestAdjust ScaleEdgeEnd
                
            for scalePt in ScalePt.values do
                for termoPt in TermoPt.values do
                    yield TestTermo(termoPt,scalePt)            
        ]

type Summary = 
    {   AllDone : bool
        HasFailed : bool
    } 
       
type Dak.ProductInfo with     

    member x.TestConcResult testPt =
        x.Product.TestConc.TryFind testPt
        |> Option.map(fun testResult ->   
            let pgs = x.GetPgs testPt.ScalePt
            (testResult, pgs,  concErrorlimit x.ProductType pgs )) 

    member x.TestAdjustResult scaleEdgePt =         
        x.Product.TestAdjust.TryFind scaleEdgePt
        |> Option.map(fun testResult ->   
            let pgs = x.GetPgs scaleEdgePt.ScalePt
            (testResult, pgs,  concErrorlimit x.ProductType pgs ))    

    member x.TestCurrentResult testPt =
        x.Product.TestCurr.TryFind testPt
        |> Option.map(fun testResult ->   
            let pgs = x.GetPgs testPt.ScalePt 
            let scale = x.ProductType.Scale.Value            
            (   testResult,
                concToCurrent scale <| concErrorlimit x.ProductType pgs ,
                concToCurrent scale pgs ) ) 

    member x.TermoError (t,gas) = 
        let pgs = x.GetPgs gas
        if t=TermoNorm || t=TermoNormRet then 
            x.Product.Var.TryFind (MilVar.conc,gas,t) 
            |> Option.map (fun c ->
                TestValueError
                    (   value = c,
                        nominal = pgs,
                        limit = concErrorlimit x.ProductType pgs ))
        else
            let termo0pt = 
                match t with
                | Termo90 -> TermoHigh
                | _ -> TermoNorm 
            (   x.Product.Var.TryFind (MilVar.conc,gas,t),
                x.Product.Var.TryFind (MilVar.conc,gas,termo0pt) ,
                termoErrorlimit x.ProductType pgs (gas,t) x.Product )
            |> Option.map3( fun (c,c0,limit) -> 
                TestValueError
                    (   value = c,
                        nominal = c0,
                        limit = limit ) )   

    member x.TestPorog1Result testPt = 
        x.Product.TestPorog1.TryFind testPt
        |> Option.map(fun testResult ->
            TestPorog
                (   value = testResult,
                    valid = fst testPt.ScalePt.ValidPorogs
                ) ) 

    member x.TestPorog2Result testPt = 
        x.Product.TestPorog2.TryFind testPt
        |> Option.map(fun testResult ->
            TestPorog
                (   value = testResult,
                    valid = snd testPt.ScalePt.ValidPorogs
                ) ) 
    member x.TestResult test = 
        
        match test with
        | TestConc testPt -> 
            x.TestConcResult testPt 
            |> Option.map TestValueError

        | TestAdjust pt ->
            x.TestAdjustResult pt
            |> Option.map TestValueError

        | TestCurr testPt -> 
            x.TestCurrentResult testPt
            |> Option.map TestValueError

        | TestPorog1 testPt ->
            x.TestPorog1Result testPt
        
        | TestPorog2 testPt ->
            x.TestPorog2Result testPt
            
        | TestTermo (a,b) ->
            x.TermoError (a,b)

    member x.MaxError =
        [  for test in TestPt.values -> x.TestConcResult test ] 
        |> List.choose id    
        |> function 
            | [] -> None
            | xs -> 
                List.maxBy errorPercent xs
                |> TestValueError
                |> Some 

    member x.Variation = 
        match (x.Product.TestConc.TryFind Test22, x.Product.TestConc.TryFind Test24) with
        | Some a, Some b -> 
            Some <| TestValueError ( a - b, 0m, (concErrorlimit x.ProductType <| x.GetPgs ScaleMid) / 2m)
        | _ -> None

    member x.Summary =
        let results1 =            
            [   for test in TestPt.values do 
                    yield TestConc(test)
                    yield TestCurr(test)
                    if x.ProductType.HasRele then
                        yield TestPorog1 test
                        yield TestPorog2 test ]
            |> List.map x.TestResult
        let results = 
            [   yield x.Variation ] @ results1

        {   AllDone = List.forall Option.isSome results
            HasFailed = 
                results
                |> List.choose id
                |> List.exists TestResult.IsTestFailed 
        } 
        
type Product with
    static member New getPgs productType addr = 
        {   Product.NewEmpty with 
                Addr = addr
                Coef = Map.ofList <| initKefsValues 0m getPgs productType }
type Party with
    static member NewWithOneProduct = 
        let party = Party.NewEmpty
        let product = Product.New party.GetPgs party.PartyInfo.ProductType 0uy 
        party.WithProducts [ product ]
        
    

    static member New name productType pgs1 pgs2 pgs3 count =         
        let party = 
            Party.NewWith
                { PartyInfo.NewEmpty with 
                    Name = name
                    ProductType = productType }
                { PartyData.NewEmpty with 
                    BallonConc = Map.ofList [ScaleBeg,pgs1; ScaleMid,pgs2; ScaleEnd,pgs3] }
        [1uy..count] 
        |> List.map( Product.New party.GetPgs party.PartyInfo.ProductType )        
        |> party.WithProducts 
        

