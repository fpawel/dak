namespace Dak

type Gas =
    | ``CO₂`` 
    | ``CH₄`` 
    | ``C₃H₈`` 
    | ``C₆H₁₄`` 
    | ``CH∑``

    member x.What =         
        match x with
        | ``CO₂`` -> "CO₂"
        | ``CH₄`` -> "CH₄"
        | ``C₃H₈`` -> "C₃H₈"
        | ``C₆H₁₄`` -> "C₆H₁₄"
        | ``CH∑`` -> "∑CH"
    member x.IsCH = Gas.isCH x
    member x.Code = 
        match x with
        | ``CO₂`` -> 4m
        | ``CH₄`` -> 5m
        | ``C₃H₈`` -> 7m
        | ``C₆H₁₄`` -> 7m
        | ``CH∑`` -> 7m

    member x.UnitsCode = 
        match x with
        | ``CO₂`` -> 7m
        | _ -> 14m

    member x.Units = 
        match x with
        | ``CO₂`` -> "об.д.%"
        | _ -> "%НКПР"

    member x.ComponentCode = 
        match x with
        | ``CO₂`` -> 0m
        | ``CH₄`` -> 1m
        | ``C₃H₈`` -> 15m
        | ``C₆H₁₄`` -> 15m
        | ``CH∑`` -> 15m

    static member what (x:Gas) = x.What

    static member isCH = function  
        | ``CO₂`` -> false 
        | _ -> true 

    

type Scale =     
    
    | Scale4 | Scale10 | Scale20 | Scale50 | Scale100 
    
    member x.Value = 
        match x with
        | Scale4   -> 4m
        | Scale10  -> 10m
        | Scale20  -> 20m 
        | Scale50  -> 50m 
        | Scale100 -> 100m 

    member x.Code = 
        match x with
        | Scale4   -> 57m
        | Scale10  -> 7m
        | Scale20  -> 9m 
        | Scale50  -> 0m 
        | Scale100 -> 21m 

    member x.Null = 
        match x with
        | Scale50  -> 5m 
        | _ -> 0m 

    member x.Porogs = 
        match x with
        | Scale4  -> 0.5m, 1m
        | Scale10 -> 1.25m, 2.5m
        | Scale20 -> 2.5m, 5m
        | _  -> 7m, 12m 
         

    
type ProductType = 
    {   Code        : int
        Scale       : Scale
        Gas         : Gas
        HasHart     : bool
        HasRele     : bool
        TermoMin    : decimal
        TermoMax    : decimal        
    }
    member x.What =
        sprintf "%d %s %M..%M %s" x.Code x.Gas.What x.Scale.Null x.Scale.Value x.Gas.Units
    static member GetWhat (x : ProductType) = 
        x.What

[<AutoOpen>]
module private ProductTypeHelp =
    type HasHart =
        | Hart 
        | NoHart
        member x.ToBool = x = Hart

    type HasRele =
        | Rele
        | NoRele
        member x.ToBool = x = Rele

    let ``new`` code scale gas tmin tmax (hart : HasHart) (rele:HasRele) =  
        {   Code        = code
            Scale       = scale
            Gas         = gas
            HasHart     = hart.ToBool
            HasRele     = rele.ToBool
            TermoMin    = tmin
            TermoMax    = tmax
        }

    let co code hart rele = 
        [   for sc in [Scale4; Scale10; Scale20] ->
                ``new`` code sc ``CO₂`` -40m 80m hart rele
        ]

    let ch code gas tmin tmax hart rele = 
        ``new`` code Scale100 gas tmin tmax hart rele

    let values = 
        [   yield! co 26 NoHart Rele
            yield  ch 27 ``CH₄`` -40m 80m NoHart Rele
            yield  ch 29 ``CH₄`` -60m 60m Hart Rele
            yield  ch 30 ``CH∑`` -60m 60m NoHart Rele
            yield! co 31 NoHart NoRele
            yield  ch 32 ``CH₄`` -40m 80m NoHart NoRele
            yield  ch 33 ``CH∑`` -40m 60m NoHart NoRele
            yield ``new`` 34 Scale50 ``C₆H₁₄`` 15m 80m Hart Rele
            yield! co 35 Hart Rele
            yield  ch 36 ``CH₄`` -40m 80m Hart Rele
            yield  ch 37 ``CH₄`` -60m 80m Hart Rele
            yield  ch 38 ``CH∑`` -60m 80m Hart Rele
        ]

type ProductType with 

    static member values = values

    static member index x = 
        List.findIndex ((=) x) values