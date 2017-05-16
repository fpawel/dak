module Dak.Pasport

open System
open Tree
open Html
open Alchemy

[<AutoOpen>]
module private Helpers = 
    
    let css = 
        IO.File.ReadAllText("content/report.css")

    let classValue = class' "value"
    let classParamName = class' "param-name"

    let td_optstr = 
        Option.map (sprintf "%M")
        >> Option.withDefault ""
        >> fun x -> td [%% x; classValue]
        
    let td_empty =
        td [%% "&nbsp;"]

    let valueError = function
        | None -> td_empty
        | Some (v : TestResult) ->
            td[ %% string v
                class' ( if v.IsFailed then "invalid-error-value" else "valid-error-value" ) ]

    type Dak.ProductInfo with

        member x.ValueErrorElement = 
            x.TestResult >> valueError 
        
        member x.TestResultsTable =
            let tablehead = 
                [   yield %% "Параметр" 
                    for test in TestPt.values do 
                        yield %% test.ScalePt.What             
                    yield %% "Макс. погр." 
                    yield %% "Вариация"
                    yield %% "Кор.0"
                    yield %% "Кор.K" ]
                |> List.map (fun x -> td [x])
                |> thead 

            let (~&&) = x.TestResult >> valueError 

            let tablebody =
                [   yield [ yield td [%% "Концентрация"  ]
                            for test in TestPt.values do 
                                yield TestConc test |> x.ValueErrorElement
                            yield valueError x.MaxError 
                            yield valueError x.Variation   
                            yield TestAdjust ScaleEdgeBeg |> x.ValueErrorElement
                            yield TestAdjust ScaleEdgeEnd |> x.ValueErrorElement  ] |> tr 

                    yield [ yield td [%% "Ток"  ]
                            for test in TestPt.values do 
                                yield TestCurr test |> x.ValueErrorElement 
                            yield! List.repeat 4 td_empty ] |> tr 

                    
                    if x.ProductType.HasRele then
                        yield [ yield td [%% "Порог 1"  ]
                                for test in TestPt.values do 
                                    yield TestPorog1 test |> x.ValueErrorElement 
                                yield! List.repeat 4 td_empty ] |> tr 

                        yield [ yield td [%% "Порог 2"  ]
                                for test in TestPt.values do 
                                    yield TestPorog2 test |> x.ValueErrorElement  
                                yield! List.repeat 4 td_empty ] |> tr 
                ] |> tbody

            table [
                class' "table-border-black-1"
                tablehead
                tablebody
            ]
        
        member x.TitleTable partyDate  = 
            let value a = 
                td [%% a; classValue]
            let param a = 
                td [%% a; classParamName]
            let xpgs pt =
                x.GetPgs pt |> sprintf "%M" |> value
                            
            table[ 
                width "100%"
                tbody[
                    tr[ param "Дата изготовления"
                        value ( DateTime.format "dd MMMM yyyy года" partyDate)
                        
                        param "ПГС1"
                        xpgs ScaleBeg 
                        
                        param "Газ"
                        value x.ProductType.Gas.What

                        param "Сетевой адрес ДАК"
                        value <| string x.Product.Addr
                    ]

                    tr[ param "Серийный номер"
                        value <| string x.Product.Serial

                        param "ПГС3"
                        xpgs ScaleMid  

                        param "Шкала"
                        value <| sprintf "%M-%M %s" x.ProductType.Scale.Null x.ProductType.Scale.Value x.ProductType.Gas.Units

                        param "Сетевой адрес МИЛ-82"
                        td_optstr <| x.Product.Coef.TryFind Coef.AddrMil  
                    ] 

                    tr[ param "Исполнение"
                        value <| sprintf "ИБЯЛ.418414.071-%d" x.ProductType.Code

                        param "ПГС4"
                        xpgs ScaleEnd 

                        param "Температурный диапазон"
                        value <| sprintf "%M...%M ⁰C" x.ProductType.TermoMin x.ProductType.TermoMax
                    ]
                ]
            ]                
       
        member x.SummaryText = 
            let wht = sprintf "соответствует требованиям ИБЯЛ.418414.071-%d" x.ProductType.Code
            let summaryText, summaryClass = 
                match x.Summary.AllDone, x.Summary.HasFailed with
                | true, false ->
                    wht, "valid-error-value" 
                | _, true ->
                    "не " + wht, "invalid-error-value"
                | false,_ -> 
                    "не все проверки были выполнены", "invalid-error-value"  
            p [ span [
                    class' "result" 
                    %% "Результат:" 
                ]
                span[
                    %% summaryText
                    class' summaryClass 
                ]                
            ]

        member x.MilTable = 
           
            let tempPts = [
                yield TermoNorm
                yield TermoLow
                yield TermoHigh
                if x.ProductType.Code = 37 then
                    yield Termo90
                yield TermoNormRet
            ]
            let lineTemperatures = 
                [ yield th [ rowspan 2; %% "Газ" ]
                  for t in tempPts do 
                    yield th [
                        %% t.What    
                        class' "cell-right-border"
                        colspan MilVar.values.Length
                    ]                    
                ] |>  tr
            let lineVars = 
                [ for n = 0 to tempPts.Length - 1 do
                    for var in MilVar.values do
                        yield th [ 
                            yield %% MilVar.name var 
                            if var = MilVar.var1 then
                                yield class' "cell-right-border"
                        ]
                    
                ] |>  tr
            let (~&&) = x.TestResult >> valueError 
            let linesGases = 
                [ for gas in ScalePt.values -> 
                    [   yield th [%% gas.What]   
                        for t in tempPts do 
                            yield x.ValueErrorElement (TestTermo (t,gas))
                            for var in MilVar.values.Tail do
                                let text = 
                                    x.Product.Var.TryFind(var,gas,t)
                                    |> Option.map (sprintf "%M")
                                    |> Option.withDefault ""
                                yield 
                                    td [
                                        yield %% text
                                        if var = MilVar.var1 then
                                            yield class' "cell-right-border"
                                    ]
                        
                    ] |> tr                   
                ]
            table [
                class' "table-border-black-1"                
                thead[
                    lineTemperatures
                    lineVars
                ]  
                tbody linesGases            
            ]
            
        member x.TestHartReport = 
            if not x.ProductType.HasHart then [] else
            let res ok x = 
                span[
                    %% x
                    class' <| if ok then "valid-error-value" else "invalid-error-value"
                ]
            [   yield h2 [
                    %% "Проверка HART протокола"
                ]
                match x.Product.TestHart with 
                | None -> 
                    yield res false "не выполнялась"
                | Some { Date = date; Result = r } -> 
                    yield %% DateTime.format "dd MMMM yyyy года" date
                    yield  
                        match r with 
                        | None -> res true "выполнена успешно"
                        | Some error -> res false error
            ]
        

    type Party with
        member party.ProductPasport product =     
            let i = party.ProductInfo product  
            let eSection = 
                [   h1[ id' product.ID
                        class' "product-title"            
                        %% "Паспорт ДАК-М"]    
                    i.TitleTable party.PartyInfo.Date
                    h2 [
                        %% "Основная погрешность и вариация показаний"
                    ]
                    i.TestResultsTable
                    i.SummaryText 
                    h2 [
                        %% "Техпрогон в диапазоне рабочих температур"
                    ]
                    i.MilTable] @ i.TestHartReport
                |> section

            let eNam =             
                [   "href" <! ("#" + product.ID); 
                    %% sprintf "№%M-#%d" product.Serial product.Addr]               
                |> tag "a" 
            eNam, eSection

let createNew party =
    let navs, xs = 
        party.PartyData.Products 
        |> List.map party.ProductPasport 
        |> List.unzip
    [   div[
            yield id' "left"
            yield! navs ]
        div[
            yield id' "right"
            yield! xs ] ] 
    |> html5 css "Индивидуальные паспорта ДАК-М"
    |> Seq.toStr "\n" stringify