#load "Dak\Coef.fs"
#load "Dak\ProductType.fs"
#load "Utils\StrUtils.fs"
#load "Utils\FsharpReflectionUtils.fs"
#load "Utils\Utils.fs"
#load "Utils\DateTimeUtils.fs"
#load "Utils\PathUtils.fs"
#load "Utils\Logging.fs"
#load "Dak\Dak.fs" 
#load "Dak\Alchemy.fs"

open System
open Dak 
let src = 
    [|  yield """
type ProductViewModel with"""
        for (v,g,t) as var  in termoVars do
            let propName = termoVarProperty var
            let symb = MilVar.symb v
            let arg = sprintf "(MilVar.%s, %s, %s)" symb g.Property t.Property 
            yield sprintf """   
    [<Category("МИЛ: %s")>]
    [<DisplayName("%s, %s")>]
    [<MyBrowseableAttribute>]
    member x.%s 
        with get () = 
            x.GetTermoMilVarStr %s
        and set value = 
            if value <> x.%s then
                x.SetTermoMilVarStr %s value""" 
                    (MilVar.descr v) 
                    t.What g.What 
                    propName arg propName arg
        for test in Alchemy.Test.values do
            yield sprintf """   
    [<MyBrowseableAttribute>]
    [<Category("%s")>]
    [<DisplayName("%s")>]
    member x.%s =  
        x.GetTestResultStr (%s)""" test.Category test.DisplayName test.Property test.Symbol

        for var in DakTestData.values do
            yield sprintf """   
    [<MyBrowseableAttribute>]
    [<Category("%s")>]
    [<DisplayName("%s")>]
    member x.%s  
        with get() = x.GetDakTestDataStr (%s)
        and set value = 
            if x.%s <> value then
                x.SetDakTestDataStr (%s) value """
                        var.Category var.DisplayName 
                        var.Property var.Symb
                        var.Property var.Symb


        
            
    |]

let filename = IO.Path.Combine(__SOURCE_DIRECTORY__, "ViewModels", "ProductViewModel.fs")
let lines = ResizeArray( IO.File.ReadAllLines filename )
let PosBegin = Seq.findIndex ( (=) "///<Generated>" ) lines
let PosEnd = Seq.findIndex ( (=) "///</Generated>" ) lines
for n = PosBegin  to  PosEnd - 2 do
    lines.RemoveAt (PosBegin + 1)
lines.InsertRange(PosBegin + 1, src)
IO.File.WriteAllLines(filename, lines.ToArray())

Seq.iter (printfn "%s") lines