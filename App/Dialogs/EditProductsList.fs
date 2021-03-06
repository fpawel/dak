﻿module Dak.Dialogs.EditProductsList

open System
open System.Windows.Forms
open System.ComponentModel

//open MyWinForms.TopDialog
open Dak.MainWindow
open Dak
open Dak.View

[<AutoOpen>]
module private Helpers =
    type P = Dak.ViewModel.ProductViewModel
    let party = AppData.party
    let popupDialog = MyWinForms.PopupDialog.create
    type Dlg = MyWinForms.PopupDialog.Options
    let none _ = None
    let form = MainWindow.form


    let getSelectedProducts() = 
        seq{ for x in gridProducts.SelectedCells -> x.RowIndex, x.OwningRow }
        |> Map.ofSeq
        |> Map.toList
        |> List.map( fun (_,x) -> x.DataBoundItem :?> P )


type ProductTypesConverter() = 
    inherit StringConverter()
    override __.GetStandardValuesSupported _ = true
    override __.GetStandardValuesExclusive _ = true
    override __.GetStandardValues _ =       
        ProductType.values
        |> Seq.toArray
        |> Array.map( fun x -> x.What)
        |> TypeConverter.StandardValuesCollection

    
[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type PartyInfo = 
    {   [<DisplayName("Наименование")>]    
        [<Description("Наименование партии")>]
        mutable Name : string

        [<DisplayName("Исполнение")>]    
        [<Description("Исполнение приборов партии")>]
        [<TypeConverter (typeof<ProductTypesConverter>) >]
        mutable ProductType : string

        [<DisplayName("ПГС1")>]    
        [<Description("Концентрация ПГС1, начало шкалы")>]
        mutable Pgs1  : decimal
        [<DisplayName("ПГС2")>]    
        [<Description("Концентрация ПГС3, середина шкалы")>]
        mutable Pgs2  : decimal
        [<DisplayName("ПГС2")>]    
        [<Description("Концентрация ПГС4, конец шкалы")>]
        mutable Pgs3  : decimal 

        [<DisplayName("Количество приборов")>]    
        [<Description("Количество приборов в партии")>]
        mutable Count  : byte  }

let addProducts (b:Button) = 
    let tb = new TextBox(Width = 200, Dock = DockStyle.Left, Text = "1")
    let getValue() = 
        let b,v = Int32.TryParse tb.Text
        if b  && v > 0 && v<21 then
            Some v
        else
            None
    tb.MouseWheel.Add <| fun e ->
        let b,v = Int32.TryParse tb.Text
        if b then
            let d = if e.Delta>0 then 1 else (-1)
            if v<>0 || d>0 then
                tb.Text <- sprintf "%d" <| v + d  
    let errorProvider = 
        new ErrorProvider( BlinkStyle = ErrorBlinkStyle.NeverBlink )
    errorProvider.SetIconAlignment (tb, ErrorIconAlignment.MiddleRight)
    errorProvider.SetIconPadding (tb, 2)
    tb.Validating.Add <| fun e ->
        match getValue() with
        | Some _ -> errorProvider.Clear()                
        | None ->
            errorProvider.SetError(tb, "Введите число от 1 до 20" )
            e.Cancel <- true

    let dialog,validate  =             
        popupDialog
            { Dlg.def() with 
                Dlg.Text = Some "Введите количество добавляемых в партию приборов от 1 до 20" 
                Dlg.ButtonAcceptText = "Добавить" 
                Dlg.Title = "Добавить приборы"
                Width = 300
                Dlg.Content = 
                    let p = new Panel(Height = tb.Height + 10)
                    p.Controls.Add(tb)
                    p }
            getValue 
            ( fun value -> 
                iterate value party.CreateNewProduct )
    tb.TextChanged.Add <| fun _ -> validate()
    dialog.Show(b)

let deleteProducts (b:Button) =
    let delete() =  
        getSelectedProducts()
        |> List.iter (fun p -> 
            party.DeleteProduct p
            Chart.removeProductSeries p.Product.ID |> ignore )
    let dialog, _  =            
        popupDialog
            { Dlg.def() with 
                Dlg.Text = 
                    getSelectedProducts() 
                    |> Seq.toStr ", " (fun x -> x.What)
                    |> sprintf "Подтвердите необходимость удаления приборов %s" 
                    |> Some
                Dlg.ButtonAcceptText = "Удалить" 
                Dlg.Title = "Удалить приборы" }
            ( fun () -> Some () )
            delete
    dialog.Show(b)

open Alchemy
let createNewParty (b:Button) = 
    let d = 
        {   Name = "-"
            ProductType = ProductType.values.[0].What
            Pgs1 = 0m
            Pgs2 = 49m
            Pgs3 = 98m 
            Count = 1uy}
    let g = new PropertyGrid(SelectedObject = d, 
                                ToolbarVisible = false, Height = 250,
                                PropertySort = PropertySort.Alphabetical)
    let popup1,_ = 
        popupDialog
            { Dlg.def() with 
                Text = None 
                Content = g
                ButtonAcceptText = "Создать новую партию"
                Title = "Создать новую партию"
                Width = 400 }
            ( fun () -> Some () )
            ( fun () ->
                let prodType = 
                    ProductType.values 
                    |> List.tryFind ( fun x -> x.What = d.ProductType)
                    |> Option.withDefault ProductType.values.[0]
                let b = Party.New d.Name prodType d.Pgs1 d.Pgs2 d.Pgs3 d.Count
                party.Party <- b
                AppData.save()
                if Tabsheet.GetSelected () = TabsheetChart then
                    AppData.updateChartSeriesList ()
                    )
    popup1.Closing.Add <| fun e ->
        if MyWinForms.Utils.isPropGridEditing g then
            e.Cancel <- true
    popup1.Show(b)