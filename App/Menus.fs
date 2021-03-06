﻿module Dak.View.Menus

open System
open System.Windows.Forms
open System.Drawing
open System.Collections.Generic
open System.ComponentModel
open System.ComponentModel.DataAnnotations

open MyWinForms.TopDialog
open Dak.MainWindow
open Dak.Operations
open Thread2
open Dak
open Dak.Operations.PartyWorks
open Dak.View
open Dak.View.TopBar

[<AutoOpen>]
module private Helpers =
    type P =ViewModel.ProductViewModel     
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

    let simpleMenu = MyWinForms.Utils.buttonsMenu (new Font("Consolas", 12.f)) ( Some 300 ) 


    let popupNumberDialog<'a>     
        prompt title tryParse work 
        (btn : Button) 
        (parentPopup : MyWinForms.Popup) =
        let tb = new TextBox(Width = 290, Text = string party.NewValidAddr )                    
        let dialog,validate  = 
            popupDialog 
                { Dlg.def() with 
                    Text = Some prompt
                    ButtonAcceptText = "Применить" 
                    Title = title
                    Width = 300
                    Content = tb }
                ( fun () -> 
                    tryParse tb.Text)
                ( fun (value : 'a) ->  
                    parentPopup.Hide()
                    work value ) 
        tb.TextChanged.Add <| fun _ -> validate()                        
        dialog.Show btn


    let popupTermociclingDialog  
        (btn : Button) 
        (parentPopup : MyWinForms.Popup) =
        let ph = new Panel(Width = 290)
        let addctrl (c:Control) = 
            c.Parent <- ph
            c.Dock <- DockStyle.Top
        let tb caption text =
            addctrl (new Label(Text = caption, AutoSize = true) )
            let tb = new TextBox(Text = text)           
            addctrl tb
            tb
        let tbCount = tb "Количество термоциклов" "3"
        let tbTempMin = tb "Нижняя температура, \"С" (string <| party.GetTermoTemperature TLow)
        let tbTempMax = tb "Верхняя температура, \"С" (string <| party.GetTermoTemperature THigh)
        let tbTime = tb "Время прогрева, час:мин" "1:00"
        
        ph.stretchHeightToContent 50
        ph.InvertChildrenOrder()

        let tryGetData() = 
            maybe {
                let! count = 
                    let b, count = Int32.TryParse tbCount.Text
                    if not b || count < 1 || count > 10 then None else Some count
                let! tempMin  = String.tryParseDecimal tbTempMin.Text
                let! tempMax  = String.tryParseDecimal tbTempMax.Text
                let! time  = 
                    let b,v = TimeSpan.TryParse tbTime.Text
                    if not b || v < TimeSpan.FromMinutes 1. || v > TimeSpan.FromHours 24. 
                    then None 
                    else Some v
                return!
                    if tempMax <= tempMin then None else
                    Some( count, tempMin, tempMax, time )
            }
                 
        let dialog,validate  = 
            popupDialog 
                { Dlg.def() with 
                    ButtonAcceptText = "Старт" 
                    Title = "Термоциклирование"
                    Width = 300
                    Content = ph }
                tryGetData
                ( fun x ->  
                    parentPopup.Hide()
                    Run.TermoChamber.termocicling x ) 
        for tb in [tbCount; tbTempMin; tbTempMax; tbTime] do
            tb.TextChanged.Add <| fun _ -> validate()                        
        dialog.Show btn

let productToolsPopup = 
    let setAddr = 
        popupNumberDialog 
            "Ведите сетевой адрес от 1 до 127" 
            "Установка сетевого адреса"
            ( fun s ->
                let b,v = Byte.TryParse s
                if b  && v > 0uy && v<128uy then Some v else None)
            (decimal >> Run.setAddr)
    
    [   yield "Установка адреса", setAddr
        yield!
            Cmd.values
            |> List.filter( (<>) Cmd.setAddy ) 
            |> List.map( fun cmd -> 
                let what = Cmd.what cmd
                what,
                    popupNumberDialog 
                        (sprintf "Введите значение аргумента команды %A" what)
                        what
                        String.tryParseDecimal
                        (fun value -> party.WriteModbus (cmd,value) |> ignore ) ) 
    ]
    |> simpleMenu
    
let pneumoToolsPopup =         
    [   yield! ScalePt.values |> List.map ( fun gas -> 
            ScalePt.what gas, fun _ _  -> Run.Pneumoblock.switch gas )
        yield "Выкл.", fun _ _ -> Run.Pneumoblock.close()  ]
    |> simpleMenu

let termoToolsPopup = 
    
    let setpoint = 
        popupNumberDialog 
            "Введите значение уставки термокамеры"
            "Задать уставку термокамеры"
            String.tryParseDecimal
            Run.TermoChamber.setSetpoint 
    let do' f _  (x : MyWinForms.Popup) = 
        x.Close()
        f()

    [   yield "Термоциклирование", popupTermociclingDialog
        yield "Старт", do' Run.TermoChamber.start
        yield "Стоп", do' Run.TermoChamber.stop
        yield "Уставка", setpoint  
        yield "Температура", do' Run.TermoChamber.read ]
    |> simpleMenu

let private initButtons1 = 
    let buttons1placeholder = 
        new Panel
            (   Parent = TabsheetParty.BottomTab, Dock = DockStyle.Top, Height = 89 )

    
    let imgbtn left top key tooltip f = 
        let x = 
            new Button( Parent = buttons1placeholder, Left = left, Top = top,
                        ImageKey = key, Width = 40, Height = 40,
                        FlatStyle = FlatStyle.Flat,
                        ImageList = Widgets.Icons.instance.imageList1)
        MainWindow.setTooltip x tooltip
        x.Click.Add <| fun _ ->  
            f x
        x

    let btnOpenParty = imgbtn 3 3 "open" "Открыть ранее сохранённую партию" Dialogs.OpenParty.showDialog
    let btnNewParty = imgbtn 46 3 "add" "Создать новую партию" Dialogs.EditProductsList.createNewParty

    let btnAddProd = imgbtn 3 46 "additem" "Добавить в партию новые приборы" Dialogs.EditProductsList.addProducts
    let btnDelProd = 
        let b = imgbtn 46 46 "removeitem" "Удалить выбранные приборы из партии" Dialogs.EditProductsList.deleteProducts
        b.Visible <- false
        let g = gridProducts
        g.SelectionChanged.Add <| fun _ ->
            b.Visible <- g.SelectedCells.Count > 0 
        b

    Thread2.IsRunningChangedEvent.addHandler <| fun (_,isRunning) ->
        [btnOpenParty; btnNewParty; btnAddProd; btnDelProd ]
        |> Seq.iter(fun b -> b.Enabled <- not isRunning )
        

    let _ = imgbtn 89 3 "todo" "Выбрать опрашиваемые параметры" ( fun b ->
        let popup = 
            MyWinForms.Utils.popupConfig 
                "Опрашиваемые параметры" 
                (Dialogs.SelectVars()) 
                ( fun _ g -> g.PropertySort <- PropertySort.Alphabetical )
        popup.Closed.Add( fun _ ->
           View.Products.GridProductColumns.updateVisibilityByConfig()  )
        popup.Show b )    

    fun () -> ()
    
open Dak.View.TopBar

let initialize = 
    
    let buttonRun = new Button( Parent = TopBar.thread1ButtonsBar, Dock = DockStyle.Left, AutoSize = true,
                                ImageKey = "run",
                                Text = (sprintf "%A" Thread2.scenary.Value.FullName),
                                ImageAlign = ContentAlignment.MiddleLeft,
                                TextImageRelation = TextImageRelation.ImageBeforeText,
                                TextAlign = ContentAlignment.MiddleCenter,
                                FlatStyle = FlatStyle.Flat,
                                ImageList = Widgets.Icons.instance.imageList1)
    TopBar.thread1ButtonsBar.Controls.Add <| new Panel(Dock = DockStyle.Left, Width = 3)
    MainWindow.setTooltip buttonRun ("Выполнить " + buttonRun.Text)
    buttonRun.Click.Add <| fun _ ->  
        Thread2.run true Thread2.scenary.Value
    Thread2.scenary.AddChanged <| fun (prevOp,newOp) ->
        buttonRun.Text <- sprintf "%A" newOp.FullName
        MainWindow.setTooltip buttonRun ("Выполнить " + buttonRun.Text)
        buttonRun.AutoSize <- false
        buttonRun.AutoSize <- true

    let (<==) (text,tooltip) f = 
        let b = new Button( Parent = TopBar.thread1ButtonsBar, Dock = DockStyle.Left, 
                            FlatStyle = FlatStyle.Flat,
                            Text = text, AutoSize = true )
        b.Click.Add <| fun _ ->  
            f b    
        MainWindow.setTooltip b tooltip
        TopBar.thread1ButtonsBar.Controls.Add <| new Panel(Dock = DockStyle.Left, Width = 3)

    let imgBtn (key,tooltip) f = 
        let b = new Button( Parent = TopBar.thread1ButtonsBar, Dock = DockStyle.Left, 
                            ImageList = Widgets.Icons.instance.imageList1,
                            FlatStyle = FlatStyle.Flat, ImageKey = key,
                            Width = 40, Height = 40,
                            AutoSize = true )
        b.Click.Add <| fun _ ->  
            f b    
        MainWindow.setTooltip b tooltip
        TopBar.thread1ButtonsBar.Controls.Add <| new Panel(Dock = DockStyle.Left, Width = 3)
        
        
    imgBtn ( "loop","Опрос выбранных параметров приборов партии" ) <| fun _ ->
        Operations.Run.runInterrogate()
    
    imgBtn ("network", "Управление приборами по Modbus") 
        productToolsPopup.Show 

    imgBtn ("pneumo", "Управление пневмоблоком") 
        pneumoToolsPopup.Show 

    imgBtn  ("termochamber", "Управление термокамерой")
        termoToolsPopup.Show 

    imgBtn  ("testconn", "Проверка связи с приборами и оборудованием") 
        Run.testConnect

    
    let btnSelectScenary = 
        new Button( Parent = TopBar.thread1ButtonsBar, Dock = DockStyle.Left, AutoSize = true,
                    ImageKey = "script", Width = 40, Height = 40,
                    FlatStyle = FlatStyle.Flat,
                    ImageList = Widgets.Icons.instance.imageList1)
    MainWindow.setTooltip btnSelectScenary "Выбрать сценарий настройки"
    btnSelectScenary.Click.Add <| fun _ ->  
        Dialogs.ChooseScenary.showSelectScenaryDialog btnSelectScenary
    TopBar.thread1ButtonsBar.Controls.Add <| new Panel(Dock = DockStyle.Left, Width = 3)

    initButtons1()

    let buttonSettings = 
        new Button( Parent = right, Height = 40, Width = 40, Visible = true,
                    ImageList = Widgets.Icons.instance.imageList1,
                    FlatStyle = FlatStyle.Flat,
                    Dock = DockStyle.Right, ImageKey = "settings")
    right.Controls.Add <| new Panel(Dock = DockStyle.Right, Width = 3)
    setTooltip buttonSettings "Параметры приложения" 

    buttonSettings.Click.Add <| fun _ ->            
        let popup = 
            MyWinForms.Utils.popupConfig 
                "Параметры" 
                (Dialogs.AppConfigView())
                ( fun _ g -> g.PropertySort <- PropertySort.Alphabetical )
        popup.Font <- form.Font        
        popup.Show(buttonSettings)        
    Thread2.scenary.Set (Scenaries.main())
    fun () -> ()