module Dak.MainWindow

#nowarn "40"

open System
open System.Windows.Forms
open System.Drawing

open MyWinForms.Utils

[<AutoOpen>]
module private Helpers =
    type CheckBoxColumn = MyWinForms.GridViewCheckBoxColumn
    type TextColumn = DataGridViewTextBoxColumn
    let (~%%) x = x :> DataGridViewColumn

    let tooltip = new ToolTip(AutoPopDelay = 5000, InitialDelay = 1000,  ReshowDelay = 500, ShowAlways = true)

let aboutForm = 
    let x = new Widgets.AboutForm() 
    x.LabelVersion.Text <-  
        try
            Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString()
        with _ -> 
            ""
    x.Deactivate.Add(fun _ -> 
        x.Hide()
        )
    x

let form =     
    let x = new Form(Font = new Font("Consolas", 12.f), WindowState = FormWindowState.Maximized )
    let path = IO.Path.Combine( IO.Path.ofExe, "icon.ico")
    try        
        let customIcon = new Icon( path )
        x.Icon <- customIcon
    with e ->
        Logging.error "fail to set icon.ico from %A : %A" path e
    let mutable isClosed = false    
    x

let setTooltip<'a when 'a :> Control > (x:'a) text = 
    tooltip.SetToolTip(x, text)

let mainLayer = new Panel( Parent = form, Dock = DockStyle.Fill)

let rightTabContentPlaceholder,setActivePageTitle = 
    let par1 = new Panel(Parent = mainLayer, Dock = DockStyle.Fill)
    let rightTabPagePlaceholder = new Panel(Parent = par1, Dock = DockStyle.Fill)

    let p = new Panel(Dock = DockStyle.Top, Height = 30, Parent = par1)
    let _ = new Panel(Parent = p, Dock = DockStyle.Top, Height = 5)
    let x = new Label(Parent = p, Dock = DockStyle.Top, Height = 20, TextAlign = ContentAlignment.MiddleLeft)
    let _ = new Panel(Parent = p, Dock = DockStyle.Top, Height = 5)

    x.SetInfoStyle()
    rightTabPagePlaceholder,(fun s -> x.Text <- s )

let tabButtonsPlaceholder, leftBottomTabContentPlaceHolder = 
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Left, Width = 3)
    let leftPanel = new Panel(Parent = mainLayer, Dock = DockStyle.Left, Width = 200)
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Left, Width = 3)

    
    let leftTabContenPlaceholder = new Panel(Parent = leftPanel, Dock = DockStyle.Fill)  
    let _ = new Panel(Parent = leftPanel, Dock = DockStyle.Top, Height = 10)

    let tabButtonsPlaceholder = new Panel(Parent = leftPanel, Dock = DockStyle.Top)
    let _ = new Panel(Parent = leftPanel, Dock = DockStyle.Top, Height = 10)

    
    tabButtonsPlaceholder, leftTabContenPlaceholder 

let bottomLayer = 
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Bottom, Height = 3)
    let x = new Panel(Parent = mainLayer, Dock = DockStyle.Bottom, Height = 25)
    let _ = new Panel(Parent = mainLayer, Dock = DockStyle.Bottom, Height = 3)    
    x

let labelPerformingInfo = 
    new Label(Parent = bottomLayer, Dock = DockStyle.Fill, Text = "",
                TextAlign = ContentAlignment.MiddleLeft )

module HardwareInfo = 
    let private (~%%) x = MyWinForms.Components.LeftInfoBlock(bottomLayer, x)
    let termo = %% "Термокамера"
    let peumo = %% "ПГС"

    let initialize = 
        [   termo; peumo  ] |> List.iter (fun x -> x.hide() )
        fun() -> ()

open HardwareInfo

type Tabsheet = 
    | TabsheetParty
    | TabsheetScenary
    | TabsheetChart
    | TabsheetCoefs
    member x.Title = Tabsheet.title x
    member x.Descr = Tabsheet.descr x
    static member values = FSharpType.unionCasesList<Tabsheet>
    
    static member title = function
        | TabsheetParty ->   "Партия"
        | TabsheetScenary -> "Сценарий"
        | TabsheetChart -> "График"
        | TabsheetCoefs -> "Коэф-ты"

    static member descr = function
        | TabsheetParty ->   "Партия настраиваемых приборов"
        | TabsheetScenary -> "Сценарий настройки приборов партии"
        | TabsheetChart -> "Графики измеряемых параметров приборов партии"
        | TabsheetCoefs -> "Коэффициенты приборов партии"

module private TabPagesHelp =
    let content = 
        Tabsheet.values 
        |> List.map(fun x -> 
            let p1 = new Panel( Dock = DockStyle.Fill, Parent = rightTabContentPlaceholder, Visible = false, AutoScroll = true)
            let p2 = new Panel( Dock = DockStyle.Fill, Parent = leftBottomTabContentPlaceHolder, Visible = false, AutoScroll = true)
            x, (p1,p2))
        |> Map.ofList

type Tabsheet with
    member x.BottomTab = snd TabPagesHelp.content.[x]
    member x.RightTab = fst TabPagesHelp.content.[x]
    static member content x =
        TabPagesHelp.content.[x]
    member x.ShowContent() =
        Tabsheet.showContent x
    static member showContent tabPage =        
        Tabsheet.values 
        |> List.iter ( fun x -> 
            let v = x=tabPage
            x.BottomTab.Visible <- v
            x.RightTab.Visible <- v)

let newLogginWebbrowser parent = 
    let webb =  
        new WebBrowser(Parent = parent, BackColor = TabsheetScenary.RightTab.BackColor, 
                        Dock = DockStyle.Fill, AllowNavigation = true, Url = null,
                        IsWebBrowserContextMenuEnabled = true, 
                        AllowWebBrowserDrop = false )
    webb.DocumentCompleted.Add <| fun _ ->
        webb.AllowNavigation <- false
        if  webb.Document <> null && webb.Document.Body <> null then 
            webb.Document.Body.ScrollIntoView(false)
    webb

let webbJournal = newLogginWebbrowser TabsheetScenary.RightTab

let newGridView parent name = 
    let x = 
        GridView.newFlexible() 
        |> GridView.withNoSortColumns
    x.Parent <- parent
    x.Name <- name
    x.BackgroundColor <- TabsheetScenary.RightTab.BackColor    
    x

module ScenaryColumn =
    let name = 
        let x = 
            new BrightIdeasSoftware.OLVColumn(Text = "Операция", MinimumWidth = 200, Width = 350, 
                                                    IsEditable = false,
                                                    WordWrap = true, Sortable = false)
        x

    let time = 
        let x = new BrightIdeasSoftware.OLVColumn(Text = "Задержка", MinimumWidth = 90, Sortable = false )
        x.CellEditUseWholeCell <- Nullable false
        x

    let status = new BrightIdeasSoftware.OLVColumn(Text = "Статус", MinimumWidth = 90, Sortable = false, IsEditable = false)

let treeListViewScenary = 
    let splt = new Splitter(Parent = TabsheetScenary.RightTab, Dock = DockStyle.Left, Width = 3, BackColor = Color.LightGray)
    
    let pan = new Panel (Parent = TabsheetScenary.RightTab, Dock = DockStyle.Left,
                            MinimumSize = Size(300,0), MaximumSize = Size(1000,0),
                            Width = Config.App.config.View.ScnDetailTextSplitterDistance)
    let x = new BrightIdeasSoftware.TreeListView()
    x.Parent <- pan
    x.Dock <- DockStyle.Fill
    x.UseNotifyPropertyChanged <- true
    x.CellEditActivation <- BrightIdeasSoftware.ObjectListView.CellEditActivateMode.SingleClick
    let eddec = new BrightIdeasSoftware.EditingCellBorderDecoration ( UseLightbox = true )
    
    x.AddDecoration( eddec )
    

    form.FormClosing.Add <| fun _ ->
        Config.App.config.View.ScnDetailTextSplitterDistance <- pan.Width

    x.Columns.Clear()
    x.Columns.Add ScenaryColumn.name |> ignore
    x.Columns.Add ScenaryColumn.time |> ignore
    x.Columns.Add ScenaryColumn.status |> ignore

    x.HierarchicalCheckboxes <- true


    x

let gridProducts = 
    newGridView TabsheetParty.RightTab "PartyDataGrid"
    //|> GridView.withDisableSelection

let TextBlockPartyInfo = 
    new Label(Parent = TabsheetParty.RightTab, Font = new Font("Consolas", 14.f), Dock = DockStyle.Top)
    
 
let gridCoefs =  
    let x = 
        new DataGridView( Parent = TabsheetCoefs.RightTab,  
                        AutoGenerateColumns = false, 
                        Dock = DockStyle.Fill,
                        BackgroundColor = form.BackColor,
                        Name = "KefsGrid",
                        ColumnHeadersHeight = 40, 
                        RowHeadersWidthSizeMode = DataGridViewRowHeadersWidthSizeMode.DisableResizing,
                        RowHeadersWidth = 30,
                        AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.None,
                        AllowUserToResizeRows = false,
                        AllowUserToAddRows = false,
                        AllowUserToDeleteRows = false,                                
                        BorderStyle = BorderStyle.None  )
    x.Columns.Add( new MyWinForms.GridViewCheckBoxColumn() ) |> ignore

    ["№"; "Коэф-т"] |> List.iter(fun s -> 
        x.Columns.Add( new TextColumn( ReadOnly = true, HeaderText = s ) ) |> ignore )
    let selectedCoefsConfig = IntRanges.parseSet Config.App.config.View.SelectedCoefs
    Dak.Coef.values |> List.iteri(fun n kef -> 
        x.Rows.Add() |> ignore
        let row = x.Rows.[x.Rows.Count-1]
        let cs =  row.Cells
        cs.[0].Value <- selectedCoefsConfig.Contains kef
        cs.[1].Value <- kef 
        cs.[2].Value <- Dak.Coef.what kef)    
    x

let getRowOfCoef coef  =
    gridCoefs.Rows 
    |> Seq.cast<DataGridViewRow>
    |> Seq.tryFind( fun row -> 
        match row.Cells.[1].Value with
        | :? int as k when k = coef -> true
        | _ -> false )
    |> Option.getWithDefault(fun () -> 
        failwithf "no row of kef %d" coef )

let getCoefOfRow (row:DataGridViewRow) =    
    gridCoefs.Rows 
    |> Seq.cast<DataGridViewRow>
    |> Seq.filter( fun r -> Object.ReferenceEquals(row,r) )
    |> Seq.tryHead
    |> Option.map(fun row -> 
        match row.Cells.[1].Value with
        | :? int as k  -> k
        | x -> failwithf "the type of \"row.Cells.[1].Value\" must be int: %A" x 
        )
    |> Option.getWithDefault(fun () -> 
        failwith "no kef for this row" )

module SelectedCoefsRows =

    let get() = 
        form.PerformThreadSafeAction <| fun () ->
            gridCoefs.Rows 
            |> Seq.cast<DataGridViewRow>
            |> Seq.map(fun row ->
                let coef = getCoefOfRow row
                coef,  
                    let x = row.Cells.[0].Value in
                    if x = null then false else
                    x :?> bool)
            |> Seq.filter snd
            |> Seq.map fst
            |> Set.ofSeq
            
    let set coefs =
        form.PerformThreadSafeAction <| fun () ->
            gridCoefs.Rows 
            |> Seq.cast<DataGridViewRow>
            |> Seq.iter(fun row -> 
                row.Cells.[0].Value <- Set.contains (getCoefOfRow row) coefs
                )

//let productsToolsLayer = new Panel(Parent = TabsheetParty.BottomTab, Dock = DockStyle.Left, Width = 40 ) 
    
let errorMessageBox title message = 
    Logging.error "%A, %s" title message
    form.PerformThreadSafeAction <| fun () ->
        MessageBox.Show( message, title, MessageBoxButtons.OK, MessageBoxIcon.Error ) 
        |> ignore

let onExeption (e:Exception) = 
    Logging.error "Исключение %A" e 
    form.PerformThreadSafeAction <| fun () ->
        MessageBox.Show( sprintf "%A" e ,"Исключение", MessageBoxButtons.OK, MessageBoxIcon.Error ) 
        |> ignore
    System.Environment.Exit(1)
    failwith ""


      
let private getGrids() = 
    form.enumControls
        (fun x -> 
            if x.GetType()=  typeof<DataGridView>  then                     
                Some (x :?> DataGridView) 
            else None)
        id

let initialize =
    form.FormClosing.Add <| fun _ -> 
        Config.App.config.View.Grids <-
            getGrids()
            |> Seq.map( fun g -> 
                let v : Config.App.View.Grid =
                    {   ColWidths = [for c in g.Columns -> c.Width]
                        ColumnHeaderHeight = g.ColumnHeadersHeight }
                g.Name, v )     
            |> Map.ofSeq

    let rec h = EventHandler( fun _ _ -> 
        form.Activated.RemoveHandler h

        // показать оштбку файла конфигурации если есть
        match Config.App.error with
        | None -> ()
        | Some error ->
            MessageBox.Show( error, "Ошибка файла конфигурации", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            |> ignore
        
        for g in getGrids() do 
            let dt = Config.App.config.View.Grids.TryFind g.Name
            
            g.ColumnHeadersHeight <-
                match dt with
                | Some { ColumnHeaderHeight = h} -> h
                | _ -> g.ColumnHeadersHeight
                |> max (let sz = TextRenderer.MeasureText( "X", g.ColumnHeadersDefaultCellStyle.Font )
                        sz.Height + 7 )
            [for c in g.Columns -> c ]  |> List.iteri( fun n c -> 
                let w = 
                    match dt with            
                    | Some { ColWidths = dt } when n < dt.Length ->  dt.[n]
                    | _ -> 
                        let sz = TextRenderer.MeasureText( c.HeaderText, c.HeaderCell.Style.Font )
                        sz.Width + 10
                c.Width <- max 50 w )

        aboutForm.Hide()
        aboutForm.FormBorderStyle <- FormBorderStyle.FixedDialog
        aboutForm.ControlBox <- false
        aboutForm.ShowInTaskbar <- false
        aboutForm.ShowIcon <- true )
    form.Activated.AddHandler h    

    aboutForm.Show()
    aboutForm.Refresh()
    fun () -> ()
   