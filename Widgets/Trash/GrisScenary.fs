module MainWindow

let gridScenary = 
    let splt = new Splitter(Parent = TabsheetScenary.RightTab, Dock = DockStyle.Left, Width = 3, BackColor = Color.LightGray)
    
    let pan = new Panel (Parent = TabsheetScenary.RightTab, Dock = DockStyle.Left,
                            MinimumSize = Size(300,0), MaximumSize = Size(1000,0),
                            Width = Config.App.config.View.ScnDetailTextSplitterDistance)
    let x = newGridView pan "ScenaryGridView"

    form.FormClosing.Add <| fun _ ->
        Config.App.config.View.ScnDetailTextSplitterDistance <- pan.Width

    x.Columns.AddRange            
        [|  %% new TextColumn(DataPropertyName = "Name", HeaderText = "Операция", 
                               AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill)
            %% new TextColumn(DataPropertyName = "Delaytime", HeaderText = "Задержка", 
                                AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells) 
            %% new TextColumn(DataPropertyName = "Status", HeaderText = "Статус", 
                                AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells) |]
    
    x


// thread2.fs

let scenary = 
    let x = Ref.Observable(Operation.CreateSingle("", none) none)
    x.AddChanged <| fun (o,n) ->
        if scenaryKeepRunning.Value  then
            failwithf "can not change performing scenary from %A to %A" o.FullName n.FullName
        operations.Clear()
        let ops = Operation.tree n
        ops |> List.iter ( fun x -> 
            let i = Operation.GetRunInfo  x 
            i.Root <- Some ops.[0]
            operations.Add i )        

        if gridScenary.RowCount > 0 then
            gridScenary.CurrentCell <- gridScenary.Rows.[0].Cells.[0]
    x