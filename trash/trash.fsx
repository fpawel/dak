let private textboxPartyConfigWithLabel labelText value update =
    let p = new Panel(Parent = TabsheetParty.BottomTab, Dock = DockStyle.Top, Height = 30)
    //let _ = new Panel(Parent = TabsheetParty.BottomTab, Dock = DockStyle.Left, Height = 5)
    let tb = new TextBox(Parent = p, Dock = DockStyle.Fill, 
                            BorderStyle = BorderStyle.None)
    let _ = new Panel(Parent = p, Dock = DockStyle.Left, Width = 10)
    let _ = new Label(Parent = p, Dock = DockStyle.Left, AutoSize = true, Text = labelText )
    let _ = new Panel(Parent = p, Dock = DockStyle.Left, Width = 10)              
             
    tb.Text <- value
    tb.TextChanged.Add <| fun _ ->
        update (String.tryParseDecimal tb.Text)    
    
// ввод ПГС
for gas in ScalePt.values do     
    textboxPartyConfigWithLabel 
        gas.What (string <|  party.GetPgs gas)
        (   Option.getWithDefault (fun () -> 
                let scale = party.GetProductType().Scale.Value
                ScalePt.defaultBallonConc scale gas )
            >> party.SetPgs gas )
    
    

// ввод температур
for temperature in TemperaturePt.values do    
    let what = 
        match temperature with
        | TNorm -> "+20 ⁰C"
        | TLow -> "T-"        
        | THigh -> "T+"
        | T90 -> "+90 ⁰C"
    textboxPartyConfigWithLabel 
        what (string <|  party.GetTermoTemperature temperature)
        (   Option.getWithDefault (fun () -> temperature.DefaultTemperature )
            >> party.SetTermoTemperature temperature )