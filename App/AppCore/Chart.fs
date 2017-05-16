module Dak.Chart

open System
open System.Windows.Forms
open System.Drawing
open System.Windows.Forms.DataVisualization.Charting
open System.Collections.Generic

open MyWinForms.Utils
open MainWindow

let mutable physVar = MilVar.conc

[<AutoOpen>]
module private Helpers =
    type Key = PhysVarValues.Key

    let prodIdSeries = Dictionary<ID,Series>()
    
let chart = 
    let chart = new Chart(Parent = TabsheetChart.RightTab, Dock = DockStyle.Fill)
    let ar = new ChartArea()
        
    ar.CursorX.IsUserSelectionEnabled <- true
    ar.CursorX.IntervalType <- DateTimeIntervalType.Seconds
    ar.CursorY.IntervalType <- DateTimeIntervalType.Auto
    ar.CursorY.Interval <- 0.1
    chart.ChartAreas.Add(ar)
        

    let ax = ar.AxisX
    ax.LabelStyle.Format <- "dd/MM-HH:mm:ss";
    ax.IntervalAutoMode <- IntervalAutoMode.VariableCount;
    ax.LineDashStyle <- ChartDashStyle.Dash;
    ax.MajorGrid.LineDashStyle <- ChartDashStyle.Dash;
    ax.MajorGrid.LineWidth <- 1

    let ax = ar.AxisY            
    ax.IntervalAutoMode <- IntervalAutoMode.VariableCount;
    ax.LineDashStyle <- ChartDashStyle.Dash;
    ax.MajorGrid.LineDashStyle <- ChartDashStyle.Dash;
    ax.MajorGrid.LineWidth <- 1
    chart.Legends.Add(new Legend(Docking = Docking.Bottom) )
    chart

let axisScalingViewModel = MyWinForms.Components.ChartAxisScalingViewModel(chart)

type ProductSeriesInfo = 
    {   Party : Repository.PartyPath
        Product : ID
        Name : string }

let addProductSeries i =
    let k : Key = {Party = i.Party; Product = i.Product; Var = physVar}
    let series = 
        let g = Guid.NewGuid()
        let guidString = Convert.ToBase64String(g.ToByteArray()).Replace("=", "").Replace("+", "")
        new Series( ChartType = SeriesChartType.Line,
                    XValueType = ChartValueType.DateTime,
                    BorderWidth = 3,
                    LegendText = i.Name,
                    BorderDashStyle = ChartDashStyle.Dash,
                    Name="Series" + guidString)

    series.MarkerStyle <- MarkerStyle.Circle
    series.MarkerSize <- 10

    chart.Series.Add(series)
    prodIdSeries.[i.Product] <- series
    PhysVarValues.getSavedValues k
    |> List.iter (series.Points.AddXY >> ignore)
    PhysVarValues.getNewValues k
    |> List.iter (series.Points.AddXY >> ignore)
        
let removeProductSeries productId =
    let b,v = prodIdSeries.TryGetValue productId
    b && prodIdSeries.Remove productId && chart.Series.Remove(v)

let setProductLegend productId legendText =
    let b,v = prodIdSeries.TryGetValue productId
    if b then
        v.LegendText <- legendText

let clear() =
    prodIdSeries.Clear()
    chart.Series.Clear()

let addProductValue productId var (value:decimal) =
    if var = physVar then
        let b,series = prodIdSeries.TryGetValue productId
        if b then
            series.Points.AddXY( DateTime.Now, value ) |> ignore