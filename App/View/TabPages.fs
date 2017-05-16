module Dak.View.TabPages


open MyWinForms.Utils

open MainWindow
open Dak

       
let getSelected, setSelected,_ =
    radioButtons 
        tabButtonsPlaceholder 
        Tabsheet.values
        Tabsheet.title
        Tabsheet.descr
        (fun tabPage -> 
            setActivePageTitle tabPage.Title
            tabPage.ShowContent() ) 

module TabChart =
    let update() = 
        if getSelected() = TabsheetChart then
            AppData.updateChartSeriesList ()