﻿namespace Dak.Dialogs

open System
open System.ComponentModel

open Dak
[<AutoOpen>]
module private Helpers =
    let party = AppData.party

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type TemperatureConfigView() =

    [<DisplayName("T-")>]    
    [<Description("Пониженная температура")>]
    member x.L 
        with get() = party.GetTermoTemperature TLow
        and set v = party.SetTermoTemperature TLow v  

    [<DisplayName("НКУ")>]    
    [<Description("Нормальная температура")>]
    member x.N 
        with get() = party.GetTermoTemperature TNorm
        and set v = party.SetTermoTemperature TNorm v 

    [<DisplayName("T+")>]    
    [<Description("Повышенная температура")>]
    member x.H 
        with get() = party.GetTermoTemperature THigh
        and set v = party.SetTermoTemperature THigh v 

    [<DisplayName("+90 ⁰C")>]    
    [<Description("Температура +90 ⁰C")>]
    member x.H80 
        with get() = party.GetTermoTemperature T90
        and set v = party.SetTermoTemperature T90 v 

    override __.ToString() = ""

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type PartyConfigView() =
    [<DisplayName("Исполнение")>]    
    [<Description("Исполнение приборов партии")>]
    [<TypeConverter (typeof<EditProductsList.ProductTypesConverter>) >]
    member x.ProductType 
        with get() = party.ProductType
        and set v = 
            party.ProductType <- v
            Thread2.scenary.Set (Operations.Scenaries.main())
            if MainWindow.Tabsheet.GetSelected () =  MainWindow.TabsheetChart then
                AppData.updateChartSeriesList ()
            
    [<DisplayName("Наименование")>]    
    [<Description("Наименование партии")>]
    member x.Name 
        with get() = party.Name
        and set v = 
            party.Name <- v

    [<DisplayName("ПГС1")>]    
    [<Description("ПГС1, начало шкалы, концентрация ")>]
    member x.PgsGas0
        with get() = party.GetPgs ScaleBeg
        and set v = party.SetPgs ScaleBeg v 

    [<DisplayName("ПГС3")>]    
    [<Description("ПГС3, середина шкалы, концентрация ")>]
    member x.PgsGas1
        with get() = party.GetPgs ScaleMid
        and set v = party.SetPgs ScaleMid v 

    [<DisplayName("ПГС4")>]    
    [<Description("ПГС4, конец шкалы, концентрация ")>]
    member x.PgsGas5
        with get() = party.GetPgs ScaleEnd
        and set v = party.SetPgs ScaleEnd v 

    [<DisplayName("Температура")>]
    [<Description("Значения температур уставки термокамеры в температурных точках термокомпенсации приборов")>]
    member val  Temperature = TemperatureConfigView() with get,set

    override __.ToString() = ""

type AppConfigView() = 

    let config = Config.App.config
    
    [<DisplayName("Партия")>]    
    member val  Party = PartyConfigView() with get,set

    [<DisplayName("СОМ приборы")>]
    [<Description("Имя СОМ порта, к которому подключены настраиваемые приборы, пневмоблок и стенд 6026")>]
    [<TypeConverter (typeof<Config.Comport.ComPortNamesConverter>) >]
    member x.ComportProducts
        with get() = config.Main.ComportProducts.PortName
        and set v = 
            if v <> config.Main.ComportProducts.PortName then
                config.Main.ComportProducts.PortName <- v

    [<DisplayName("СОМ термокамера")>]
    [<Description("Имя СОМ порта, к которому подключена термокамера")>]
    [<TypeConverter (typeof<Config.Comport.ComPortNamesConverter>) >]
    member x.ComportTermo
        with get() = config.Main.ComportTermo.PortName
        and set v = 
            if v <> config.Main.ComportTermo.PortName then
                config.Main.ComportTermo.PortName <- v

    [<DisplayName("СОМ HART модем")>]
    [<Description("Имя СОМ порта, к которому подключен HART модем")>]
    [<TypeConverter (typeof<Config.Comport.ComPortNamesConverter>) >]
    member x.ComportHart
        with get() = config.Main.ComportHart.PortName
        and set v = 
            if v <> config.Main.ComportHart.PortName then
                config.Main.ComportHart.PortName <- v

    [<DisplayName("Параметры оборудования")>]
    [<Description("Параметры стенда и термокамеры")>]
    member x.Main
        with get() = config.Main
        and set v = 
            config.Main <- v

    override __.ToString() = ""

