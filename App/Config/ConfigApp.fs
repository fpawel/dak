module Config.App

open System
open System.ComponentModel

open MyWinForms.Converters

module View = 
    type Grid =  
        {   mutable ColWidths : int list
            mutable ColumnHeaderHeight : int 
            
        }

    type Config =  
        {   mutable PartyId : string
            mutable ScnDetailTextSplitterDistance : int 
            mutable InterrogateMilVars :  int Set
            mutable InterrogateStend6026 : bool 
            mutable SelectedCoefs : string }
        static member create() = 
            {   PartyId = ""
                ScnDetailTextSplitterDistance = 0 
                InterrogateMilVars = Set.ofList [0; 2; 4; 12; 14; 16]
                InterrogateStend6026 = true
                SelectedCoefs = "0-500" 
            }

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
 type Stend = 
    {   [<DisplayName("Сетевой адрес блока измерения тока")>]
        [<Description("Сетвой адрес блока измерения тока")>]
        mutable Addr : byte

        [<DisplayName("Сетевой адрес пневмоблока")>]
        [<Description("Сетвой адрес пневмоблока")>]
        mutable PneumoblockAddr : byte

        [<DisplayName("Показывать посылки")>]
        [<Description("Показывать посылки")>]
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable ShowLogs : bool         
    }
    override __.ToString() = ""

type TermoChamberControler = 
    | Temp2500
    | Temp800

module TermoChamberControlerHelp = 
    let temp2500 = "TEMP 2500"
    let temp800 = "TEMP 800"
    let vs = 
        [   Temp2500, temp2500
            Temp800, temp800 ]
        |> Map.ofList
    let sv = 
        [   temp2500, Temp2500
            temp800, Temp800 ]
        |> Map.ofList

    type TermoChamberControlerTypeConverter() = 
        inherit  StringConverter()

        override __.GetStandardValuesSupported _ = true
        override __.GetStandardValuesExclusive _ = true

        override this.GetStandardValues _ =         
            TypeConverter.StandardValuesCollection 
                [|  Temp2500
                    Temp800|]
    
        override __.ConvertTo(_,_,value,_) =
            vs.TryFind (value |> box :?> TermoChamberControler)
            |> Option.withDefault temp2500
            |> box
    
        override __.ConvertFrom(_,_,value) =
            sv.TryFind (value |> box :?> string)
            |> Option.withDefault Temp2500
            |> box
        

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type Termo = 
    {   [<DisplayName("Погрешность уставки")>]
        [<Description("""Минимальная разница между показаниями и уставкой термокамеры, при которой температура считается установившейся \"С""")>]
        mutable SetpointErrorLimit : decimal

        [<DisplayName("Таймаут уставки")>]
        [<Description("""Максимальная длительность уставки термокамеры, по истечении которой выполнение настройки будет прекращено с сообщением об ошибке""")>]
        [<TypeConverter(typeof<TimeSpanConverter>)>]
        mutable SetpointDeadline : TimeSpan 

        [<DisplayName("Версия контролера")>]
        [<Description("""Версия контролера""")>]
        [<TypeConverter(typeof<TermoChamberControlerHelp.TermoChamberControlerTypeConverter>)>]
        mutable TermoChamberControler : TermoChamberControler }


    override __.ToString() = ""

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type Main = 
    {   [<DisplayName("СОМ порт приборов")>]
        [<Description("Настройка параметров приёмопередачи СОМ порта, к которому подключены настраиваемые приборы и стенд")>]
        mutable ComportProducts : Comport.Config 

        [<DisplayName("СОМ порт термокамеры")>]
        [<Description("Параметры приёмопередачи СОМ порта, к которому подключена термокамера")>]
        mutable ComportTermo : Comport.Config 

        [<DisplayName("СОМ порт HART")>]
        [<Description("Настройка параметров приёмопередачи СОМ порта, к которому подключен HART модем")>]
        mutable ComportHart : Comport.Config 

        [<DisplayName("Термокамера")>]
        [<Description("Параметры термокамеры")>]
        mutable Termo : Termo
        
        [<DisplayName("Стенд")>]
        [<Description("Параметры стенда")>]
        mutable Stend : Stend

        [<DisplayName("Пауза калибровки тока")>]
        [<Description("Длительность паузы при калибровке тока")>]
        [<TypeConverter(typeof<TimeSpanConverter>)>]
        mutable AdjustCurrentDelay : TimeSpan
    }
    override __.ToString() = ""



type Config = 
    {   View : View.Config
        mutable Main : Main        
    }
    
    static member create() = 
        {   View = View.Config.create()
            Main = 
                {   ComportHart = Comport.Config.WithDescr "HART" 
                    ComportProducts = Comport.Config.WithDescr "приборы" 
                    ComportTermo = Comport.Config.WithDescr "термокамера"
                    Termo = 
                        {   TermoChamberControler = Temp2500
                            SetpointDeadline = TimeSpan.FromHours 4.
                            SetpointErrorLimit = 2m
                        }
                    Stend = 
                        {   Addr = 0x63uy
                            PneumoblockAddr = 0x32uy
                            ShowLogs = false
                        }                    
                    AdjustCurrentDelay = TimeSpan.FromSeconds 3.
                }
        }

let config, error, save = Json.Config.create "app.config.json" Config.create