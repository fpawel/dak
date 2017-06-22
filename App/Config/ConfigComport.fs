module Config.Comport

open System
open System.IO

open System
open System.ComponentModel
open System.ComponentModel.DataAnnotations
open System.IO.Ports

open MyWinForms.Converters

type ComPortNamesConverter() = 
    inherit  StringConverter()
    override this.GetStandardValuesSupported _ = true
    override this.GetStandardValuesExclusive _ = true
    override this.GetStandardValues _ =         
        SerialPort.GetPortNames() 
        |> TypeConverter.StandardValuesCollection

let comPortBoudRates = 
    [|  1200
        2400
        9600
        19200
        38400
        57600
        115200 |] 

type ComPortBoudRatesConverter() = 
    inherit Int32Converter()
    override this.GetStandardValuesSupported _ = true
    override this.GetStandardValuesExclusive _ = true
    override this.GetStandardValues _ =         
        TypeConverter.StandardValuesCollection( comPortBoudRates )

type ComPortParityConverter() = 
    inherit Int32Converter()
    override this.GetStandardValuesSupported _ = true
    override this.GetStandardValuesExclusive _ = true
    override this.GetStandardValues _ =         
        [|  Parity.None
            Parity.Odd
            Parity.Even
            Parity.Mark            
            Parity.Space
        |] 
        |> Array.map int        
        |> TypeConverter.StandardValuesCollection

type ComPortStopBitsConverter() = 
    inherit Int32Converter()
    override this.GetStandardValuesSupported _ = true
    override this.GetStandardValuesExclusive _ = true
    override this.GetStandardValues _ =         
        [|  StopBits.None
            StopBits.One
            StopBits.Two
            StopBits.OnePointFive
        |] 
        |> Array.map int
        |> TypeConverter.StandardValuesCollection

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type Config =
    {   [<DisplayName("Порт")>]
        [<Description("Выбор имени используемого СОМ порта")>]
        [<TypeConverter (typeof<ComPortNamesConverter>) >]
        mutable PortName : string

        [<DisplayName("Таймаут, мс")>]
        [<Description("Длительность ожидания ответа от прибора в милисекундах")>]   
        mutable Deadline : int

        [<DisplayName("Задержка отправки, мс")>]
        [<Description("Задержка отправки запроса прибору в милисекундах")>]
        mutable Delay : int

        [<DisplayName("Время ожидания символа, мс")>]
        [<Description("Длительность ожидания символа ответа в милисекундах")>]
        mutable Chartime : int

        [<DisplayName("Колличество повторов")>]
        [<Description("Колличество повторов запроса прибору")>]
        mutable RepeatCount : int

        [<DisplayName("Показывать посылки")>]
        [<Description("Разрешить показывать посылки приёмопередачи СОМ порта в консоли данного приложения")>] 
        [<TypeConverter(typeof<YesNoConverter>)>]
        mutable CanLog : bool 

        [<DisplayName("Скорость передачи")>]        
        [<Description("Скорость передачи СОМ порта, заданная в килобитах в секунду (бодах)")>]
        [<TypeConverter (typeof<ComPortBoudRatesConverter>) >]
        mutable BaudRate : int 

        [<DisplayName("Контроль чётности")>]        
        [<Description("Установить способ проверки бита чётности")>]
        [<TypeConverter (typeof<ComPortParityConverter>) >]
        mutable Parity : int

        [<DisplayName("Стоп-бит")>]        
        [<Description("Установить количество стоповых битов")>]
        [<TypeConverter (typeof<ComPortStopBitsConverter>) >]
        mutable StopBits : int

        [<Browsable(false)>]
        Description : string }  
    override x.ToString() = 
        if String.IsNullOrEmpty x.PortName then "не установлен" else
        sprintf "%s, %d Б/с, %d" x.PortName x.BaudRate x.Deadline

    static member New() = {   
        PortName = ""
        Deadline = 2000
        Delay = 100
        Chartime = 50
        RepeatCount = 5
        CanLog = false 
        BaudRate = 9600
        Parity = int Parity.None
        StopBits = int StopBits.One
        Description = "-" }

    static member WithDescr s = { Config.New() with Description = s}

