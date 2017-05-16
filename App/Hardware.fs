module Hardware 
open System
open System.Text

open WinFormsControlUtils

type private R = Mdbs.Request
let private cfg = Config.App.config

module Pneumo =

    let clapan = Ref.Observable<byte option>(None)

    let private codeToString = function        
        | ValueInRange (0uy,3uy) true as code -> sprintf "ПГС%d" (code + 1uy)
        | _ -> "отключить"

    let isOpened() = 
        match clapan.Value with
        | None -> false
        | Some 1uy 
        | Some 2uy
        | Some 3uy -> true
        | _ -> false 
    
    let switch (code:byte)  = 
        let port = cfg.Main.ComportProducts
        // 0x10uy  [| 0uy; 0x10uy; 0uy; 1uy; 2uy; 0uy; code |]
        let req = 
            {   R.addy = cfg.Main.Stend.PneumoblockAddr
                R.cmd = 0x10uy
                R.data = [| 0uy; 0x20uy; 0uy; 1uy; 2uy; 0uy; code |]
                R.what = codeToString code |> sprintf "пневмоблок - %s" }
        let r = Mdbs.getResponse port req (fun _ -> "OK") ( fun _ ->  Ok code )
        
        match r with
        | Ok _ -> 
            clapan.Value <- Some code
            sprintf "%d" code, None
        | Err e -> "Ошибка", Some e
        |> MainWindow.HardwareInfo.peumo.setTextSafe (Logging.fromResult r)
        Result.map (fun _ -> ()) r

module Termo =   

    type TermoState = 
        | Start | Stop | Setpoint of decimal
        static member what = function
            | Start -> "cтарт"            
            | Stop -> "cтоп"
            | Setpoint(v) -> sprintf "уставка %g\"C" v

        member x.What = TermoState.what x

    type Request =  
        | Write of TermoState
        | Read
        static member what = function
            | Write s -> s.What.ToUpper()
            | _ -> "запрос температуры"
        static member requestString = function
            | Read -> "01RRD,02,0001,0002"
            | Write Start -> "01WRD,01,0101,0001"
            | Write Stop -> "01WRD,01,0101,0004"
            | Write (Setpoint v) -> 
                let s = v*10m |> int |> sprintf "%X"
                let s1 = 
                    let len = s.Length
                    if len=4 then s else        
                    if len>4 then s.Substring(len-4, 4) else
                    String.init (4-len) (fun _ -> "0") + s  

                "01WRD,01,0102," + s1 

    let state = Ref.Observable(None)
    let temperature = Ref.Observable(None)

    [<AutoOpen>]
    module private Helpers = 
        let bytesToAscii = Seq.toArray >> System.Text.Encoding.ASCII.GetString

        let getResponse1 req = 
            let scmd = Request.requestString req
            let port = cfg.Main.ComportTermo
            let result = 
                [|  yield 2uy
                    yield! sprintf "%s\r\n" scmd |> Text.Encoding.ASCII.GetBytes |]    
                |> Comport.getResponse port
                |> Result.map bytesToAscii
            match req with
            | Write _ ->                
                Logging.write 
                    (if Result.isOk result then Logging.Info else Logging.Error) 
                    "Термокамера, %s, %s, %s : %A" (Request.what req) 
                    port.PortName scmd result
            | _ -> ()

            result

    
        let formatError req responseString = 
            //if notKeepRunning() then ignore() else
            if String.IsNullOrEmpty responseString then 
                "Термокамера не отвечает"
            else
                sprintf "Неправильный формат ответа термокамеры. %A -> %s." req responseString

        let validWriteResponse = sprintf "%c01WRD,OK\r\n" '\u0002'
        let checkWriteResponse s = if s = validWriteResponse then Ok() else Err "invalid response on write"
    
        type Regex = System.Text.RegularExpressions.Regex

        let parseTemperature response =
            let m = Regex.Match(response, "01RRD,OK,([0-9a-fA-F]{4,4}),([0-9a-fA-F]{4,4})\r\n$")
            if m.Success && m.Groups.Count=3 |> not then Err "can't parse responsed temperature (1)" else 
            let (~%%) (n:int) = Hex.tryParse m.Groups.[n].Value
            match %% 1, %% 2 with 
            | Some t, Some setpoint -> Ok( (decimal t) / 10m, (decimal setpoint) / 10m )
            | _ -> Err "can't parse responsed temperature (2)"



        let getResponse2<'a> request (parse : string -> Result<'a,string>) = 
            getResponse1 request
            |> Result.bind ( fun response -> 
                parse response
                |> Result.mapErr( fun err ->                     
                    sprintf "%s, %s" err <| formatError request response ) )

    let read() = 
        let r = getResponse2 Read parseTemperature
        temperature.Value <- Some r
        
        match r with
        | Ok (x,y) -> sprintf "%M (%M)" x y, None
        | Err e -> "Ошибка", Some e
        |> MainWindow.HardwareInfo.termo.setTextSafe (Logging.fromResult r)

        r

    let write newstate = 
        let r = getResponse2 (Write newstate) checkWriteResponse
        let r1 = Result.map(fun _ -> newstate ) r
        state.Value <- Some r1

        match r with
        | Ok () -> newstate.What, None
        | Err e -> "Ошибка", Some e
        |> MainWindow.HardwareInfo.termo.setTextSafe (Logging.fromResult r)

        r

    let start() = write Start

    let stop() = write Stop

    let setSetpoint setpoint = result {
        Logging.info "Уставка термокамеры %M" setpoint
        do! stop()
        do! write (Setpoint setpoint)
        return! start() }
   
module private SetupTermoHelpers =
    
    type S = {
        destT : decimal
        startTime : DateTime }

    let rec loop s isKeepRunning work = result {
    
        if (not <| isKeepRunning()) then return! Err "прервано" else
        let! (temperature,setPointTemperature) = Termo.read()
        if abs( s.destT - temperature ) < cfg.Main.Termo.SetpointErrorLimit then
            return temperature 
        else
            if DateTime.Now - s.startTime > cfg.Main.Termo.SetpointDeadline then
                return! Err <| sprintf "таймаут %A" cfg.Main.Termo.SetpointDeadline
            else
                do! work()
                return! loop s isKeepRunning work }

let setupTermo destTemperature isKeepRunning work = 
    result {
        Logging.info "Начало прогрева %M\"C" destTemperature
        do! Termo.setSetpoint destTemperature
        let! resTemp = 
            SetupTermoHelpers.loop 
                {   destT = destTemperature
                    startTime = DateTime.Now } 
                isKeepRunning 
                work        
        Logging.info "Прогрев %M\"C завершён с температурой %M\"C" destTemperature resTemp 
        return () } 
    |> Result.mapErr(fun err -> 
        Logging.error "Прогрев %M\"C завершён с ошибкой : %s" destTemperature err
        err )


module Hart = 
    let rec tryGetData = function
        | [] -> []
        | _::_::0xFFuy::0xFFuy::b1::bytes when b1<>0xFFuy-> b1::bytes
        | _::bytes -> tryGetData bytes

    let getData = function
        | [] -> Err "нет ответа"
        | bytes -> 
            match tryGetData bytes with        
            | List.Rev (crc :: dataBytes)  ->
                if crc = List.reduce (^^^) dataBytes then 
                    Ok bytes
                else 
                    sprintf "несоответствие CRC: %s" (bytesToStr bytes )
                    |> Err
            | _ -> 
                bytesToStr bytes 
                |> sprintf "некорректные данные: %s" 
                |> Err

    let private getResponse = 
        Comport.getResponse  Config.App.config.Main.ComportHart
            
    let on() = result {
        let! response = 
            getResponse 
                [|  0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 
                    0x02uy; 0x00uy; 0x00uy; 0x00uy; 0x02uy; |] 
        let fail s = 
            sprintf "ответ на запрос инициализации %s не соответсвует образцу %s" (bytesToStr response) s
            |> Err
        
        match Array.toList response with
        |   0x06uy::0x00uy::0x00uy::0x18uy::_::_::0xFEuy::_::_::_::_::_::_::_::_::b9::b10::b11::rx ->
            match List.rev rx with
            | _::0x01uy::0x93uy::0x60uy::0x93uy::0x60uy::_ ->
                return (b9,b10,b11)
            | _ -> 
                return! 
                    fail """ 0x06uy::0x00uy::0x00uy::0x18uy::_::_::
                    0xFEuy::_::_::_::_::_::_::_::_::
                    b9::b10::b11::_::
                    0x01uy::0x93uy::0x60uy::0x93uy::0x60uy::_"""
                                        
        | _ -> 
            return! 
                fail "0x06uy::0x00uy::0x00uy::0x18uy::_::_::0xFEuy::_::_::_::_::_::_::_::_::b9::b10::b11::*"
    }

    let private txff = [|0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; |]


    let readConc addy = result {
        let xb1, xb2, xb3 = addy
        let tx =    
            [|  0x82uy; 0x22uy; 0xB4uy; xb1; xb2; xb3; 
                0x01uy; 0x00uy; |]                
        let tx = 
            [|  yield! txff
                yield! tx; 
                yield Array.reduce (^^^) tx |]
        let! response = getResponse tx
        match Array.toList response with
        | 0x86uy::0x22uy::0xB4uy::b1::b2::b3::0x01uy::0x07uy::_::_::dx::cb1::cb2::cb3::cb4::_ ->
            let xs = [|cb4;cb3;cb2;cb1|]
            return!
                try
                    BitConverter.ToSingle (xs, 0) |> float |> Ok
                with e ->
                    sprintf "не удалось преобразовать %s в число: %s, ответ %s" (bytesToStr xs) e.Message (bytesToStr response)
                    |> Err
        | _ -> 
            return! 
                sprintf "ответ на запрос концентрации %s не соответсвует образцу 0x86uy::0x22uy::0xB4uy::b1::b2::b3::0x01uy::0x07uy::_::_::dx::cb1::cb2::cb3::cb4::_"
                    (bytesToStr response)
                |> Err
        }

    let off =
        let pat1 = """не соответсвует образцу 
            0x86uy::0x22uy::0xB4uy::b1::b2::b3::
            0x80uy::0x06uy::_::_::br0::br1::br2::br3::_"""
        fun addy -> result {
            let xb1, xb2, xb3 = addy
            let br = 
                BitConverter.GetBytes(9600.f) 
                |> Array.rev
            assert (br.Length=4)
            let tx =    
                    [|  yield!  [|  0x82uy; 0x22uy; 0xB4uy; xb1; xb2; xb3; 
                                    0x80uy; 0x04uy;  |] 
                        yield! br |]
            let tx = 
                [|  yield! txff
                    yield! tx; 
                    yield tx |> Array.reduce (^^^) |]
            let! response = getResponse tx
            let what = sprintf "ответ на запрос переключения скорости обмена на 9600 %s" (bytesToStr response)
            match Array.toList response with    
            | 0x86uy::0x22uy::0xB4uy::b1::b2::b3::0x80uy::0x06uy::_::_::br0::br1::br2::br3::_  ->
                let brs = [br0; br1; br2; br3]
                if brs = Array.toList br then 
                    return ()
                else
                    return!
                        sprintf "%s: %s != %s"
                            what (bytesToStr brs) (bytesToStr br)
                        |> Err
            | _ -> 
                return!  sprintf "%s %s" what pat1 |> Err
        }
        