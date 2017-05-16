module Hart

open System
open System.Diagnostics.Contracts



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
            
let init() = result {
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



let switchOff =
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