module Stend6026

let private config = Config.App.config.Main

let switch addr =
    Logging.info "Стенд 6026, переключение #%d" addr 
    let stendModbusReg = (addr-1)*2   
    match Mdbs.read3bytes config.ComportProducts config.Stend.Addr stendModbusReg 2 with
    | Err x -> Err x
    | Ok _ -> Ok ()

let read addr  = 
    
    let stendModbusReg = (addr-1)*2    
    match Mdbs.read3bytes config.ComportProducts config.Stend.Addr stendModbusReg 2 with
    | Err x -> Err x
    | Ok (d1::d2::_::d4::_) ->
        let (~%%) = decimal         
        let current = ( (%% d1) * 256m + (%% d2) )/100m
        let (<->) x n = (x &&& n)=0uy
        let p1 = d4 <-> 0b010uy
        let p2 = d4 <-> 0b100uy 
        if config.Stend.ShowLogs then
            Logging.info "#%d, 6026, Iвых=%M, ПОРОГ1=%b, ПОРОГ2=%b" addr current p1 p2
            
        Ok (current, p1, p2)
    | Ok (BytesToStr s) -> 
        sprintf "Неправильный ответ стенда 6026: %s" s
        |> Err
