module Dak.Coef

open System
open System.ComponentModel
open System.Text.RegularExpressions

let SoftVer     = 0
let ProductType = 1
let Year        = 2
let Serial      = 3
let Kef4        = 4
let Units       = 5
let Gas         = 6
let Scale       = 7
let Scale0      = 8
let ScaleEnd    = 9
let PGS1        = 10
let PGS3        = 11
let AddrMil     = 33
let ProductTypeCode
                = 323
let Range       = 334
let Porog1      = 325
let Porog2      = 328
let Offset1     = 335
let Offset2     = 336
let I4          = 337
let I20         = 338

let private info = 
    [   SoftVer,    "Версия программы"
        ProductType,"Тип прибора"
        Year,       "Год выпуска"
        Serial,     "Серийный номер"
        Kef4,       "Максимальное число регистров в таблице регистров прибора"
        Units,      "Единицы измерения"
        Gas,        "Тип газа"
        Scale,      "Тип шкалы"
        Scale0,     "Начало шкалы"
        ScaleEnd,   "Начало конец"
        PGS1,       "ПГС1"
        PGS3,       "ПГС3"
        AddrMil,    "MODBUS модбас адрес датчика"
        ProductTypeCode,       
                    "Исполнение ДАК-М"
        Range,      "Диапазон измерений ДАК-М"
        Porog1,     "Порог 1 ДАК-М"
        Porog2,     "Порог 2 ДАК-М"
        Offset1,    "Смещение нуля тока ДАК-М"
        Offset2,    "Смещение конца шкалы тока ДАК-М"
        I4,         "Ток начала шкалы (4 мА) ДАК-М"
        I20,        "Ток конца шкалы (20 мА) ДАК-М"
    ]

let values = 
    List.map fst info  

let what = 
    let m = Map.ofList info
    fun n ->
        match m.TryFind n with
        | Some x -> x
        | _ -> ""

let reg x = 224 + 2 * x
let cmd x = ( 0x80 <<< 8 ) + x
        

