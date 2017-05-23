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

let Cchlin0 = 16
let Cchlin1 = 17
let Cchlin2 = 18
let Cchlin3 = 19

let ChtNull0    = 23
let ChtNull1    = 24
let ChtNull2    = 25


let KChtSens0   = 26
let KChtSens1   = 27
let KChtSens2   = 28

let AddrMil     = 33

let KChtMid0    = 37
let KChtMid1    = 38
let KChtMid2    = 39

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

        Cchlin0, "Коэфф. при 0-ой степени кривой линеаризации"
        Cchlin1, "Коэфф. при 1-ой степени кривой линеаризации"
        Cchlin2, "Коэфф. при 2-ой степени кривой линеаризации"
        Cchlin3, "Коэфф. при 3-ей степени кривой линеаризации"

        ChtNull0, "Коэфф. при 0-ой степени полинома коррекции нуля от температуры"
        ChtNull1, "Коэфф. при 1-ой степени полинома коррекции нуля от температуры"
        ChtNull2, "Коэфф. при 2-ой степени полинома коррекции нуля от температуры"

        KChtSens0, "Коэфф. при 0-ой степени полинома коррекции чувствительности от температуры"
        KChtSens1, "Коэфф. при 1-ой степени полинома коррекции чувствительности от температуры"
        KChtSens2, "Коэфф. при 2-ой степени полинома коррекции чувствительности от температуры"

        AddrMil,    "MODBUS модбас адрес датчика"

        KChtMid0,   "Коэфф. при 0-ой степени полинома кор. середины шкалы от температуры"
        KChtMid1,   "Коэфф. при 1-ой степени полинома кор. середины шкалы от температуры"
        KChtMid2,   "Коэфф. при 2-ой степени полинома кор. середины шкалы от температуры"

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
        

