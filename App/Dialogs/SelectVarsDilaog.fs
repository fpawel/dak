namespace Dak.Dialogs
open System
open System.ComponentModel
open Dak

type InterrogateConverter() =
    inherit MyWinForms.Converters.BooleanTypeConverter("Опрашивать", "Не опрашивать")

[<TypeConverter(typeof<ExpandableObjectConverter>)>]
type SelectVars() = 
    let cfg = Config.App.config.View

    [<DisplayName("Конц.")>]
    [<Description("Концентрация")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Conc 
        with get() =
            Set.contains MilVar.conc cfg.InterrogateMilVars
        and set value =
            cfg.InterrogateMilVars <- 
                (if value then Set.add else Set.remove) MilVar.conc cfg.InterrogateMilVars            
            

    [<DisplayName("Iизл")>]
    [<Description("Ток излучателя")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Curr 
        with get() =
            Set.contains MilVar.curr cfg.InterrogateMilVars 
        and set value =
            cfg.InterrogateMilVars <- 
                (if value then Set.add else Set.remove) MilVar.curr cfg.InterrogateMilVars            
            

    [<DisplayName("Var1")>]
    [<Description("Var1")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Var1 
        with get() =
            Set.contains MilVar.var1 cfg.InterrogateMilVars 
        and set value =
            cfg.InterrogateMilVars <- 
                (if value then Set.add else Set.remove) MilVar.var1 cfg.InterrogateMilVars
                

    [<DisplayName("Т\"С")>]
    [<Description("Температура")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Temp 
        with get() =
            Set.contains MilVar.temp cfg.InterrogateMilVars 
        and set value =
            cfg.InterrogateMilVars <- 
                (if value then Set.add else Set.remove) MilVar.temp cfg.InterrogateMilVars            
            

    [<DisplayName("Uраб")>]
    [<Description("Рабочий канал")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Workk 
        with get() =
            Set.contains MilVar.workk cfg.InterrogateMilVars 
        and set value =
            cfg.InterrogateMilVars <- 
                (if value then Set.add else Set.remove) MilVar.workk cfg.InterrogateMilVars            
            

    [<DisplayName("Uоп")>]
    [<Description("Опорный канал")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Refk 
        with get() =
            Set.contains MilVar.refk cfg.InterrogateMilVars 
        and set value =
            cfg.InterrogateMilVars <- 
                (if value then Set.add else Set.remove) MilVar.refk cfg.InterrogateMilVars            

    [<DisplayName("I вых. дак.")>]
    [<Description("Ток выхода ДАК и состояния контактов реле")>]
    [<TypeConverter (typeof<InterrogateConverter>) >]
    member x.Idak
        with get() =
            cfg.InterrogateStend6026
        and set value =
            cfg.InterrogateStend6026 <- value
            

    override x.ToString() = cfg.InterrogateMilVars |> Seq.toStr ", " MilVar.name