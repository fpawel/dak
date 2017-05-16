module Programm

open System
open System.IO
open System.Windows.Forms
open System.Threading

open Dak.View
open Dak.ViewModel

type Guid = Runtime.InteropServices.GuidAttribute

let main () = 
    try 
        Application.EnableVisualStyles() 
        Application.SetCompatibleTextRenderingDefault true 
        
        let config = Config.App.config
        let party = Dak.AppData.party
        let form = MainWindow.form

        MainWindow.initialize()
        TabPages.setSelected MainWindow.TabsheetParty
        TopBar.initialize()
        Products.initialize()        
        Scenary.initialize()
        Thread2.initialize()
        Chart.initialize()
        Report.initialize()
        Menus.initialize()
        Ref.``check referencies was initialized``()
        
        Application.Run MainWindow.form 
        Dak.AppData.save()
        
        
    with e ->
//        Logging.error "%A" e 
        MessageBox.Show( sprintf "%A" e ,"Исключение", MessageBoxButtons.OK, MessageBoxIcon.Error ) |> ignore   
    Config.App.save()


let onAnotherInstanceExist() = 
    MessageBox.Show( "Нельзя создать более одного экземпляра приложения" ,"Производство СТМ-30М", MessageBoxButtons.OK, MessageBoxIcon.Information ) |> ignore   
    

let mutexid = 
    let ax = Reflection.Assembly.GetExecutingAssembly().GetCustomAttributes(typeof<Guid>,true)
    let a = ax.[0] :?> Guid
    let id = a.Value.ToString()
    "Global\\" + id

open System.Security.Principal

open System.Security.AccessControl

[<EntryPoint>]
[<STAThread>]
do
    
    use mutex = new Mutex(false, mutexid)
    let si = new SecurityIdentifier(WellKnownSidType.WorldSid, null)

    let allowEveryoneRule =             
        new MutexAccessRule(si, MutexRights.FullControl, AccessControlType.Allow)   
    let securitySettings = new MutexSecurity();
    securitySettings.AddAccessRule(allowEveryoneRule);
    mutex.SetAccessControl(securitySettings)
        
    try
        let hasHandle = mutex.WaitOne(0, false);
        if not hasHandle then onAnotherInstanceExist() else
        main()
    with
    | :? AbandonedMutexException ->
        onAnotherInstanceExist()