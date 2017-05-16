module Dak.Repository

open System
open System.IO
open System.Collections.ObjectModel

open MBrace.FsPickler

module Path = 
    let rootFolder = "ProductionData"
    let root = 
        let x = Path.Combine(IO.Path.ofExe, rootFolder)
        createDirectory x
        x
    let batch, year, month, day = IO.Path.ofDateTime rootFolder

type PartyPath = 
    {   ID : ID 
        Date : DateTime }
    static member getFolderPath (canCreate,h:PartyPath)  = 
        //getPartyFolderPath canCreate h.Date h.Id

        let (~%%) = string
        let month = h.Date.Date.ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
        let path = Path.Combine(Path.batch false h.Date.Date, h.ID )
        if canCreate then
            createDirectory path
        path

    static member getFilePath (canCreate,h)  =             
        [|  PartyPath.getFolderPath (canCreate,h)
            sprintf "%s.party"  h.ID |]
        |> Path.Combine
        

    member x.GetFolderPath canCreate  = 
        PartyPath.getFolderPath (canCreate,x)

    member x.GetFilePath canCreate  = 
        PartyPath.getFilePath (canCreate,x)

    static member fromPartyHead (x:PartyInfo) = 
        {   ID = x.ID
            Date = x.Date }

type PartyInfo with 
    static member path = PartyPath.fromPartyHead 
    member x.Path = PartyPath.fromPartyHead x

    

[<AutoOpen>]
module private Helpers = 
    let binarySerializer = FsPickler.CreateBinarySerializer()

    let doWithFile<'a> pathToFile (x:FileMode) (f : FileStream -> 'a) = 
        try
            let file = new FileStream( pathToFile, x ) 
            let r = f file 
            file.Close()
            Ok r
        with e ->             
            Logging.debug "Ошибка обращения к файлу %s, %A, %A" pathToFile x e 
            Err e.Message    
        
    let pathToCorruptedFiles = lazy (                
        let x = Path.Combine( IO.Path.ofExe, "__CORRUPTED__" ) 
        if Directory.Exists x |> not then 
            Directory.CreateDirectory x |> ignore
        x )

    let moveCorruptedFiles (src:string) = 
        let filename = Path.GetFileName src
        Logging.warn "Файл %s повреждён!" filename 
        try 
            let dest = ( Path.Combine(pathToCorruptedFiles.Force(), filename) )
            if File.Exists dest then
                File.Delete dest |> ignore
            File.Move( src, dest ) 
        with e ->
            Logging.error  "Ошибка переноса файла %s, %A" filename e.Message 
            Logging.debug  "%A" e
    
let productFolderPath (canCreate, h, productId) =        
        let partyFolderPath  = PartyPath.getFolderPath(canCreate,h)
        let path = Path.Combine( partyFolderPath, sprintf "%s.product" productId)
        if canCreate then
            createDirectory path
        path

let varPath (canCreate,h,productId, var ) = 
        let productPath = productFolderPath(canCreate,h,productId)
        let fileName = sprintf "%s.series" <| MilVar.name var
        Path.Combine(productPath, fileName ) 

let readPartiesHeaders() =
    Directory.GetFiles( Path.root, "*.party", SearchOption.AllDirectories)
    |> Array.choose( fun filename -> 
        let r = doWithFile filename FileMode.Open <| fun stream ->
            binarySerializer.Deserialize<PartyInfo>(stream)
        match r  with
        | Err x ->  
            moveCorruptedFiles filename                    
            None
        | Ok x -> Some ( x.ID, x) )
    |> Map.ofArray

module Party =
    open Alchemy
    type SaveDataType = PartyInfo * PartyData
    
    let open_ h : Result<Party,string> =
        let fileName = PartyPath.getFilePath(false, PartyPath.fromPartyHead h)
        if File.Exists fileName |> not then
            Err (sprintf "файл  %A не найден" fileName)
        else
            doWithFile fileName FileMode.Open <| fun stream ->
                let head = binarySerializer.Deserialize<PartyInfo> (stream, leaveOpen = true)
                let data = binarySerializer.Deserialize<PartyData> (stream)
                stream.Close()
                Party.NewWith head data
                
    let save (party_ : Party) = 
        let party = party_.WithProducts party_.PartyData.Products
        doWithFile 
            (PartyPath.getFilePath(true, PartyPath.fromPartyHead party.PartyInfo)) 
            FileMode.Create <| fun stream -> 
            binarySerializer.Serialize
                (   stream, 
                    { party.PartyInfo with 
                        Serials = List.map Product.GetSerial party.PartyData.Products}, 
                    leaveOpen = true)
            binarySerializer.Serialize( stream, party.PartyData)
            stream.Close()    

    let createNew() =
        let p = Party.NewWithOneProduct
        Result.map (fun () -> p) (save p )
    
    let openById partyId partiesHeaders  = 
        if Map.isEmpty partiesHeaders then createNew() else
        
        partiesHeaders
        |> Map.tryFind partyId
        |> Option.getWithDefault( fun () -> 
            Logging.warn "партия %A отсутсвует в репозитории"  partyId
            partiesHeaders |> Map.toList |> List.map snd 
            |> List.maxBy ( fun (x:PartyInfo) -> x.Date ) )
        |> open_
        
        

    let delete id partiesHeaders =
        partiesHeaders |> Map.tryFind id        
        |> Option.map(fun h ->
            let partyFolderPath = PartyPath.getFolderPath (false,h)
            if Directory.Exists partyFolderPath |>  not then 
                Err ("файл удаляемой партии не найден : " + partyFolderPath) 
            else
                try            
                    Directory.Delete( partyFolderPath, true )
                    Ok ()
                with e ->
                    Err e.Message )
        |> Option.getWithDefault(fun () -> 
            Err "удаляемая партия отсутствует в базе данных" )

let partiesHeadersDateTree partiesHeaders =
    let par x y = x,y
    partiesHeaders
    |> Seq.groupBy( fun (x : PartyInfo) -> x.Date.Year )
    |> Seq.sortBy fst
    |> Seq.map( fun ( year, xs) -> 
        xs 
        |> Seq.groupBy( fun x -> x.Date.Month )
        |> Seq.sortBy fst
        |> Seq.map( fun ( month, xs) ->  
            xs 
            |> Seq.groupBy( fun x -> x.Date.Day )
            |> Seq.sortBy fst
            |> Seq.map( fun ( day, xs) -> par day xs )
            |> par month )
        |> par year )