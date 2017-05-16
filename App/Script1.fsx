let pairs (ns : int []) = 
    [   for nx, x in Seq.indexed ns do
            for y in ns.[nx+1..] do
                yield x,y]

let triples ns =    
    let mutable triples = []    
    let mutable ones = []    
    let mutable pairs = pairs ns
    let del k = pairs <- List.filter( (<>) k) pairs
    while not pairs.IsEmpty do
        let (x,y) as xy = pairs.Head
        del xy
        match List.tryFind(fun (a,_) -> a = y) pairs with 
        | None -> 
            ones <- xy :: ones 
        | Some ((a,b) as ab) -> 
            del ab
            match List.tryFind((=) (x,b) ) pairs  with 
            | None ->  
                triples <- [xy; ab] :: triples 
            | Some xb -> 
                del xb
                triples <- [ xy; ab; xb ] :: triples 
    pairs <- ones
    
    while not pairs.IsEmpty do
        let (x,y) as xy = pairs.Head
        del xy
        match List.tryFind(fun (a,b) -> a = x || a = y || b=x || b = y) pairs with 
        | None -> 
            triples <- [xy] :: triples 
        | Some ((a,b) as ab) -> 
            del ab
            triples <- [xy; ab] :: triples 
    triples

let testElem xy = 
    List.filter( List.exists( (=) xy) )
    >> function 
        | [_] -> ()
        | ex -> failwithf ": %A" ex

let sort input = 
    let mutable input = List.sortByDescending List.length input
    let mutable result = []    
    let (~%%) = 
        List.map(fun (x,y) -> [x;y])
        >> List.concat
        >> Set.ofList
    let mutable s = %% ( List.head input)
    while List.isEmpty input |> not do  
        let xy = 
            input 
            |> List.filter(fun xs -> 
                Set.intersect (%% xs) s |> Set.isEmpty
                )
            |> List.tryHead
            |> Option.defaultValue input.Head
        result <- xy :: result
        s <- %% xy
        input <- List.filter ( (<>) xy ) input
    result

open System

let rng = new Random()

let shuffle (org:_[]) = 
    let arr = Array.copy org
    let max = (arr.Length - 1)
    let randomSwap (arr:_[]) i =
        let pos = rng.Next(max)
        let tmp = arr.[pos]
        arr.[pos] <- arr.[i]
        arr.[i] <- tmp
        arr   
    [|0..max|] |> Array.fold randomSwap arr

let test ns =
    let xs =
        ns 
        |> triples
        |> sort
    let count =
        xs 
        |> List.map (
            List.map(fun (x,y) -> [x;y])
            >> List.concat )
        |> List.concat
        |> List.length
    printfn "count - %d" count   
    List.iteri (printfn "%d : %A") xs
    let pairs = pairs ns
    for x,y in pairs do 
        testElem (x,y) xs

test [|1;2;3;4;8;7;6;5|]

test <| shuffle [|1..8|]