module Dak.Operations.Delay

open System
open Thread2
open Dak.Operations.PartyWorks

let onStart = Ref.Initializable<_>(sprintf "Delay.start %s:%s" __LINE__ __SOURCE_FILE__ )
let onStop = Ref.Initializable<_>(sprintf "Delay.stop %s:%s" __LINE__ __SOURCE_FILE__ )
let onUpdate = Ref.Initializable<_>(sprintf "Delay.stop %s:%s" __LINE__ __SOURCE_FILE__ )

let mutable private keepRunning = false

let cancel() = keepRunning <- false

let perform (what:string) interrogate gettime  = 
    onStart.Value what gettime
    keepRunning <- true
    let start'time = DateTime.Now
    let result = 
        maybeErr{
            while keepRunning && isKeepRunning() && (DateTime.Now - start'time < gettime()) do
                onUpdate.Value start'time gettime
                if interrogate then
                    do! party.Interrogate()
                else
                    Threading.Thread.Sleep 10 }
    keepRunning <- false
    onStop.Value() 
    result

