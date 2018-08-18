open XYArray
open Domain
open Game
open System

[<EntryPoint>]
let main argv =
    let blockFields = [0..8] |> List.map (fun v->(19s<x>, y.lift v))
    let board = Option.get (XYArray.setMulti Field.Block initBoard blockFields)
    let b,s = evaluateBoardProgression board (Score 0u)
    printfn "%A %d" b (Score.getValue s)
    0 // return an integer exit code