open System
open XYArray

module Domain =    
    type Field = Empty | Block
    type Board = xyArray<Field>
    type Coordinate = {X:int16<x>; Y:int16<y>}
    type Block = Coordinate Set

    let initBoard = XYArray.init  20s<x> 10s<y> (fun _ _ -> Empty)

    type Score = Score of uint32
    type InProgress = { Board:Board; Score:Score; ActiveBlock:Block }
    type State = Start | InProgress of InProgress | End of Score

    type Direction = Left | Right
    [<RequireQualifiedAccess>]
    type UserInput = None | Move of Direction | Rotate of Direction | FallDown | Exit | Restart

    let blocks = 
        [
            [ "XXX"
              "  X"];

            [ "XX"
              "XX" ];

           [ "XXXXX"];

           [ "XXX"
             " X"];

           [ "XX"
             " XX"];
        ] |> List.map 
            (List.mapi (fun y -> 
                    Seq.mapi (fun x ch -> if ch = ' ' then None else  Some {X = (int16)x*1s<x>; Y = (int16)y*1s<y>}) 
                    >> Seq.choose id)
                >> Seq.collect id
                >> Set)

module List =
    let getRandomElement (random:System.Random) l =
        let pos = random.Next(List.length l - 1)
        l.[pos]

module Game =
    open Domain
    
    let generateActiveBlock blockDef random (sizeX:int16<x>) =
        let el = List.getRandomElement random blockDef
        el |> Set.map (fun c -> { c with X=c.X+(sizeX/2s) })

    let loop random userInput state = 
        match (state, userInput) with
        | _, UserInput.Restart -> InProgress {Board = initBoard; Score= Score 0u; ActiveBlock=(generateActiveBlock blocks random 10s<x>)}
        | Start, _
        | End _, _ -> state
        | InProgress progress, UserInput.None -> 
            state

    let print state = 
        match state with
        | Start -> printfn "Press s to start"
        | InProgress progress -> 
            for y in [0s .. (int16)progress.Board.maxY - 1s] |> List.map (fun el -> el*1s<y>) do
                printf "|"
                for x in [0s .. (int16)progress.Board.maxsX  - 1s] |> List.map (fun el -> el*1s<x>) do
                    if progress.ActiveBlock.Contains { X = x; Y= y} 
                        then "X" 
                        else if XYArray.get x y progress.Board = Some Empty then " " else "*"
                    |> printf "%s" 
                printfn "|"
            let (Score score) = progress.Score in printfn "Score: %d" score
            printfn "%A" progress.ActiveBlock
        | End (Score score) -> printfn "Score: %d" score


    loop (System.Random ()) UserInput.Restart Start
    |> print
    


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code