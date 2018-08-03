// Learn more about F# at http://fsharp.org

open System


module Domain =
    [<Measure>] type x
    [<Measure>] type y

    module XYArray =
        (*
           0  1  2 
           3  4  5
           6  7  8
           9  10 11

          len = 12

          x = 3, y = 0

        *)
        type 'a xyArray = private { arr: 'a array; maxX : int }
        with member arr.maxY = arr.arr.Length / arr.maxX

        let private getXY point maxX=
            let x = int16 (point % maxX) * 1s<x>
            let y = int16 (point / maxX) * 1s<y>
            x, y    

        let init (x : int16<x>) (y:int16<y>) f =
            let x = int x
            let y = int y
            { arr = Array.init (x * y) (fun point -> 
                let x, y = getXY x point
                f x y); maxX = x }


        let get x y arr = ()
        let set x y arr = ()

    type Field = Empty | Block
    type Board = Field [,] 
    type Coordinate = {X:int; Y:int}
    type Block = Coordinate Set
    let initBoard = Array2D.init  20 10 (fun _ _ -> Empty)

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
                    Seq.mapi (fun x ch -> if ch = ' ' then None else  Some {X = x; Y = y}) 
                    >> Seq.choose id)
                >> Seq.collect id
                >> Set)

module List =
    let getRandomElement (random:System.Random) l =
        let pos = random.Next(List.length l - 1)
        l.[pos]

module Game =
    open Domain
    
    let generateActiveBlock blockDef random sizeX =
        let el = List.getRandomElement random blockDef
        el |> Set.map (fun c -> { c with X=c.X+(sizeX/2) })

    let loop random userInput state = 
        match (state, userInput) with
        | _, UserInput.Restart -> InProgress {Board = initBoard; Score= Score 0u; ActiveBlock=(generateActiveBlock blocks random 10)}
        | Start, _
        | End _, _ -> state
        | InProgress progress, UserInput.None -> 
            state

    let print state = 
        match state with
        | Start -> printfn "Press s to start"
        | InProgress progress -> 
            for y = 0  to Array2D.length1 progress.Board - 1 do
                printf "|"
                for x = 0  to Array2D.length2 progress.Board  - 1 do
                    if progress.ActiveBlock.Contains { X = x; Y= y} 
                        then "X" 
                        else if progress.Board.[y, x] = Empty then " " else "*"
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