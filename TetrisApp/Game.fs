module Game
    open XYArray
    open Domain
    open ListExt
   
    let generateActiveBlock blockDef random (sizeX:int16<x>) =
        let el = getRandomElement random blockDef
        el |> Set.map (fun c -> { c with X=c.X+(sizeX/2s) })
    
    let rotateActiveBlock inProgressState direction = 
        let rotateBlock (block:Block) (angle:float) : Block=
            block 
            |> Set.map (fun c -> 
                {
                    X=x.lift(removeUnit c.X * (int16) (cos angle) - removeUnit c.Y * (int16) (sin angle));
                    Y=y.lift(removeUnit c.Y * (int16) (cos angle) + removeUnit c.X * (int16) (sin angle))
                })
        let isInBound block =
            block |> Set.forall (fun c -> 
                c.X<=inProgressState.Board.maxX
                && c.X >= 0s<x>
                && c.Y<=inProgressState.Board.maxY
                && c.Y >= 0s<y>)
        let rotateActiveBlock' inProgressState angle=
            let ab=rotateBlock inProgressState.ActiveBlock angle
            if isInBound ab then
                {inProgressState with ActiveBlock=ab}
            else inProgressState
        match direction with
        | Left -> rotateActiveBlock' inProgressState (System.Math.PI/2.0)
        | Right -> rotateActiveBlock' inProgressState (-System.Math.PI/2.0)

    let loop random userInput state = 
        match (state, userInput) with
        | _, UserInput.Restart -> InProgress {Board = initBoard; Score= Score 0u; ActiveBlock=(generateActiveBlock blocks random 10s<x>)}
        | Start, _
        | End _, _ -> state
        | InProgress _, UserInput.None -> 
            state
        | InProgress inProgressState, UserInput.Rotate direction ->
            InProgress (rotateActiveBlock inProgressState direction)

    let print state = 
        match state with
        | Start -> printfn "Press s to start"
        | InProgress progress -> 
            for y in [0s .. (int16)progress.Board.maxY - 1s] |> List.map (y.lift) do
                printf "|"
                for x in [0s .. (int16)progress.Board.maxX  - 1s] |> List.map (x.lift) do
                    if progress.ActiveBlock.Contains { X = x; Y= y} 
                        then "X" 
                        else if XYArray.get x y progress.Board = Some Empty then " " else "*"
                    |> printf "%s" 
                printfn "|"
            let (Score score) = progress.Score in printfn "Score: %d" score
            printfn "%A" progress.ActiveBlock
        | End (Score score) -> printfn "Score: %d" score