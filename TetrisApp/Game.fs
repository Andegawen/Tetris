module Game
    open XYArray
    open Domain
   
    let generateActiveBlock blockDef random (sizeX:int16<x>) =
        let el = ListExt.getRandomElement random blockDef
        el |> Set.map (fun c -> { c with X=c.X+(sizeX/2s) })
    
    let isBlockInBound inProgressState block =
            block |> Set.forall (fun c -> 
                c.X<=inProgressState.Board.maxX
                && c.X >= 0s<x>
                && c.Y<=inProgressState.Board.maxY
                && c.Y >= 0s<y>)

    let rotateActiveBlock inProgressState direction = 
        let rotateBlock (block:Block) (angle:float) : Block=
            let elXmin = block |> Set.toList |> List.minBy (fun c->c.X);
            let elYmax = block |> Set.toList |> List.maxBy (fun c->c.Y);
            let shift = {X=elXmin.X; Y=elYmax.Y}
            block 
            |> Set.map (fun c -> 
                let coordinateShiftedToOrigin = {X= (c.X - shift.X); Y = (c.Y - shift.Y)}
                let xv = XYArray.removeUnit coordinateShiftedToOrigin.X
                let yv = XYArray.removeUnit coordinateShiftedToOrigin.Y
                let cooridinateRotatedInOrigin = {
                    X=x.lift(xv * (int16) (cos angle) - yv  * (int16) (sin angle));
                    Y=y.lift(yv * (int16) (cos angle) + xv * (int16) (sin angle))
                }
                let coordinateRotatedAndShiftedToOriginal = 
                    {X= cooridinateRotatedInOrigin.X + shift.X; Y=cooridinateRotatedInOrigin.Y + shift.Y}
                coordinateRotatedAndShiftedToOriginal)

        let rotateActiveBlock' inProgressState angle=
            let ab=rotateBlock inProgressState.ActiveBlock angle
            if isBlockInBound inProgressState ab then
                {inProgressState with ActiveBlock=ab}
            else inProgressState
        match direction with
        | Left -> rotateActiveBlock' inProgressState (System.Math.PI/2.0)
        | Right -> rotateActiveBlock' inProgressState (-System.Math.PI/2.0)

    let moveBlock block shift =
        let (x,y) = shift
        block
        |> Set.map (fun c -> {X=c.X+x; Y=c.Y+y})
    let moveActiveBlock inProgressState direction =
        
        let moveActiveBlock' inProgressState shift = 
            let ab=moveBlock inProgressState.ActiveBlock shift
            if isBlockInBound inProgressState ab then
                {inProgressState with ActiveBlock=ab}
            else inProgressState
        match direction with
        | Left -> moveActiveBlock' inProgressState (-1s<x>,0s<y>)
        | Right -> moveActiveBlock' inProgressState (1s<x>,0s<y>)

    let loop random userInput state = 
        match (state, userInput) with
        | _, UserInput.Restart -> InProgress {Board = initBoard; Score= Score 0u; ActiveBlock=(generateActiveBlock blocks random 10s<x>)}
        | Start, _
        | End _, _ -> state
        | InProgress _, UserInput.None -> 
            state
        | InProgress inProgressState, UserInput.Rotate direction ->
            InProgress (rotateActiveBlock inProgressState direction)
        | InProgress inProgressState, UserInput.Move direction ->
            InProgress (moveActiveBlock inProgressState direction)

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