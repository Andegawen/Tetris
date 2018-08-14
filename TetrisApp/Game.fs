module Game
    open XYArray
    open Domain
    open ListExt
   
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

    let fallDownBlock (board:Board) (activeBlock:Block) =
        let findYShift (board:Board) (block:Block) : int16<y> =
            let ``x to the most bottom Y from block`` =
                block
                |> Set.toList
                |> List.groupBy (fun c->c.X)
                |> List.map (fun (x,col)-> (x,(List.maxBy (fun c->c.Y) col).Y))
            
            let findTheMostTopNonEmptyY (xv:int16<x>) (startY:int16<y>) (board:Board) =
                [(XYArray.removeUnit startY) .. (XYArray.removeUnit board.maxY)-1s]
                |> List.map XYArray.y.lift
                |> List.map (fun yv -> (yv, XYArray.get xv yv board))
                |> List.filter (fun (yv, bv) -> bv = Some Field.Block)
                |> ListExt.minBy (fun (yv,bv) -> yv)
                |> Option.bind (fun (yv, bv)-> Some yv)
            
            //find non empty board Y
            ``x to the most bottom Y from block``
            |> List.map (fun (xv,yv)-> 
                let yBoard = findTheMostTopNonEmptyY xv (yv+1s<y>) board
                let shift = match yBoard with
                            | Some value -> value-(1s<y>)-yv
                            | None -> board.maxY-(1s<y>)-yv
                shift)
            |> List.min
        
        let theSmallestShiftY = findYShift board activeBlock
        let block = moveBlock activeBlock (0s<XYArray.x>, theSmallestShiftY)
        
        let coords = block |> Set.toList |> List.map (fun c->(c.X,c.Y))
        Option.get (XYArray.setMulti coords Field.Block board)

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
        | InProgress inProgressState, UserInput.FallDown ->
            let evaluateBoardProgression board score =
                // let groupedByX =
                //     board
                //     |> XYArray.toSeq
                //     |> seq.groupBy (fun ((x,y),v)->x)
                //     |> seq.toList
                // let rows = 
                //     groupedByX
                //     |> List.filter (fun (g,c)->List.length c = board.maxX)
                (board, score)
            let isBlockClashing block board =
                block                
                |> Set.map (fun c -> XYArray.get c.X c.Y board)
                |> Set.exists (fun field -> field = (Some Field.Block))
                
            let boardAfterBlockFallDown = fallDownBlock inProgressState.Board inProgressState.ActiveBlock
            let (boardAfterLinesEval,score) = evaluateBoardProgression boardAfterBlockFallDown inProgressState.Score
            let newActiveBlock = generateActiveBlock blocks random 10s<x>
            if (isBlockClashing newActiveBlock boardAfterLinesEval) 
            then 
                End score
            else
            InProgress {Board =boardAfterLinesEval; ActiveBlock = newActiveBlock; Score=score}

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