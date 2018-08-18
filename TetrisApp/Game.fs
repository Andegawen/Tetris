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
                |> List.filter (fun (yv, bv) -> bv = (Some Domain.Field.Block))
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
        Option.get (XYArray.setMulti Field.Block board coords)
    let evaluateBoardProgression board (score:Score) :(Board*Score) =
        let blockBoardRepresentation = 
            board
            |> XYArray.toSeq
            |> Seq.map (fun ((x,y),v)-> ({X=x;Y=y},v))
            |> Seq.filter (fun (c,v)->v= Field.Block)
            |> Seq.map (fun (c,v)->c)
            |> Seq.toList
        let fullRows =
            blockBoardRepresentation
            |> List.groupBy (fun c->c.Y)
            |> List.filter (fun (groupY,c)-> (int16)(List.length c) = (removeUnit board.maxY))
            |> List.map (fun (groupY,c)-> groupY)
            |> List.sort
        let rec getRanges minY maxY ys =
            match ys with
            | el1 :: tail -> seq {yield (minY, el1); yield! (getRanges el1 maxY tail)}
            | [] -> seq {yield (minY,maxY)}
        let isInRange (value:int16<y>) (r:int16<y>*int16<y>) =
            let a,b =r
            let lhs = abs(value-a)+abs(b -value)
            let rhs = abs(b-a)
            lhs = rhs
        let newBoard = 
            let ranges =
                fullRows 
                |> getRanges 0s<y> board.maxY
                |> Seq.toList
                |> Seq.mapi (fun it (min, max) -> (it, (min, max)))
                |> Seq.toList
            blockBoardRepresentation
            |> List.groupBy(fun c-> 
                Option.get (List.tryFind (fun (it,r)->isInRange (c.Y) r) ranges))
            |> List.collect(fun ((it,_), group) -> List.map(fun p->{p with Y=p.Y+y.lift it}) group)
            |> List.map (fun s->(s.X, s.Y))
            |> XYArray.setMulti Field.Block board
            |> Option.get
        let scoreValue = (Score.getValue score);
        let valueToAdd = (List.length fullRows)*100
        let newscore = Score (scoreValue + (uint32)(valueToAdd))
        (newBoard, newscore)

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
            let isBlockClashing block board =
                block                
                |> Set.map (fun c -> XYArray.get c.X c.Y board)
                |> Set.exists (fun field -> field = (Some Field.Block))

            let boardAfterBlockFallDown = fallDownBlock inProgressState.Board inProgressState.ActiveBlock
            let (boardAfterLinesEval,newScore) = evaluateBoardProgression boardAfterBlockFallDown inProgressState.Score
            let newActiveBlock = generateActiveBlock blocks random 10s<x>
            if (isBlockClashing newActiveBlock boardAfterLinesEval) 
            then 
                End newScore
            else
            InProgress {Board =boardAfterLinesEval; ActiveBlock = newActiveBlock; Score=newScore}

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