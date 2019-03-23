module Game
    open XYArray
    open Domain
    open System
   
    let generateActiveBlock blockDef random (sizeX:int16<x>) =
        let el = ListExt.getRandomElement random blockDef
        el |> Set.map (fun c -> { c with X=c.X+(sizeX/2s) })
    
    let isBlockInBound board block =
        let isCoordinateInBoardFrame (board:Board) coordinate =
            XYArray.get coordinate.X coordinate.Y board 
            |> Option.map (fun field -> field = Field.Empty) = Some true
        block |> Set.forall (fun c -> isCoordinateInBoardFrame board c)

    let leftTopRotationPoint (block:Block) =
        let blockList = block |> Set.toList
        let elXmin = blockList |> List.minBy (fun c-> c.X);
        let elYmax = blockList |> List.maxBy (fun c-> c.Y);
        {X=elXmin.X; Y=elYmax.Y}

    let centerRotationPoint (block:Block)=    
        let blockList = block |> Set.toList
        let getX = fun c->c.X
        let getY = fun c->c.Y
        let elXmin = blockList |> List.minBy (fun c->c.X) |> getX;
        let elXmax = blockList |> List.maxBy (fun c->c.X) |> getX;
        let elYmin = blockList |> List.minBy (fun c->c.Y) |> getY;
        let elYmax = blockList |> List.maxBy (fun c->c.Y) |> getY;

        let xv=Math.Round((double)(elXmax-elXmin)/2.0, MidpointRounding.AwayFromZero)
        let yv=Math.Round((double)(elYmax-elYmin)/2.0, MidpointRounding.AwayFromZero)
        let x = (int)xv |> x.lift
        let y = (int)yv |> y.lift
        let rotPoint = {X=elXmin+x; Y=elYmin+y}
        rotPoint

    let convertTo1based block = block |> Seq.map (fun c->{X=c.X+1s<x>; Y=c.Y+1s<y>})
    let convertTo0based block = block |> Seq.map (fun c->{X=c.X-1s<x>; Y=c.Y-1s<y>})
    let convertYaxis block = block |> Seq.map (fun c->{c with Y=c.Y*(-1s)})
    let rotateBlockDueToPoint (block:Block) (angle:float) (rotationPointDueToOrigin:Block->Coordinate): Block=
        let b=(convertTo1based >> convertYaxis) block |> Set.ofSeq
        let shiftToRotationOrigin = rotationPointDueToOrigin(b)
        let radAngle = angle * Math.PI/180.0
        let rotatedBlock =
            b
            |> Set.map (fun c -> 
                let coordinateShiftedToOrigin = {X= (c.X - shiftToRotationOrigin.X); Y = (c.Y - shiftToRotationOrigin.Y)}
                let xv = XYArray.removeUnit coordinateShiftedToOrigin.X
                let yv = XYArray.removeUnit coordinateShiftedToOrigin.Y
                let cooridinateRotatedInOrigin = {
                    X=x.lift(xv * (int16) (cos radAngle) - yv  * (int16) (sin radAngle));
                    Y=y.lift(yv * (int16) (cos radAngle) + xv * (int16) (sin radAngle))
                }
                let coordinateRotatedAndShiftedToOriginal = 
                    {X= cooridinateRotatedInOrigin.X + shiftToRotationOrigin.X; Y=cooridinateRotatedInOrigin.Y + shiftToRotationOrigin.Y}
                coordinateRotatedAndShiftedToOriginal)
        rotatedBlock 
        |> (convertYaxis >> convertTo0based) |> Set.ofSeq
    
        
    let rotateActiveBlock inProgressState = 
        let rotateActiveBlock' inProgressState angle=
            let ab=rotateBlockDueToPoint inProgressState.ActiveBlock angle centerRotationPoint
            if isBlockInBound inProgressState.Board ab then
                {inProgressState with ActiveBlock=ab}
            else inProgressState
        rotateActiveBlock' inProgressState 90.0

    let moveBlock block shift =
        let (x,y) = shift
        block
        |> Set.map (fun c -> {X=c.X+x; Y=c.Y+y})

    let moveActiveBlock inProgressState direction =
        let moveActiveBlock' inProgressState shift = 
            let ab=moveBlock inProgressState.ActiveBlock shift
            if isBlockInBound inProgressState.Board ab then
                {inProgressState with ActiveBlock=ab}
            else inProgressState
        match direction with
        | Left -> moveActiveBlock' inProgressState (-1s<x>, 0s<y>)
        | Right -> moveActiveBlock' inProgressState (1s<x>, 0s<y>)
        | Down -> moveActiveBlock' inProgressState (0s<x>, 1s<y>)

    // ToDo problem with a first line in the board! Non empty field above don't go down!
    // it's not the issue for other lines
    let fallDownBlock (board:Board) (activeBlock:Block)  : Board=
        let findYShift (board:Board) (block:Block) : int16<y> =
            let ``x to the most bottom Y from block`` =
                block
                |> Set.toList
                |> List.groupBy (fun c->c.X)
                |> List.map (fun (x,col)-> (x, (List.maxBy (fun c->c.Y) col).Y))
            
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
        
        let coords = block |> Set.toList |> List.map (fun c->(c.X, c.Y))
        Option.get (XYArray.setMulti Field.Block board coords)

    let evaluateBoardProgression board (score:Score) :(Board*Score) =
        let blockBoardRepresentation = 
            board
            |> XYArray.toSeq
            |> Seq.map (fun ((x,y),v)-> ({X=x;Y=y}, v))
            |> Seq.filter (fun (c,v)->v= Field.Block)
            |> Seq.map (fun (c,v)->c)
            |> Seq.toList
        let fullRows =
            blockBoardRepresentation
            |> List.groupBy (fun c->c.Y)
            |> List.filter (fun (groupY,c)-> (int16)(List.length c) = (removeUnit board.maxX))
            |> List.map (fun (groupY,c)-> groupY)
            |> List.sort
        let getRanges minY maxY ys =
            let rec getRanges' minY maxY ys =
                match ys with
                | el1 :: tail -> seq {yield (minY, el1); yield! (getRanges' el1 maxY tail)}
                | [] -> seq {yield (minY, maxY)}
            getRanges' minY maxY ys
        let newBoard = 
            let shiftToRanges =
                let ranges = fullRows 
                            |> getRanges 0s<y> (board.maxY-1s<y>)
                            |> Seq.toList;
                let rangeMax = (List.length ranges)-1
                ranges |> List.mapi (fun it (min, max) -> (y.lift (rangeMax-it), (min, max)))
            let isInRange (value:int16<y>) (r:int16<y>*int16<y>) =
                let a,b =r
                let lhs = abs(value-a)+abs(b-value)
                let rhs = abs(b-a)
                lhs = rhs
            blockBoardRepresentation
            |> List.filter (fun c-> not(List.contains c.Y fullRows))
            |> List.groupBy(fun c-> 
                fst (Option.get (List.tryFind (fun (_,r)->isInRange (c.Y) r) shiftToRanges)))
            |> List.collect(fun (shift, group) -> List.map(fun p->{p with Y=p.Y+shift}) group)
            |> List.map (fun s->(s.X, s.Y))
            |> XYArray.setMulti Field.Block initBoard
            |> Option.get
        
        let rows = List.length fullRows
        let newLines = score.Lines + rows;
        let newScoreValue = score.Value + rows*100
        let level = newLines / 10 + 1

        let newscore = {Level = level; Value=newScoreValue; Lines=newLines}
        (newBoard, newscore)

    let rec nextState random userInput state = 
        match (state, userInput) with
        | _, Command.Restart -> 
            let activeBlock = generateActiveBlock blocks random (initBoard.maxX/2s)
            let nextBlock = generateActiveBlock blocks random (initBoard.maxX/2s)
            InProgress {Board = initBoard; Score= {Level=1; Value=0; Lines=0}; ActiveBlock=activeBlock; NextBlock=nextBlock}
        | Start, Command.Exit -> End <| {Level=1; Value=0;Lines=0}
        | Start, _
        | End _, _ -> state
        | InProgress st, Command.FallDownByTime -> 
            let newBlock = moveBlock st.ActiveBlock (0s<x>, 1s<y>)
            if(isBlockInBound st.Board newBlock) then
                InProgress {st with ActiveBlock=newBlock}
            else
                nextState random Command.FallDown state
        | InProgress st, Command.Exit -> End st.Score
        | InProgress _, Command.None -> 
            state
        | InProgress inProgressState, Command.Rotate ->
            InProgress (rotateActiveBlock inProgressState)
        | InProgress inProgressState, Command.Move direction ->
            InProgress (moveActiveBlock inProgressState direction)
        | InProgress inProgressState, Command.FallDown ->
            let isBlockClashing block board =
                block                
                |> Set.map (fun c -> XYArray.get c.X c.Y board)
                |> Set.exists (fun field -> field = (Some Field.Block))

            let boardAfterBlockFallDown = fallDownBlock inProgressState.Board inProgressState.ActiveBlock
            let (boardAfterLinesEval,newScore) = evaluateBoardProgression boardAfterBlockFallDown inProgressState.Score
            let newActiveBlock = generateActiveBlock blocks random (initBoard.maxX/2s)
            if (isBlockClashing newActiveBlock boardAfterLinesEval) 
            then
                End newScore
            else
                InProgress {
                    Board = boardAfterLinesEval
                    ActiveBlock = inProgressState.NextBlock
                    NextBlock = newActiveBlock
                    Score=newScore
                    }
    

    let isEnd state =
        match state with
        | State.End _ -> true
        | _ -> false


            
            
