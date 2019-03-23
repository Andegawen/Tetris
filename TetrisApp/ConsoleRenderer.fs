module ConsoleRenderer
    open XYArray
    open Domain
    open Game
    open System
    open System.Text

    let printBlock (block:Block) =
        let maxY =
            block
            |> Seq.maxBy (fun c->c.Y)
            |> fun c -> (int16)c.Y
        let minY =
            block
            |> Seq.minBy (fun c->c.Y)
            |> fun c -> (int16)c.Y
        let minX =
            block
            |> Seq.minBy (fun c->c.X)
            |> fun c -> (int16)c.X
        let maxX =
            block
            |> Seq.maxBy (fun c->c.X)
            |> fun c -> (int16)c.X
        let sb = StringBuilder()
        for y in [minY .. maxY] |> List.map (y.lift) do
                for x in [minX .. maxX] |> List.map (x.lift) do
                    if block.Contains { X = x; Y= y} 
                        then sb.Append "X" |> ignore
                        else 
                            sb.Append " " |> ignore
                sb.AppendLine "" |> ignore 
        sb.ToString()

    let printBoardWithBlock (board:Board) (activeBlock:Block) =
        let sb = StringBuilder()
        for y in [0s .. (int16)board.maxY - 1s] |> List.map (y.lift) do
                sb.Append "|" |> ignore
                for x in [0s .. (int16)board.maxX  - 1s] |> List.map (x.lift) do
                    if activeBlock.Contains { X = x; Y= y} 
                        then sb.Append "X" |> ignore
                        else 
                            ignore <| if XYArray.get x y board = Some Empty then sb.Append " " else sb.Append "*"
                sb.AppendLine "|" |> ignore
        sb.ToString()            
            
    let print state = 
        Console.Clear()
        match state with
        | Start -> printfn "Press s to start"
        | InProgress progress -> 
            let sb = StringBuilder()
            sb.AppendLine <| printBoardWithBlock progress.Board progress.ActiveBlock |> ignore
            sb.AppendLine <| sprintf "Score: %A" progress.Score |> ignore
            sb.AppendLine <| "Next block:" |> ignore
            sb.AppendLine <| sprintf "%s" (printBlock progress.NextBlock) |> ignore
            Console.Write( sb.ToString())
        | End score -> printfn "Score: %A" score

    let waitForCommand : Async<Command> = async {
        let mutable key = ConsoleKeyInfo();
        
        while (Console.KeyAvailable) do
            key <- Console.ReadKey(true)

        let result = 
            match key.Key with
            | ConsoleKey.S -> Command.Restart
            | ConsoleKey.Spacebar -> Command.FallDown
            | ConsoleKey.UpArrow -> Command.Rotate
            | ConsoleKey.RightArrow -> Command.Move Direction.Right
            | ConsoleKey.LeftArrow -> Command.Move Direction.Left
            | ConsoleKey.DownArrow -> Command.Move Direction.Down
            | ConsoleKey.Escape -> Command.Exit
            | _ -> Command.None
        return result
    }

    let setTimerInterval (state:State) (timer:System.Timers.Timer) =
        match state with
        | Start ->  timer.Interval <- 1000.0
        | InProgress state -> 
            let interval = 1100.0 - (float)state.Score.Level * 100.0; 
            timer.Interval <- interval
        | End _ -> timer.Interval <- 1000.0

    let getGameTimer =
        let t = new System.Timers.Timer()
        t.AutoReset <- true;
        t    


    let play = 
        let r = System.Random 0
        let nextState = nextState r
        let mutable state = State.Start
        
        use timer = getGameTimer
        setTimerInterval state timer
        
        timer.Elapsed.Add(fun evArgs -> 
            state <- nextState Command.FallDownByTime state
            print state)
        print state
        async{
        while (not (isEnd state)) do    
                let! command = waitForCommand
                match (state, command) with
                | Start, _ -> timer.Stop() 
                | (InProgress _, Command.Move Direction.Down) -> setTimerInterval state timer
                | (InProgress _, Command.FallDown) -> setTimerInterval state timer
                | (InProgress _, _) -> ()
                | (End _,_) -> timer.Stop()

                Async.Sleep 50 |> Async.RunSynchronously
                let newstate = nextState command state
                match (state, newstate) with
                | Start, InProgress _ -> timer.Start()
                | _ -> ()
                if state <> newstate then 
                    match state, newstate with
                    | InProgress oldP, InProgress newP when oldP.Score.Level <> newP.Score.Level -> setTimerInterval state timer
                    | _ -> ()
                    state <- newstate
                    print state 
                else
                    state <- newstate
        } |> Async.RunSynchronously