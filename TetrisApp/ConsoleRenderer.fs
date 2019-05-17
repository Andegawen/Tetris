module ConsoleRenderer
    open XYArray
    open Domain
    open Game
    open System
    open System.Text
    open Events

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
            sb.AppendLine <| sprintf "Level: %i Score: %i Lines: %i" progress.Score.Level progress.Score.Value progress.Score.Lines |> ignore
            sb.AppendLine <| "Next block:" |> ignore
            sb.AppendLine <| sprintf "%s" (printBlock progress.NextBlock) |> ignore
            Console.Write( sb.ToString())
        | End score -> printfn "Score: %A" score

    let waitForCommand (eventPublisher:EventHub) = 
        let waitForCommand' = async {
            let mutable key = ConsoleKeyInfo();
            
            while (Console.KeyAvailable) do
                key <- Console.ReadKey(true)

            let result = 
                match key.Key with
                | ConsoleKey.S -> Some Command.Restart
                | ConsoleKey.Spacebar -> Some Command.FallDown
                | ConsoleKey.UpArrow -> Some Command.Rotate
                | ConsoleKey.RightArrow -> Some <| Command.Move Direction.Right
                | ConsoleKey.LeftArrow -> Some <| Command.Move Direction.Left
                | ConsoleKey.DownArrow -> Some <| Command.Move Direction.Down
                | ConsoleKey.Escape -> Some Command.Exit
                | _ -> None
            return result
        }
        async{
            while true do
                let! cmd = waitForCommand'
                cmd |> Option.bind (fun c-> 
                    eventPublisher.Publish(c)
                    None) |> ignore
        }
        

    let setTimerInterval (level:int) (timer:System.Timers.Timer) =
        timer.Interval <- 1100.0 - (float)level * 100.0; 

    let getGameTimer =
        let t = new System.Timers.Timer()
        t.AutoReset <- true;
        t    


    let play = 
        let eventHub = EventHub()
        use tokenSource = new System.Threading.CancellationTokenSource()
        let token = tokenSource.Token
        
        let r = System.Random 0
        let nextState = nextState r
        let mutable state = State.Start
        
        use timer = getGameTimer
        timer.Stop()
        timer.Elapsed.Add(fun _ -> eventHub.Publish(Command.FallDownByTime))
        print state
        eventHub.Subscribe(fun cmd ->
                match (state, cmd) with
                | Start, _ -> timer.Stop() 
                | (InProgress p, Command.Move Direction.Down) -> setTimerInterval p.Score.Level timer
                | (InProgress p, Command.FallDown) -> setTimerInterval p.Score.Level timer
                | (InProgress _, _) -> ()
                | (End _,_) -> timer.Stop(); tokenSource.Cancel()

                let newstate = nextState cmd state
                match (state, newstate) with
                | Start, InProgress p -> setTimerInterval p.Score.Level timer; timer.Start()
                | _ -> ()
                if state <> newstate then 
                    match state, newstate with
                    | InProgress oldP, InProgress newP when oldP.Score.Level <> newP.Score.Level 
                        -> setTimerInterval newP.Score.Level timer
                    | _ -> ()
                    state <- newstate
                    print state 
                else
                    state <- newstate)
        
        let command = waitForCommand eventHub
        try
            Async.RunSynchronously(command, cancellationToken=token)
        with 
        | :? System.OperationCanceledException as ex -> ()
        
        
        
        
        