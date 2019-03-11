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
            let (Score score) = progress.Score
            sb.AppendLine <| sprintf "Score: %d" score |> ignore
            sb.AppendLine <| "Next block:" |> ignore
            sb.AppendLine <| sprintf "%s" (printBlock progress.NextBlock) |> ignore
            Console.Write( sb.ToString())
        | End (Score score) -> printfn "Score: %d" score
        


    let readKeys : Async<UserInput> = async {
        let mutable key = ConsoleKeyInfo();
        
        while (Console.KeyAvailable) do
            key <- Console.ReadKey(true)

        let result = 
            match key.Key with
            | ConsoleKey.S -> UserInput.Restart
            | ConsoleKey.Z -> UserInput.Rotate RotateDirection.CCW
            | ConsoleKey.X -> UserInput.Rotate RotateDirection.CW
            | ConsoleKey.Spacebar -> UserInput.FallDown
            | ConsoleKey.RightArrow -> UserInput.Move Direction.Right
            | ConsoleKey.LeftArrow -> UserInput.Move Direction.Left
            | ConsoleKey.DownArrow -> UserInput.Move Direction.Down
            | ConsoleKey.Escape -> UserInput.Exit
            | _ -> UserInput.None
        return result
    }

    let play = 
        let r = System.Random 0
        let mutable state = State.Start
        print state
        async{
        while (not (isEnd state)) do    
                let! input = readKeys
                Async.Sleep 50 |> Async.RunSynchronously
                let command = if input = UserInput.None then UserInput.IncreaseCounter else input
                let newstate = nextState r command state
                if state <> newstate then 
                    state <- newstate
                print state 
                else
                    state <- newstate
        } |> Async.RunSynchronously