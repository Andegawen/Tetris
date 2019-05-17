module Domain
    open XYArray

    type Field = Empty | Block
    type Board = xyArray<Field>
    type Coordinate = {X:int16<x>; Y:int16<y>}
    type Block = Coordinate Set

    let initBoard = XYArray.init 10s<x> 20s<y> (fun _ _ -> Empty)


    type Score = {Level:int; Value:int; Lines:int}

    type InProgress = { 
      Board:Board
      Score:Score
      ActiveBlock:Block
      NextBlock:Block
      }

    type State = 
      | Start
      | InProgress of InProgress
      | End of Score

    type Direction = Left | Right | Down
    type RotateDirection = CW | CCW
    
    [<RequireQualifiedAccess>]
    type Command =
      | Move of Direction
      | Rotate
      | FallDown
      | Exit
      | Restart
      | FallDownByTime

    let blocks : Block list = 
        [
            [ "XXX"
              "  X"];
            
            [ "XXX"
              "X  "];

            [ "XX"
              "XX" ];

           [ "XXXXX"];

           [ "XXX"
             " X"];

           [ "XX"
             " XX"];

           [ " XX"
             "XX"];
        ] |> List.map 
            (List.mapi (fun yv -> 
                    Seq.mapi (fun xv ch -> if ch = ' ' then None else  Some {X = x.lift xv; Y = y.lift yv}) 
                    >> Seq.choose id)
                >> Seq.collect id
                >> Set)