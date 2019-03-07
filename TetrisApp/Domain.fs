module Domain
    open XYArray

    type Field = Empty | Block
    type Board = xyArray<Field>
    type Coordinate = {X:int16<x>; Y:int16<y>}
    type Block = Coordinate Set

    let initBoard = XYArray.init  20s<x> 10s<y> (fun _ _ -> Empty)

    type TimedownCounter = int

    type Score = Score of uint32
                  static member getValue (Score a) = a
    type InProgress = { Board:Board; Score:Score; ActiveBlock:Block; NextDownfallCounter:TimedownCounter }

    type State = 
      | Start
      | InProgress of InProgress
      | End of Score

    type Direction = Left | Right | Down
    type RotateDirection = CW | CCW
    
    [<RequireQualifiedAccess>]
    type UserInput = 
      | None
      | Move of Direction
      | Rotate of RotateDirection
      | FallDown
      | Exit
      | Restart

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