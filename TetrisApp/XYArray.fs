    (*
       0  1  2 
       3  4  5
       6  7  8
       9  10 11

      len = 12

      x = 3, y = 4
    *)

module XYArray
    [<Measure>]
    type x =
        static member lift (v:int) = (int16)v * 1s<x>
        static member lift (v:int16) = v * 1s<x>
    [<Measure>]
    type y =
        static member lift (v:int) = (int16)v * 1s<y>
        static member lift (v:int16) = v * 1s<y>

    let removeUnit (x:int16<_>) =
        int16 x

    type 'a xyArray = private { arr: 'a array; maximumX : int16<x> }
    with 
        member public arr.maxX = arr.maximumX
        member public arr.maxY = y.lift ((int16)arr.arr.Length / (removeUnit arr.maxX))

    let private getXY (point:int) (maxX:int16<x>)=
        let x = x.lift point % maxX
        let y = (int16)point / maxX * (1s<y*x>)
        x, y

    let init (x : int16<x>) (y:int16<y>) f =
        let xdimless = int x
        let ydimless = int y
        { arr = Array.init (xdimless * ydimless) (fun point -> 
            let x, y = getXY point x
            f x y); maximumX = x }

    let private get' (xv:int16<x>) yv arr =
        let accessIndex = (int)(xv+yv*(arr.maximumX/1s<y>))
        arr.arr.[accessIndex]

    let get (xv:int16<x>) (yv:int16<y>) arr = 
        if xv<arr.maximumX && yv<arr.maxY && xv >=0s<x> && yv >=0s<y>
        then
            Some (get' xv yv arr)
        else
            None
    let toSeq (arr: 'a xyArray) = 
        let xs = [0s .. (removeUnit arr.maxX)-1s] |> List.map x.lift
        let ys = [0s .. (removeUnit arr.maxY)-1s] |> List.map y.lift
        seq {
            for x in xs do
                for y in ys do
                    yield ((x,y), get' x y arr)
        }

    let set (xv:int16<x>) (yv:int16<y>) value arr =         
        let initEl (x : int16<x>) (y:int16<y>) = 
            if xv = x && yv=y then
                value
            else
                Option.get (get x y arr)
        if xv<arr.maxX && yv<arr.maxY && xv >=0s<x> && yv >=0s<y>
        then
            Some (init arr.maxX arr.maxY initEl)
        else
            None


    let setMulti value board coords = 
        let rec setMulti' cs board = 
            match cs with
            | (xv,yv) :: tail -> 
                let newBoard = Option.bind (fun b-> set xv yv value b) board
                setMulti' tail newBoard
            | [] -> board
        setMulti' coords (Some board)
    let toString (f:'a->string) (board: 'a xyArray) =
        let ys = [0s .. (int16)board.maxY - 1s] |> List.map (y.lift)
        let xs = [0s .. (int16)board.maxX  - 1s] |> List.map (x.lift)
        let s = seq { for y in ys do
                        yield '|'
                        for x in xs do
                            yield! f (get' x y board)
                        yield '|'
                        yield '\n' }
        System.String.Concat s
    let fromString (f:char->'a) (str:string) =
        let rows = str.Split('\n')
        let z = 
            rows 
            |> Array.map (fun r-> 
                                 let chars = r.ToCharArray()
                                 (Array.length chars, chars |> Array.map f))
        let s = Set.ofArray (Array.map (fst) z)
        if Set.count s = 1
        then
            let initFunc xv yv maxX (arr:'a array)= 
                let accessIndex = (int)(xv+yv*(maxX/1s<y>))
                arr.[accessIndex]

            let x = s |> Set.toList |> List.head |> x.lift
            let y = Array.length rows |> y.lift
            let normalArray = z |> Array.collect snd
            Some (init x y (fun xv yv -> initFunc xv yv x normalArray))
        else
            None        
