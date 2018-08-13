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


    let get (xv:int16<x>) yv arr = 
        if xv<arr.maximumX && yv<arr.maxY && xv >=0s<x> && yv >=0s<y>
        then
            let accessIndex = (int)(xv+yv*(arr.maxX/1s<y>))
            Some arr.arr.[accessIndex]
        else
            None

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


    let setMulti coords value board = 
        let rec setMulti' cs board = 
            match cs with
            | (xv,yv) :: tail -> 
                let newBoard = Option.bind (fun b-> set xv yv value b) board
                setMulti' tail newBoard
            | [] -> board
        setMulti' coords (Some board)