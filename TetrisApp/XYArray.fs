    (*
       0  1  2 
       3  4  5
       6  7  8
       9  10 11

      len = 12

      x = 3, y = 4
    *)

module XYArray
    [<Measure>] type x
    [<Measure>] type y

    type 'a xyArray = private { arr: 'a array; maxX : int16<x> }
    with 
        member public arr.maxsX = arr.maxX
        member public arr.maxY = (int16)(arr.arr.Length / (int)arr.maxX) * 1s<y>

    let private getXY (point:int) (maxX:int16<x>)=
        let x = (int16)point*1s<x> % maxX
        let y = (int16)point / maxX * (1s<y*x>)
        x, y

    let init (x : int16<x>) (y:int16<y>) f =
        let xdimless = int x
        let ydimless = int y
        { arr = Array.init (xdimless * ydimless) (fun point -> 
            let x, y = getXY point x
            f x y); maxX = x }


    let get (x:int16<x>) y arr = 
        if x<arr.maxX && y<arr.maxY 
        then
            let accessIndex = (int)(x+y*((int16)arr.maxX * 1s<x>/1s<y>))
            Some arr.arr.[accessIndex]
        else
            None

    let set (x:int16<x>) (y:int16<y>) value arr =         
        let initEl (xx : int16<x>) (yy:int16<y>) = 
            if xx = x && yy=y then
                value
            else
                Option.get (get xx yy arr)
        if x<arr.maxX && y<arr.maxY 
        then
            Some (init arr.maxX arr.maxY initEl)
        else
            None