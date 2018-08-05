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

    type 'a xyArray = private { arr: 'a array; maxX : int }
    with 
        member public arr.maxsX = arr.maxX
        member public arr.maxY = arr.arr.Length / arr.maxX

    let private getXY point maxX=
        let x = int16 (point % maxX) * 1s<x>
        let y = int16 (point / maxX) * 1s<y>
        x, y    

    let init (x : int16<x>) (y:int16<y>) f =
        let x = int x
        let y = int y
        { arr = Array.init (x * y) (fun point -> 
            let x, y = getXY point x
            f x y); maxX = x }


    let get (x:int16<x>) y arr = 
        if ((int)x)<arr.maxX && ((int)y)<arr.maxY 
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
        if ((int)x)<arr.maxX && ((int)y)<arr.maxY 
        then
            Some (init ((int16)arr.maxX*1s<x>) ((int16)arr.maxY*1s<y>) initEl)
        else
            None