#load "XYArray.fs"
open XYArray

let initEl (x : int16<XYArray.x>) (y:int16<XYArray.y>) = 
    let x = int x
    let y = int y
    x*y

let x = XYArray.init 3s<XYArray.x> 2s<XYArray.y> initEl

XYArray.get 4s<XYArray.x> 1s<XYArray.y> x = None
XYArray.get 2s<XYArray.x> 3s<XYArray.y> x = None
XYArray.get 1s<XYArray.x> 1s<XYArray.y> x = Some 1
XYArray.get 2s<XYArray.x> 1s<XYArray.y> x = Some 2



let newX = XYArray.set 1s<XYArray.x> 1s<XYArray.y> 13 x 
XYArray.get 1s<XYArray.x> 1s<XYArray.y> newX = Some 13
XYArray.get 2s<XYArray.x> 1s<XYArray.y> newX = Some 2