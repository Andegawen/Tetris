#load "XYArray.fs"
open XYArray

let initEl (x : int16<x>) (y:int16<y>) = 
    let x = int x
    let y = int y
    x*y

let arr = XYArray.init 3s<x> 2s<y> initEl

XYArray.get 4s<x> 1s<y> arr = None
XYArray.get 2s<x> 3s<y> arr = None
XYArray.get 1s<x> 1s<y> arr = (Some 1)
XYArray.get 2s<x> 1s<y> arr = (Some 2)

printfn "array as an seq: %A" (XYArray.toSeq arr)


let newXOutOfBound = XYArray.set 5s<x> 1s<y> 13 arr 
newXOutOfBound = None

let newX = XYArray.set 1s<x> 1s<y> 13 arr 
(XYArray.get 1s<x> 1s<y> (Option.get newX)) = Some 13
(XYArray.get 2s<x> 1s<y> (Option.get newX)) = Some 2


let initZeros (x : int16<x>) (y:int16<y>) = 
    let x = int x
    let y = int y
    0
let board = XYArray.init 3s<x> 3s<y> initZeros
let boardWithChanges = Option.get (XYArray.setMulti 1 board ([(0s<x>,0s<y>); (1s<x>,1s<y>); (2s<x>,2s<y>);]))

XYArray.get 0s<x> 0s<y> boardWithChanges = Some 1
XYArray.get 1s<x> 1s<y> boardWithChanges = Some 1
XYArray.get 2s<x> 2s<y> boardWithChanges = Some 1
XYArray.get 0s<x> 1s<y> boardWithChanges = Some 0