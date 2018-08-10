#load "XYArray.fs"
open XYArray
#load "Domain.fs"
open Domain
#load "ListExt.fs"
#load "Game.fs"
open Game

//rotation
let r = loop (System.Random ()) (UserInput.Restart) Start
let rl = r |> loop (System.Random ()) (UserInput.Rotate Left)
let rr = r |> loop (System.Random ()) (UserInput.Rotate Right)
print r
print rl
print rr

//move
let m = loop (System.Random ()) (UserInput.Restart) Start
let ml = m |> loop (System.Random ()) (UserInput.Move Left)
let mr = m |> loop (System.Random ()) (UserInput.Move Right)
print m
print ml
print mr
