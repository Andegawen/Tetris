#load "XYArray.fs"
open XYArray
#load "Domain.fs"
open Domain
#load "ListExt.fs"
#load "Game.fs"
open Game

//rotation
let randomState = loop (System.Random ()) (UserInput.Restart) Start
let centerActiveBlock state= 
    match state with
    | InProgress progress -> InProgress{progress with ActiveBlock=progress.ActiveBlock |> Set.map(fun c->{c with Y=c.Y+5s<y>})}
    | _ -> state

let r = centerActiveBlock randomState
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

//falldown
let f = loop (System.Random ()) (UserInput.Restart) Start
let f1 = loop (System.Random ()) (UserInput.FallDown) f
let f2 = loop (System.Random ()) (UserInput.FallDown) f1
let f3 = loop (System.Random ()) (UserInput.FallDown) f2

print f
print f1
print f2
print f3