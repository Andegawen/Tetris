#load "XYArray.fs"
open XYArray
#load "Domain.fs"
open Domain
#load "ListExt.fs"
#load "Game.fs"
open Game

let nextState = nextState (System.Random ())

//rotation
let randomState = nextState (UserInput.Restart) Start
let centerActiveBlock state= 
    match state with
    | InProgress progress -> InProgress{progress with ActiveBlock=progress.ActiveBlock |> Set.map(fun c->{c with Y=c.Y+5s<y>})}
    | _ -> state

let r = centerActiveBlock randomState
let rl = r |> nextState (UserInput.Rotate CCW)
let rr = r |> nextState (UserInput.Rotate CW)
print r
print rl
print rr

//move
let m = nextState (UserInput.Restart) Start
let ml = m |> nextState (UserInput.Move Left)
let mr = m |> nextState (UserInput.Move Right)
print m
print ml
print mr

//falldown
let f = nextState (UserInput.Restart) Start
let f1 = nextState (UserInput.FallDown) f
let f2 = nextState (UserInput.FallDown) f1
let f3 = nextState (UserInput.FallDown) f2

print f
print f1
print f2
print f3