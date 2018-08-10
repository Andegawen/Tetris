#load "XYArray.fs"
open XYArray
#load "Domain.fs"
open Domain
#load "ListExt.fs"
open ListExt
#load "Game.fs"
open Game


let z = loop (System.Random ()) (UserInput.Restart) Start

let z' = z |> loop (System.Random ()) (UserInput.Rotate Left)
print z
print z'