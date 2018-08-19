#load "XYArray.fs"
open XYArray
#load "Domain.fs"
open Domain
#load "ListExt.fs"
#load "Game.fs"
open Game

let stringBoard =
                    "                    \n"+
                    "     XXXXX          \n"+
                    "                    \n"+
                    "XXX                 \n"+
                    "            X       \n"+
                    "           X X      \n"+
                    "XXXXXXXXXXXX XXXXXXX\n"+
                    "XXXXXXXXXXXXXXXXXXXX\n"+
                    "XXXXXXXXXXXXXXXXXXXX\n"+
                    "XXXX   XXX    XXXXXX\n"+
                    "XXXXXXXXXXXXXXXXXXXX"

let stringBoardExpt =
                    "                    \n"+
                    "                    \n"+
                    "                    \n"+
                    "     XXXXX          \n"+
                    "                    \n"+
                    "XXX                 \n"+
                    "            X       \n"+
                    "           X X      \n"+
                    "XXXXXXXXXXXX XXXXXXX\n"+
                    "XXXX   XXX    XXXXXX"

let boardBlockFunc = (fun c->if c='X' then Field.Block else Field.Empty)
let board = XYArray.fromString boardBlockFunc stringBoard |> Option.get
let score0 = Score 0u
let boardResult,s = evaluateBoardProgression board score0 
let boardResultStr = (XYArray.toString (fun f -> if f=Field.Block then "X" else " ") boardResult)
let boardExpectation = XYArray.fromString boardBlockFunc stringBoardExpt |> Option.get

let comparer = (fun ((x1,y1),f1) ((x2,y2),f2)->if x1=x2 && y1=y2 && f1 = f2 then 0 else 1)
Seq.compareWith comparer (XYArray.toSeq boardResult) (XYArray.toSeq boardExpectation)

printfn "%s" boardResultStr
printfn "%s" stringBoardExpt

