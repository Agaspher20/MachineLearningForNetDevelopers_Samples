#load "Game.fs"
#load "CrudeBrain.fs"
#load "InitialState.fs"
#load "Loop.fs"

open Game.Game
open Game.CrudeBrain
open Game.InitialState
open Game.Loop

let simulate (decide: Brain -> State -> Act) iters runs =
    let rec recursiveLoop (state:GameState) (brain:Brain) (iter:int) =
        let (updated,brain) =
            loop
                state
                brain
                (fun brain state -> Game.CrudeBrain.decide brain state)
                (fun score previousState currentState -> 0)

        if iter < iters
        then recursiveLoop updated brain (iter + 1)
        else updated.Score

    [ for run in 1 .. runs -> recursiveLoop initialGameState Map.empty 0 ]

// Simulating different brains
printfn "Random decision"
let random = simulate (fun _ _ -> Game.CrudeBrain.randomDecide()) 500 20
printfn "Average score: %.0f" (random |> Seq.averageBy float)

printfn "Crude brain"
let crudeBrain = simulate Game.CrudeBrain.decide 500 20
printfn "Average score: %.0f" (crudeBrain |> Seq.averageBy float)
