namespace Game
    
    open System.Threading

    open Game
    open CrudeBrain

    module Loop =
        let loop (state:GameState) (brain:Brain) (decide:Brain -> State -> Act) (render:int -> GameState -> GameState -> int) =
            let size = state.Size
            let currentState = visibleState size state.Board state.Hero
            let decision = decide brain currentState

            // world update
            let player = state.Hero |> applyDecision size decision
            let board = updateBoard state.Board player
            let gain = computeGain state.Board player
            let score = state.Score + gain

            // learning
            let nextState = visibleState size board player
            let experience = {
                State = currentState;
                Action = decision;
                Reward = gain |> float;
                NextState = nextState;
            }
            let brain = learn brain experience
            let updated = { Board = board; Hero = player; Score = score; Size = size }

            // world rendering
            let _ = render score state updated

            Thread.Sleep 20

            (updated,brain)
