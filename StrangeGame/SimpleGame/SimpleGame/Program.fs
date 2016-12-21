namespace Game

    open System
    open System.Threading
    open Game
    open InitialState
    open Rendering
    open CrudeBrain
    open Loop

    module Program =
        
        [<EntryPoint>]
        let main argv =
            let rec recursiveLoop (state:GameState) =
                let updated =
                    loop
                        state
                        Map.empty
                        Game.CrudeBrain.decide
                        (fun score previousState currentState ->
                            renderScore score
                            renderPlayer previousState.Hero currentState.Hero
                            renderBoard previousState.Board currentState.Board
                            0)
                recursiveLoop updated

            recursiveLoop initialGameState

            0 // return an integer exit code
