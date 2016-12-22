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
            let rec recursiveLoop (state:GameState) (brain:Brain) =
                let (updated,brain) =
                    loop
                        state
                        brain
                        Game.CrudeBrain.decide
                        (fun score previousState currentState ->
                            renderScore score
                            renderPlayer previousState currentState
                            renderBoard previousState currentState
                            0)
                recursiveLoop updated brain

            prepareDisplay initialGameState
            recursiveLoop initialGameState Map.empty

            0 // return an integer exit code
