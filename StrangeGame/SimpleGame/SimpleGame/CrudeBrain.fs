namespace Game
    
    open System
    open Game

    module CrudeBrain =
        // My current direction and surrounding cells
        type State = int list
        type Experience = {
            State:State; // where I was
            Action:Act; // what I did
            Reward:float; // what reward I got
            NextState:State; // where I ended
        }

        // one possible course of action I can take
        // when I am in that state
        type Strategy = { State:State; Action:Act; }

        // Strategies I tried, and what reward
        // I should expect from them
        type Brain = Map<Strategy,float>

        let rng = Random()

        let choices = [| Straight; Left; Right |]
        let randomDecide() = choices.[rng.Next(3)]
        
        let alpha = 0.2 // learning rate
        let gamma = 0.5 // discount rate

        let nextValue (brain:Brain) (state:State) =
            choices
            |> Seq.map (fun action ->
                match brain.TryFind { State = state; Action = action } with
                | Some (value) -> value
                | None -> 0.)
            |> Seq.max
        let learn (brain:Brain) (experience:Experience) =
            let strategy = { State = experience.State; Action = experience.Action }
            let vNext = nextValue brain experience.NextState

            match brain.TryFind strategy with
            | Some(value) ->
                brain.Add(strategy, (1. - alpha) * value + alpha * (experience.Reward + gamma * vNext))
            | None ->
                brain.Add(strategy, (alpha * (experience.Reward + gamma * vNext)))

        let decide (brain:Brain) (state:State) =
            let knownStrategies =
                choices
                |> Array.map (fun action -> { State = state; Action = action })
                |> Array.filter (fun strategy -> brain.ContainsKey strategy)
            match knownStrategies.Length with
            | 0 -> randomDecide()
            | _ ->
                choices
                |> Seq.maxBy (fun action ->
                    let strategy = { State = state; Action = action }
                    
                    match brain.TryFind strategy with
                    | Some(value) -> value
                    | None -> 0.0)

        let offsets =
            [ (-1, -1)
              (-1, 0)
              (-1, 1)
              (0, -1)
              (0, 1)
              (1, -1)
              (1, 0)
              (1, 1)
            ]

        let rotate dir (x,y) =
            match dir with
            | North -> (x,y)
            | South -> (-x,-y)
            | West -> (-y,x)
            | East -> (y,-x)

        let visibleState (size:Size) (board:Board) (hero:Hero) =
            let (dir,pos) = hero.Direction, hero.Position

            offsets
            |> List.map (rotate dir)
            |> List.map (fun (x,y) ->
                onboard size { Top = pos.Top + x; Left = pos.Left + y}
                |> tileAt board)
