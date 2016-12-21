namespace Game

    open System
    open Game

    module InitialState =
        let size = { Width = 40; Height = 20 }
        let player = { Position = { Top = 10; Left = 20 }; Direction = North }
        let rng = Random()
        let board =
            [ for top in 0 .. size.Height - 1 do
                for left in 0 .. size.Width - 1 do
                    if rng.NextDouble() > 0.5
                    then
                        let pos = { Top = top; Left = left }
                        let cell = if rng.NextDouble() > 0.5 then Trap else Treasure
                        yield pos, cell ]
            |> Map.ofList
        let score = 0
        let initialGameState = { Board = board; Hero = player; Score = score; Size = size }
