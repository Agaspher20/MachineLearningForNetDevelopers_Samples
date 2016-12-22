namespace Game

    open System
    open Game

    module InitialState =
        let size = { Width = 40; Height = 20 }
        let player = { Position = { Top = 10; Left = 20 }; Direction = North }
        let rng = Random()
        let board = Array2D.init size.Width size.Height (fun left top ->
            rng.Next(tileValues.Length))
        let score = 0
        let initialGameState = { Board = board; Hero = player; Score = score; Size = size }
