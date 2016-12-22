namespace Game
    module Game =
        type Dir =
            | North
            | West
            | South
            | East

        type Act =
            | Left
            | Right
            | Straight

        type Pos = { Top:int; Left:int }

        type Hero = { Position:Pos; Direction:Dir }

        type Board = int[,]

        type Size = { Width:int; Height:int }

        type GameState = { Board:Board; Hero:Hero; Score:int; Size:Size }

        let inline (%%%) (x:int) (y:int) =
            if x >= 0 then x%y
            else y + (x%y)

        let onboard (size:Size) (pos:Pos) =
            { Top = pos.Top %%% size.Height;
              Left = pos.Left %%% size.Width; }

        let moveTo (size:Size) (dir:Dir) (pos:Pos) =
            match dir with
                | North -> { pos with Top = (pos.Top - 1) %%% size.Height }
                | South -> { pos with Top = (pos.Top + 1) %%% size.Height }
                | West -> { pos with Left = (pos.Left - 1) %%% size.Width }
                | East -> { pos with Left = (pos.Left + 1) %%% size.Width }

        let takeDirection (act:Act) (dir:Dir) =
            match act with
            | Straight -> dir
            | Left ->
                match dir with
                | North -> East
                | East -> South
                | South -> West
                | West -> North
            | Right ->
                match dir with
                | North -> West
                | West -> South
                | South -> East
                | East -> North

        let applyDecision (size:Size) (action:Act) (hero:Hero) =
            let newDirection = hero.Direction |> takeDirection action
            { Position = moveTo size newDirection hero.Position; Direction = newDirection }

        let tileAt (board:Board) (pos:Pos) = board.[pos.Left, pos.Top]

        let tileValues = [| -100; -50; 50; 100 |]

        let computeGain (board:Board) (hero:Hero) =
            let currentPosition = hero.Position
            let cellType = tileAt board currentPosition
            tileValues.[cellType]

        let rng = System.Random()

        let updateBoard (board:Board) (player:Hero) =
            let currentPosition = player.Position
            let updatedBoard = board |> Array2D.copy
            updatedBoard.[currentPosition.Left, currentPosition.Top] <- rng.Next(tileValues.Length)
            updatedBoard
