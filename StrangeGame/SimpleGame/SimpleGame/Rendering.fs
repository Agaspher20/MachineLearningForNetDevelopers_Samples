namespace Game
    
    open System
    open Game

    module Rendering =
        let topOffset = 2
        let colors =
            [|
                ConsoleColor.DarkRed
                ConsoleColor.Red
                ConsoleColor.DarkYellow
                ConsoleColor.Yellow
            |]
        let heroSign = "Ж"
        let cellSign = "·"
        let creatureColor = ConsoleColor.White
        let offset (pos:Pos) = (pos.Left, pos.Top + topOffset)
        let writeAt (left,top) color (txt:string) =
            Console.ForegroundColor <- color
            Console.SetCursorPosition(left,top)
            Console.Write txt
        
        let prepareDisplay (gameState:GameState) =
            let size = gameState.Size
            let board = gameState.Board

            Console.SetWindowSize(size.Width, size.Height + topOffset)
            for x in 0 .. (size.Width - 1) do
                for y in 0 .. (size.Height - 1) do
                    let cursorPosition = { Left = x; Top = y }
                    cursorPosition |> offset |> Console.SetCursorPosition
                    Console.ForegroundColor <- colors.[tileAt board cursorPosition]
                    Console.Write cellSign

        let renderPlayer (before:GameState) (after:GameState) =
            let oldPosition = before.Hero.Position
            let newPosition = after.Hero.Position
            let oldTileType = tileAt after.Board oldPosition

            writeAt (offset oldPosition) colors.[oldTileType] cellSign
            writeAt (offset newPosition) creatureColor heroSign

        let renderScore score =
            writeAt (0,0) ConsoleColor.White (sprintf "Score: %i    " score)

        let renderBoard (before:GameState) (after:GameState) =
            renderScore after.Score
            renderPlayer before after
