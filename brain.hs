data BfTerminal
    = Down
    | Up
    | MoveLeft
    | MoveRight
    | Out
    | In
    | LoopForward
    | LoopBackward
    | Comment
    deriving (Show, Eq)

type BfProgram = [BfTerminal]

bfTerminalMap :: Char -> BfTerminal
bfTerminalMap input =
    case input of
        '-' -> Down
        '+' -> Up
        '<' -> MoveLeft
        '>' -> MoveRight
        '.' -> Out
        ',' -> In
        '[' -> LoopForward
        ']' -> LoopBackward

bfAltTerminalMap :: Char -> BfTerminal
bfAltTerminalMap input
  | input == '-' = Down
  | input == '+' = Up
  | input == '<' = MoveLeft
  | input == '>' = MoveRight
  | input == '.' = Out
  | input == ',' = In
  | input == '[' = LoopForward
  | input == ']' = LoopBackward
  | otherwise = Comment

simpleBfLexer :: String -> BfProgram
simpleBfLexer input = map bfAltTerminalMap input

countSymbol :: BfProgram -> BfTerminal -> Int
countSymbol program symbol =
    length (filter (\x -> x == symbol) program)

countLoopForward :: BfProgram -> Int
countLoopForward program =
    countSymbol program LoopForward

countLoopBackward :: BfProgram -> Int
countLoopBackward program =
    countSymbol program LoopBackward

simpleBfParse :: BfProgram -> Bool
simpleBfParse symbols =
    countLoopForward symbols == countLoopBackward symbols
