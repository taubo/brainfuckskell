import qualified Data.Char as DChar

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

type Tape a = ([a], a, [a])
type BfTape = Tape Int
type BfExecution = Tape BfTerminal

{-
bfProgramToExecution :: BfProgram -> BfExecution
bfProgramToExecution program =
-}
-- bfRun :: BfExecution -> IO ()
-- bfRun tape =

bfRunBasic :: BfProgram -> BfTape -> IO ()
bfRunBasic (Up: prog) tape = do bfRunBasic prog (upCell tape)
bfRunBasic (Down: prog) tape = do bfRunBasic prog (downCell tape)
bfRunBasic (MoveLeft: prog) tape = do bfRunBasic prog (leftCell tape)
bfRunBasic (MoveRight: prog) tape = do bfRunBasic prog (rightCell tape)
bfRunBasic (In: prog) tape = do bfRunBasic prog (upCell tape)
bfRunBasic (Out: prog) tape = do bfRunBasic prog (upCell tape)
bfRunBasic ([]) tape = do print tape

emptyBfTape :: BfTape
emptyBfTape = ([], 0, [])

upCell :: BfTape -> BfTape
upCell (prev, cell, post) = (prev, cell + 1, post)

downCell :: BfTape -> BfTape
downCell (prev, cell, post) = (prev, cell - 1, post)

rightCell :: BfTape -> BfTape
rightCell (prev, cell, []) = (cell : prev, 0, [])
rightCell (prev, cell, x:post) = (cell : prev, x, post)

leftCell :: BfTape -> BfTape
leftCell ([], cell, post) = ([], 0, cell : post)
leftCell (x:prev, cell, post) = (prev, x, cell : post)

outCell :: BfTape -> Char
outCell (_, cell, _) = DChar.chr cell

inCell :: Char -> BfTape -> BfTape
inCell inputChar (prev, _, post) = (prev, DChar.ord inputChar, post)

simpleBfValidator :: String -> Bool
simpleBfValidator program =
    simpleBfParse . simpleBfLexer $ program
