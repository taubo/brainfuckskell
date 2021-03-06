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
simpleBfLexer = map bfAltTerminalMap

countSymbol :: BfProgram -> BfTerminal -> Int
countSymbol program symbol =
    length (filter (== symbol) program)

-- nop :: BfProgram -> BfProgram
-- nop [_ : prog] = prog

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
bfRunBasic (Up: prog) tape =
    bfRunBasic prog (upCell tape)

bfRunBasic (Down: prog) tape =
    bfRunBasic prog (downCell tape)

bfRunBasic (MoveLeft: prog) tape =
    bfRunBasic prog (leftCell tape)

bfRunBasic (MoveRight: prog) tape =
    bfRunBasic prog (rightCell tape)

bfRunBasic (In: prog) tape = do
    input <- getChar
    bfRunBasic prog (inCell input tape)

bfRunBasic (Out: prog) tape = do
    putStr [outCell tape]
    bfRunBasic prog tape

-- if the cell is equal to 0, jump at the instruction after the matching ]
bfRunBasic loop@(LoopForward: prog) tape =
    return ()

bfRunBasic loop@(LoopBackward: prog) tape =
    return ()

bfRunBasic [] tape =
    return ()

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
simpleBfValidator =
    simpleBfParse . simpleBfLexer
