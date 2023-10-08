{-
--Name: Mark Ian Braun
--Martikelnummer: 8175858
Group: 04
-}
import Data.List (mapAccumL)

data Direction = North | South | East | West
    deriving (Show, Eq)

data Color = Red | Blue | Green | Yellow
    deriving (Show, Eq)

type Line = ((Int, Int), (Int, Int), Color)

data State = State
    { position :: (Int, Int)
    , direction :: Direction
    , pen :: Bool
    , color :: Color
    } deriving (Show)

-- Command types
data Command 
    = Move Int         -- Move forward by a certain number of steps
    | Turn Direction   -- Turn to face a given direction
    | PenUp            -- Lift the pen (stop drawing)
    | PenDown          -- Lower the pen (start drawing)
    | SetColor Color   -- Set the color of the pen
    deriving (Show, Eq)

-- Function to update position based on direction
move :: Direction -> Int -> (Int, Int) -> (Int, Int)
move North s p =  (fst p, snd p + s)
move South s p = (fst p, snd p - s)
move East s p = (fst p + s, snd p)
move West s p = (fst p - s, snd p)

-- Fill the run command that takes a list of commands and an initial state, and return a list of line segments representing the trail left by the turtle:
--run ([Move 2]) (State {position = (4,6), direction = West, pen = True, color = Red})
run :: [Command] -> State -> (State, [[Line]])
run [] state = (state, [])
run ((Move x):cs) state = 
    let newpos = move (direction state) x (position state)
        line = (position state, newpos, color state)
        (finalState, lines) = run cs (state {position = newpos})
    in if pen state == True 
        then (finalState, [line]:lines)
            else (finalState, []:lines)
run ((Turn d):cs) state = run cs updatedState
    where 
        updatedState = state{direction = d}
run (PenUp:cs) state = run cs updatedState
    where 
        updatedState = state{pen = False}
run (PenDown:cs) state = run cs updatedState
    where 
        updatedState = state{pen = True}
run ((SetColor c):cs) state = run cs updatedState
    where 
        updatedState = state{color = c}

-- Fill the interpreter using all given
interpret :: [Command] -> State-> (State, [Line])
interpret c state = 
    let (finalState, lines) = run c state
    in (finalState, concat lines)

main :: IO ()
main = do
    let cmds = [PenDown, Move 10, Turn East, Move 10, PenUp, SetColor Red, PenDown, Move 10, Move 10]
    let initialState = State (0, 0) North False Blue
    let (finalState, trail) = run cmds initialState
    putStrLn "Trail:"
    print trail
    putStrLn "Final State:"
    print finalState
