{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where 

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point, DeltaBoard)
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random ( uniformR, RandomGen(split), StdGen, Random (randomR))
import Data.Maybe (isJust)

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty 
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our porpouse.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and 
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East
-- >>> opositeMovement North == South
-- >>> opositeMovement South == North
-- >>> opositeMovement East == West
-- >>> opositeMovement West == East


-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation. 
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint (BoardInfo n i) sg = (newPoint , g1')
  where (g1, g2)  = split sg
        (n', g1') = uniformR (1, n) g1
        (i', _) = uniformR (1, i) g2
        newPoint  = (n', i')
{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}


-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake x0 (SnakeSeq x1 seq) = x0 == x1 || isJust (x0 `S.elemIndexL` seq)

{-
This is a test for inSnake. It should return 
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq

-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead (BoardInfo h w) (GameState (SnakeSeq (x, y) _) _ mov _) =
  case mov of
    North -> if x - 1 <= 0 then (h, y) else (x - 1, y)
    South -> if x + 1  > h then (1, y) else (x + 1, y)
    East  -> if y + 1  > w then (x, 1) else (x, y + 1)
    West  -> if y - 1 <= 0 then (x, w) else (x, y - 1)
{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, StdGen)
newApple bi app@(GameState ss x0 move sg) =
    if x0' == x0 || x0' `inSnake` ss
      then newApple bi app{randomGen = sg'}
      else (x0', sg')
  where (x0', sg') = makeRandomPoint bi sg
{- We can't test this function because it depends on makeRandomPoint -}


-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
-- 

move :: BoardInfo -> GameState -> ([Board.RenderMessage] , GameState)
move bi s@(GameState (SnakeSeq oldHead sb) applePos _ g) =
  if isColision
    then ([Board.GameOver], s)
    else 
      case isEatingApple of
        True ->
          case sb of
            S.Empty ->
              let newSnake = SnakeSeq newHead (S.singleton oldHead)
                  newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
              in ([Board.RenderBoard delta, Board.Score], newState)
            xs ->
              let newSnake = SnakeSeq newHead (oldHead :<| xs)
                  newState = s {snakeSeq = newSnake, applePosition = newApplePos, randomGen = g'}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (newApplePos, Board.Apple)]
              in ([Board.RenderBoard delta, Board.Score], newState)
        False ->
          case sb of
            S.Empty ->
              let newSnake = SnakeSeq newHead S.empty
                  newState = s {snakeSeq = newSnake}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Empty)]
              in ([Board.RenderBoard delta], newState)
            x :<| S.Empty  ->
              let newSnake = SnakeSeq newHead (S.singleton oldHead)
                  newState = s {snakeSeq = newSnake}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (x, Board.Empty)]
              in ([Board.RenderBoard delta], newState)
            x :<| (xs :|> t)  ->
              let newSnake = SnakeSeq newHead (oldHead :<| x :<| xs)
                  newState = s {snakeSeq = newSnake}
                  delta = [(newHead, Board.SnakeHead), (oldHead, Board.Snake), (t, Board.Empty)]
              in ([Board.RenderBoard delta], newState)

  where newHead           = nextHead bi s
        isColision        = newHead `elem` sb
        isEatingApple     = newHead == applePos
        (newApplePos, g') = newApple bi s
{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1) 
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
