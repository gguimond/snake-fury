{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs ,MultiParamTypeClasses #-}

{-|
This module defines the logic of the game and the communication with the `Board.RenderState`
-}
module GameState where 

-- These are all the import. Feel free to use more if needed.
import RenderState (BoardInfo (..), Point, DeltaBoard, HasBoardInfo (getBoardInfo))
import qualified RenderState as Board
import Data.Sequence ( Seq(..))
import qualified Data.Sequence as S
import System.Random ( uniformR, RandomGen(split), StdGen, Random (randomR), mkStdGen )
import Data.Maybe (isJust)
import Control.Monad.Reader (ReaderT (runReaderT), ask, asks, runReader, MonadReader (local), Reader)
import Control.Monad.State.Strict (StateT, get, put, modify, gets, runStateT, MonadState, State, runState)
import Control.Monad.RWS.Class (MonadState(state))

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

newtype GameStep m a = GameStep {runGameStep :: ReaderT BoardInfo (StateT GameState m) a}

instance Functor m => Functor (GameStep m) where
  -- if m is a Functor then (StateT GameState m) is a Functor, and so it is (ReaderT BoardInfo (StateT GameState m) 
  -- Because GameStep is a silly wrapper aroung (ReaderT ...), in order to define fmap we just need to wrap-unwrap
  fmap :: Functor m => (a -> b) -> GameStep m a -> GameStep m b
  fmap f (GameStep r) = GameStep $ fmap f r 

-- For applicative, is exactly the same.
instance Monad m => Applicative (GameStep m) where
  pure a = GameStep $ pure a
  (GameStep f) <*> (GameStep r) = GameStep $ f <*> r 

-- For Monad is a little bit tricker, but still easy. You just need to puzzle-up the types
instance Monad m => Monad (GameStep m) where
  (>>=) :: Monad m => GameStep m a -> (a -> GameStep m b) -> GameStep m b
  (GameStep r) >>= f = GameStep $ r >>= (runGameStep . f)

-- Notice you don't need to have MonadReader BoardInfo m, because m is the monad inside StateT. 
-- The type in GameStep is already a (ReaderT BoardInfo ... ) so it is an instance of MonadReader BoardInfo
-- as long as m is a monad
instance Monad m => MonadReader BoardInfo (GameStep m) where 
  ask :: Monad m => GameStep m BoardInfo
  ask = GameStep ask
  local :: Monad m => (BoardInfo -> BoardInfo) -> GameStep m a -> GameStep m a
  local f (GameStep r) = GameStep $ local f r

instance Monad m => MonadState GameState (GameStep m) where 
  state :: Monad m => (GameState -> (a, GameState)) -> GameStep m a
  state f = GameStep $ state f

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Movement

-- New class for object which has access to GameState.
class HasGameState state where
  getGameState :: state -> GameState
  setGameState :: state -> GameState -> state

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
makeRandomPoint :: (MonadReader env m, HasBoardInfo env, MonadState state m, HasGameState state) =>  m Point
makeRandomPoint = do
  BoardInfo n i <- asks getBoardInfo
  gstate        <- gets getGameState
  let (g1, g2)  = split (randomGen gstate)
      (n', g1') = uniformR (1, n) g1
      (i', _) = uniformR (1, i) g2
      newPoint  = (n', i')
  modify $ flip setGameState gstate{randomGen = g1'}
  pure newPoint
  
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
newApple :: (MonadReader env m, HasBoardInfo env, MonadState state m, HasGameState state) =>  m Point
newApple = do
  bi <- ask
  gstate@(GameState snake_body old_apple move sg) <- gets getGameState
  new_apple <- makeRandomPoint
  if new_apple == old_apple || new_apple `inSnake` snake_body
    then newApple
    else modify (`setGameState` gstate{applePosition = new_apple}) >> pure new_apple

{- We can't test this function because it depends on makeRandomPoint -}

-- | move the snake's head forward without removing the tail. (This is the case of eating an apple)
extendSnake :: (MonadReader env m, HasBoardInfo env, MonadState state m, HasGameState state) => Point -> m DeltaBoard
extendSnake new_head = do
  binfo <- ask
  gstate <- gets getGameState
  SnakeSeq old_head snake_body <- gets (snakeSeq . getGameState)
  let new_snake = SnakeSeq new_head (old_head :<| snake_body)
      delta     = [(new_head, Board.SnakeHead), (old_head, Board.Snake)]
  modify $ flip setGameState gstate{snakeSeq = new_snake}
  pure delta

-- | displace snake, that is: remove the tail and move the head forward (This is the case of not eating an apple)
displaceSnake :: (MonadReader env m, HasBoardInfo env, MonadState state m, HasGameState state) => Point -> m DeltaBoard
displaceSnake new_head = do
  binfo <- ask
  gstate <- gets getGameState
  SnakeSeq old_head snake_body <- gets (snakeSeq . getGameState)
  case snake_body of
    S.Empty -> let new_snake = SnakeSeq new_head S.empty
                   delta = [(new_head, Board.SnakeHead), (old_head, Board.Empty)]
                in modify (`setGameState` gstate{snakeSeq = new_snake}) >> pure delta
    xs :|> t -> let new_snake = SnakeSeq new_head (old_head :<| xs)
                    delta = [(new_head, Board.SnakeHead), (old_head, Board.Snake), (t, Board.Empty)]
                 in modify ( `setGameState` gstate{snakeSeq = new_snake}) >> pure delta

step :: (MonadReader env m, HasBoardInfo env, MonadState state m, HasGameState state) => m [Board.RenderMessage]
step = do
  bi <- asks getBoardInfo
  gstate@(GameState s applePos _ _) <- gets getGameState
  let newHead           = nextHead bi gstate
      isColision        = newHead `inSnake` s
      isEatingApple     = newHead == applePos
  if | isColision -> pure [Board.GameOver]
     | isEatingApple -> do  delta <- extendSnake newHead
                            newApplePos <- newApple
                            let delta' = (newApplePos, Board.Apple):delta
                            pure [Board.RenderBoard delta', Board.Score]
     | otherwise -> do delta <- displaceSnake newHead 
                       pure [Board.RenderBoard delta]

move ::  (MonadReader env m, HasBoardInfo env, MonadState state m, HasGameState state) => Event -> m [Board.RenderMessage]
move Tick = step
move (UserEvent input_movement) = do
  gstate@(GameState _ _  current_movement _) <- gets getGameState
  if current_movement == opositeMovement input_movement
    then step
    else modify (`setGameState` gstate{movement = input_movement}) >> step