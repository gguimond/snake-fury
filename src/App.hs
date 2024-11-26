{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where
import GameState (GameState, move, HasGameState (getGameState, setGameState))
import RenderState (RenderState (score, gameOver), BoardInfo, render, HasRenderState (getRenderState, setRenderState), HasBoardInfo (getBoardInfo))
import Control.Monad.Reader (MonadReader (ask), asks, ReaderT (runReaderT))
import Control.Monad.State (MonadState (get), gets, StateT (runStateT), evalStateT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import EventQueue (EventQueue, readEvent, setSpeed)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)


data AppState = AppState GameState RenderState
data Env = Env BoardInfo

newtype App m a = App {runApp :: ReaderT Env (StateT AppState m) a}
  deriving (Functor , Applicative, Monad, MonadState AppState, MonadReader Env, MonadIO)

instance HasGameState AppState where
  getGameState (AppState g _ )   = g
  setGameState (AppState _ r) g = AppState g r

instance HasRenderState AppState where
  getRenderState (AppState _ r )   = r
  setRenderState (AppState g _ ) r = AppState g r

instance HasBoardInfo Env where
  getBoardInfo (Env b) = b

gameStep :: (MonadReader env m, HasBoardInfo env, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameStep queue = liftIO (readEvent queue) >>= move >>= render

gameloop :: (MonadReader env m, HasBoardInfo env, MonadState state m, HasGameState state, HasRenderState state, MonadIO m) => EventQueue -> m ()
gameloop queue = do
  s <- gets (score . getRenderState)
  new_speed <- liftIO $ setSpeed s queue
  liftIO $ threadDelay new_speed
  gameStep queue
  game_over <- gets (gameOver . getRenderState)
  unless game_over $ gameloop queue

run :: Env -> AppState -> EventQueue -> IO ()
run env app queue = runApp (gameloop queue) `runReaderT` env `evalStateT` app