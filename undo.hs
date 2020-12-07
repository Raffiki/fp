{-# LANGUAGE DatatypeContexts #-} 
{-# LANGUAGE FunctionalDependencies #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 

module Undo where

import Control.Monad.State
import Control.Monad.Identity


data History s = History { current :: s, undos :: [s], redos :: [s] }
    deriving (Eq, Show, Read)

blankHistory s = History { current = s, undos = [], redos = [] }


newtype Monad m => UndoT s m a = UndoT (StateT (History s) m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

class (MonadState s m) => MonadUndo s m | m -> s where
    undo :: m Bool -- undo the last state change, returns whether successful
    redo :: m Bool -- redo the last undo
    history :: m (History s) -- gets the current undo/redo history
    checkpoint :: m () -- kill the history, leaving only the current state

instance (Monad m) => MonadState s (UndoT s m) where
    get = UndoT $ do
            ur <- get
            return (current ur)
    put x = UndoT $ do
              ur <- get
              put $ History { current = x, undos = current ur : undos ur
                            , redos = [] }

instance (Monad m) => MonadUndo s (UndoT s m) where
    undo = UndoT $ do
        ur <- get
        case undos ur of
            []     -> return False
            (u:us) -> do put $ History { current = u, undos = us
                                       , redos = current ur : redos ur }
                         return True
    redo = UndoT $ do
        ur <- get
        case redos ur of
            []     -> return False
            (r:rs) -> do put $ History { current = r, undos = current ur : undos ur
                                       , redos = rs }
                         return True
    history = UndoT $ get
    checkpoint = UndoT $ do
        s <- liftM current get
        put $ blankHistory s
        
evalUndoT (UndoT x) s = evalStateT x (blankHistory s)
execUndoT (UndoT x) s = liftM current $ execStateT x (blankHistory s)

newtype Undo s a = Undo (UndoT s Identity a)
    deriving (Functor, Applicative, Monad, MonadState s, MonadUndo s)

evalUndo (Undo x) s = runIdentity $ evalUndoT x s
execUndo (Undo x) s = runIdentity $ execUndoT x s


type GameState = (Bool, String)


playGame :: String -> Undo GameState GameState
playGame x = do
    (done, res) <- get
    _ <- (case x of
            "undo" -> (\_ -> ()) <$> undo 
            _      -> put (done, res ++ x))
    get

startState :: GameState
startState = (False, "")

main = loop startState
  where
    loop s = do
      input <- getLine
      let (d, i) = evalUndo (playGame input) s
      _ <- print i

      let (d, i) = execUndo (playGame input) s
      _ <- print i
      loop (d, i)
