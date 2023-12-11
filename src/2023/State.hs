{-# LANGUAGE TupleSections #-}

module State where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.List

import Utils

-- General state

newtype State s a = State {runState :: s -> (a, s)}

evalState :: State s a -> s -> a
evalState m = fst . runState m

execState :: State s a -> s -> s
execState m = snd . runState m

instance Functor (State s) where
  fmap f m = State $ first f . runState m

instance Applicative (State s) where
  pure x = State (x,)

  mf <*> mx = State $ \s -> let (f, s')  = runState mf s
                                (x, s'') = runState mx s'
                             in (f x, s'')

instance Monad (State s) where
  mx >>= f = State $ \s -> let (x, s') = runState mx s
                            in runState (f x) s'

get :: State s s
get = State $ \s -> (s, s)

gets :: (s -> a) -> State s a
gets f = fmap f get

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ ((),) . f

-- State for a grid map

data MapStateVar t s = MapStateVar { tiles :: [[t]]
                                   , coord :: (Int, Int)
                                   , other :: s
                                   }

newtype MapState t s a = MapState { unMapState :: State (MapStateVar t s) a }

instance Functor (MapState t s) where
  fmap f m = MapState $ f <$> unMapState m

instance Applicative (MapState t s) where
  pure x = MapState $ pure x

  mf <*> mx = MapState $ unMapState mf <*> unMapState mx

instance Monad (MapState t s) where
  mx >>= f = MapState $ unMapState mx >>= unMapState . f

runMapState :: [[t]] -> (Int, Int) -> s -> MapState t s a -> a
runMapState ts coord other m = evalState (unMapState m)
                             $ MapStateVar ts coord other

-- Finding location inside grid

atLeftEdge :: MapState t s Bool
atLeftEdge = MapState $ gets $ (== 0) . fst . coord

atRightEdge :: MapState t s Bool
atRightEdge = MapState $ do
  (x,y) <- gets coord
  ts    <- gets tiles
  pure $ elem x $ fmap (subtract 1 . length) (ts !? y)

atTopEdge :: MapState t s Bool
atTopEdge = MapState $ gets $ (== 0) . snd . coord

atBottomEdge :: MapState t s Bool
atBottomEdge = MapState $ do
  (x,y) <- gets coord
  ts    <- gets tiles
  pure $ y == length ts - 1

isInside :: (Int, Int) -> MapState t s Bool
isInside (x,y) = MapState $ do
  ts <- gets tiles
  let rows = length ts
      cols = maybe 0 length $ headMaybe ts
  pure $ 0 <= x && x < cols && 0 <= y && y < rows

-- Moving along the grid

moveLeft :: MapState t s Bool
moveLeft = MapState $ do
  atLeftEdge <- unMapState atLeftEdge
  unless atLeftEdge $ modify (\s -> s{ coord = first (subtract 1) (coord s) })
  pure $ not atLeftEdge

moveRight :: MapState t s Bool
moveRight = MapState $ do
  atRightEdge <- unMapState atRightEdge
  unless atRightEdge $ modify (\s -> s{ coord = first (+ 1) (coord s) })
  pure $ not atRightEdge

moveUp :: MapState t s Bool
moveUp = MapState $ do
  atTopEdge <- unMapState atTopEdge
  unless atTopEdge $ modify (\s -> s{ coord = first (subtract 1) (coord s) })
  pure $ not atTopEdge

moveDown :: MapState t s Bool
moveDown = MapState $ do
  atBottomEdge <- unMapState atBottomEdge
  unless atBottomEdge $ modify (\s -> s{ coord = first (+ 1) (coord s) })
  pure $ not atBottomEdge

moveTo :: (Int, Int) -> MapState t s Bool
moveTo coord = MapState $ do
  isInside <- unMapState $ isInside coord
  when isInside $ modify (\s -> s{ coord = coord })
  pure isInside

-- Getters

getCoord :: MapState t s (Int, Int)
getCoord = MapState $ gets coord

getTile :: MapState t s t
getTile = MapState $ do
  (x,y) <- gets coord
  ts    <- gets tiles
  pure $ fromJust (fromJust (ts !? y) !? x)

getTiles :: MapState t s [[t]]
getTiles = MapState $ gets tiles

getRow :: MapState t s [t]
getRow = MapState $ do
  (x,y) <- gets coord
  ts    <- gets tiles
  pure $ fromJust (ts !? y)

getCol :: MapState t s [t]
getCol = MapState $ do
  (x,y) <- gets coord
  ts    <- gets tiles
  pure $ fromJust (transpose ts !? y)

getOther :: MapState t s s
getOther = MapState $ gets other

getsOther :: (s -> a) -> MapState t s a
getsOther f = MapState $ gets $ f . other

-- Setters

setTile :: t -> MapState t s ()
setTile t = undefined

putOther :: s -> MapState t s ()
putOther other = MapState $ modify (\s -> s{ other = other })

modifyOther :: (s -> s) -> MapState t s ()
modifyOther f = MapState $ modify (\s -> s{ other = f (other s) })
