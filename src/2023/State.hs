{-# LANGUAGE Rank2Types #-}

module State ( MapState
             , runMapState
             , evalMapState
             , execMapState
             , getCoord
             , getBounds
             , getTile
             , getTiles
             , getRow
             , getCol
             , getOther
             , getsOther
             , isInside
             , atLeftEdge
             , atRightEdge
             , atTopEdge
             , atBottomEdge
             , putTile
             , modifyTile
             , putOther
             , modifyOther
             , moveTo
             , moveLeft
             , moveRight
             , moveUp
             , moveDown
             , wrapLeft
             , wrapRight
             , wrapUp
             , wrapDown
             )where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.ST  (ST, runST)

import qualified Data.Array.MArray as MA
import Data.Array.MArray (getElems, readArray, writeArray, modifyArray)
import Data.Array.ST     (STArray, newListArray)
import Data.Ix           (range, inRange)
import Data.STRef        (STRef, newSTRef, readSTRef, writeSTRef, modifySTRef)
import Data.Tuple        (swap)

import Utils             (chunksOf)

-- State for a grid map
-- Convention: coord is (col, row)
-- Convention: top left corner is (0,0)

data MapStateRefs r t a = MapStateRefs { tiles  :: STArray r (Int, Int) t
                                       , coord  :: STRef r (Int, Int)
                                       , other  :: STRef r a
                                       }

newtype MapState t s a =
  MapState { unMapState :: forall r . MapStateRefs r t s -> ST r a }

instance Functor (MapState t s) where
  fmap f m = MapState (fmap f . unMapState m)

instance Applicative (MapState t s) where
  pure x = MapState (const $ pure x)

  mf <*> mx = MapState (\r -> unMapState mf r <*> unMapState mx r)

instance Monad (MapState t s) where
  mx >>= f = MapState (\r -> unMapState mx r >>= (\x -> unMapState (f x) r))

runMapState :: [[t]] -> (Int, Int) -> s -> MapState t s a -> Maybe (a, s)
runMapState ts (row, col) other m
  | not isWellFormed = Nothing
  | otherwise        = Just $ runST $ do
      tilesR <- newListArray ((0, 0), (rows-1, cols-1)) (concat ts)
      coordR <- newSTRef (row, col)
      otherR <- newSTRef other
      answer <- unMapState m $ MapStateRefs tilesR coordR otherR
      other' <- readSTRef otherR
      pure (answer, other')
  where
    rows    = length ts        :: Int
    cols    = length (head ts) :: Int

    isWellFormed :: Bool
    isWellFormed = 0 < rows && 0 < cols && all ((== cols) . length) ts
                  && 0 <= row && row < rows && 0 <= col && col < cols

evalMapState :: [[t]] -> (Int, Int) -> s -> MapState t s a -> Maybe a
evalMapState ts coord other m = fst <$> runMapState ts coord other m

execMapState :: [[t]] -> (Int, Int) -> s -> MapState t s a -> Maybe s
execMapState ts coord other m = snd <$> runMapState ts coord other m

-- Getters

getCoord :: MapState t s (Int, Int)
getCoord = MapState (readSTRef . coord)

getBounds :: MapState t s (Int, Int)
getBounds = MapState (fmap snd . MA.getBounds . tiles)

getTile :: MapState t s t
getTile = MapState $ \refs -> do
  coord <- readSTRef (coord refs)
  readArray (tiles refs) coord

getTiles :: MapState t s [[t]]
getTiles = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  elems             <- getElems (tiles refs)
  pure (chunksOf cols elems)

getRow :: MapState t s [t]
getRow = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  (col, row)        <- readSTRef (coord refs)
  let readElem = readArray (tiles refs)
      is       = range ((0, row), (cols - 1, row))
  mapM readElem is

getCol :: MapState t s [t]
getCol = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  (col, row)        <- readSTRef (coord refs)
  let readElem = readArray (tiles refs)
      is       = range ((col, 0), (col, rows - 1))
  mapM readElem is

getOther :: MapState t s s
getOther = MapState (readSTRef . other)

getsOther :: (s -> a) -> MapState t s a
getsOther f = MapState (fmap f . readSTRef . other)

-- Finding location inside grid

atLeftEdge :: MapState t s Bool
atLeftEdge = MapState $ \refs -> do
  (col,row) <- readSTRef (coord refs)
  pure $ col == 0

atRightEdge :: MapState t s Bool
atRightEdge = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  (col,row) <- readSTRef (coord refs)
  pure $ col == cols - 1

atTopEdge :: MapState t s Bool
atTopEdge = MapState $ \refs -> do
  (col,row) <- readSTRef (coord refs)
  pure $ row == 0

atBottomEdge :: MapState t s Bool
atBottomEdge = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  (col,row) <- readSTRef (coord refs)
  pure $ row == rows - 1

isInside :: (Int, Int) -> MapState t s Bool
isInside (x,y) = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  pure $ 0 <= x && x < cols && 0 <= y && y < rows

-- Setters

putTile :: t -> MapState t s ()
putTile t = MapState $ \refs -> do
  coord <- readSTRef (coord refs)
  writeArray (tiles refs) coord t

modifyTile :: (t -> t) -> MapState t s ()
modifyTile f = MapState $ \refs -> do
  coord <- readSTRef (coord refs)
  modifyArray (tiles refs) coord f

putOther :: s -> MapState t s ()
putOther o = MapState $ \refs -> writeSTRef (other refs) o

modifyOther :: (s -> s) -> MapState t s ()
modifyOther f = MapState $ \refs -> modifySTRef (other refs) f

moveTo :: (Int, Int) -> MapState t s Bool
moveTo c = MapState $ \refs -> do
  isInside <- unMapState (isInside c) refs
  when isInside $ writeSTRef (coord refs) c
  pure isInside

moveLeft :: MapState t s Bool
moveLeft = MapState $ \refs -> do
  atLeftEdge <- unMapState atLeftEdge refs
  unless atLeftEdge $ modifySTRef (coord refs) $ first (subtract 1)
  pure $ not atLeftEdge

moveRight :: MapState t s Bool
moveRight = MapState $ \refs -> do
  atRightEdge <- unMapState atRightEdge refs
  unless atRightEdge $ modifySTRef (coord refs) $ first (+ 1)
  pure $ not atRightEdge

moveUp :: MapState t s Bool
moveUp = MapState $ \refs -> do
  atTopEdge <- unMapState atTopEdge refs
  unless atTopEdge $ modifySTRef (coord refs) $ second (subtract 1)
  pure $ not atTopEdge

moveDown :: MapState t s Bool
moveDown = MapState $ \refs -> do
  atBottomEdge <- unMapState atBottomEdge refs
  unless atBottomEdge $ modifySTRef (coord refs) $ second (+ 1)
  pure $ not atBottomEdge

wrapLeft :: MapState t s ()
wrapLeft = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  modifySTRef (coord refs) $ first (mod cols . subtract 1)

wrapRight :: MapState t s ()
wrapRight = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  modifySTRef (coord refs) $ first (mod cols . (+ 1))

wrapUp :: MapState t s ()
wrapUp = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  modifySTRef (coord refs) $ second (mod rows . subtract 1)

wrapDown :: MapState t s ()
wrapDown = MapState $ \refs -> do
  (_, (cols, rows)) <- MA.getBounds (tiles refs)
  modifySTRef (coord refs) $ second (mod rows . (+ 1))
