{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import qualified Data.Vector as V
import qualified Data.Map as M
import GHC.Generics
import qualified Data.Set as S
import Control.Lens (makeLenses, makePrisms)

coord :: Int -> Int -> Int
coord x y = x + y * n

n = 3 :: Int

directions :: [ Direction ]
directions = [(-1, 0),(0, 1),(1, 0),(0,-1)]

opposite :: Direction -> Direction
opposite (x,y) = (-x, -y)

type Coord = (Int, Int)
type Direction = (Int,Int)
type Entropy = Double

newtype Tile a = Tile (V.Vector a) deriving (Generic, Eq, Ord, Show)
untile (Tile a ) = a

type Compat a = M.Map (Tile a, Direction) (S.Set (Tile a))

data Values a = Values {
  _weights :: Weights,
  _tiles :: M.Map (Tile a) TileVals,
  _entropy :: Entropy
  } deriving (Show)

data TileVals = TileVals {
  _tileWeight :: Int,
  _tileLogWeight :: Double,
  _compat :: M.Map Direction Int, -- number of compatible tiles in that direction
  _possible :: Bool
  } deriving Show

data Weights = Weights {
  _numPatterns :: Int,
  _sumsOfWeights :: Int,
  _sumsOfWeightLogWeights :: Double
  } deriving Show

$(makeLenses ''Values)
$(makeLenses ''Weights)
$(makeLenses ''TileVals)

  
type Wave a = M.Map Coord (Values a)
  --TODO: we really want this instead
-- type Wave a = V.Vector (V.Vector (Values a))

data Quantum a b = Collapsed (Wave a) | SuperPos (Wave a) | Result b deriving (Show)
makePrisms ''Quantum
