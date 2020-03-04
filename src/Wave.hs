{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Wave where
-- import Data.V2
import Codec.Picture
import Debug.Trace

import qualified Data.Vector as V
import Data.Vector.Instances
import Data.Either
import Data.List
import Control.Monad
import qualified Data.Set as S
import Data.Traversable
import Data.Bool

-- import Data.PSQueue hiding (foldr)
-- import qualified Data.HashMap.Strict as H -- strict or lazy?
import qualified Data.Map.Strict as M
import Control.Lens

import GHC.Generics hiding (to)
import Data.Maybe


type Entropy = Double
  
type Coord = (Int, Int)
type Direction = (Int,Int)
type Wave a = M.Map Coord (Values a)

  
-- | Find the element with the lowest nonzero entropy 
  -- with this lowest he chooses a random *valid* tile for this index
  -- and bans all other tile possibilities,
  -- then he propagates this tile choice to the the neighbouring pixels
-- observe :: Wave -> Index

  -- we should use a minheap to keep track of the min entropy at each iteration
-- minEntropy :: Entropies -> Maybe Coord
-- minEntropy q = findMin q >>= Just . key
 -- map on some noise to the entropies and then return coord with min entropy

coord x y = x + y * tileWidth
n = 2 :: Int
tileWidth = 2
tileHeight = 2
directions :: [ Direction ]
directions = [(-1, 0),(0, 1),(1, 0),(0,-1)]

calculateEntropy sumWeights sumWeightLogWeights = log sumWeights - sumWeightLogWeights / sumWeights

newtype Tile a = Tile (V.Vector a) deriving (Generic, Eq, Ord, Show)

type Compatible = Bool

data Values a = Values {
  _weights :: Weights,
  _tiles :: M.Map (Tile a) TileVals,
  _entropy :: Entropy
  } deriving (Show)
data TileVals  = TileVals  {
  _tileWeight :: Double,
  _tileLogWeight :: Double,
  _compat :: M.Map Direction Int,
  _possible :: Bool
  } deriving Show

data Weights = Weights {
  _numPatterns :: Int,
  _sumsOfWeights :: Double,
  _sumsOfWeightLogWeights :: Double
  } deriving Show

$(makeLenses ''Values)
$(makeLenses ''Weights)
$(makeLenses ''TileVals)
  -- we should generalize the index of this structure to allow for different representations
  -- of the input
  -- TODO : think about how to generalize the dimension of the input array, though I doubt this is easy mainly because of verifying valid patterns
-- type Wave a = M.IntMap (Values a)
  -- | an ordering of maybes such that Just is greater than Nothing
  -- | we could create a newtype for Maybe which has this instance as the default
justGreater :: Ord a => Maybe a -> Maybe a -> Ordering
justGreater Nothing (Just _) = LT
justGreater (Just _) Nothing = GT
justGreater (Just a) (Just b) = compare a b
justGreater Nothing Nothing = EQ



minEntropy :: Wave a -> Maybe Coord
minEntropy  = (fmap fst) . minimumBy ent . map checkNumPatterns . M.toList 
  where
    checkNumPatterns c@(coord, vals) = bool (Just c) Nothing  $
                                        vals ^. (weights . numPatterns) == 1
      
  -- TODO : Nothing will always be less than just so we will return Nothing as soon as one pattern is finalized
    getEnt = (^? _Just . _2 . entropy)
    ent a b =  justGreater  (getEnt a) (getEnt b)

-- updateEntropies :: Wave a -> Entropies -> Coord -> Entropies
updateEntropies :: Coord -> Wave a -> Wave a 
updateEntropies coord wave =  wave & (ix coord . entropy) .~ (calculateEntropy sumsOfW sumsOfWLogW) 
  where sumsOfW = wave ^. (singular $ ix  coord . weights . sumsOfWeights)
        sumsOfWLogW =  wave ^. (singular $ ix  coord . weights . sumsOfWeightLogWeights)


banTile :: (Eq a, Ord a)=> Coord -> Tile a -> Wave a ->Wave a
banTile index tile  wave = newWave
  where
    newWave = wave
      & (ix index . tiles . ix tile . possible ) .~ False
      & (struct . numPatterns) %~ ( flip (-) 1)
      & (struct . sumsOfWeights) %~ (\x -> fromMaybe x  ((x-) <$> tileWeights))
      & (struct . sumsOfWeightLogWeights) %~ (\x -> fromMaybe x ((x-) <$> tileLogWeights))

    struct = ix index . weights 
    tileWeights =  wave ^? (ix index . tiles . ix tile .  tileWeight) 
    tileLogWeights = wave ^? (ix index . tiles . ix tile .  tileLogWeight) 


 -- TODO: make a data structure which holds the number of compatible patterns in each direction
  -- for  each pixel

compatible :: forall a . (Eq a, Ord a) => Compat a -> Wave a -> Wave a
compatible propPatterns wave =  foldr (\coord ls -> 
                                             ls  <> M.foldrWithKey (buildCompatible coord) wave propPatterns)

                          emptyCompat (zip [0..48] [0..48])
  where
    emptyCompat = M.empty
    buildCompatible :: Coord -> (Tile a, Direction) -> S.Set (Tile a) -> Wave a -> Wave a
    -- this doesn't really do what we want because it wont set it if it's not already there
    --  kind of want this to be an update though actually because TileVals will already be instantiated
    --                                                          ^^^ (want this to be `at`)
    buildCompatible coord k v wave = wave & (ix coord . tiles . (ix $ fst k ) . compat .  (ix $ snd k)) .~ (S.size v)


  -- data structure of all compatible patterns
type Compat a = M.Map (Tile a, Direction) (S.Set (Tile a))

-- | Generate initial compatibility matrix such that the returned pattern combinations are
--   compatible at each (dx,dy) offset
propagatorPatterns :: (Eq a, Ord a) => [Tile a]  -> Compat a
propagatorPatterns patterns = foldr addPattern compatSet allAgree
  where
    addPattern :: (Eq a, Ord a) => (Tile a, Tile a, Direction) -> Compat a -> Compat a
    addPattern (p1,p2,i) hMap  = case hMap ^? ix (p1,i) of
      Nothing -> hMap &  at (p1,i) ?~ S.singleton p2
      Just s  -> hMap &  ix (p1,i) %~ S.insert p2
      
    compatSet = M.empty
    allAgree = do
          p1 <- patterns
          p2 <- patterns
          d <-  directions
          guard $ agrees p1 p2 d
          pure $ (p1, p2, d)
  
  

agrees :: Eq a => Tile a -> Tile a -> Direction -> Bool
agrees (Tile p1) (Tile p2) (dx, dy)  = 
  let xmin = if dx < 0 then 0 else dx
      xmax = if dx < 0 then n + dx else  n
      ymin = if dy < 0 then 0 else dy
      ymax = if dy < 0 then n + dy else n
  in and $ do
    x <-  [xmin..xmax-1] 
    y <-  [ymin..ymax-1]
    -- pure $ trace ("show x" ++ show x) $ id   x
    -- return $ p1 V.! coord x y  == p2 V.! coord (x-dx) (y-dy)
    return $ p1 V.! (coord x y)  == p2 V.! coord (x-dx) (y-dy)



propagate :: (Eq a, Ord a) => Tile a -> Coord -> Compat a -> Wave a -> Wave a
propagate tile (x1,y1) propagator wave  =
  foldr  decrementNeighbours wave directions 

  where decrementNeighbours d@(dx,dy) hMap =
          let x2 = (x1 + dx + n) `mod` n
              y2 = (y1 + dy + n) `mod` n
              tiles =  propagator ^. (singular  $ ix (tile, d) )
  -- fold with a possible recursive case, which means we need to fold on the wave
          in S.foldr (decrementNeighbour d (x2,y2) ) hMap tiles

        decrementNeighbour d coord tile wave  =
          case wave ^? (ix coord.tiles. ix tile . compat . ix d) == Just 1 of

            True -> propagate tile coord  propagator $ banTile coord tile $  wave & (ix coord.tiles.ix tile . compat . ix d) %~ (-) 1 
            False ->  wave & (ix coord.tiles.ix tile . compat . ix d) %~ (-) 1 

-- data Quantum a = Collapsed (Wave a) | SuperPos (Wave a)
data Quantum a b = Collapsed (Wave a) | SuperPos (Wave a) | Result b deriving (Show)
makePrisms ''Quantum
collapseWave :: (Eq a, Ord a) => Compat a -> Quantum a (M.Map Coord [Tile a]) -> Quantum  a (M.Map Coord [Tile a])
collapseWave compat wave =  case wave of
  Collapsed wave -> Result $ M.map  (\vals -> M.keys $ M.filter (^. possible ) (vals ^. tiles) ) wave
  SuperPos wave -> case  minEntropy wave of
                     Nothing -> collapseWave compat (Collapsed wave) 
                       -- TODO: still need to select a random tile i guess
                     Just coord ->
                       let tile = fst $ head $ M.toList  $ wave ^. (singular $ ix coord . tiles)
                       in collapseWave compat . SuperPos
                          $! propagate tile coord compat 
                          $! updateEntropies coord
                          $! banTile coord tile wave
  Result res -> error "nope"

  -- take in a coordinate and a transformation function and return a tile
readTile :: Pixel a => Image a -> Coord -> (Coord -> Coord) -> Tile a
readTile img (x,y) f = Tile . V.fromList $ do
  
        dx <- [0..n-1]
        dy <- [0..n-1]
        let coord = f $ (x-dx `mod` (imageWidth img) , y-dy `mod` (imageHeight img)) 
        return $ uncurry  (pixelAt img)  $ coord

wLogW t =  t * log t 
parseImage :: Image PixelRGB8 -> (Compat PixelRGB8, Quantum PixelRGB8 b)
parseImage img@Image{..} = (propPatterns, SuperPos wave)
  where

    wave = M.fromList $ zip ([(x,y)  | x <- [0..4], y <-  [0..4]]) $ repeat Values { _entropy = initialEntropy
                                                                                                      , _tiles = constructTiles
                                                                                                      , _weights = initialWeights
                                                                      }
    
    initialWeights  = Weights { _numPatterns =  M.size constructTiles
    -- initialWeights  = Weights { _numPatterns = 2
                              , _sumsOfWeights = sumW
                              , _sumsOfWeightLogWeights = sumWLogW
                              }
    initialEntropy  = log sumW - sumWLogW / sumW

    sumW =  sum $  constructTiles ^.. (folded .  tileWeight)
    sumWLogW = sum  $  constructTiles ^.. (folded . tileWeight . to wLogW )

    constructTiles = foldr addTile M.empty tiles

    addTile :: Ord a => Tile a -> M.Map (Tile a) TileVals  ->  M.Map (Tile a) TileVals
    addTile tile tileMap = case tileMap ^? ix tile of
      Nothing -> tileMap & at tile ?~ TileVals { _tileWeight = 1
                                               , _tileLogWeight = 0
                                               , _compat = M.fromList $ zip directions $ repeat 0
                                               , _possible = True
                                               }
      Just vals@TileVals{..} ->  tileMap & ix tile %~ \v -> v { _tileWeight = _tileWeight +1
                                                              , _tileLogWeight = log (_tileWeight + 1) }

    propPatterns = propagatorPatterns $  tiles
      
      
    tiles = do
          x <- [0..imageWidth-1]
          y <- [0..imageHeight-1]
          guard $  x - n+1 >= 0 && y - n + 1  >= 0
          return $  readTile img (x,y) id 

   -- for imagewidth and height readtile write it to the set
   -- then write in waits and start the loopedy loop




  --somehow setting everything to false
output ::  M.Map Coord [Tile PixelRGB8] -> Image PixelRGB8
output wave =   generateImage renderWave  5 5 
  where renderWave x y = V.head $ untile $ head $  wave ^. ((singular $  ix (x,y)) )
  -- where renderWave x y =  maybe (PixelRGB8 0 0 0) id $  V.head <$> untile <$> head  <$> wave ^? ( ix (x,y) )
        untile (Tile a ) = a
 -- | gridPos better name 
        
  

-- imageWidth = 48
-- imageHeight = 48

main :: IO ()
main = do
  decoder <- readImage "3Bricks.png"
  let img = convertRGB8 $ fromRight (error "broken") decoder  
  let collapsed = uncurry collapseWave  (parseImage img)  
  print $ imageHeight img
  let out = output $  collapsed ^. (_Result )
  -- pure ()
  writePng  "output.png"   out 
          -- makePatterns img >>= propagatorPatterns >>= collapseWave 

