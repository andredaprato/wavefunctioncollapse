{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Wave where
-- import Data.V2
import System.Random
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
  --TODO: we really want this instead
-- type Wave a = V.Vector (V.Vector (Values a))

  
-- | Find the element with the lowest nonzero entropy 
  -- with this lowest he chooses a random *valid* tile for this index
  -- and bans all other tile possibilities,
  -- then he propagates this tile choice to the the neighbouring pixels
-- observe :: Wave -> Index

  -- we should use a minheap to keep track of the min entropy at each iteration
-- minEntropy :: Entropies -> Maybe Coord
-- minEntropy q = findMin q >>= Just . key
 -- map on some noise to the entropies and then return coord with min entropy
debug a = trace (show $ a) a

 -- | gridPos better name 
coord x y = x + y * tileWidth
n = 5 :: Int
tileWidth = 2
tileHeight = 2
directions :: [ Direction ]
directions = [(-1, 0),(0, 1),(1, 0),(0,-1)]
opposite (x,y) = (-x, -y)


calculateEntropy :: Int -> Double -> Double
calculateEntropy sumWeights sumWeightLogWeights
  | sumWeights <= 0 = 0
  | otherwise = log (fromIntegral $ sumWeights) - sumWeightLogWeights / (fromIntegral $ sumWeights)

newtype Tile a = Tile (V.Vector a) deriving (Generic, Eq, Ord, Show)

type Compatible = Bool

data Values a = Values {
  _weights :: Weights,
  _tiles :: M.Map (Tile a) TileVals,
  _entropy :: Entropy
  } deriving (Show)
data TileVals  = TileVals  {
  _tileWeight :: Int,
  _tileLogWeight :: Double,
  _compat :: M.Map Direction Int,
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

  -- | an ordering of maybes such that Just is less than Nothing
  -- | we could create a newtype for Maybe which has this instance as the default
justLess :: Ord a => Maybe a -> Maybe a -> Ordering
justLess Nothing (Just _) = GT
justLess (Just _) Nothing = LT
justLess (Just a) (Just b) = compare a b
justLess Nothing Nothing = EQ



minEntropy :: Wave a -> Maybe Coord
minEntropy  wave =  wave & (fmap fst) . minimumBy ent . map checkNumPatterns . (M.toList) 
  where
    checkNumPatterns c@(coord, vals) =   bool (Just c) Nothing  $
                                       vals ^. (weights . numPatterns) == 1
      
    getEnt = (^? _Just . _2 . entropy)
    ent a b =  justLess  ( getEnt a) ( getEnt b)

  -- TODO: entropies should be updated on every tile ban
updateEntropies :: Coord -> Wave a -> Wave a 
updateEntropies coord wave =  wave & (ix coord . entropy) .~ calculateEntropy sumsOfW sumsOfWLogW
  where sumsOfW = wave ^. (singular $ ix  coord . weights . sumsOfWeights)
        sumsOfWLogW =  wave ^. (singular $ ix  coord . weights . sumsOfWeightLogWeights)


banTile :: (Eq a, Ord a)=> Coord -> Tile a -> Wave a ->Wave a
banTile coord tile  wave =  --debugF (wave ^? ix coord . entropy )
  updateEntropies coord $  newWave
  where
    newWave =  wave
      & (ix coord . tiles . ix tile . possible ) .~ False
      & (struct .  numPatterns) %~  (\x -> case x == 1 of
                                        True -> error "contradiction"
                                        -- True -> 1
                                        False -> subtract 1 x)
      & (struct . sumsOfWeights) %~ (\(x) -> fromMaybe x  ((x-) <$>  tileWeights))
      & (struct . sumsOfWeightLogWeights) %~ (\x -> fromMaybe x ((x-) <$> tileLogWeights))

    struct = ix coord . weights 
    tileWeights =  wave ^? (ix coord . tiles . ix tile .  tileWeight) 
    tileLogWeights = wave ^? (ix coord . tiles . ix tile .  tileLogWeight) 



-- compatible :: forall a . (Eq a, Ord a) => Compat a -> Wave a -> Wave a
-- compatible propPatterns wave =  foldl (\ls coord  -> 
--                                              ls  <> M.foldlWithKey (buildCompatible coord) wave propPatterns)

--                           emptyCompat ([(x,y) | x <- [0..14], y <- [0..14]])
--   where
--     emptyCompat = M.empty
--     buildCompatible :: Coord -> Wave a ->  (Tile a, Direction) -> S.Set (Tile a) ->  Wave a
--   -- we are folding propPatterns at each coordinate and
--   -- at each coordinate, setting the number of compatible tiles in each direction to the size of the compatible tile set 

--   -- want to update in the opposite direction
--     buildCompatible coord wave  k v = wave & (ix coord . tiles . (ix $ fst k ) . compat .  (ix $  opposite $ snd k)) .~ (S.size  v)
                                          -- in debugF (res ^? (ix coord . tiles . (ix $ fst k ) . compat .  (ix $  opposite $ snd k)) ) res

compatible :: forall a . (Eq a, Ord a) => Compat a -> Tile  a -> M.Map Direction Int 
compatible propPatterns tile = M.foldlWithKey (buildCompatible tile) emptyCompat propPatterns
  where
    emptyCompat = M.empty
    buildCompatible ::  Tile a -> M.Map Direction Int -> (Tile a, Direction) -> S.Set (Tile a) ->  M.Map Direction Int
  -- we are folding propPatterns at each coordinate and
  -- at each coordinate, setting the number of compatible tiles in each direction to the size of the compatible tile set 

  -- want to update in the opposite direction
    buildCompatible tile map  k v = map & (at $  opposite $ snd k) ?~ (S.size  v)
 
debugF f a = trace (show f) a

  -- data structure of all compatible patterns
type Compat a = M.Map (Tile a, Direction) (S.Set (Tile a))

-- | Generate initial compatibility matrix such that the returned pattern combinations are
--   compatible at each (dx,dy) offset
propagatorPatterns :: (Eq a, Ord a) => [Tile a]  -> Compat a
propagatorPatterns patterns = foldl addPattern compatSet allAgree
  where
    addPattern :: (Eq a, Ord a) =>Compat a -> (Tile a, Tile a, Direction) ->  Compat a
    addPattern  hMap (p1,p2,i)  = case hMap ^? ix (p1,i) of
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
    -- return $ p1 V.! (debug $ coord x y)  == p2 V.! (debug $ coord (x-dx) (y-dy))
    return $ p1 V.! coord x y  == p2 V.! coord (x-dx) (y-dy)



propagate :: (Show a, Eq a, Ord a) => Tile a -> Coord -> Compat a -> Wave a -> Wave a
propagate tile (x1,y1) propagator wave  = 
  foldl  decrementNeighbours wave directions 

  where decrementNeighbours  hMap d@(dx,dy) =
          let x2 = (x1 + dx + n) `mod` n
              y2 = (y1 + dy + n) `mod` n
  ----------------------------------------------------------------- error here
              tiles =  case propagator ^? (ix (tile, d) )  of
                Nothing -> S.empty
                Just s -> s
                
          in S.foldl (decrementNeighbour d (x2,y2) ) hMap tiles

        decrementNeighbour d coord wave tile   =
          case  wave ^? (ix coord.tiles. ix tile . compat . ix d) == Just 1 of

            True -> propagate tile coord  propagator $ banTile coord tile $  wave & (ix coord.tiles.ix tile . compat . ix d) %~ subtract 1 
            False ->  wave & (ix coord.tiles.ix tile . compat . ix d) %~ subtract 1 
        waveAt = wave ^? ix (x1,y1).tiles.ix tile . compat 

-- data Quantum a = Collapsed (Wave a) | SuperPos (Wave a)
data Quantum a b = Collapsed (Wave a) | SuperPos (Wave a) | Result b deriving (Show)
makePrisms ''Quantum
-- collapseWave :: (Show a , Eq a, Ord a, RandomGen g) => g ->  Compat a -> Quantum a (M.Map Coord [Tile a]) -> Quantum  a (M.Map Coord [Tile a])
collapseWave g compat q = go g compat q
  where go g compat q = case q of
          Collapsed wave -> Result $ M.map (\vals -> case M.keys $ M.filter ((^. possible) ) (vals ^. tiles)  of
                                                [] -> [Tile $ V.fromList [PixelRGB8 43 17 0]]
                                                xs -> xs
                                            ) wave
          SuperPos wave -> case minEntropy wave of
                     Nothing -> collapseWave g compat (Collapsed wave) 
                     Just coord ->
                       let tile =  fst $ randomInList g $ M.toList  $ M.filter ((^. possible)) $  wave ^. (singular $ ix coord . tiles)
                           bannees =   M.keys $ M.filterWithKey (\key val -> key /= tile &&  (val ^. possible)) $  wave ^. (singular $ ix coord . tiles)
                       -- TODO: still need to select a random tile i guess
   -- WE WANT TO BAN EVERY TILE BUT THIS ONE!!!!!
                       in go (snd $ next g) compat . SuperPos
                          $ foldl (\wave tile ->  propagate tile coord compat $ banTile coord tile wave ) wave bannees

          Result res -> error "nope"

randomInList :: RandomGen g => g -> [a] -> a
randomInList g list = list !! index
  where index = fst $ randomR (0, length list - 1) g 
  -- take in a coordinate and a transformation function and return a tile
readTile :: Show a => Pixel a => Image a -> Coord -> (Coord -> Coord) -> Tile a
readTile img (x,y) f = Tile . V.fromList $ do
  dx <- [0..n-1]
  dy <- [0..n-1]
  let coord = f $ (x-dx `mod` (imageWidth img) , y-dy `mod` (imageHeight img)) 
  return $  uncurry  ((pixelAt img))  $ coord

wLogW t =  (fromIntegral t) * log (fromIntegral t) 
parseImage ::RandomGen g => g -> Image PixelRGB8 -> (Compat PixelRGB8, Quantum PixelRGB8 b)
parseImage g img@Image{..}  = (propPatterns, SuperPos wave)
  where

    wave = M.fromList
      $ zip ([(x,y)  | x <- [0..imageWidth-1], y <-  [0..imageHeight-1]])
      $ map   (\x -> Values { _entropy = initialEntropy * (x * 1e-6)
                            , _tiles = constructTiles
                            , _weights = initialWeights
                            })
                     ((randoms @Double) g )
    
    initialWeights  = Weights { _numPatterns =  debug $ M.size constructTiles
    -- initialWeights  = Weights { _numPatterns = 2
                              , _sumsOfWeights = sumW
                              , _sumsOfWeightLogWeights = sumWLogW
                              }
    initialEntropy   = calculateEntropy sumW sumWLogW

    sumW =  sum $  constructTiles ^.. (folded .  tileWeight)
    sumWLogW = sum  $  constructTiles ^.. (folded . tileWeight . to wLogW )

    constructTiles = foldl addTile M.empty tiles

    -- addTile :: Ord a =>  M.Map (Tile a) TileVals  -> Tile a -> M.Map (Tile a) TileVals
    addTile tileMap tile  = case tileMap ^? ix tile of
      Nothing -> tileMap & at tile ?~ TileVals { _tileWeight = 1
                                               , _tileLogWeight = 0
                                               -- , _compat = M.fromList $ zip directions $ repeat 0
                                               , _compat = compatible propPatterns tile 
                                               , _possible = True
                                               }
      Just vals@TileVals{..} ->  tileMap & ix tile %~ \v -> v { _tileWeight =  _tileWeight +1
                                                              , _tileLogWeight = log (fromIntegral _tileWeight + 1) }

    propPatterns = propagatorPatterns $  tiles
      
      
    tiles = do
          x <- [0..imageWidth-1]
          y <- [0..imageHeight-1]
          guard $  x - n+1 >= 0 && y - n + 1  >= 0
          return $  readTile img (x,y) id 


untile (Tile a ) = a
output ::  Coord -> M.Map Coord [Tile PixelRGB8] -> Image PixelRGB8
output (x,y ) wave =   generateImage  renderWave x y
  where renderWave x y = V.head $ untile $ head $   wave ^. ((singular $  ix (x,y)) )
        
  

-- imageWidth = 48
-- imageHeight = 48

main :: IO ()
main = do
  -- decoder <- readImage "3Bricks.png"
  decoder <- readImage "Flowers.png"
  -- decoder <- readImage "ColoredCity.png"
  gen <- getStdGen
  let img = convertRGB8 $ fromRight (error "broken") decoder  
  let collapsed = uncurry (collapseWave gen) (parseImage gen img)  
  print $ imageHeight img 
  let out = output (imageWidth img,imageHeight img) $  collapsed ^. (_Result )
  writePng  "output.png"   out 
          -- makePatterns img >>= propagatorPatterns >>= collapseWave 

