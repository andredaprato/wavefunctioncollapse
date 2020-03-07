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
import qualified Data.Map.Strict as M
import Control.Lens

import GHC.Generics hiding (to)
import Data.Maybe
debug a = trace (show $ a) a

debugF f a = trace (show f) a


 -- | gridPos better name 
coord x y = x + y * n
n = 3 :: Int
directions :: [ Direction ]
directions = [(-1, 0),(0, 1),(1, 0),(0,-1)]
opposite (x,y) = (-x, -y)
outHeight = 16
outWidth = 16
-- imgHeight = 31
-- imgWidth  = 32

calculateEntropy :: Int -> Double -> Double
calculateEntropy sumWeights sumWeightLogWeights
  | sumWeights <= 0 = 0
  | otherwise = log (fromIntegral $ sumWeights) - sumWeightLogWeights / (fromIntegral $ sumWeights)


type Coord = (Int, Int)
type Direction = (Int,Int)
type Entropy = Double

newtype Tile a = Tile (V.Vector a) deriving (Generic, Eq, Ord, Show)

  -- data structure of all compatible patterns
type Compat a = M.Map (Tile a, Direction) (S.Set (Tile a))


data Values a = Values {
  _weights :: Weights,
  _tiles :: M.Map (Tile a) TileVals,
  _entropy :: Entropy
  } deriving (Show)

data TileVals = TileVals {
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

  
type Wave a = M.Map Coord (Values a)
  --TODO: we really want this instead
-- type Wave a = V.Vector (V.Vector (Values a))

data Quantum a b = Collapsed (Wave a) | SuperPos (Wave a) | Result b deriving (Show)
makePrisms ''Quantum


  
  -- | an ordering of maybes such that Just is less than Nothing
  -- | we could create a newtype for Maybe which has this instance as the default
justLess :: Ord a => Maybe a -> Maybe a -> Ordering
justLess Nothing (Just _) = GT
justLess (Just _) Nothing = LT
justLess (Just a) (Just b) = compare a b
justLess Nothing Nothing = EQ



minEntropy :: RandomGen g => g -> Wave a -> Maybe Coord
minEntropy  g = fmap fst . snd .  minimumBy ent . map checkNumPatterns . zip (randoms @Double g ) . M.toList 
  where
    checkNumPatterns (g, c@(coord, vals)) =   bool (g, Nothing) (g, Just c)   $
                                       (vals ^. (weights . numPatterns)) > 1
      
    getEnt = (^? _Just . _2 . entropy)
    ent (g1, a) (g2, b) =  justLess  (addNoise g1 $  getEnt a) (addNoise g2 $  getEnt b)

    addNoise g = fmap ((+) (g * 1e-6)   )

updateEntropies :: Coord -> Wave a -> Wave a 
updateEntropies coord wave =  wave & (ix coord . entropy) .~ calculateEntropy sumsOfW sumsOfWLogW
  where sumsOfW = wave ^. (singular $ ix  coord . weights . sumsOfWeights)
        sumsOfWLogW =  wave ^. (singular $ ix  coord . weights . sumsOfWeightLogWeights)


  -- probably better that this does not return the wave but just the necessary values 
banTile :: (Eq a, Ord a) => Coord -> Tile a -> Wave a ->Wave a
banTile coord tile  wave =  --debugF (wave ^? ix coord . entropy )
  updateEntropies coord $  newWave
  where
    newWave =  wave
      & (ix coord . tiles . ix tile . possible ) .~ False
      & (struct .  numPatterns) %~  (\x -> case x == 1 of
                                        -- True -> error "contradiction"
                                        True -> 1
                                        False -> subtract 1 x)
      & (struct . sumsOfWeights) %~ (\(x) -> x -  tileWeights)
      & (struct . sumsOfWeightLogWeights) %~ (\x -> x - tileLogWeights)
      & (ix coord . tiles . ix tile . compat  ) %~ (const $ M.fromList $ zip directions (repeat 0))

    struct = ix coord . weights 
    tileWeights =  wave ^. (singular $ ix coord . tiles . ix tile .  tileWeight) 
    tileLogWeights = wave ^. (singular $ ix coord . tiles . ix tile .  tileLogWeight) 

compatible :: forall a . (Eq a, Ord a, Show a) => Compat a -> Tile  a -> M.Map Direction Int 
compatible propPatterns tile = M.foldlWithKey (buildCompatible tile) emptyCompat (M.restrictKeys propPatterns (S.fromList $ zip (repeat tile) directions ) )
  where
    emptyCompat = M.empty
    buildCompatible ::  Tile a -> M.Map Direction Int -> (Tile a, Direction) -> S.Set (Tile a) ->  M.Map Direction Int
    buildCompatible tile map  k v = map & (at $ opposite $ snd k) ?~ (S.size  v)
 

-- | Generate initial compatibility matrix such that the returned pattern combinations are
--   compatible at each (dx,dy) offset
propagatorPatterns :: (Eq a, Ord a, Show a) => [Tile a]  -> Compat a
propagatorPatterns tiles = let x = foldl addPattern compatSet allAgree
                           in debugF (tiles == (nub $ fst <$> M.keys x)) x
                             
                               
  where
    addPattern :: (Eq a, Ord a, Show a) =>Compat a -> (Tile a, Tile a, Direction) ->  Compat a
    addPattern  compat (p1,p2,d)  = case compat ^? ix (p1,d) of
      Nothing -> compat &  at (p1,d) ?~ S.singleton p2
      Just s  -> compat &  ix (p1,d) %~ (  S.insert p2 )
    allAgree = do
          p1 <- tiles
          p2 <- tiles
          d <-  directions
          guard $ agrees p1 p2 d
          pure $ (p1, p2, d)
    compatSet = M.empty
  
  

agrees :: Eq a => Tile a -> Tile a -> Direction -> Bool
agrees (Tile p1) (Tile p2) (dx, dy)  = 
  let xmin = if dx < 0 then 0 else dx
      xmax = if dx < 0 then n + dx else  n
      ymin = if dy < 0 then 0 else dy
      ymax = if dy < 0 then n + dy else n
  in and $ do
    x <-  [xmin..xmax-1] 
    y <-  [ymin..ymax-1]
    return $ p1 V.! coord x y  == p2 V.! coord (x-dx) (y-dy)

dxdy (x,y) (dx,dy) = ((x + dx + outWidth) `mod` outWidth , (y + dy  + outHeight) `mod` outHeight)

propagate :: (Show a, Eq a, Ord a) => Tile a -> Coord -> Compat a -> Wave a -> Wave a
propagate tile xy@(x1,y1) propagator wave  = 
  foldl (\w (t, d) -> propagate t (dxdy xy d) propagator w ) (snd decrementAll) (fst decrementAll)

  where
    decrementAll = foldl  (\(ts, w) d ->
                             let x = decrementNeighbours w d in (fst x ++ ts, snd x ))
                   ([], wave) directions 

    decrementNeighbours  hMap d@(dx,dy) =
          
          let (x2, y2) = dxdy (x1,y1) d

              tiles = case x2 < 0 || x2 >= outWidth || y2 < 0 || y2 >= outHeight of
                False -> fromMaybe S.empty $ propagator ^? (ix (tile, d) )  
                True -> S.empty
              toNeighbours =  S.foldl (\(banned, wave) tile -> case decrementNeighbour d (x2,y2) wave tile of
                                          (Nothing, w) -> (banned, w)
                                          (Just t, w) -> ((t, d) : banned , w)
                                      ) ([] , hMap) tiles
                
          in toNeighbours

    decrementNeighbour d coord wave tile =
          case  wave ^? (ix coord.tiles. ix tile . compat . ix d) == Just 1 of
            True -> (Just tile , banTile coord tile $  wave & (ix coord.tiles.ix tile . compat . ix d) %~ subtract 1 )

            False -> (Nothing ,  wave & (ix coord.tiles.ix tile . compat . ix d) %~ subtract 1 )

-- collapseWave :: (Show a , Eq a, Ord a, RandomGen g) => g ->  Compat a -> Quantum a (M.Map Coord [Tile a]) -> Quantum  a (M.Map Coord [Tile a])
collapseWave g compat q = go g compat q
  where go g compat q = case q of
          Collapsed wave -> Result $ M.map (\vals -> case M.keys $ M.filter ((^. possible) ) (vals ^. tiles)  of
                                                [] -> [Tile $ V.fromList [PixelRGB8 255 0 255]]
                                                xs -> xs
                                            ) wave
          SuperPos wave -> case minEntropy g wave of
                     Nothing -> collapseWave g compat (Collapsed wave) 
                     Just coord ->
                       let tile =  fst $ randomInList (nextGen g) $ M.toList  $ M.filter ((^. possible)) $  wave ^. (singular $ ix coord . tiles)
                           bannees =   M.keys $ M.filterWithKey (\key val -> key /= tile &&  (val ^. possible)) $  wave ^. (singular $ ix coord . tiles)
                           bannedWave = foldl (\w tile ->   banTile coord tile w ) wave bannees
                           
                       in go (nextGen  g) compat . SuperPos
                          $  foldl (\w tile -> propagate tile coord compat w) bannedWave bannees

          Result res -> error "nope"
nextGen = snd . split 
randomInList :: RandomGen g => g -> [a] -> a
randomInList g list = list !! index
  where index = fst $ randomR (0, length list - 1) g 

  -- take in a coordinate and a transformation function and return a tile
readTile ::  Pixel a => Image a -> Coord -> (Coord -> Coord) -> Maybe (Tile a)
readTile img (x,y) f = fmap (Tile . V.fromList) . sequenceA $ do
  dx <- [0..n-1]
  dy <- [0..n-1]
  let coord = f $ (x+dx, y+dy ) 
  return $ readPix img coord

readPix img (x,y)= do
  guard $ x>=0 && x < imageWidth img
  guard $ y>=0 && y < imageHeight img
  return $ pixelAt img  x y

wLogW t =  (fromIntegral t) * log (fromIntegral t) 

  
parseImage ::RandomGen g => g -> Image PixelRGB8 -> (Compat PixelRGB8, Quantum PixelRGB8 b)
parseImage g img@Image{..}  = (propPatterns, SuperPos wave)
  where

    wave = M.fromList
      -- $ zip ([(x,y)  | x <- [0..imageWidth-1], y <-  [0..imageHeight-1]])
      $ zip ([(x,y)  | x <- [0..outWidth-1], y <-  [0..outHeight-1]])
      $ repeat  Values { _entropy = initialEntropy 
                       , _tiles = constructTiles
                       , _weights = initialWeights
                        }
    
    initialWeights  = Weights { _numPatterns = M.size constructTiles
                              , _sumsOfWeights = sumW
                              , _sumsOfWeightLogWeights = sumWLogW
                              }
    initialEntropy   = calculateEntropy sumW sumWLogW

    sumW =  sum $  constructTiles ^.. (folded .  tileWeight)
    sumWLogW = sum  $  constructTiles ^.. (folded . tileWeight . to wLogW )

    constructTiles = foldl addTile M.empty tiles

    addTile :: M.Map (Tile PixelRGB8) TileVals  -> Tile PixelRGB8 -> M.Map (Tile PixelRGB8) TileVals
    addTile tileMap tile  = case tileMap ^? ix tile of
      Nothing -> tileMap & at tile ?~ TileVals { _tileWeight = 1
                                               , _tileLogWeight = 0
                                               , _compat = compatible propPatterns tile
                                               , _possible = True
                                               }
      Just vals@TileVals{..} ->  tileMap & ix tile %~ \v -> v { _tileWeight =  _tileWeight +1
                                                              , _tileLogWeight = log (fromIntegral _tileWeight + 1) }

    propPatterns = propagatorPatterns  $ nub tiles 
      
      
    tiles = do
          x <- [0..imageWidth-1]
          y <- [0..imageHeight-1]
          fromMaybe []  $ return <$>  readTile img (x,y) id 


untile (Tile a ) = a
output ::  Coord -> M.Map Coord [Tile PixelRGB8] -> Image PixelRGB8
output (x,y ) wave =   generateImage  renderWave x y
  where renderWave x y = V.head $ untile $ head $   wave ^. (singular $  ix (x,y))
        
  
  -- it doesn't like nonsquare input
main :: IO ()
main = do
  -- decoder <- readImage "3Bricks.png"
  decoder <- readImage "Flowers.png"
  -- decoder <- readImage "ColoredCity.png"
  -- decoder <- readImage "Skyline2.png"
  -- decoder <- readImage "Dungeon.png"
  gen <- getStdGen
  let img = convertRGB8 $ fromRight (error "broken") decoder  
  let collapsed = uncurry (collapseWave gen) (parseImage gen img)  
  print $ imageHeight img 
  print $ imageWidth img
  let out = output (outWidth,outHeight) $  collapsed ^. (_Result )
  writePng  "output.png"   out 

