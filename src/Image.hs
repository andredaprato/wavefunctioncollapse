{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RecordWildCards #-}
module Image where

import           Codec.Picture
import           Control.Lens
import           Control.Monad
import           Data.List (nub)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector as V
import           System.Random
import           Types
import           Wave

-- | Returns the number of tiles which are compatible in each direction with the input tile 
compatible :: forall a . (Eq a, Ord a, Show a) => Compat a -> Tile a -> M.Map Direction Int 
compatible propPatterns tile =
  M.foldlWithKey (buildCompatible tile) emptyCompat $ M.restrictKeys propPatterns (S.fromList $ zip (repeat tile) directions ) 
  where
    emptyCompat = M.empty
    buildCompatible ::  Tile a -> M.Map Direction Int -> (Tile a, Direction) -> S.Set (Tile a) ->  M.Map Direction Int
    buildCompatible tile map  k v = map & (at $ opposite $ snd k) ?~ (S.size  v)

-- | Generate initial compatibility matrix such that the returned pattern combinations are
--   compatible at each (dx,dy) offset
mkCompatMatrix :: (Eq a, Ord a, Show a) => [Tile a]  -> Compat a
mkCompatMatrix tiles = foldl addPattern compatSet allAgree
 where
    addPattern :: (Eq a, Ord a, Show a) =>Compat a -> (Tile a, Tile a, Direction) ->  Compat a
    addPattern  compat (p1,p2,d)  = case compat ^? ix (p1,d) of
      Nothing -> compat &  at (p1,d) ?~ S.singleton p2
      Just s  -> compat &  ix (p1,d) %~ S.insert p2 
    allAgree = do
          p1 <- tiles
          p2 <- tiles
          d <-  directions
          guard $ agrees p1 p2 d
          return $ (p1, p2, d)
    compatSet = M.empty

-- take in a coordinate and a transformation function and return a tile
readTile ::  Pixel a => Image a -> Coord -> (Coord -> Coord) -> Maybe (Tile a)
readTile img (x,y) f = fmap (Tile . V.fromList) . sequenceA $ do
  dx <- [0..n-1]
  dy <- [0..n-1]
  let coord = f $ (x+dx `mod` imageWidth img, y+dy `mod` imageHeight img) 
  return $ readPix img coord

readPix img (x,y)= do
  guard $ x >= 0 && x < imageWidth img
  guard $ y >= 0 && y < imageHeight img
  return $ pixelAt img  x y

xLogX t =  (fromIntegral t) * log (fromIntegral t) 

parseImage ::RandomGen g => g -> Image PixelRGB8 -> Coord -> (Compat PixelRGB8, Wave PixelRGB8 )
parseImage g img@Image{..} (outHeight, outWidth)  = (propPatterns, wave)
  where

    wave = M.fromList
      $ zip [(x,y)  | x <- [0..outWidth-1], y <-  [0..outHeight-1]]
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
    sumWLogW = sum  $  constructTiles ^.. (folded . tileWeight . to xLogX )

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

    propPatterns = mkCompatMatrix  $ nub tiles 
      
      
    tiles = do
          x <- [0..imageWidth-1]
          y <- [0..imageHeight-1]
          fromMaybe []  $ pure <$>  readTile img (x,y) id 


mkImg ::  Coord -> M.Map Coord [Tile PixelRGB8] -> Image PixelRGB8
mkImg (x,y ) wave =   generateImage  renderWave x y
  where renderWave x y = V.head $ untile $ head $   wave ^. (singular $  ix (x,y))
