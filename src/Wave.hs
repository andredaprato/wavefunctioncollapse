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

import           System.Environment
import           System.Random
import           Codec.Picture

import           Control.Lens
import           Control.Monad
import           Data.Bool
import           Data.Either
import           Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Traversable
import qualified Data.Vector as V
import           Data.Vector.Instances
import           GHC.Generics hiding (to)
import           Data.Maybe


import           Types
  
  -- | an ordering of maybes such that Just is less than Nothing
  -- | we could create a newtype for Maybe which has this instance as the default
justLess :: Ord a => Maybe a -> Maybe a -> Ordering
justLess Nothing (Just _) = GT
justLess (Just _) Nothing = LT
justLess (Just a) (Just b) = compare a b
justLess Nothing Nothing = EQ

dxdy :: Coord -> Coord -> Coord -> Coord
dxdy (outHeight, outWidth) (x,y) (dx,dy) = ((x + dx + outHeight) `mod` outHeight , (y + dy  + outWidth) `mod` outWidth )

nextGen :: RandomGen g => g -> g
nextGen = snd . split 

randomInList :: RandomGen g => g -> [a] -> a
randomInList g list = list !! index
  where index = fst $ randomR (0, length list - 1) g 

calculateEntropy :: Int -> Double -> Double
calculateEntropy sumWeights sumWeightLogWeights
  | sumWeights <= 0 = 0
  | otherwise = log (fromIntegral $ sumWeights) - sumWeightLogWeights / (fromIntegral $ sumWeights)

minEntropy :: RandomGen g => g -> Wave a -> Maybe Coord
minEntropy  g = fmap fst . snd .  minimumBy ent . map checkNumPatterns . zip (randoms @Double g ) . M.toList 
  where
    checkNumPatterns (g, c@(coord, vals)) =   bool (g, Nothing) (g, Just c)   $
                                              (vals ^. (weights . numPatterns)) > 1
      
    getEnt = (^? _Just . _2 . entropy)
    ent (g1, a) (g2, b) =  justLess  (addNoise g1 $  getEnt a) (addNoise g2 $  getEnt b)

    addNoise g = fmap $ (+) (g * 1e-6)   

updateEntropies :: Coord -> Wave a -> Wave a 
updateEntropies coord wave =  wave & (ix coord . entropy) .~ calculateEntropy sumsOfW sumsOfWLogW
  where sumsOfW = wave ^. (singular $ ix  coord . weights . sumsOfWeights)
        sumsOfWLogW =  wave ^. (singular $ ix  coord . weights . sumsOfWeightLogWeights)


  -- probably better that this does not return the wave but just the necessary values 
banTile :: (Eq a, Ord a) => Coord -> Wave a -> Tile a -> Wave a
banTile coord wave tile = updateEntropies coord $ newWave
  where
    newWave =  wave
      & (ix coord . tiles . ix tile . possible ) .~ False
      & (struct .  numPatterns) %~  (\x -> case x == 1 of
                                        True -> error "contradiction"
                                        False -> subtract 1 x)
      & (struct . sumsOfWeights) %~ (\(x) -> x -  tileWeights)
      & (struct . sumsOfWeightLogWeights) %~ (\x -> x - tileLogWeights)
      & (ix coord . tiles . ix tile . compat  ) %~ (const $ M.fromList $ zip directions (repeat 0))

    struct = ix coord . weights 
    tileWeights =  wave ^. (singular $ ix coord . tiles . ix tile .  tileWeight) 
    tileLogWeights = wave ^. (singular $ ix coord . tiles . ix tile .  tileLogWeight) 

 
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


propagate :: (Show a, Eq a, Ord a) => Coord -> Tile a -> Coord -> Compat a -> Wave a -> Wave a
propagate dims tile xy propagator wave  = 
  foldl (\w (t, d) -> propagate dims t (dxdy dims xy d) propagator w ) (snd decrementAll) (fst decrementAll)

  where
    -- propagate the change to neighbouring pixels and accumulate a list of
    -- neighbouring tiles which need to be banned
    decrementAll = foldl  (\(ts, w) d ->
                             let x = decrementNeighbours w d in (fst x ++ ts, snd x ))
                   ([], wave) directions 

    decrementNeighbours  wave d@(dx,dy) =
          let (x2, y2) = dxdy dims xy d

              tiles =  fromMaybe S.empty $ propagator ^? (ix (tile, d) )  

              toNeighbours =  S.foldl (\(banned, wave) tile -> case decrementNeighbour d (x2,y2) wave tile of
                                          (Nothing, w) -> (banned, w)
                                          (Just t, w) -> ((t, d) : banned , w)
                                      ) ([] , wave) tiles
          in toNeighbours

    decrementNeighbour d coord wave tile =
          case  wave ^? (ix coord.tiles. ix tile . compat . ix d) == Just 1 of
            True -> (Just tile, flip (banTile coord) tile $  wave & (ix coord . tiles . ix tile . compat . ix d) %~ subtract 1 )

            False -> (Nothing,  wave & (ix coord . tiles . ix tile . compat . ix d) %~ subtract 1 )

observe dims g compat  wave  = case minEntropy g wave of
  Nothing -> M.map (\vals -> case M.keys $ M.filter ((^. possible) ) (vals ^. tiles)  of
                                        [] -> [Tile $ V.fromList [PixelRGB8 255 0 255]]
                                        xs -> xs
                            ) wave
  Just coord ->
    let tile =  fst $ randomInList (nextGen g) $ M.toList  $ M.filter ((^. possible)) $  wave ^. (singular $ ix coord . tiles)
        bannees =   M.keys $ M.filterWithKey (\key val -> key /= tile &&  (val ^. possible)) $  wave ^. (singular $ ix coord . tiles)
        bannedWave = foldl (banTile coord) wave bannees
                     
    in observe dims (nextGen  g) compat 
       $  foldl (\w tile -> propagate dims tile coord compat w) bannedWave bannees

