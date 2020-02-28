module Main where

data This a where
  This1 :: This Int
  

main :: IO ()
main = putStrLn "Hello, Haskell!"
