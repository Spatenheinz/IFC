module Lib where

import Data.SBV
import Data.Maybe (fromJust)

prop = do [x, y] <- sInt8s ["x", "y"]
          forAll ["x", "y"] $ x * 2 .== y

demo = do
  res <- prove prop
  if not $ modelExists res
    then putStrLn "Pew, it was true"
    else do
      putStrLn "Oh no, I found a counter-example (enjoy the nice printing):"
      print $ getModelDictionary res
      putStrLn "\nOr with nicer printing\n"
      putStrLn $ concat ["Have you considered x = ", show $ getInt8 "x" res,
                         " and y = ", show $ getInt8 "y" res]
  where
   getInt8 :: String -> ThmResult -> Int8
   getInt8 v model = fromJust $ getModelValue v model
