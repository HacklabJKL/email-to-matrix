module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Control.Monad (foldM)
import JsonFilter

import Debug.Trace

kama = [("etunimi", "Matti")
       ,("sukunimi", "Meikäläinen")
       ,("juu", "<Juupajuu>")
       ]

main = do
  bs <- BL.getContents
  maybe (error "Error in JSON processing") BL.putStr $ do
    input <- decode bs
    output <- myFilter input
    pure $ encode output

myFilter :: Value -> Maybe Value
myFilter orig = do
  paths <- tuple $ jsonFind (jsonEq "KEY") orig
  -- Figure out what's the repeating item
  let (linePath, linePath2) = basePaths paths
  -- Separate line template from the document
  templateDoc <- jsonRemove linePath2 orig >>= jsonRemove linePath
  templateLine <- jsonGet linePath orig
  -- Factories can be outside Java, too!
  newLine <- lineFactory templateLine
  -- Let's do it
  let inserter doc pair = do
        item <- newLine pair
        jsonAdd linePath item doc
  foldM inserter templateDoc $ reverse kama

-- |Given the template, create a factory which fills in the template
lineFactory :: (ToJSON a, ToJSON b) => Value -> Maybe ((a, b) -> Maybe Value)
lineFactory template = do
  pathToKey <- single $ jsonFind (jsonEq "KEY") template
  pathToValue <- single $ jsonFind (jsonEq "VALUE") template
  pure $ \(k,v) -> jsonReplace pathToKey (toJSON k) template >>= jsonReplace pathToValue (toJSON v)

-- |Convert list to single value, Nothing if it doesn't contain 1 element.
single :: [a] -> Maybe a
single [a] = Just a
single _ = Nothing

-- |Convert list to tuple or Nothing if doesn't contain 2 elements.
tuple :: [a] -> Maybe (a, a)
tuple [a,b] = Just (a,b)
tuple _  = Nothing

-- | Find diversion point between two paths (first difference in paths)
basePaths :: (Path, Path) -> (Path, Path)
basePaths (Path in1, Path in2) = (Path out1, Path out2)
  where (out1, out2) = unzip $ commons in1 in2

-- |Find common prefix of two lists, returning until the first difference.
commons :: Eq a => [a] -> [a] -> [(a, a)]
commons (a:as) (b:bs) = (a,b) : if a == b
                                then commons as bs
                                else []
commons [] _ = []
commons _ [] = []
