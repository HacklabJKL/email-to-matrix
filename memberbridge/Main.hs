#!/usr/bin/env runhaskell

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Control.Monad (foldM)
import Control.Exception.Base
import System.Environment (getEnv)

import JsonFilter
import EmailParser

data ProcessException = ProcessException String deriving Show

instance Exception ProcessException

-- |Throw an exception if value is Nothing, otherwise wraps it to an
-- applicative (which is probably IO or Maybe in your case).
unmay :: Applicative f => String -> Maybe a -> f a
unmay _ (Just a) = pure a
unmay e _ = throw $ ProcessException e

main = do
  -- Read raw email message from a file
  emailFile <- getEnv "MEMBERBOT_EMAIL"
  rawEmail <- readFile emailFile
  -- Read AST from stdin
  bs <- BL.getContents
  -- Manipulate AST
  input <- unmay "Invalid incoming JSON" $ decode bs
  fields <- unmay "E-mail parsing failed" $ emailToFields rawEmail
  output <- unmay "Template failed" $ myFilter fields input
  -- Write manipulated AST to stdout
  BL.putStr $ encode output

myFilter :: (ToJSON a, ToJSON b) => [(a, b)] -> Value -> Maybe Value
myFilter fields orig = do
  paths <- unmay "Template must contain exactly two KEY fields on the same depth" $
    tuple $ jsonFind (jsonEq "KEY") orig
  -- Figure out the repeating item
  let (linePath, linePath2) = basePaths paths
  -- Separate line template from the document
  templateLine <- jsonGet linePath orig
  templateDoc <- jsonRemove linePath2 orig >>= jsonRemove linePath
  -- Factories can be outside Java, too!
  newLine <- lineFactory templateLine
  -- Add a line for each key-value pair
  let inserter doc pair = do
        item <- newLine pair
        jsonAdd linePath item doc
  foldM inserter templateDoc $ reverse fields

-- |Given the template, create a factory which fills in the template
lineFactory :: (ToJSON a, ToJSON b) => Value -> Maybe ((a, b) -> Maybe Value)
lineFactory template = do
  pathToKey <- single $ jsonFind (jsonEq "KEY") template
  pathToValue <- unmay "Template must contain VALUE" $
    single $ jsonFind (jsonEq "VALUE") template
  pure $ \(k,v) -> jsonReplace pathToKey (toJSON k) template >>= jsonReplace pathToValue (toJSON v)

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
