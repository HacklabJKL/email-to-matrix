{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JsonFilter where

import Data.Aeson
import GHC.Exts (toList)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM
import Backports (unsnoc)

newtype Path = Path [PathItem] deriving (Show, Eq)

data PathItem = JKey Key | JIx Int deriving (Show, Eq)

type Operation = Value -> Maybe Value

-- |Find the path to that specified value
jFind :: (Value -> Bool) -> Value -> [Path]
jFind test a = append $ case a of
  Object o -> concatMap (addPath JKey) $ toList $ jFind test <$> o
  Array v  -> concatMap (addPath JIx) $ toList $ V.indexed $ jFind test <$> v
  _        -> []
  where append = if test a
                 then (Path []:)
                 else id

-- |Helper function to convert a key to a path item and append it to the path of all items
addPath :: Functor f => (a -> PathItem) -> (a, f Path) -> f Path
addPath f (a,bs) = append <$> bs
  where append (Path p) = Path $ f a : p

-- |Helper for doing exact match on any JSON encodable value.
jsonEq :: ToJSON a => a -> Value -> Bool
jsonEq needle a = toJSON needle == a

-- |Get value at given path. Nothing if path is invalid.
jsonGet :: Path -> Value -> Maybe Value
jsonGet (Path (p:ps)) val = case (p,val) of
  (JIx i, Array v)   -> v V.!? i >>= jsonGet (Path ps)
  (JKey k, Object o) -> KM.lookup k o >>= jsonGet (Path ps)
  _                  -> Nothing
jsonGet _ val = Just val

-- |Inserts item to given path.
jsonAdd :: Path -> Value -> Value -> Maybe Value
jsonAdd (Path fullPath) new val = case unsnoc fullPath of
  Just (p, JIx i) -> jsonAlter (Path p) (vectorAdd i new) val
  Just (p, JKey k) -> jsonAlter (Path p) (mapAdd k new) val

-- |Removes item at given path.
jsonRemove :: Path -> Value -> Maybe Value
jsonRemove (Path fullPath) val = case unsnoc fullPath of
  Just (p, JIx i) -> jsonAlter (Path p) (vectorRemove i) val
  Just (p, JKey k) -> jsonAlter (Path p) (mapRemove k) val

-- |Replaces item at given path with a new value.
jsonReplace :: Path -> Value -> Value -> Maybe Value
jsonReplace p new val = jsonAlter p (const $ Just new) val

-- |Manipulate JSON recursively, navigating to the given path and
-- doing given operation.
jsonAlter :: Path -> Operation -> Value -> Maybe Value
jsonAlter (Path path) op val = case (path, val) of
  -- Operate
  ([], v) -> op v
  -- Recurse deeper
  (JIx i:ps, Array v)   -> v V.!? i >>= jsonAlter (Path ps) op >>= \new -> Just $ Array $ v V.// [(i, new)]
  (JKey k:ps, Object o) -> KM.lookup k o >>= jsonAlter (Path ps) op >>= \new -> Just $ Object $ KM.insert k new o
  _                     -> Nothing

-- |Removes key from the map, Fails if key doesn't exist or not an object.
mapAdd :: Key -> Value -> Operation
mapAdd k new (Object o) = if KM.member k o
                          then Nothing
                          else Just $ Object $ KM.insert k new o
mapAdd _ _ _ = Nothing

-- |Removes key from the map, Fails if key doesn't exist or not an object.
mapRemove :: Key -> Operation
mapRemove k (Object o) = if KM.member k o
                         then Just $ Object $ KM.delete k o
                         else Nothing
mapRemove _ _ = Nothing

-- |Insert item to an array. Fails if array index out of bounds or not an array.
vectorAdd :: Int -> Value -> Operation
vectorAdd i a (Array v) = if V.length v < i then Nothing else Just $ Array new
  where (start, end) = V.splitAt i v
        new = V.concat [start,V.singleton a,end]
vectorAdd _ _ _ = Nothing

-- |Remove item from an array. Fails if array index out of bounds or not an array.
vectorRemove :: Int -> Operation
vectorRemove i (Array v) = if V.length v <= i then Nothing else Just $ Array new
  where (start, rest) = V.splitAt i v
        end = V.tail rest
        new = start V.++ end
vectorRemove _ _ = Nothing

-- |Helper for manipulating the path
withPath f (Path a) = Path $ f a
