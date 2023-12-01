{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module JsonFilter where

import Data.Aeson
import GHC.Exts (toList)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM

newtype Path = Path [PathItem] deriving (Show, Eq, Ord)

data PathItem = JKey Key | JIx Int deriving (Show, Eq, Ord)

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

jsonRemove :: Path -> Value -> Maybe Value
jsonRemove (Path path) val = case (path, val) of
  -- Delete
  ([JIx i], Array v)    -> Array <$> vectorRemove i v
  ([JKey k], Object o)  -> if KM.member k o
                           then Just $ Object $ KM.delete k o
                           else Nothing
  -- Recurse deeper
  (JIx i:ps, Array v)   -> v V.!? i >>= jsonRemove (Path ps) >>= \new -> Just $ Array $ v V.// [(i, new)]
  (JKey k:ps, Object o) -> KM.lookup k o >>= jsonRemove (Path ps) >>= \new -> Just $ Object $ KM.insert k new o
  _                     -> Nothing

vectorRemove :: Int -> V.Vector a -> Maybe (V.Vector a)
vectorRemove i v = if V.length v <= i then Nothing else Just new
  where (start, rest) = V.splitAt i v
        end = V.tail rest
        new = start V.++ end


-- |Helper for manipulating the path
withPath f (Path a) = Path $ f a
