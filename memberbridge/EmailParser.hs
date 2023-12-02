{-# LANGUAGE OverloadedStrings #-}
module EmailParser (emailToFields) where

import Data.List (uncons)

-- |Very simple email to fields parser. Assumes that input is UTF-8.
emailToFields :: String -> Maybe [(String, String)]
emailToFields raw = do
  let xs = lines raw
  -- Payload is separated by an empty line
  (_, payload) <- span' (/= "") xs
  -- Free-form message is separated by "msg" line
  (kvStr, msg) <- span' (/= "msg") payload
  -- Field name and payload separated by a single space. The rest is msg.
  kv <- mapM (span' (/=' ')) kvStr
  -- Put msg field to the end of items
  pure $ kv ++ [("msg", unlines msg)]

-- |Slightly different version of "regular" span, dropping the first
-- matching item and returning Nothing in case of no match.
span' :: (a -> Bool) -> [a] -> Maybe ([a], [a])
span' f x = case uncons rest of
  Just (_, end) -> Just (start, end)
  _             -> Nothing
  where (start, rest) = span f x
