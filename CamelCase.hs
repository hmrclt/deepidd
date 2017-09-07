module CamelCase where

import Data.Char

toCamelCase :: String -> String
toCamelCase a = concat $ inner (words (takeWhile (/= '(') a))
  where inner (h:t) = fmap toLower h : fmap upperFst t
        inner _ = []
        upperFst (h:t) = toUpper h : fmap toLower t
        upperFst _ = ""

toCamelCase' :: String -> String
toCamelCase' a = concat $ inner (words (takeWhile (/= '(') a))
  where inner = fmap upperFst
        upperFst (h:t) = toUpper h : fmap toLower t
        upperFst _ = ""
