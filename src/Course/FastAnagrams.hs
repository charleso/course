{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams s file =
  flip (<$>) (readFile file) $ fastAnagrams' s . lines

fastAnagrams' :: Chars -> List Chars -> List Chars
fastAnagrams' s contents = let set = S.fromList (hlist (NoCaseString <$> contents))
                           in map ncString $ filter (flip S.member set) (NoCaseString <$> permutations s)

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Ord NoCaseString where
  compare = compare `on` (map toLower . ncString)

instance Show NoCaseString where
  show = show . ncString
