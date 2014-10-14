{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

-- https://gist.github.com/markhibberd/fcad2c2b727169e2fb5b

import Data.List (find)
import Data.Maybe (listToMaybe)

{-
  Lets start ^^ by enabling some language extensions. GADTs to encode our data types and
  Rank2Types for the existental we will need to hide our types when we don't care about
  the invariant.
 -}


{-
  Lets start with our key data type, we want to be able to represent
  keys that point to a String, an Int or a Bool.
 -}
data Key a where
  KeyS :: Key String
  KeyI :: Key Int
  KeyB :: Key Bool


{-
  Lets than create an "entry in a bag" that holds any well typed
  key-value pair. Note well that we explicitly want to hide the
  invariants that our keys maintain at this point.
 -}
data Entry = forall a. Entry (Key a) a

{-
  A simple representation of a Bag as a list of entries.
 -}
type Bag =
  [Entry]

flag :: Key Bool
flag = KeyB

counter :: Key Int
counter = KeyI

name :: Key String
name = KeyS

stuff :: Bag
stuff = [Entry flag True, Entry counter 1, Entry name "hello"]

get :: Key a -> Bag -> Maybe a
get k ((Entry k2 v) : t) = case (k, k2) of
  (KeyS, KeyS) -> Just v
  (KeyI, KeyI) -> Just v
  (KeyB, KeyB) -> Just v
  _            -> get k t
get _ [] = Nothing

{-
NOTES: GADT let you turn an existential back into a type
-}
