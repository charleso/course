{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.TicTacToe where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind
import Course.Monad
import qualified Prelude as P

type Position = (P1, P1)
data P1 = A | B | C deriving (Eq, Show)
type Square = (Position, Player)
type Board2 = List Square
data Board = Board Board2 deriving (Eq, Show)
data Player = O | X deriving (Eq, Show)

startGame :: Board
startGame = Board Nil

foldGame :: a -> (Board2 -> a) -> (Board2 -> a) -> (Board2 -> a) -> Board -> a
foldGame s ip d w (Board b) = case length b of
  0 -> s
  _ -> if isWon' b then w b else if length b == 9 then d b else ip b

move :: Position -> Player -> Board -> Optional Board
move pos pl = foldGame (Full $ Board $ next :. Nil) moveNext (const Empty) (const Empty)
  where next = (pos, pl)
        moveNext Nil      = Empty
        moveNext (h :. t) = if positionIsOccupied pos (h :. t) || (getPlayer h) == pl then Empty
                            else Full $ Board (next :. h :. t)

whoWon :: Board -> Optional Player
whoWon = foldGame Empty (const Empty) (const Empty) whoWon'

isWon' :: Board2 -> Bool
isWon' = foldOptional (const True) False . whoWon'

whoWon' :: Board2 -> Optional Player
whoWon' b = check2 O <+> check2 X
  where combs :: Player -> List (Position, Position, Position)
        combs p = let b' = map getPos (filter ((p ==) . getPlayer) b) in do
          a1 <- b'
          a2 <- b'
          a3 <- b'
          if a1 /= a2 && a2 /= a3 && a1 /= a3 then return (a1, a2, a3) else Nil
        check :: (Position, Position, Position) -> Bool
        check ps = case ps of
          ((x1, y1), (x2, y2), (x3, y3)) ->
            (x1 == x2 && x2 == x3) ||
            (y1 == y2 && y2 == y3) ||
            (x1 == y1 && x2 == y2 && x3 == y3) ||
            (x1 == y3 && x2 == y2 && x3 == y1)
        check2 :: Player -> Optional Player
        check2 p = if isEmpty (filter check (combs p)) then Empty else Full p

playerAt :: Position -> Board2 -> Optional Player
playerAt pos = mapOptional getPlayer . find ((pos ==) . getPos)

takeBake :: Board -> Board
takeBake = Board . foldGame Nil back back back
  where tailOp Nil      = Empty
        tailOp (_ :. t) = Full t
        back Nil      = Nil
        back (_ :. t) = t

positionIsOccupied :: Position -> Board2 -> Bool
positionIsOccupied pos = exists (pos ==) . map getPos

getPos :: (Position, Player) -> Position
getPos = fst

getPlayer :: (Position, Player) -> Player
getPlayer = snd

-- prop_1 = forall Board b. forall Position p. such that (not (positionIsOccupied p b)). takeBack(move(p, b)) == b
