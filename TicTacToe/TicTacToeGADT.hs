{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

import           Control.Monad
import           Data.List
import           Data.Maybe

type Position = (P1, P1)
data P1 = A | B | C deriving (Eq, Show)
type Square = (Position, Player)
type Board2 = [Square]
data Board a where
  BoardIP :: Board2 -> Board GameIP
  BoardD  :: Board2 -> Board GameDone
data Player = O | X deriving (Eq, Show)

instance Show (Board a) where
  show (BoardIP b) = show b
  show (BoardD b)  = show b

data GameDone = GameDone deriving Show
data GameIP = GameIP deriving Show
data GameW = forall a. GameW (Maybe (Board a))

game :: b -> (forall a. Board a -> b) -> GameW -> b
game _ x (GameW (Just b)) = x b
game x _ (GameW Nothing) = x

main = print . (game "" show) . move (A, A) O $ startGame

startGame :: Board GameIP
startGame = BoardIP []

foldGame :: a -> (Board2 -> a) -> (Board2 -> a) -> (Board2 -> a) -> Board b -> a
foldGame s ip d w (BoardIP b) = foldGame' s ip d w b
foldGame s ip d w (BoardD b) = foldGame' s ip d w b

foldGame' :: a -> (Board2 -> a) -> (Board2 -> a) -> (Board2 -> a) -> Board2 -> a
foldGame' s ip d w b = case length b of
          0 -> s
          _ -> if isWon' b then w b else if length b == 9 then d b else ip b

whoWon :: Board GameDone -> Maybe Player
whoWon = foldGame Nothing (const Nothing) (const Nothing) whoWon'

move :: Position -> Player -> Board GameIP -> GameW
move pos pl (BoardIP b) = GameW $ foldGame' (Just $ BoardIP $ next : []) moveNext (const (Nothing)) (const (Nothing)) b
  where next = (pos, pl)
        moveNext []      = Nothing
        moveNext (h : t) = if positionIsOccupied pos (h : t) || (getPlayer h) == pl then Nothing
                           else Just $ BoardIP (next : h : t)

isWon' :: Board2 -> Bool
isWon' = isJust . whoWon'

whoWon' :: Board2 -> Maybe Player
whoWon' b = check2 O `mplus` check2 X
  where combs :: Player -> [(Position, Position, Position)]
        combs p = let b' = map getPos (filter ((p ==) . getPlayer) b) in do
          a1 <- b'
          a2 <- b'
          a3 <- b'
          if a1 /= a2 && a2 /= a3 && a1 /= a3 then return (a1, a2, a3) else []
        check :: (Position, Position, Position) -> Bool
        check ps = case ps of
          ((x1, y1), (x2, y2), (x3, y3)) ->
            (x1 == x2 && x2 == x3) ||
            (y1 == y2 && y2 == y3) ||
            (x1 == y1 && x2 == y2 && x3 == y3) ||
            (x1 == y3 && x2 == y2 && x3 == y1)
        check2 :: Player -> Maybe Player
        check2 p = if null (filter check (combs p)) then Nothing else Just p

playerAt :: Position -> Board2 -> Maybe Player
playerAt pos = fmap getPlayer . find ((pos ==) . getPos)

takeBake :: Board a -> Board GameIP
takeBake = BoardIP . foldGame [] back back back
  where tailOp []      = Nothing
        tailOp (_ : t) = Just t
        back []      = []
        back (_ : t) = t

positionIsOccupied :: Position -> Board2 -> Bool
positionIsOccupied pos = any (pos ==) . map getPos

getPos :: (Position, Player) -> Position
getPos = fst

getPlayer :: (Position, Player) -> Player
getPlayer = snd

-- prop_1 = forall Board b. forall Position p. such that (not (positionIsOccupied p b)). takeBack(move(p, b)) == b
