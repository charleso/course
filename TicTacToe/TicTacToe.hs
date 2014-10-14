import           Control.Monad
import           Data.List
import           Data.Maybe

type Position = (P1, P1)
data P1 = A | B | C deriving (Eq, Show)
type Square = (Position, Player)
type Board2 = [Square]
data Board = Board Board2 deriving (Eq, Show)
data Player = O | X deriving (Eq, Show)

startGame :: Board
startGame = Board []

foldGame :: a -> (Board2 -> a) -> (Board2 -> a) -> (Board2 -> a) -> Board -> a
foldGame s ip d w (Board b) = case length b of
  0 -> s
  _ -> if isWon' b then w b else if length b == 9 then d b else ip b

move :: Position -> Player -> Board -> Maybe Board
move pos pl = foldGame (Just $ Board $ next : []) moveNext (const Nothing) (const Nothing)
  where next = (pos, pl)
        moveNext []      = Nothing
        moveNext (h : t) = if positionIsOccupied pos (h : t) || (getPlayer h) == pl then Nothing
                            else Just $ Board (next : h : t)

whoWon :: Board -> Maybe Player
whoWon = foldGame Nothing (const Nothing) (const Nothing) whoWon'

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

takeBake :: Board -> Board
takeBake = Board . foldGame [] back back back
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
