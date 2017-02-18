-- Model the rules of Rummy
module Rummy where

import Deck
import System.Random
import Data.List

-- | All known data about a game
data Game = Game {
    phase  :: Phase   -- ^ Phase of play for the current player
  , places :: [Place] -- ^ State of all places, head is current player's place
  , table  :: Table   -- ^ Status of common play elements
  , rndGen :: StdGen  -- ^ Random number generator (updated after random events)
  }

-- StdGen is not showable so show Game without it
instance Show Game where
  show g = "Game {"
    ++ "phase = " ++ (show $ phase g)
    ++ ", places = " ++ (show $ places g)
    ++ ", table = " ++ (show $ table g)
    ++ "}"

-- | Phases of play for each player
data Phase =
    Draw    -- ^ Player must draw from discard or from draw pile
  | Meld    -- ^ Player may play or add to any number of melds or discard
  | Win     -- ^ Game is over because current player has won
  deriving (Eq, Show, Enum)

-- | Player's current state in the game
data Place = Place {
    name         :: String     -- ^ Player's name
  , hand         :: Pile       -- ^ Cards in player's hand
  , publics      :: Pile       -- ^ Cards in hand that were visibly drawn (from discard pile)
  , discardTaken :: Maybe Card -- ^ Player drew this card from discard pile
  }
  deriving (Eq, Show)

-- | An ordered group of cards
type Pile = [Card]

-- | Resources shared by all players
data Table = Table {
    melds    :: [Pile] -- ^ Melds in play (visible to all)
  , draws    :: Pile   -- ^ Draw pile (invisible)
  , discards :: Pile   -- ^ Discard pile (visible to all)
  }
  deriving (Eq, Show)

-- | Create a new ready-to-play game
mkGame :: StdGen -> [String] -> Game
mkGame gen names = deal $
  Game Draw (mkPlace <$> names) (Table [] [] Deck.pack) gen

-- | Create an empty place for a player
mkPlace :: String -> Place
mkPlace n = Place n [] [] Nothing

-- | Convert a game to its next state
type Action = Game -> Game

-- | Perform deal for all players
deal :: Action
deal game
    | numPlaces game == 2 = deal' 10 game -- 2 players get 10 cards each
    | numPlaces game <= 4 = deal' 7 game -- 3-4 players get 7 cards each
    | otherwise = error $ "min 2, max 4 players; got " ++ show (numPlaces game)
  where
    deal' :: Int -> Game -> Game
    deal' numCards = drawToDiscard . nextTurn . (dealPlaces numCards)

-- | Flip a card from draw pile to discard pile
drawToDiscard :: Action
drawToDiscard g@(Game _ _ t@(Table _ (d:ds) xs) _) =
  g { table = t { draws = ds, discards = d:xs } }

-- | Advance turn to the next player
nextTurn :: Action
nextTurn g@(Game _ (p:ps) _ _) = g { places = ps ++ [p], phase = Draw }

-- | Deal all players the specified number of cards
dealPlaces :: Int -> Action
dealPlaces numCards g = (repeatAction (numPlaces g) (nextTurn . (repeatAction numCards drawToPlace))) g

-- | Repeat an action n times
repeatAction :: Int -> Action -> Action
repeatAction n act g = iterate act g !! n

-- | Flip a card from draw pile to current player's hand
drawToPlace :: Action
drawToPlace g@(Game _ (p:ps) _ _) =
  g2 { places = p { hand = c:(hand p) } : ps } where
  (c, g2) = drawFromDraws g

-- | Take a card out of draw pile, shuffling discards if necessary
drawFromDraws :: Game -> (Card, Game)
drawFromDraws g@(Game _ _ t@(Table _ (d:ds) _) _) = (d, g { table = t { draws = ds } })
drawFromDraws g = drawFromDraws $ shuffleDiscards g

-- | Shuffle discard pile and replace draw pile with it
shuffleDiscards :: Action
shuffleDiscards g@(Game _ _ t@(Table _ _ discardPile) gen) =
    g { table = t { draws = newDrawPile, discards = [] }, rndGen = newGen }
  where
    (newDrawPile, newGen) = shuffleGen gen discardPile

-- | Return number of players in a game
numPlaces :: Game -> Int
numPlaces = length . places

-- | Draw a single card from non-empty discard pile
drawFromDiscards :: Game -> (Card, Game)
drawFromDiscards g@(Game _ _ t@(Table _ _ (c:cs)) _) = (c, g { table = t { discards = cs } })

-- | Add a card to discard pile
addToDiscards :: Card -> Action
addToDiscards c g@(Game _ _ t@(Table _ _ cs) _) = g { table = t { discards = c:cs }}

-- | The current place (player)
currentPlace :: Game -> Place
currentPlace g = head $ places g

-- | Type for all possible game moves
data Move =
    DrawFromDraws       -- ^ Take an unknown card from the draw pile
  | DrawFromDiscards    -- ^ Take the top discard card
  | PlayMeld Pile       -- ^ Put a 3+ straight or a 3+ kind on the table
  | AddToMeld Card Pile -- ^ Play a card extending an existing meld
  | DiscardCard Card    -- ^ Put a card on top of discard pile, proceed to next player
  deriving (Eq, Show, Ord)

-- | All legal moves
allMoves :: Game -> [Move]
allMoves (Game Win _ _ _) = [] -- No moves possible when game is over
allMoves (Game Draw _ _ _) = sort [DrawFromDraws, DrawFromDiscards]
allMoves g@(Game Meld _ _ _) = sort $ concat $ [meldMoves, addMoves, discardMoves] <*> [g]

-- | All possible meld-playing moves
meldMoves :: Game -> [Move]
meldMoves g = filter (isDiscardPossible g) $ fmap PlayMeld (allMelds $ hand $ currentPlace g)

-- | All possible melds which can be composed from given cards
allMelds :: Pile -> [Pile]
allMelds cs = filter isMeld $ subsequences sorted where
  sorted = sort cs

-- | True if (sorted) pile forms a meld (3+ cards, straight or of a kind)
isMeld :: Pile -> Bool
isMeld cs = (3 <= (length $ take 3 cs)) && (isRun cs || isKind cs)

-- | True if the move, applied to a game, still allows discard
isDiscardPossible :: Game -> Move -> Bool
isDiscardPossible (Game _ ((Place _ _ _ Nothing):_) _ _) _ = True
isDiscardPossible g m = case doMove m g of
  g2@(Game _ ((Place _ (c:[]) _ (Just d)):_) _ _) -> d /= c || any (isAddToMeldOf c) (addMoves g2)
  -- ^ False if single non-meldable card must be discarded
  _ -> True

-- | True if the move is an AddToMeld using the specified card
isAddToMeldOf :: Card -> Move -> Bool
isAddToMeldOf c (AddToMeld c2 _) = c == c2
isAddToMeldOf _ _ = False

-- | True if (sorted) cards found are in a row and same suit
isRun :: Pile -> Bool
isRun [] = False
isRun (_:[]) = True
isRun (c1:c2:cs) =
     (suit c1) == (suit c2)            -- Suits match
  && (value c1 /= (maxBound :: Value)) -- Value can increase
  && ((succ $ value c1) == value c2)   -- Value does increase
  && (isRun $ c2:cs)                   -- The rest are run-meldy too

-- | True if cards are all same kind
isKind :: Pile -> Bool
isKind [] = False
isKind ((Card v _):cs) = all (\c -> (value c) == v) cs

-- | All possible add-to-meld moves
addMoves :: Game -> [Move]
addMoves g = filter (isDiscardPossible g) $
    filter isValidAdd ((fmap AddToMeld handCards) <*> tableMelds)
  where
    handCards = hand $ head $ places g
    tableMelds = melds $ table g
    isValidAdd (AddToMeld c ms) = isMeld $ sort (c:ms)

-- | All possible discard moves.
-- Note: it is illegal to discard a card just taken from the discard pile
discardMoves :: Game -> [Move]
discardMoves g = fmap DiscardCard $ filter (\c -> Just c /= discardTaken p) (hand $ p)
  where p = currentPlace g

-- | Make a move
doMove :: Move -> Action

doMove DrawFromDraws g = (drawToPlace g) { phase = Meld }

-- | Draw from the discard pile
doMove DrawFromDiscards g = let
  (c, g2) = drawFromDiscards g
  p = currentPlace g2
  p2 = p { hand = c:(hand p), publics = c:(publics p), discardTaken = Just c }
  in g2 { phase = Meld, places = p2:(tail $ places g2) }

-- | Play a complete meld to the table (merging existing melds if possible)
doMove (PlayMeld m) g = g { phase = ph2, places = p:(tail $ places g), table = t2 }
  where
    p = dropCards m $ currentPlace g
    t2 = tableWithMeld m $ table g
    ph2 = if hand p == [] then Win else Meld

-- | Play a card into a specific meld
doMove (AddToMeld c m) g = g { phase = ph2, places = p:(tail $ places g), table = t2 }
  where
    t = table g
    p = dropCards [c] $ currentPlace g
    t2 = t { melds = mergeMeld (sort $ c:m) (delete m (melds t)) } -- Table w/new meld
    ph2 = if hand p == [] then Win else Meld

-- | Move discard out of hand and onto table
doMove (DiscardCard c) g = g3 where
  p = (dropCards [c] $ currentPlace g) { discardTaken = Nothing }
  t = table g
  t2 = t { discards = c:(discards t) }
  ph2 = if hand p == [] then Win else Draw
  g2 = g { phase = ph2, table = t2, places = p:(tail $ places g) }
  g3 | ph2 == Win = g2
     | otherwise = nextTurn g2

-- | Drop the specified set of cards from a place
dropCards :: Pile -> Place -> Place
dropCards cs p = p { hand = (hand p) \\ cs, publics = (publics p) \\cs }

-- Combine a meld into melds on the table
tableWithMeld :: Pile -> Table -> Table
tableWithMeld m t = t { melds = mergeMeld m (melds t) }

-- Add a valid meld to a list of melds, combining it with an existing one if possible
mergeMeld :: Pile -> [Pile] -> [Pile]
mergeMeld m1 [] = [m1]
mergeMeld m1 (m2:ms)
    | isMeld (m12) = mergeMeld m12 ms
    | otherwise = m2:(mergeMeld m1 ms)
  where
    m12 = sort (m1 ++ m2)

--- True if game is in the win state for the current player
isWin :: Game -> Bool
isWin = (== Win) . phase
