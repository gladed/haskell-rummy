-- Model the rules of Rummy
module Rummy where

import Deck
import System.Random
import Data.List
import Lens.Simple

-- | All known data about a game
data Game = Game {
    phase  :: Phase   -- ^ Phase of play for the current player
  , places :: [Place] -- ^ State of all places, head is current player's place
  , table :: Table   -- ^ Status of common play elements
  , rndGen :: StdGen  -- ^ Random number generator (updated after random events)
  }

-- | Lens for Places in Game
places_ :: Lens' Game [Place]
places_ = lens places (\g newPlaces -> g { places = newPlaces })

-- Current is a Lens for the topmost Place
current :: Lens' Game Place
current = places_ . head_

-- | Lens for head of list
head_ :: Lens' [a] a
head_ = lens head (\xs x -> x:(tail xs))

-- | Lens for table (in which I violate convention and use _ backwards)
table_ :: Lens' Game Table
table_ = lens table (\g t -> g { table = t })

-- | Lens for draw pile
draws_ :: Lens' Game Pile
draws_ = table_ . tableDraws where
  tableDraws = lens draws (\t ds -> t { draws = ds })

-- | Lens for discard pile
discards_ :: Lens' Game Pile
discards_ = table_ . tableDiscards where
  tableDiscards = lens discards (\t xs -> t { discards = xs })

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
mkGame gen names = deal $ Game Draw (mkPlace <$> names) (Table [] [] Deck.pack) gen

-- | Create an empty place for a player
mkPlace :: String -> Place
mkPlace n = Place n [] [] Nothing

-- | Convert a game to its next state
type Action = Game -> Game

-- | Perform deal for all players
deal :: Action
deal game
    | numPlaces game == 2 = dealCards 10 game -- 2 players get 10 cards each
    | numPlaces game <= 4 = dealCards 7 game -- 3-4 players get 7 cards each
    | otherwise = error $ "min 2, max 4 players; got " ++ show (numPlaces game)
  where
    dealCards numCards = drawToDiscard . nextTurn . (dealPlaces numCards)

-- | Deal all players the specified number of cards
dealPlaces :: Int -> Action
dealPlaces numCards g = repeatAction (numPlaces g) (nextTurn . (repeatAction numCards drawToPlace)) g

-- | An action that repeats another one n times
repeatAction :: Int -> Action -> Action
repeatAction n act g = iterate act g !! n

-- | Flip a card from draw pile to discard pile
drawToDiscard :: Action
drawToDiscard = uncurry addToDiscards . drawFromDraws

-- | Add a card to discard pile
addToDiscards :: Card -> Action
addToDiscards c = over discards_ (\xs -> c:xs)

-- | Advance turn to the next player
nextTurn :: Action
nextTurn g | isWin g = g -- ^ No change if win already
nextTurn g = setPhase Draw . (over places_ rotate) . over current (\p -> p { discardTaken = Nothing }) $ g 

-- | Pop first element and push onto back
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

-- | Flip a card from draw pile to current player's hand
drawToPlace :: Action
drawToPlace = uncurry takeCard . drawFromDraws where
  takeCard c = over current f where f p = p { hand = c:(hand p) }

-- | Take a card out of draw pile, shuffling discards if necessary
drawFromDraws :: Game -> (Card, Game)
drawFromDraws g = case (view draws_ g) of
  [] -> (drawFromDraws $ shuffleDiscards g) -- ^ No cards. Shuffle and try again. 
  (d:ds) -> (d, over draws_ (\_ -> ds) g)   -- ^ Take top card from draw pile

-- TODO: On third shuffle it's a DRAW
-- TODO: no possible win it's a DRAW (no melds possible with either hand or cards in deck)
-- TODO: Count "going Rummy" e.g. all melds on same turn

-- | Shuffle discard pile and replace draw pile with it
shuffleDiscards :: Action
shuffleDiscards g = g { table = (table g) { draws = newDrawPile, discards = [] }, rndGen = newGen }
  where
    (newDrawPile, newGen) = shuffleGen (rndGen g) (view discards_ g)

-- | Return number of players in a game
numPlaces :: Game -> Int
numPlaces = length . places

-- | Draw a single card from non-empty discard pile
drawFromDiscards :: Game -> (Card, Game)
drawFromDiscards g@(Game _ _ t@(Table _ _ (c:cs)) _) = (c, g { table = t { discards = cs } })

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
allMoves g = sort $ case phase g of
  Win -> []
  Draw -> [DrawFromDraws, DrawFromDiscards]
  Meld -> concat $ [meldMoves, addMoves, discardMoves] <*> [g]

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
isDiscardPossible g m = case play m g of
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

class Play a where
  play :: a -> Action

-- Play any kind of move here
instance Play Move where
  play DrawFromDraws = setPhase Meld . drawToPlace
  play DrawFromDiscards = setPhase Meld . uncurry takeDiscard . drawFromDiscards
  play (PlayMeld m) = checkWin . dropCards m . tableWithMeld m
  play (AddToMeld c m) = checkWin . dropCards [c] . tableWithMeld m . dropMeld m
  play (DiscardCard c) = nextTurn . checkWin . addToDiscards c . dropCards [c]

-- | Accept a card into current Place's hand from the discard pile
takeDiscard :: Card -> Action
takeDiscard c = over current take where
  take p@(Place _ hs pubs _) = p { hand = c:hs, publics = c:pubs, discardTaken = Just c }

-- | Transition game to specified phase
setPhase :: Phase -> Action
setPhase p g = g { phase = p }

-- | Transition game to Win if the current player has no more cards
checkWin :: Action
checkWin g | (hand $ currentPlace g) == [] = setPhase Win g
checkWin g = g

-- | Remove cards from the current place
dropCards :: Pile -> Action
dropCards cs = over current drop where
  drop p = p { hand = (hand p) \\ cs, publics = (publics p) \\ cs }

-- | Remove a single meld from the table
dropMeld :: Pile -> Action
dropMeld m = over table_ f where f t = t { melds = (delete m (melds t)) }

-- | Put a new meld on the table
tableWithMeld :: Pile -> Action
tableWithMeld m = over table_ f where f t = t { melds = mergeMelds m (melds t) }

-- | Add a valid meld to a list of melds, combining it with an existing one if possible
mergeMelds :: Pile -> [Pile] -> [Pile]
mergeMelds m1 [] = [m1]
mergeMelds m1 (m2:ms)
    | isMeld (m1_2) = mergeMelds m1_2 ms
    | otherwise = m2:(mergeMelds m1 ms)
  where
    m1_2 = sort (m1 ++ m2)

--- | True if game is in the win state for the current player
isWin :: Game -> Bool
isWin = (== Win) . phase
