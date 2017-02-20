-- Model the rules of Rummy
module Rummy where

import Deck
import System.Random
import Data.List
import Lens.Simple

-- | All known data about a game
data Game = Game {
    places   :: [Place] -- ^ State of all places, head is current player's place
  , phase    :: Phase   -- ^ Phase of play for the current player
  , melds    :: [Pile]  -- ^ Melds in play (visible to all)
  , draws    :: Pile    -- ^ Draw pile (invisible)
  , discards :: Pile    -- ^ Discard pile (visible to all)
  , shuffles :: Int     -- ^ Number of shuffles in game (max 2)
  , rndGen   :: StdGen  -- ^ Random number generator (updated after random events)
  }

-- StdGen is not showable so show Game without it
instance Show Game where
  show g = "Game {"
    ++ "phase = " ++ (show $ phase g)
    ++ ", places = " ++ (show $ places g)
    ++ ", melds = " ++ (show $ melds g)
    ++ ", draws = " ++ (show $ draws g)
    ++ ", discards = " ++ (show $ discards g)
    ++ "}"

-- | Phases of play for each player
data Phase =
    Start   -- ^ Player must draw from discard or from draw pile
  | Meld    -- ^ Player may play or add to any number of melds or discard
  | Win     -- ^ Game is over because current player has won
  | Draw    -- ^ Game is over because nobody can win (stalemate)
  deriving (Eq, Show, Enum)

-- | Player's current state in the game
data Place = Place {
    name         :: String     -- ^ Player's name
  , hand         :: Pile       -- ^ Cards in player's hand
  , publics      :: Pile       -- ^ Cards in hand that were visibly drawn (from discard pile)
  , discardTaken :: Maybe Card -- ^ Player drew this card from discard pile
  , rummy        :: Bool       -- ^ Player is eligible for rummy
  }
  deriving (Eq, Show)

-- | An ordered group of cards
type Pile = [Card]

-- | Lens for current place
current :: Lens' Game Place
current = places_ . head_

-- | Lens on current player's hand
hand_ :: Lens' Game Pile
hand_ = current . (lens hand (\p h -> p { hand = h }))

-- | A lens on the head of a list
head_ :: Lens' [a] a
head_ = lens head (\xs x -> x:(tail xs))

places_ :: Lens' Game [Place]
places_ = lens places (\g newPlaces -> g { places = newPlaces })

draws_ :: Lens' Game Pile
draws_ = lens draws (\g newDraws -> g { draws = newDraws })

discards_ :: Lens' Game Pile
discards_ = lens discards (\g xs -> g { discards = xs })

-- | Create a new ready-to-play game
mkGame :: StdGen -> [String] -> Game
mkGame gen names = deal $ Game (mkPlace <$> names) Start [] [] Deck.pack 0 gen

-- | Create an empty place for a player
mkPlace :: String -> Place
mkPlace n = Place n [] [] Nothing True

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
nextTurn g | isOver g = g               -- ^ Already terminal
  | shuffles g > 2 = g { phase = Draw } -- ^ Stalemate if too many shuffles
  | otherwise = setPhase Start . (over places_ rotate) 
      . over current (\p -> p { discardTaken = Nothing }) $ g 

-- | If game isn't terminal then disable rummy bit for the current place
notRummy g | isOver g = g
  | otherwise = over current (\p -> p { rummy = False }) g

-- | Pop first element and push onto back
rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

-- | Flip a card from draw pile to current player's hand
drawToPlace :: Action
drawToPlace = uncurry takeCard . drawFromDraws where
  takeCard c = over hand_ (\h -> c:h)

-- | Take a card out of draw pile, shuffling discards if necessary
drawFromDraws :: Game -> (Card, Game)
drawFromDraws g = case (view draws_ g) of
  [] -> (drawFromDraws $ shuffleDiscards g) -- ^ No cards. Shuffle and try again. 
  (d:ds) -> (d, over draws_ (\_ -> ds) g)   -- ^ Take top card from draw pile

-- | Shuffle discard pile and replace draw pile with it
shuffleDiscards :: Action
shuffleDiscards g = g { draws = newDrawPile, discards = [], shuffles = (shuffles g) + 1, 
    rndGen = newGen }
  where
    (newDrawPile, newGen) = shuffleGen (rndGen g) (view discards_ g)

-- | Return number of players in a game
numPlaces :: Game -> Int
numPlaces = length . places

-- | Draw a single card from non-empty discard pile
drawFromDiscards :: Game -> (Card, Game)
drawFromDiscards = onTuple (head . view discards_, over discards_ (\(c:cs) -> cs))

-- | Apply to both functions in a tuple
onTuple :: (a -> b, a -> c) -> a -> (b, c)
onTuple (f1, f2) a = (f1 a, f2 a)

-- | Type for all possible game moves
data Move =
    DrawFromDraws       -- ^ Take an unknown card from the draw pile
  | DrawFromDiscards    -- ^ Take the top discard card
  | PlayMeld Pile       -- ^ Put a 3+ straight or a 3+ kind into melds
  | AddToMeld Card Pile -- ^ Play a card extending an existing meld
  | DiscardCard Card    -- ^ Put a card on top of discard pile, proceed to next player
  deriving (Eq, Show, Ord)

-- | All legal moves
allMoves :: Game -> [Move]
allMoves g = sort $ case phase g of
  Start -> [DrawFromDraws, DrawFromDiscards]
  Meld  -> concat $ [meldMoves, addMoves, discardMoves] <*> [g]
  _     -> []

-- | All possible meld-playing moves
meldMoves :: Game -> [Move]
meldMoves g = filter (canDiscardAfter g) $ fmap PlayMeld (allMelds $ view hand_ g)

-- | All possible melds which can be composed from given cards
allMelds :: Pile -> [Pile]
allMelds cs = filter isMeld $ subsequences sorted where
  sorted = sort cs

-- | True if (sorted) pile forms a meld (3+ cards, straight or of a kind)
isMeld :: Pile -> Bool
isMeld cs = (3 <= (length $ take 3 cs)) && (isRun cs || isKind cs)

-- | True if the move, applied to a game, still allows discard
canDiscardAfter :: Game -> Move -> Bool
canDiscardAfter g m = canDiscard $ play m g where
  canDiscard g = case view hand_ g of
    (c:[]) -> (isDiscardable g c) || any (isAddToMeldOf c) (addMoves g)
    _ -> True

-- | True if the specific card is legal to discard from the current hand
isDiscardable :: Game -> Card -> Bool
isDiscardable g c = Just c /= (discardTaken $ view current g)

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
addMoves g = filter (canDiscardAfter g) $
    filter isValidAdd ((fmap AddToMeld (view hand_ g)) <*> (melds g))
  where
    isValidAdd (AddToMeld c ms) = isMeld $ sort (c:ms)

-- | All possible discard moves.
-- Note: it is illegal to discard a card just taken from the discard pile
discardMoves :: Game -> [Move]
discardMoves g = fmap DiscardCard $ filter (isDiscardable g) $ view hand_ g

-- Make all moves playable
class Play a where
  play :: a -> Action

instance Play Move where
  play DrawFromDraws = setPhase Meld . drawToPlace
  play DrawFromDiscards = setPhase Meld . uncurry takeDiscard . drawFromDiscards
  play (PlayMeld m) = checkState . dropCards m . addMeld m
  play (AddToMeld c m) = checkState . dropCards [c] . addMeld m . dropMeld m
  play (DiscardCard c) = nextTurn . notRummy . checkState . addToDiscards c . dropCards [c]

-- | Accept a card into current Place's hand from the discard pile
takeDiscard :: Card -> Action
takeDiscard c = over current take where
  take p = p { hand = c:(hand p), publics = c:(publics p), discardTaken = Just c }

-- | Transition game to specified phase
setPhase :: Phase -> Action
setPhase p g = g { phase = p }

-- | Transition game to Win or Draw if necessary
checkState :: Action
checkState g | (view hand_ g) == [] = setPhase Win g
checkState g = g

-- | Remove cards from the current place
dropCards :: Pile -> Action
dropCards cs = over current drop where
  drop p = p { hand = (hand p) \\ cs, publics = (publics p) \\ cs }

-- | Remove a single meld from
dropMeld :: Pile -> Action
dropMeld m g = g { melds = (delete m (melds g)) }

-- | Put a new meld into play
addMeld :: Pile -> Action
addMeld m g = g { melds = mergeMelds m (melds g) }

-- | Add a valid meld to a list of melds, combining it with an existing one if possible
mergeMelds :: Pile -> [Pile] -> [Pile]
mergeMelds m1 [] = [m1]
mergeMelds m1 (m2:ms)
    | isMeld (m1_2) = mergeMelds m1_2 ms
    | otherwise = m2:(mergeMelds m1 ms)
  where
    m1_2 = sort (m1 ++ m2)

--- | True if game is in the win state for the current player
isOver :: Game -> Bool
isOver g = case phase g of
  Win -> True
  Draw -> True
  _ -> False

-- | Given the terminating state of a game, calculates scores for each player
score :: Game -> [(String, Int)]
score g 
    | phase g == Win = (name (view current g), maybeDouble $ allPoints g):loserPoints
    | otherwise = fmap noPoints $ places g
  where
    noPoints p = (name p, 0)
    maybeDouble = if rummy (view current g) then (*2) else id
    loserPoints = fmap noPoints (tail $ places g)

-- | Points present in all hands   
allPoints :: Game -> Int
allPoints = foldl (+) 0 . fmap points . concat . fmap hand . places

-- | The number of points for a card
points :: Card -> Int
points c 
  | (value c) >= Jack = 10
  | otherwise = 1 + (fromEnum $ value c)
