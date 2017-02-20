import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty.HUnit
import Deck
import Rummy
import System.Random (getStdGen, mkStdGen)
import Data.List

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
    cardTests,
    rummyTests
  ]

instance Arbitrary Suit where
  arbitrary = elements [ (minBound :: Suit) .. ]

cardTests = testGroup "Card"
  [
    testCase "52 in deck" $
      (@=?) 52 $ length pack
  , testCase "12 face cards only" $
      (@=?) 12 $ length $ filter ((>=Jack) . value) pack
  , testProperty "shuffled /= shuffled" isShuffled
  , testProperty "shuffledGen /= shuffledGen" isShuffledGen
  ]

-- Thanks to http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck
isShuffled :: Property
isShuffled = monadicIO $ do
  shuffle1 <- run $ Deck.shuffle pack
  shuffle2 <- run $ Deck.shuffle pack
  Test.QuickCheck.Monadic.assert $ shuffle1 /= shuffle2

isShuffledGen :: Property
isShuffledGen = monadicIO $ do
  gen <- run $ getStdGen
  let
    (deck1, gen1) = shuffleGen gen pack -- Reuse the RandomGen in next shuffle
    deck2 = fst (shuffleGen gen1 deck1)
  Test.QuickCheck.Monadic.assert $ deck1 /= deck2

-- Create a fresh game with a pseudorandom deck for specified player count
gameFor :: Int -> Game
gameFor num = mkGame (mkStdGen 777) names where
  names = (:[]) <$> take num ['a'..]

setDraws :: [Card] -> Game -> Game
setDraws cs g = g { table = (table g) { draws = cs } }

setDiscards :: [Card] -> Game -> Game
setDiscards cs g = g { table = (table g) { discards = cs } }

setMelds :: [[Card]] -> Game -> Game
setMelds ms g = g { table = (table g) { melds = ms } }

setPlace :: Place -> Game -> Game
setPlace p g = g { places = p : (tail $ places g) }

fourKind = [(Card Ace Spade), (Card Ace Heart), (Card Ace Diamond), (Card Ace Club)]

testMeldsHand = [(Card Nine Heart), (Card King Heart)]
testMelds = [
    [ (Card Ten Heart), (Card Jack Heart), (Card Queen Heart) ]
  , [ (Card King Diamond), (Card King Spade), (Card King Club)]
  ]
expectedMelds = [
    AddToMeld (Card Nine Heart) [(Card Ten Heart), (Card Jack Heart), (Card Queen Heart)]
  , AddToMeld (Card King Heart) [(Card Ten Heart), (Card Jack Heart), (Card Queen Heart)]
  , AddToMeld (Card King Heart) [(Card King Diamond), (Card King Spade), (Card King Club)]
  ]

mergableMeld = sort [ (Card Three Club), (Card Four  Club), (Card Five Club)  ]

singleMeld   = sort [ (Card Six   Club), (Card Seven Club), (Card Eight Club) ]
singleMeldGame = setPlace (Place "b" singleMeld [] Nothing) $ gameFor 2

doFirstMove g = play (allMoves g !! 0) g

rummyTests = testGroup "Rummy"
  [ testCase "10 cards in hand for all players of 2-player game" $
      True @=? all (\p -> ((length $ hand p) == 10)) (places $ gameFor 2)
  , testCase "7 cards in hand for all players of 4-player game" $
      True @=? all (\p -> ((length $ hand p) == 7)) (places $ gameFor 4)
  , testCase "b is first player" $
      "b" @=? (name $ head $ places $ gameFor 3)
  , testCase "discard has one card" $
      1 @=? (length $ discards $ table $ gameFor 2)
  , testCase "current player starts with two moves" $
      2 @=? (length $ allMoves $ gameFor 2)
  , testCase "current player can draw from draws" $
      [Card Ace Spade] @=? (hand $ currentPlace $ play DrawFromDraws $ setDraws [Card Ace Spade] $ setPlace (mkPlace "b") $ gameFor 2)
  , testCase "current player can draw from discards" $
      [Card King Spade] @=? (hand $ currentPlace $ play DrawFromDiscards $ setDiscards [Card King Spade] $ setPlace (mkPlace "b") $ gameFor 2)
      -- [DiscardCard $ Card Ace Spade, DiscardCard $ Card King Hearts] @=? (moves $ setPlace (Place "b" [Card King Hearts] Nothing) $ setDraws [Card Ace Spade] $ move DrawFromDrawPile $ gameFor 2)
  , testCase "find straights" $
      [ [(Card Ten Heart), (Card Jack Heart), (Card Queen Heart)]
      , [(Card Jack Heart), (Card Queen Heart), (Card King Heart)]
      , [(Card Ten Heart), (Card Jack Heart), (Card Queen Heart), (Card King Heart)]
      ] @=? (allMelds [
        (Card Ace Spade), (Card Queen Heart), (Card Jack Heart), (Card Ten Heart)
      , (Card King Heart) ])
  , testCase "find kinds" $
     ((sort (filter (\xs -> length xs >= 3) $ subsequences (sort fourKind))) @=?
      (sort $ allMelds fourKind))
  , testCase "find meld moves" $
      [ PlayMeld [(Card Ten Heart), (Card Jack Heart), (Card Queen Heart)] ] @=?
        (meldMoves $ setPlace (Place "b" [(Card Ten Heart), (Card Jack Heart), (Card Queen Heart)] [] Nothing) $ gameFor 3)
  , testCase "find add moves" $
      expectedMelds @=?
        (addMoves $ setPlace (Place "b" testMeldsHand [] Nothing) $ setMelds testMelds $ gameFor 3)
  , testCase "find discard moves" $
      [ (DiscardCard (Card Ten Club)) ] @=?
        (discardMoves $ setPlace (Place "b" [(Card Ten Club), (Card Two Heart)] [] (Just (Card Two Heart))) $ gameFor 2)
  , testCase "meld available" $
      (PlayMeld singleMeld) @=? ((allMoves $ setPhase Meld singleMeldGame) !! 0)
  , testCase "meld left on table" $
      [ singleMeld ] @=? (melds $ table $ doFirstMove $ setPhase Meld singleMeldGame)
  , testCase "meld gone from hand" $
      [ ] @=? (hand $ currentPlace $ doFirstMove $ setPhase Meld singleMeldGame)
  , testCase "meld wins game" $
      Win @=? (phase $ play ((allMoves $ setPhase Meld singleMeldGame) !! 0) singleMeldGame)
  , testCase "melds are merged" $
      [ sort (singleMeld ++ mergableMeld) ] @=? (melds $ table $ play ((allMoves $ setPhase Meld singleMeldGame) !! 0) $ setMelds [ mergableMeld ] singleMeldGame)
  , testCase "no meld if no discard" $
      (sort [
          (DiscardCard (Card Three Club))
        , (DiscardCard (Card Four Club))
        , (DiscardCard (Card Five Club)) ]) @=?
      (allMoves $ setPhase Meld $ setPlace (Place "b" [(Card Three Club), (Card Four Club), (Card Five Club), (Card Ace Heart)] [] (Just (Card Ace Heart))) $ gameFor 2)
  , testCase "add if meldable discard" $
      (sort [
          (DiscardCard (Card Three Club))
        , (AddToMeld (Card Three Club) [(Card Four Club), (Card Five Club), (Card Six Club)])
        , (AddToMeld (Card Seven Club) [(Card Four Club), (Card Five Club), (Card Six Club)])
      ]) @=?
      (allMoves $ setMelds [[(Card Four Club), (Card Five Club), (Card Six Club)]] $ setPhase Meld $ setPlace (Place "b" [(Card Three Club), (Card Seven Club)] [] (Just (Card Seven Club))) $ gameFor 2)
  , testCase "no add if no discard" $
      (sort [ (DiscardCard (Card Three Club)) ]) @=?
      (allMoves
         $ setMelds [[(Card Four Club), (Card Five Club), (Card Six Club)]]
         $ setPhase Meld
         $ setPlace (Place "b" [(Card Three Club), (Card Ace Heart)] [] (Just (Card Ace Heart)))
         $ gameFor 2)

  , testCase "detect non-win" $
      False @=? (isWin $ gameFor 2)

  , testCase "detect win" $
      True @=? (isWin
        $ doFirstMove
        $ setPhase Meld
        $ setPlace (Place "b" [(Card Ace Spade)] [] Nothing)
        $ gameFor 2)

  , testCase "shuffle discards" $
      1 @=? (length $ draws
                $ table
                $ doFirstMove
                $ setDraws []
                $ setDiscards [(Card Six Heart), (Card Ace Spade)]
                $ gameFor 2)
  ]
