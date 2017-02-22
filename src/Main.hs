module Main where

import Deck
import Rummy
import System.Random
import Data.List
import Text.Read (readMaybe)
import System.IO
import Data.Foldable (maximumBy, minimumBy, Foldable)
import Data.Ord (comparing)

class Strategy s where
  choose :: s -> [Move] -> Game -> IO Move

-- Draw if meld, meld first, discard worst

data Basic = Basic

instance Strategy Basic where
  choose s moves@(DrawFromDraws:_) g =
    if meldable (head $ discards g) g then return $ moves !! 1
    else return $ head moves
  choose s moves@(DiscardCard _:_) g = return $ maxBy (\(DiscardCard c) -> points c) moves
  choose s moves _ = return $ head moves

meldable :: Card -> Game -> Bool
meldable c g = any (\m -> isMeld (sort (c:m))) (melds g)

maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing

minBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
minBy = minimumBy . comparing

-- User input strategy

data UserInput = UserInput

instance Strategy UserInput where
  choose s ms g = do
    putStrLn $ presentGame g
    putStrLn $ "\nMoves:\n  " ++ showMoves (allMoves g)
    moveNum <- getLineInt "Your move?"
    if moveNum > length ms then do
      putStrLn "\n*** Invalid selection"
      choose s ms g
    else return $ ms !! (moveNum-1)

getLineInt :: String -> IO Int
getLineInt prompt = do  
  putStr $ prompt ++ " "
  hFlush stdout
  line <- getLine
  case readMaybe line of
    Nothing -> putStrLn "Try again" >> getLineInt prompt
    Just x -> return x

-- | Execute moves until next player or game over
playerLoop :: (Strategy s) => s -> Game -> IO Game
playerLoop s g = do
  let player = name $ head $ places g
  m <- choose s (allMoves g) g
  putStrLn $ "Player " ++ player ++ ": " ++ show m
  let g2 = play m g
  if isOver g2 || (player /= name (head $ places g2))
  then return g2
  else playerLoop s g2

gameLoop :: Strategy s => [s] -> Game -> IO Game
gameLoop (s:ss) g = do
  putStrLn "\n*** NEW TURN ***"
  g2 <- playerLoop s g
  if isOver g2 then return g2
  else gameLoop (ss ++ [s]) g2

presentGame :: Game -> String
presentGame g = 
       "\n=== Player " ++ name p ++ " ==="
    ++ "\nPlayers: " ++ intercalate ", " (fmap (\op -> name op ++ " with " ++ show (length $ hand op)) (tail $ places g))
    ++ "\nMelds in play: " ++ show (melds g)
    ++ "\nDiscard: " ++ showTopCard (discards g)
    ++ "\nHand:\n  " ++ showHand (hand p) 
  where
    p = head $ places g

showMoves :: [Move] -> String
showMoves ms = intercalate "\n  " $ (\(n,m) -> show n ++ ". " ++ show m) <$> zip [1..] ms

showHand :: [Card] -> String
showHand cs = intercalate "\n  " (show <$> sort cs)

showTopCard :: [Card] -> String
showTopCard (c:_) = show c
showTopCard _ = "None"

-- | Run a match between supplied players to some ridiculous high number
match :: Strategy s => [(String, s)] -> IO ()
match = undefined -- TODO

main :: IO ()
main = do
  gen <- getStdGen
  let game = mkGame gen ["Anna", "Barry"]
  endGame <- gameLoop [Basic, Basic] game
  putStrLn $ "Final game status: " ++ show endGame
  putStrLn $ "Game over! Scores: " ++ show (score endGame)

