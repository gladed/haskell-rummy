module Main where

import Deck
import Rummy
import System.Random
import Data.List
import Text.Read (readMaybe)
import System.IO


userInputStrategy :: Game -> IO Game
userInputStrategy = userInputLoop

userInputLoop :: Game -> IO Game
userInputLoop g = do
  let moves = allMoves g  
  putStrLn $ presentGame g
  moveNum <- getLineInt "Your move?"
  if moveNum > length moves then do
    putStrLn "\n*** Invalid selection"
    userInputLoop g
  else do
    let userMove = moves !! (moveNum-1)
    let g2 = play userMove g
    case (isOver g2, userMove) of
      (True, _) -> return g2
      (_, DiscardCard _) -> return g2
      _ -> userInputLoop g2

getLineInt :: String -> IO Int
getLineInt prompt = do  
  putStr $ prompt ++ " "
  hFlush stdout
  line <- getLine
  case readMaybe line of
    Nothing -> putStrLn "Try again" >> getLineInt prompt
    Just x -> return x

gameLoop :: Game -> IO () 
gameLoop g = do
  putStrLn "\n*** NEW TURN *** "
  g2 <- userInputLoop g
  if isOver g2 then putStrLn ("Game over! Scores: " ++ show (score g2)) else gameLoop g2


presentGame :: Game -> String
presentGame g = 
       "\n=== Player " ++ name p ++ " ==="
    ++ "\nPlayers: " ++ intercalate ", " (fmap (\op -> name op ++ " with " ++ show (length $ hand op)) (tail $ places g))
    ++ "\nMelds in play: " ++ show (melds g)
    ++ "\nDiscard: " ++ showTopCard (discards g)
    ++ "\nHand:\n  " ++ showHand (hand p) 
    ++ "\nMoves:\n  " ++ showMoves (allMoves g)
  where
    p = head $ places g

showMoves :: [Move] -> String
showMoves ms = intercalate "\n  " $ (\(n,m) -> show n ++ ". " ++ show m) <$> zip [1..] ms

showHand :: [Card] -> String
showHand cs = intercalate "\n  " (show <$> sort cs)

showTopCard :: [Card] -> String
showTopCard (c:_) = show c
showTopCard _ = "None"

main :: IO ()
main = do
  gen <- getStdGen
  let game = mkGame gen ["Anna", "Barry"]
  gameLoop game

