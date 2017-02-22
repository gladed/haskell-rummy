module Main where

import Deck
import Rummy
import System.Random
import Data.List
import Text.Read (readMaybe)
import System.IO

type Strategy = Game -> IO (Move)

basicStrategy :: Strategy
basicStrategy g = do
  let moves = allMoves g
  return $ basicMove moves g

basicMove :: [Move] -> Game -> Move
basicMove moves@(DrawFromDraws:_) g =
  if meldable (head $ discards g) g then moves !! 1
  else head moves
basicMove moves _ = head moves

meldable :: Card -> Game -> Bool
meldable c g = any (\m -> (isMeld (sort (c:m)))) (melds g)

userInputStrategy :: Strategy
userInputStrategy g = do
  let moves = allMoves g  
  putStrLn $ presentGame g
  moveNum <- getLineInt "Your move?"
  if moveNum > (length moves) then do
    putStrLn $ "\n*** Invalid selection"
    userInputStrategy g
  else return $ moves !! (moveNum-1)

getLineInt :: String -> IO Int
getLineInt prompt = do  
  putStr $ prompt ++ " "
  hFlush stdout
  line <- getLine
  case readMaybe line of
    Nothing -> putStrLn "Try again" >> getLineInt prompt
    Just x -> return x

-- | Execute moves until next player or game over
playerLoop :: Strategy -> Game -> IO (Game)
playerLoop s g = do
  let player = name $ head $ places $ g
  m <- s g
  putStrLn $ "Player " ++ player ++ ": " ++ (show m)
  let g2 = play m g
  if isOver g2 || (player /= (name $ head $ places $ g2))
  then return g2
  else playerLoop s g2

gameLoop :: [Strategy] -> Game -> IO (Game)
gameLoop (s:ss) g = do
  putStrLn "\n*** NEW TURN ***"
  g2 <- playerLoop s g
  if (isOver g2) then return g2
  else gameLoop (ss ++ [s]) g2

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
  endGame <- gameLoop [userInputStrategy, basicStrategy] game
  putStrLn ("Game over! Scores: " ++ (show $ score endGame))
