{-# Language OverloadedStrings #-}
module Main where

import Data.Text(Text)
import Data.List(intercalate)
import Text.Read(readMaybe)
import Control.Monad(forM)
import qualified Data.Map as Map

import Deck
import Card
import Battle
import Enemy
import Player
import RNG
import Field


main :: IO ()
main =
  do rng <- newRNG
     let b0 = newBattle rng char1
         initBattle = do newEnemy (Enemy 5)
                         newEnemy (Enemy 10)
                         newTurn
         Right (_,b1) = runBattleM initBattle b0
         loop s = do putStrLn (showBattle s)
                     x <- getCommand
                     let next = case x of
                                  EndTurn -> endTurn >> newTurn
                                  PlayCard x y -> playCard x y
                     case runBattleM next s of
                       Left err -> putStrLn ("*** " ++ err) >> loop s
                       Right (_,s1) -> loop s1
     loop b1


getCommand :: IO Command
getCommand =
  do cmd <- getLine
     case parseCommand cmd of
       Just ok -> pure ok
       Nothing -> do putStrLn "I don't understand."
                     getCommand

data Command = EndTurn | PlayCard Int Target

parseCommand :: String -> Maybe Command
parseCommand inp
  | inp == "e" = Just EndTurn
  | Just (x,y) <- readMaybe inp = Just (PlayCard x (TargetEnemy y))
  | Just x <- readMaybe inp = Just (PlayCard x TargetPlayer)
  | otherwise = Nothing


newBattle :: RNG -> Character -> Battle
newBattle rng c = Battle
  { player  = charPlayer c
  , deck    = set theDiscardPile (charDeck c) (emptyDeck rng)
  , enemies = Map.empty
  , nextEnemyId = 0
  , hand    = []
  }

showBattle :: Battle -> String
showBattle b = unlines $
  [ "Enemy " ++ show k ++ ": " ++ showEnemy e
  | (k,e) <- Map.toList (enemies b)
  ] ++
  [ "Hero   : " ++ showPlayer (player b)
  , "-------"
  , summary drawPile ++ " " ++ show (zip [0..] (hand b)) ++
                        " " ++ summary discardPile
  ]
  where
  summary f = "(" ++ show (length (f (deck b))) ++ ")"


--------------------------------------------------------------------------------
-- Battle Turn

newTurn :: BattleM ()
newTurn =
  do updField thePlayer refillEnergy
     cs <- updField' theDeck (drawMany 5)
     updField theHand (cs ++)

playCard :: Int -> Target -> BattleM ()
playCard cardIx target =
  do c <- updField' theHand (extract cardIx)
     b <- getBattle
     case cardAction c b target of
       Nothing -> reportError "Cannot play this card on the given target"
       Just as -> forM as $ \act -> do actDo act
                                       removeDeadEnemies
     updField theDeck (addTo theDiscardPile c)

endTurn :: BattleM ()
endTurn =
  do h <- getField theHand
     setField theHand []
     updField theDeck (addManyTo theDiscardPile h)
     updField thePlayer (set theEnergy 0)
     updField thePlayer (set theShield 0)



--------------------------------------------------------------------------------
-- Character specific


data Character = Character
  { charPlayer :: Player
  , charDeck   :: [Card]
  }

char1 :: Character
char1 = Character
  { charPlayer =
        Player
          { playerHP            = 60
          , playerShield        = 0
          , playerEnergyIncome  = 3
          , playerEnergy        = 0
          }
  , charDeck = replicate 5 ironWave ++ replicate 5 strike ++ replicate 5 defend
  }




