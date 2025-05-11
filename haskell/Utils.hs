
module Utils where

import DataTypes
import Data.Char (toUpper)
import qualified Data.Map as M
import Control.Monad.State
import System.Random (getStdRandom, randomR)


tell :: String -> StateT GameState IO ()
tell msg = modify $ \gs -> gs 


type GameMessages = [String]
type GameAction = StateT GameState IO GameMessages

generateRandom :: (Int, Int) -> IO Int
generateRandom range = getStdRandom (randomR range)



modifyPlayer :: (Player -> Player) -> GameState -> GameState
modifyPlayer f gs = gs { playerState = f (playerState gs) }


addPlayerWeights :: [Weight] -> Player -> Player
addPlayerWeights weightsToAdd player = player { weightInventory = newInv }
  where
    currentInv = weightInventory player
    newInv = foldr (\w acc -> M.insertWith (+) w 1 acc) currentInv weightsToAdd




removePlayerWeights :: [Weight] -> Player -> Maybe Player
removePlayerWeights weightsToRemove player =
    foldM subtractOne (weightInventory player) weightsToRemove >>= \newInv ->
    Just player { weightInventory = newInv }
  where
    subtractOne inv w =
        case M.lookup w inv of
            Just count | count > 1 -> Just (M.insert w (count - 1) inv)
                       | count == 1 -> Just (M.delete w inv)
            _ -> Nothing 



capitalise :: String -> String
capitalise [] = []
capitalise (h:t) = Data.Char.toUpper h : t

mapToStringList :: (Show k, Show v) => M.Map k v -> [String] -> [String]
mapToStringList m header = if M.null m then [] else header ++ map (\(k,v) -> show v ++ "x " ++ show k ++ "kg") (M.toList m)



formatItemNameForDisplay :: Item -> String
formatItemNameForDisplay item = capitalise $ replaceUnderscoresWithSpaces $ show item
  where
    replaceUnderscoresWithSpaces = map (\c -> if c == '_' then ' ' else c)