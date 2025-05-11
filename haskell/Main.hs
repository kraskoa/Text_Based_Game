-- Main.hs
{-# LANGUAGE FlexibleContexts #-}
import DataTypes
import GameData
import Actions
import NPCs -- For handleTalk
import Utils

import Control.Monad.State
import System.IO (hFlush, stdout)
import System.Random (getStdRandom, randomR, newStdGen, StdGen)
import Data.Char (toLower, isDigit)
import Data.List (isPrefixOf)
import qualified Data.Map as M
import System.Exit (exitSuccess)


-- Initialize game with random strength and money
initializeGame :: IO GameState
initializeGame = do
    initialStr <- getStdRandom (randomR (50, 100)) -- Prolog: random(50, 101, S) -> S in [50, 100]
    initialMon <- getStdRandom (randomR (0, 150))  -- Prolog: random(0, 151, M) -> M in [0, 150]
    let player = initialPlayer { strength = initialStr, money = initialMon }
    return $ initialGameState player

-- Parse player input into a Command
parseCommand :: String -> Command
parseCommand input =
    let tokens = words $ map toLower input
    in case tokens of
        ["go", loc] -> Go loc
        ["go", loc1, loc2] -> Go (loc1 ++ " " ++ loc2) -- For two-word locations like "strefa cardio"
        ["go", loc1, loc2, loc3] -> Go (loc1 ++ " " ++ loc2 ++ " " ++ loc3)    
        ["take", item] -> Take item
        ["take", item1, item2] -> Take (item1 ++ " " ++ item2)
        ["drop", item] -> Drop item
        ["drop", item1, item2] -> Drop (item1 ++ " " ++ item2)
        ["inventory"] -> Inventory
        ["inv"] -> Inventory
        ["consume", item] -> Consume item
        ["consume", item1, item2] -> Consume (item1 ++ " " ++ item2)
        ["talk", npc] -> Talk npc
        ["talk", npc1, npc2] -> Talk (npc1 ++ " " ++ npc2)
        ["buy", item] -> Buy item
        ["buy", item1, item2] -> Buy (item1 ++ " " ++ item2)
        ["wear", item] -> Wear item
        ["wear", item1, item2] -> Wear (item1 ++ " " ++ item2)
        ["look"] -> Look
        ["instructions"] -> Instructions
        ["help"] -> Instructions
        ["money"] -> CheckMoney
        ["check_money"] -> CheckMoney
        ["weights"] -> ShowWeightInventory
        ["weight_inventory"] -> ShowWeightInventory

        -- Bench commands
        ["take_bench"] -> TakeBench
        ["left_add", wStr] | all isDigit wStr ->
            case stringToWeight wStr of Just w -> LeftAddWeight w; _ -> Unknown input
        ["right_add", wStr] | all isDigit wStr ->
            case stringToWeight wStr of Just w -> RightAddWeight w; _ -> Unknown input
        ["remove_all_weights"] -> RemoveAllWeightsBench -- Shortened from Prolog for ease
        ["remove_weights"] -> RemoveAllWeightsBench
        ["check_bench"] -> CheckBench
        ["do_bench_press"] -> DoBenchPress
        ["komplementuj"] -> KomplementujRecepcjonistke -- shorthand
        ["komplementuj", "recepcjonistke"] -> KomplementujRecepcjonistke


        ["quit"] -> Quit
        ["q"] -> Quit
        _ -> Unknown input


-- Process a command and update game state
processCommand :: Command -> StateT GameState IO ()
processCommand cmd = do
    messages <- case cmd of
        Go locName               -> handleGo locName
        Take itemName            -> handleTake itemName
        Drop itemName            -> handleDrop itemName
        Inventory                -> handleInventory
        Consume itemName         -> handleConsume itemName
        Talk npcName             -> handleTalk npcName
        Buy itemName             -> handleBuy itemName
        Wear itemName            -> handleWear itemName
        Look                     -> handleLook
        Instructions             -> handleInstructions
        CheckMoney               -> handleCheckMoney
        ShowWeightInventory      -> handleShowWeightInventory

        TakeBench                -> handleTakeBench
        LeftAddWeight w          -> handleLeftAddWeightBench w
        RightAddWeight w         -> handleRightAddWeightBench w
        RemoveAllWeightsBench    -> handleRemoveAllWeightsBench
        CheckBench               -> handleCheckBench
        DoBenchPress             -> handleDoBenchPress
        KomplementujRecepcjonistke -> handleKomplementujRecepcjonistke

        Quit                     -> liftIO (putStrLn "Do zobaczenia!") >> liftIO exitSuccess >> return []
        Unknown s                -> return ["Nie rozumiem '" ++ s ++ "'. Wpisz 'help' po listę komend."]

    liftIO $ mapM_ putStrLn messages

-- Main game loop
gameLoop :: StateT GameState IO ()
gameLoop = do
    gs <- get
    -- Check for game over conditions not tied to specific actions (e.g. if score or stage triggers it)
    -- (Currently, game over is handled within actions like DoBenchPress, Consume, Go SzatniaDamska)

    liftIO $ putStr "> "
    liftIO $ hFlush stdout
    line <- liftIO getLine
    let cmd = parseCommand line
    processCommand cmd
    gameLoop -- Loop indefinitely until Quit or exitSuccess

-- Start the game
main :: IO ()
main = do
    putStrLn "Witaj w 'Klata, biceps, barki'!"
    putStrLn "Twoim celem jest odbycie treningu na siłowni."
    putStrLn "Zbierz potrzebny ekwipunek z domu, wejdź na siłownię i wykonaj ćwiczenia!"
    putStrLn "Wpisz 'help' aby zobaczyć listę komend."
    putStrLn "---------------------------------------------------------------------"

    initialGs <- initializeGame

    -- Run the initial 'look' for the player
    (initialMessages, s0) <- runStateT handleLook initialGs
    mapM_ putStrLn initialMessages

    evalStateT gameLoop s0