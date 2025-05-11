{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import Data.Char (toLower)
import Data.List (isInfixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)

-- Locations in the game
data Location
    = Dom
    | Parking
    | Recepcja
    | SzatniaMeska
    | NieczynnyPrysznic
    | SzatniaDamska
    | StrefaWolnychCiezarow
    | StrefaCardio
    | StrefaMaszyn
    | Lazienka
    deriving (Show, Eq, Ord, Read, Generic, Enum, Bounded)

-- Items the player can interact with, hold, or use
data Item
    = StrojSportowy
    | Woda
    | Karnet
    | Monster
    | Przedtreningowka
    | MalaStrzykawka
    | DuzaStrzykawka
    | CzerwonyBidon
    | Dzik -- Energy drink found in shower
    | Magnesium -- Mentioned in Prolog `has_magnesium` but not fully implemented there
    -- Weights are handled separately in weightInventory, but can be conceptualized
    | Talerz5kg
    | Talerz10kg
    | Talerz15kg
    | Talerz20kg
    | Talerz25kg
    deriving (Show, Eq, Ord, Read, Generic, Enum, Bounded)

-- Non-Player Characters
data NPC
    = Recepcjonistka
    | PodejrzanyTyp
    | SzczurBojowy
    | Brunetka
    | DuzyChlop
    | WielkiChlop
    | CzlowiekSzczuply
    | ChudySzczur
    | Swiezak -- Appears at stage 2
    deriving (Show, Eq, Ord, Read, Generic, Enum, Bounded)

-- Muscle groups for tracking lifting progress
data MuscleGroup
    = KlatkaPiersiowa
    | Barki
    | Biceps
    deriving (Show, Eq, Ord, Read, Generic)

type Weight = Int -- Type alias for weight values (e.g., 5, 10, 15 kg)

-- Represents the player's state
data Player = Player
    { strength        :: Int
    , money           :: Int
    , inventory       :: S.Set Item
    , weightInventory :: M.Map Weight Int -- Map of weight -> count
    , wearingSportswear :: Bool
    } deriving (Show, Generic)

-- Represents the state of the bench press
data BenchState = BenchState
    { benchLeft     :: [Weight]
    , benchRight    :: [Weight]
    , benchOccupied :: Bool
    } deriving (Show, Generic)

-- Represents the overall game state
data GameState = GameState
    { playerState       :: Player
    , currentLocation   :: Location
    , worldItems        :: M.Map Item Location -- Items in the game world
    , npcLocations      :: M.Map NPC Location  -- NPCs in the game world
    , activeNPCs        :: S.Set NPC           -- NPCs currently active/present
    , liftedStats       :: M.Map MuscleGroup Int
    , gameStage         :: Int                 -- Current stage of the main quest
    , gymPassUsed       :: Bool                -- Replaces `szatnia_wejscie`
    , showerChecked     :: Bool                -- Replaces `prysznic_sprawdzony`
    , benchState        :: BenchState          -- Add this field
    , chudySzczurTalked :: Bool
    , czlowiekSzczuplyTalked :: Bool
    , szczurBojowyTalked :: Bool
    , duzyChlopTalked :: Bool
    } deriving (Show, Generic)

-- For parsing commands
data Command
    = Go LocationName
    | Take String
    | Drop String
    | Inventory
    | Consume String
    | Talk String
    | Buy String
    | Wear String
    | Look
    | Instructions
    | CheckMoney
    | LeftAddWeight Weight
    | RightAddWeight Weight
    | RemoveAllWeightsBench
    | CheckBench
    | DoBenchPress
    | TakeBench
    | KomplementujRecepcjonistke
    -- | IdzDoPrysznica -- This is handled by Go NieczynnyPrysznic
    | ShowWeightInventory
    | Help
    | Quit
    | Unknown String
    deriving (Show, Eq)

type LocationName = String -- For parsing, will be resolved to `Location`

-- Helper to get item name for display (replaces Prolog's atom to string with spaces)
itemDisplayName :: Item -> String
itemDisplayName StrojSportowy = "stroj sportowy"
itemDisplayName Woda = "woda"
itemDisplayName Karnet = "karnet"
itemDisplayName Monster = "monster"
itemDisplayName Przedtreningowka = "przedtreningowka"
itemDisplayName MalaStrzykawka = "mala strzykawka"
itemDisplayName DuzaStrzykawka = "duza strzykawka"
itemDisplayName CzerwonyBidon = "czerwony bidon"
itemDisplayName Dzik = "dzik"
itemDisplayName Magnesium = "magnesium"
itemDisplayName Talerz5kg = "talerz 5kg"
itemDisplayName Talerz10kg = "talerz 10kg"
itemDisplayName Talerz15kg = "talerz 15kg"
itemDisplayName Talerz20kg = "talerz 20kg"
itemDisplayName Talerz25kg = "talerz 25kg"

npcDisplayName :: NPC -> String
npcDisplayName Recepcjonistka = "recepcjonistka"
npcDisplayName PodejrzanyTyp = "podejrzany typ"
npcDisplayName SzczurBojowy = "szczur bojowy"
npcDisplayName Brunetka = "brunetka"
npcDisplayName DuzyChlop = "duzy chlop"
npcDisplayName WielkiChlop = "wielki chlop"
npcDisplayName CzlowiekSzczuply = "czlowiek szczuply"
npcDisplayName ChudySzczur = "chudy szczur"
npcDisplayName Swiezak = "swiezak"

locationDisplayName :: Location -> String
locationDisplayName Dom = "dom"
locationDisplayName Parking = "parking"
locationDisplayName Recepcja = "recepcja"
locationDisplayName SzatniaMeska = "szatnia meska"
locationDisplayName NieczynnyPrysznic = "nieczynny prysznic"
locationDisplayName SzatniaDamska = "szatnia damska"
locationDisplayName StrefaWolnychCiezarow = "strefa wolnych ciezarow"
locationDisplayName StrefaCardio = "strefa cardio"
locationDisplayName StrefaMaszyn = "strefa maszyn"
locationDisplayName Lazienka = "lazienka"


stringToItem :: String -> Maybe Item
stringToItem s
    | "stroj" `isInfixOf` ls = Just StrojSportowy
    | "woda" `isInfixOf` ls = Just Woda
    | "karnet" `isInfixOf` ls = Just Karnet
    | "monster" `isInfixOf` ls = Just Monster
    | "przedtreningowka" `isInfixOf` ls = Just Przedtreningowka
    | "mala_strzykawka" `isInfixOf` ls || "mala strzykawka" `isInfixOf` ls = Just MalaStrzykawka
    | "duza_strzykawka" `isInfixOf` ls || "duza strzykawka" `isInfixOf` ls = Just DuzaStrzykawka
    | "czerwony_bidon" `isInfixOf` ls || "czerwony bidon" `isInfixOf` ls = Just CzerwonyBidon
    | "dzik" `isInfixOf` ls = Just Dzik
    | "magnesium" `isInfixOf` ls = Just Magnesium
    | "talerz_5" `isInfixOf` ls || "talerz 5" `isInfixOf` ls = Just Talerz5kg
    | "talerz_10" `isInfixOf` ls || "talerz 10" `isInfixOf` ls = Just Talerz10kg
    | "talerz_15" `isInfixOf` ls || "talerz 15" `isInfixOf` ls = Just Talerz15kg
    | "talerz_20" `isInfixOf` ls || "talerz 20" `isInfixOf` ls = Just Talerz20kg
    | "talerz_25" `isInfixOf` ls || "talerz 25" `isInfixOf` ls = Just Talerz25kg
    | otherwise = Nothing
  where ls = map toLower s
        toLower = Data.Char.toLower
        isInfixOf = Data.List.isInfixOf


stringToNPC :: String -> Maybe NPC
stringToNPC s
    | "recepcjonistka" `isInfixOf` ls = Just Recepcjonistka
    | "podejrzany_typ" `isInfixOf` ls || "podejrzany typ" `isInfixOf` ls = Just PodejrzanyTyp
    | "szczur_bojowy" `isInfixOf` ls || "szczur bojowy" `isInfixOf` ls = Just SzczurBojowy
    | "brunetka" `isInfixOf` ls = Just Brunetka
    | "duzy_chlop" `isInfixOf` ls || "duzy chlop" `isInfixOf` ls = Just DuzyChlop
    | "wielki_chlop" `isInfixOf` ls || "wielki chlop" `isInfixOf` ls = Just WielkiChlop
    | "czlowiek_szczuply" `isInfixOf` ls || "czlowiek szczuply" `isInfixOf` ls = Just CzlowiekSzczuply
    | "chudy_szczur" `isInfixOf` ls || "chudy szczur" `isInfixOf` ls = Just ChudySzczur
    | "swiezak" `isInfixOf` ls = Just Swiezak
    | otherwise = Nothing
  where ls = map toLower s
        toLower = Data.Char.toLower
        isInfixOf = Data.List.isInfixOf


stringToLocation :: LocationName -> Maybe Location
stringToLocation s
    | "dom" == ls = Just Dom
    | "parking" == ls = Just Parking
    | "recepcja" == ls = Just Recepcja
    | "szatnia_meska" == ls || "szatnia meska" == ls = Just SzatniaMeska
    | "nieczynny_prysznic" == ls || "nieczynny prysznic" == ls = Just NieczynnyPrysznic
    | "szatnia_damska" == ls || "szatnia damska" == ls = Just SzatniaDamska
    | "strefa_wolnych_ciezarow" == ls || "strefa wolnych ciezarow" == ls = Just StrefaWolnychCiezarow
    | "strefa_cardio" == ls || "strefa cardio" == ls = Just StrefaCardio
    | "strefa_maszyn" == ls || "strefa maszyn" == ls = Just StrefaMaszyn
    | "lazienka" == ls = Just Lazienka
    | otherwise = Nothing
  where ls = map toLower s
        toLower = Data.Char.toLower

-- Weight items (not regular items, but things that can be in weightInventory or on bench)
validWeights :: [Weight]
validWeights = [5, 10, 15, 20, 25]

stringToWeight :: String -> Maybe Weight
stringToWeight s = case reads s of
    [(w, "")] -> if w `elem` validWeights then Just w else Nothing
    [(w, "kg")] -> if w `elem` validWeights then Just w else Nothing
    _         -> Nothing