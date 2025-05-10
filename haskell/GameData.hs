-- GameData.hs
module GameData where

import DataTypes
import qualified Data.Map as M
import qualified Data.Set as S

-- Initial player stats (randomness will be handled in Main or Actions)
initialPlayer :: Player
initialPlayer = Player
    { strength = 0 -- Will be set randomly
    , money = 0    -- Will be set randomly
    , inventory = S.empty
    , weightInventory = M.empty
    , wearingSportswear = False
    }

-- Initial game state (before random values are set)
initialGameState :: Player -> GameState
initialGameState player = GameState
    { playerState = player
    , currentLocation = Dom
    , worldItems = initialWorldItems
    , npcLocations = initialNpcLocations
    , activeNPCs = initialActiveNPCs
    , liftedStats = M.fromList [(KlatkaPiersiowa, 0), (Barki, 0), (Biceps, 0)]
    , gameStage = 0
    , gymPassUsed = False
    , showerChecked = False
    , benchState = BenchState { benchLeft = [], benchRight = [], benchOccupied = False } -- Initialize BenchState
    }

-- Initial locations of items in the world
initialWorldItems :: M.Map Item Location
initialWorldItems = M.fromList
    [ (StrojSportowy, Dom)
    , (Woda, Dom)
    , (Karnet, Dom) -- Player starts with a karnet at home in Prolog, but also can buy one. Let's assume it's at home.
    , (CzerwonyBidon, Lazienka)
    -- Talerze_10_kg at strefa_cardio is mentioned but seems to be obtained via Brunetka interaction.
    -- The weight items themselves are not "at" locations like regular items; they are obtained from NPCs.
    ]

-- Initial locations of NPCs
initialNpcLocations :: M.Map NPC Location
initialNpcLocations = M.fromList
    [ (Recepcjonistka, Recepcja)
    , (PodejrzanyTyp, Parking)
    , (ChudySzczur, StrefaWolnychCiezarow)
    , (Brunetka, StrefaCardio)
    , (WielkiChlop, StrefaWolnychCiezarow)
    , (CzlowiekSzczuply, StrefaMaszyn)
    , (DuzyChlop, StrefaMaszyn)
    , (SzczurBojowy, StrefaMaszyn)
    -- Swiezak appears later, so not in initialNpcLocations but added to activeNPCs/npcLocations at stage 2
    ]

initialActiveNPCs :: S.Set NPC
initialActiveNPCs = S.fromList
    [ Recepcjonistka, PodejrzanyTyp, ChudySzczur, Brunetka, WielkiChlop,
      CzlowiekSzczuply, DuzyChlop, SzczurBojowy
    ]

-- Room connections (bidirectional)
paths :: [(Location, Location)]
paths =
    [ (Parking, Dom)
    , (Dom, Parking)
    , (Parking, Recepcja)
    , (Recepcja, Parking)
    , (Recepcja, SzatniaMeska)
    , (SzatniaMeska, Recepcja)
    , (SzatniaMeska, NieczynnyPrysznic)
    , (NieczynnyPrysznic, SzatniaMeska)
    , (Recepcja, SzatniaDamska)
    , (SzatniaDamska, Recepcja)
    , (SzatniaMeska, StrefaWolnychCiezarow)
    , (StrefaWolnychCiezarow, SzatniaMeska)
    , (SzatniaMeska, StrefaCardio)
    , (StrefaCardio, SzatniaMeska)
    , (SzatniaMeska, StrefaMaszyn)
    , (StrefaMaszyn, SzatniaMeska)
    , (SzatniaDamska, StrefaMaszyn) -- Path from SzatniaDamska to StrefaMaszyn (as per Prolog)
    , (StrefaMaszyn, SzatniaDamska) -- And back
    , (SzatniaDamska, StrefaCardio) -- Path from SzatniaDamska to StrefaCardio
    , (StrefaCardio, SzatniaDamska) -- And back
    -- Note: Prolog has SzatniaDamska <-> StrefaWolnychCiezarow, SzatniaDamska <-> StrefaCardio (duplicate?)
    -- and StrefaCardio <-> StrefaMaszyn.
    -- Let's ensure consistency with the provided Prolog:
    , (SzatniaDamska, StrefaWolnychCiezarow)
    , (StrefaWolnychCiezarow, SzatniaDamska)
    , (StrefaCardio, StrefaMaszyn)
    , (StrefaMaszyn, StrefaCardio) -- Added for symmetry if not already present
    , (SzatniaMeska, Lazienka)
    , (Lazienka, SzatniaMeska)
    ]

canGo :: Location -> Location -> Bool
canGo from to = (from, to) `elem` paths

-- Item prices
itemPrices :: M.Map Item Int
itemPrices = M.fromList
    [ (Monster, 10)
    , (Przedtreningowka, 20)
    , (Karnet, 40)          -- Price if buying, though one might be at home
    , (MalaStrzykawka, 30)
    , (DuzaStrzykawka, 50)
    ]

-- Where items can be bought
buyableAt :: M.Map Item Location
buyableAt = M.fromList
    [ (Monster, Recepcja)
    , (Przedtreningowka, Recepcja)
    , (Karnet, Recepcja)
    , (MalaStrzykawka, Parking)
    , (DuzaStrzykawka, Parking)
    ]

-- Descriptions of locations
locationDescriptions :: Location -> [String]
locationDescriptions Dom = ["Jesteś w domu. Musisz zebrać ekwipunek na siłownię!"]
locationDescriptions Parking = ["Jesteś na parkingu przed siłownią. Możesz iść do siłowni!"]
locationDescriptions Recepcja = ["Jesteś w recepcji. Możesz kupić suplementy i karnet!"]
locationDescriptions SzatniaMeska =
    [ "Jesteś w męskiej szatni. Możesz udać się do stref treningowych!"
    , "Widzisz, że jeden z pryszniców jest nieczynny, może chcesz sprawdzić co jest w środku? (go nieczynny_prysznic)"
    ]
locationDescriptions SzatniaDamska = ["Jesteś w damskiej szatni. Możesz udać się do stref treningowych!"]
locationDescriptions StrefaMaszyn = ["Jesteś w strefie maszyn."]
locationDescriptions StrefaWolnychCiezarow = ["Jesteś w strefie wolnych ciężarów."]
locationDescriptions StrefaCardio = ["Jesteś w strefie cardio."]
locationDescriptions Lazienka = ["Jesteś w łazience."]
locationDescriptions NieczynnyPrysznic = ["Jesteś przy nieczynnym prysznicu."]


-- Consumable item effects: (Strength gain, Message, Special Death (e.g. steroid overdose))
-- Special Death is Maybe String. If Just "reason", player dies.
consumableEffects :: Item -> Maybe (Int, String, Maybe String)
consumableEffects Monster          = Just (3, "Twoja siła wzrosła o 3!", Nothing)
consumableEffects Dzik             = Just (5, "Twoja siła wzrosła o 5!", Nothing) -- Prolog implies +10 but text says "przypływ siły", I'll use a moderate value
consumableEffects Przedtreningowka = Just (10, "Twoja siła wzrosła o 10!", Nothing)
consumableEffects MalaStrzykawka   = Just (30, "Twoja siła wzrosła o 30!", Just "To był twój ostatni trening. Zmarłeś na skutek przedawkowania sterydów.") -- Random death in Prolog
consumableEffects DuzaStrzykawka   = Just (60, "Twoja siła wzrosła o 60!", Just "To był twój ostatni trening. Zmarłeś na skutek przedawkowania sterydów.") -- Random death in Prolog
consumableEffects _                = Nothing -- Not consumable or no direct effect