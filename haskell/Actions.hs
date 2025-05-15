{-# LANGUAGE FlexibleContexts #-}

module Actions where

import DataTypes
import GameData
import Utils
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, find)
import Data.Maybe (isJust, fromJust, fromMaybe, listToMaybe)
import System.Random (randomRIO)
import Data.Char (toLower)
import System.Exit (exitSuccess)


handleLook :: GameAction
handleLook = do
    gs <- get
    let loc = currentLocation gs
    let desc = locationDescriptions gs loc
    let itemsHere = M.filter (== loc) (worldItems gs)
    let npcsHere = S.filter (\n -> M.lookup n (npcLocations gs) == Just loc && S.member n (activeNPCs gs)) (S.fromList [minBound..maxBound])

    let itemMessages = if M.null itemsHere
                       then []
                       else ["Tutaj znajduje się:"] ++ map (\(item, _) -> " - " ++ itemDisplayName item) (M.toList itemsHere)
    let npcMessages = if S.null npcsHere
                      then []
                      else ["Widzisz tutaj:"] ++ map (\npc -> " - " ++ npcDisplayName npc) (S.toList npcsHere)

    return $ desc ++ itemMessages ++ npcMessages

handleGo :: LocationName -> GameAction
handleGo locNameStr = do
    gs <- get
    let currentLoc = currentLocation gs
    let pState = playerState gs

    case stringToLocation locNameStr of
        Nothing -> return ["Nie znam takiego miejsca."]
        Just targetLoc ->
            if not (canGo currentLoc targetLoc)
            then return ["Nie możesz tam pójść stąd."]
            else do
                case targetLoc of
                    SzatniaMeska | currentLoc == Recepcja -> do
                        if gymPassUsed gs || S.member Karnet (inventory pState)
                        then enterSzatniaMeska "Wchodzisz do szatni męskiej!" targetLoc True
                        else do
                            liftIO $ putStrLn "Nie masz karnetu! Może chcesz sprawdzić czy skomplementowanie recepcjonistki coś da? (tak/nie)"
                            answer <- liftIO getLine
                            if map toLower answer == "tak"
                                then handleKomplementujRecepcjonistke
                                else return ["Recepcjonistka patrzy na ciebie z dezaprobatą."]
                    SzatniaDamska -> do
                        liftIO $ putStrLn "Podglądacze nie są tolerowani! Zostałeś wyrzucony z siłowni! Koniec gry."
                        liftIO exitSuccess
                        return []

                    NieczynnyPrysznic | currentLoc == SzatniaMeska -> handleIdzDoPrysznica

                    loc | loc == StrefaCardio && gameStage gs == 4 -> do 
                        put gs { gameStage = 5, currentLocation = loc }
                        lookMessages <- handleLook
                        return $ ["Przechodzisz do strefy cardio. Zapatrujesz się na ćwiczące osoby.",
                                  "Nawet nie zauważasz kiedy mija czas na kolejną serię!"] ++ lookMessages

                    loc | loc `elem` [StrefaWolnychCiezarow, StrefaCardio, StrefaMaszyn] ->
                        if not (wearingSportswear pState)
                        then return ["Nie możesz iść na trening bez stroju sportowego! Przebierz się!"]
                        else moveToLocation loc ["Wszedłeś do: " ++ locationDisplayName loc]


                    _ -> moveToLocation targetLoc ["Poszedłeś do: " ++ locationDisplayName targetLoc]

enterSzatniaMeska :: String -> Location -> Bool -> GameAction
enterSzatniaMeska msg loc setPassUsed = do
    gs <- get
    put gs { currentLocation = loc, gymPassUsed = if setPassUsed then True else gymPassUsed gs }
    lookMessages <- handleLook
    return $ [msg] ++ lookMessages

moveToLocation :: Location -> [String] -> GameAction
moveToLocation loc initialMessages = do
    modify $ \gs -> gs { currentLocation = loc }
    lookMessages <- handleLook
    return $ initialMessages ++ lookMessages


handleIdzDoPrysznica :: GameAction
handleIdzDoPrysznica = do
    gs <- get
    if currentLocation gs /= SzatniaMeska
        then return ["Nie możesz tego tutaj zrobić!"]
        else if showerChecked gs
            then return ["Prysznic jest już pusty. Nie ma tam nic więcej."]
            else do
                let pState = playerState gs
                let newPState = pState { inventory = S.insert Dzik (inventory pState) }
                put gs { playerState = newPState, showerChecked = True, currentLocation = SzatniaMeska }
                lookMessages <- handleLook
                return $ ["Podchodzisz do nieczynnego prysznica...",
                          "Nagle znajdujesz tam darmowego Dzika (napój energetyczny)!",
                          itemDisplayName Dzik ++ " został dodany do Twojego ekwipunku.",
                          "Wypij zdobyty napój i poczuj przypływ siły!",
                          "Automatycznie wracasz do szatni."] ++ lookMessages



handleTake :: String -> GameAction
handleTake itemNameStr = do
    gs <- get
    let currentLoc = currentLocation gs
    let pState = playerState gs

    
    case stringToNPC itemNameStr of
        Just npc -> return ["Nie możesz podnieść " ++ npcDisplayName npc ++ "! To osoba!"]
        Nothing -> do 
            case stringToItem itemNameStr of
                Nothing -> return ["Nie widzę czegoś takiego tutaj."]
                Just item ->
                    if S.member item (inventory pState)
                    then return ["Masz już " ++ itemDisplayName item ++ "!"]
                    else if isBuyable item currentLoc
                        then return ["Nie możesz podnieść " ++ itemDisplayName item ++ "! To przedmiot do kupienia!"]
                        else case M.lookup item (worldItems gs) of
                            Just itemLoc | itemLoc == currentLoc -> do
                                let newPState = pState { inventory = S.insert item (inventory pState) }
                                let newWorldItems = M.delete item (worldItems gs)
                                put gs { playerState = newPState, worldItems = newWorldItems }
                                return ["Podniosłeś " ++ itemDisplayName item ++ "."]
                            _ -> return ["Nie widzę " ++ itemDisplayName item ++ " tutaj."]

isBuyable :: Item -> Location -> Bool
isBuyable item loc = case M.lookup item buyableAt of
    Just buyLoc -> buyLoc == loc
    Nothing -> False


handleDrop :: String -> GameAction
handleDrop itemNameStr = do
    gs <- get
    let currentLoc = currentLocation gs
    let pState = playerState gs

    case stringToItem itemNameStr of
        Nothing -> return ["Nie masz czegoś takiego w ekwipunku."]
        Just item ->
            if S.member item (inventory pState)
            then do
                let newPState = pState { inventory = S.delete item (inventory pState) }
                let newWorldItems = M.insert item currentLoc (worldItems gs)
                put gs { playerState = newPState, worldItems = newWorldItems }
                return ["Odłożyłeś " ++ itemDisplayName item ++ "."]
            else return ["Nie masz " ++ itemDisplayName item ++ " w ekwipunku!"]


handleInventory :: GameAction
handleInventory = do
    gs <- get
    let pState = playerState gs
    let items = S.toList $ inventory pState

    weightInvMessages <- handleShowWeightInventory 

    let itemMessages = if null items
                       then ["Twój ekwipunek jest pusty (poza ciężarami)."]
                       else "Twoje przedmioty:" : map ((" - " ++) . itemDisplayName) items
    return $ itemMessages ++ [""] ++ weightInvMessages

handleShowWeightInventory :: GameAction
handleShowWeightInventory = do
    gs <- get
    let pState = playerState gs
    let weights = weightInventory pState
    if M.null weights
    then return ["Ekwipunek ciężarów jest pusty."]
    else return $ ["Ciężary w twoim ekwipunku:"] ++ map showWeight (M.toList weights)
      where showWeight (w, count) = " - " ++ show count ++ "x " ++ show w ++ "kg"



handleConsume :: String -> GameAction
handleConsume itemNameStr = do
    gs <- get
    let pState = playerState gs
    case stringToItem itemNameStr of
        Nothing -> return ["Nie masz czegoś takiego w ekwipunku."]
        Just item ->
            if not (S.member item (inventory pState))
            then return ["Nie masz " ++ itemDisplayName item ++ " w ekwipunku!"]
            else case consumableEffects item of
                Nothing -> return ["Nie możesz tego spożyć!"]
                Just (strGain, effectMsg, mDeathReason) -> do
                    let newStrength = strength pState + strGain
                    let newPState = pState { inventory = S.delete item (inventory pState)
                                           , strength = newStrength }
                    put gs { playerState = newPState }

                    let baseMessages = ["Spożywasz " ++ itemDisplayName item ++ ".", effectMsg]

                    
                    if item == MalaStrzykawka || item == DuzaStrzykawka then do
                        randomNumber <- liftIO $ randomRIO (1 :: Int, if item == MalaStrzykawka then 4 else 2) 
                        if randomNumber == 1 then
                            case mDeathReason of
                                Just deathReason -> do
                                    liftIO $ putStrLn deathReason
                                    liftIO exitSuccess 
                                    return []
                                Nothing -> return baseMessages 
                        else return baseMessages
                    else return baseMessages



handleWear :: String -> GameAction
handleWear itemNameStr = do
    gs <- get
    let pState = playerState gs
    let currentLoc = currentLocation gs

    case stringToItem itemNameStr of
        Just StrojSportowy ->
            if currentLoc /= SzatniaMeska
            then return ["Możesz się przebrać tylko w szatni męskiej."]
            else if not (S.member StrojSportowy (inventory pState))
                then return ["Nie masz stroju sportowego w ekwipunku!"]
                else if wearingSportswear pState
                    then return ["Już masz na sobie strój sportowy."]
                    else do
                        
                        
                        
                        
                        let newPState = pState { wearingSportswear = True
                                               
                                               }
                        put gs { playerState = newPState }
                        return ["Przebrałeś się w strój sportowy!"]
        Just _ -> return ["Nie możesz tego ubrać."]
        Nothing -> return ["Nie wiem co to jest '" ++ itemNameStr ++ "'."]



handleCheckMoney :: GameAction
handleCheckMoney = do
    gs <- get
    return ["Masz " ++ show (money (playerState gs)) ++ " zł na koncie."]


handleKomplementujRecepcjonistke :: GameAction
handleKomplementujRecepcjonistke = do
    gs <- get
    if currentLocation gs /= Recepcja
    then return ["Nie możesz tego tutaj zrobić!"]
    else do
        put gs { currentLocation = SzatniaMeska, gymPassUsed = True }
        lookMessages <- handleLook
        return $ ["Mówisz recepcjonistce, że ma piękny uśmiech.",
                  "Recepcjonistka przewraca oczami ale mimo to jednak pozwala ci wejść."] ++ lookMessages


handleBuy :: String -> GameAction
handleBuy itemNameStr = do
    gs <- get
    let pState = playerState gs
    let currentLoc = currentLocation gs

    case stringToItem itemNameStr of
        Nothing -> return ["Nie ma takiego przedmiotu do kupienia tutaj."]
        Just item ->
            case M.lookup item buyableAt of
                Nothing -> return ["Ten przedmiot nie jest na sprzedaż!"]
                Just validLoc | validLoc /= currentLoc -> return ["Nie możesz kupić " ++ itemDisplayName item ++ " tutaj."]
                Just _ -> 
                    case M.lookup item itemPrices of
                        Nothing -> return ["Coś poszło nie tak, ten przedmiot nie ma ceny."] 
                        Just price ->
                            if money pState >= price
                            then do
                                let newMoney = money pState - price
                                let newPState = pState { money = newMoney
                                                       , inventory = S.insert item (inventory pState) }
                                put gs { playerState = newPState }
                                let messages = [ "Kupiono " ++ itemDisplayName item ++ "."
                                               , "Pozostało Ci " ++ show newMoney ++ " zł."
                                               ]
                                let warning = if item == MalaStrzykawka || item == DuzaStrzykawka
                                              then ["Zastanów się dwa razy czy na pewno chcesz jej użyć!"]
                                              else []
                                return $ messages ++ warning
                            else return ["Nie masz wystarczająco pieniędzy!"]





addWeightsToPlayer :: [Weight] -> GameAction
addWeightsToPlayer weights = do
    modify $ \gs ->
        let p = playerState gs
            newInv = foldr (\w acc -> M.insertWith (+) w 1 acc) (weightInventory p) weights
        in gs { playerState = p { weightInventory = newInv } }
    return ["Dodano ciężary do ekwipunku: " ++ intercalate ", " (map (\w -> show w ++ "kg") weights) ++ "."]



removeWeightsFromPlayer :: [Weight] -> StateT GameState IO Bool
removeWeightsFromPlayer weightsToRemove = do
    gs <- get
    let p = playerState gs
    case removePlayerWeights weightsToRemove p of
        Just newPlayer -> do
            put gs { playerState = newPlayer }
            return True
        Nothing -> return False



handleTakeBench :: GameAction
handleTakeBench = do
    gs <- get
    let benchSt = benchState gs
    if currentLocation gs /= StrefaWolnychCiezarow
    then return ["Nie jesteś w strefie wolnych ciężarów!"]
    else if benchOccupied benchSt && gameStage gs `elem` [1,3,5]
        then return ["Wykonujesz to ćwiczenie teraz!"]
        else if gameStage gs == 0
            then do
                let newBenchSt = benchSt { benchOccupied = True }
                put gs { gameStage = 1, benchState = newBenchSt }
                msgs <- startStage 1
                return $ ["Zająłeś ławkę."] ++ msgs
            else if gameStage gs `elem` [3,5]
            then do
                let newBenchSt = benchSt { benchOccupied = True }
                put gs { benchState = newBenchSt }
                return ["Wróciłeś do ławki, gotowy na kolejną serię. Nałóż ciężary."]
            else return ["Nie możesz teraz zająć ławki, spróbuj wykonać inne zadanie."]

handleAddWeightToBench :: (BenchState -> [Weight] -> BenchState) -> Weight -> String -> GameAction
handleAddWeightToBench sideSetter weight sideName = do
    gs <- get
    let pState = playerState gs
    let benchSt = benchState gs

    if currentLocation gs /= StrefaWolnychCiezarow
    then return ["Musisz być w strefie wolnych ciężarów."]
    else if not (benchOccupied benchSt) || not (gameStage gs `elem` [1,3,5])
        then return ["Musisz najpierw zająć ławkę i być w trakcie przygotowania do serii."]
        else do
            canRemove <- removeWeightsFromPlayer [weight]
            if not canRemove
            then return ["Nie masz ciężaru " ++ show weight ++ "kg w ekwipunku."]
            else do
                gs <- get
                let benchSt = benchState gs
                let newBenchSt = sideSetter benchSt [weight]
                put gs { benchState = newBenchSt }
                checkMsgs <- handleCheckBenchInternal
                return $ ["Dodałeś ciężar " ++ show weight ++ "kg na " ++ sideName ++ " stronę sztangi."] ++ checkMsgs

handleLeftAddWeightBench :: Weight -> GameAction
handleLeftAddWeightBench w = handleAddWeightToBench (\bs weights -> bs { benchLeft = benchLeft bs ++ weights }) w "lewą"

handleRightAddWeightBench :: Weight -> GameAction
handleRightAddWeightBench w = handleAddWeightToBench (\bs weights -> bs { benchRight = benchRight bs ++ weights }) w "prawą"

handleRemoveAllWeightsBench :: GameAction
handleRemoveAllWeightsBench = do
    gs <- get
    let benchSt = benchState gs
    if currentLocation gs /= StrefaWolnychCiezarow
    then return ["Musisz być w strefie wolnych ciężarów."]
    else if not (benchOccupied benchSt) || not (gameStage gs `elem` [1,3,5])
        then return ["Musisz być przy ławce i w trakcie przygotowania do serii."]
        else do
            let weightsToAddBack = benchLeft benchSt ++ benchRight benchSt
            _ <- addWeightsToPlayer weightsToAddBack
            let newBenchSt = benchSt { benchLeft = [], benchRight = [] }
            put gs { benchState = newBenchSt }
            return ["Zdjąłeś wszystkie ciężary ze sztangi i wróciły do Twojego ekwipunku."]

handleCheckBench :: GameAction
handleCheckBench = do
    gs <- get
    let benchSt = benchState gs
    if currentLocation gs /= StrefaWolnychCiezarow
    then return ["Musisz być w strefie wolnych ciężarów."]
    else if not (benchOccupied benchSt) || not (gameStage gs `elem` [1,3,5])
        then return ["Musisz być przy ławce i w trakcie przygotowania do serii."]
        else handleCheckBenchInternal

handleCheckBenchInternal :: GameAction
handleCheckBenchInternal = do
    gs <- get
    let benchSt = benchState gs
    let bl = benchLeft benchSt
    let br = benchRight benchSt
    let wbl = sum bl
    let wbr = sum br
    let totalWeightOnBar = wbl + wbr + 20

    let messages = ["Obciążenie na sztandze (z gryfem 20kg) wynosi: " ++ show totalWeightOnBar ++ " kg."]
    if wbl == wbr
    then return $ messages ++ ["Obciążenie na sztandze jest równo rozłożone!"]
    else do
        let diff = abs (wbl - wbr)
        if diff > 70
        then do
            liftIO $ putStrLn $ intercalate "\n" (messages ++
                             [ "Obciążenie na sztandze jest nierówno rozłożone.",
                               "Różnica w obciążeniu wynosi: " ++ show diff ++ " kg.",
                               "Ciężary na lewej stronie: " ++ show bl,
                               "Ciężary na prawej stronie: " ++ show br,
                               "Sztanga się przewaliła! Wszyscy się teraz z ciebie śmieją. Ze wstydu szybko uciekłeś z siłowni."
                             ])
            handleFinish False
            return []
        else return $ messages ++
                     [ "Obciążenie na sztandze jest nierówno rozłożone, ale w granicach bezpieczeństwa.",
                       "Różnica w obciążeniu wynosi: " ++ show diff ++ " kg.",
                       "Ciężary na lewej stronie: " ++ show bl,
                       "Ciężary na prawej stronie: " ++ show br
                     ]

handleDoBenchPress :: GameAction
handleDoBenchPress = do
    gs <- get
    let pState = playerState gs
    let benchSt = benchState gs
    let currentStg = gameStage gs

    if currentLocation gs /= StrefaWolnychCiezarow
    then return ["Musisz być w strefie wolnych ciężarów."]
    else if not (benchOccupied benchSt) || not (currentStg `elem` [1,3,5])
        then return ["Musisz być przy ławce i gotowy do wyciskania (etap 1, 3 lub 5)."]
        else do
            let bl = benchLeft benchSt
            let br = benchRight benchSt
            let wbl = sum bl
            let wbr = sum br

            if wbl /= wbr then
                return [ "Obciążenie na sztandze jest nierówno rozłożone!"
                       , "Nie możesz podnieść sztangi!"
                       ]
            else do
                let totalWeightToLift = wbl + wbr + 20
                let messages = ["Podnosisz sztangę o obciążeniu " ++ show totalWeightToLift ++ " kg."]

                if totalWeightToLift <= strength pState then do
                    let newLiftedKlatka = (fromMaybe 0 $ M.lookup KlatkaPiersiowa (liftedStats gs)) + totalWeightToLift
                    let newLiftedStats = M.insert KlatkaPiersiowa newLiftedKlatka (liftedStats gs)
                    let nextStage = currentStg + 1
                    let newBenchSt = benchSt { benchOccupied = False }
                    put gs { gameStage = nextStage, liftedStats = newLiftedStats, benchState = newBenchSt }
                    stageMsgs <- startStage nextStage
                    return $ messages ++ ["Udało się! Podniosłeś " ++ show totalWeightToLift ++ " kg na klatę!"] ++ stageMsgs
                else do
                    liftIO $ putStrLn $ intercalate "\n" (messages ++ ["Podjąłeś próbę podniesienia zbyt dużego ciężaru i odniosłeś kontuzję. Koniec gry."])
                    handleFinish False
                    return []


handleFinish :: Bool -> StateT GameState IO ()
handleFinish success = do
    liftIO $ putStrLn $ if success
        then "Gratulacje! Wygrałeś!"
        else "Przegrałeś!"
    liftIO $ putStrLn "Dziękujemy za grę!"
    liftIO exitSuccess



startStage :: Int -> GameAction
startStage 1 = return
    [ "Rozpoczynasz trening na klatę! Rozglądasz się obok sztangi, ale nie ma obok niej żadnych ciężarów."
    , "Pora zebrać ciężary!"
    , "Rozejrzyj się po siłowni i przynieś ciężary, a następnie je nałóż!"
    , "Jeżeli nie znajdziesz nieużywanego sprzętu, możesz spróbować zapytać o niego innych ćwiczących."
    , "Pamiętaj, że z dwóch stron trzeba mieć tyle samo na sztandze!"
    , "Im więcej podniesiesz, tym lepszy wynik zdobędziesz!"
    , "Powodzenia!"
    ]
startStage 2 = do
    gs <- get
    
    let newNpcLocs = M.insert Swiezak StrefaWolnychCiezarow (npcLocations gs)
    let newActiveNpcs = S.insert Swiezak (activeNPCs gs)
    put gs { npcLocations = newNpcLocs, activeNPCs = newActiveNpcs }

    let pState = playerState gs
    let introMsgs =
            [ "Gratulacje udało ci się wykonać pierwszą serię!"
            , "Kiedy odpoczywasz po pierwszej serii, podchodzi do ciebie jakiś przeciętnie zbudowany chłopak z pytaniem:"
            , "Swiezak: Hej, ile zostało ci serii?"
            , "Ty: Jeszcze dwie."
            , "Swiezak: Mogę w takim razie się dołączyć?"
            , "Ty: Jasne."
            , "Swiezak: Tak w ogóle nie widziałeś przypadkiem gdzieś na siłowni czerwonego bidonu, musiałem go wczoraj zostawić. Znalazcy z pewnością się odwdzięczę."
            ]
    if S.member CzerwonyBidon (inventory pState) then do
        
        let newPState = pState { inventory = S.delete CzerwonyBidon (inventory pState) }
        put gs { playerState = newPState, gameStage = 3, npcLocations = newNpcLocs, activeNPCs = newActiveNpcs }
        stage3Msgs <- startStage 3 
        return $ introMsgs ++
                 [ "Przypominasz sobie, że podniosłeś taki bidon w łazience."
                 , "Podajesz mu bidon."
                 , "Swiezak: O, dzięki! Nie wiem co bym bez ciebie zrobił!"
                 , "Ty: Nie ma sprawy!"
                 ] ++ stage3Msgs
    else
        return $ introMsgs ++
                 [ "Ty: Niestety, nie widziałem go. Ale i tak teraz odpoczywam pójdę go poszukać (Powiedziałeś już myśląc o tym jaka nagroda cię czeka)."
                 , "Swiezak: Powodzenia!"
                 ]

startStage 3 = return 
    [ "Swiezak: W nagrodę trzymaj tę przedtreningówkę." 
    , "Ty: Dzięki!"
    , ""
    , "Spojrzałeś na datę ważności przedtreningówki i okazało się, że jest przeterminowana."
    , "Nie chcąc się narzucać, postanowiłeś nie mówić o tym swojemu nowemu znajomemu."
    , "Jednak wiesz, że nie możesz jej użyć (gra nie pozwoli jej skonsumować)."
    , ""
    , "Po odnalezieniu/oddaniu bidonu, wracasz do strefy wolnych ciężarów i szykujesz się na drugą serię."
    ]

startStage 4 = return
    [ "Gratulacje udało ci się wykonać drugą serię!"
    , "Podczas odpoczynku możesz przejść się po siłowni i porozmawiać z innymi ćwiczącymi."
    ]

startStage 5 = return 
    [ "Przygotowujesz się do trzeciej, ostatniej serii na klatę."
    , "Nałóż ciężary i daj z siebie wszystko!"
    ]

startStage 6 = do 
    liftIO $ putStrLn "Gratulacje udało ci się wykonać trening!"
    handleFinish True
    return [] 

startStage _ = return ["DEBUG: Niezdefiniowany etap gry."]



handleInstructions :: GameAction
handleInstructions = do
    gs <- get
    let loc = currentLocation gs
    let base = ["Możesz wykonać następujące akcje (polecenia):",
                "  go [miejsce]         - idź do miejsca (np. 'go parking')",
                "  look                 - rozejrzyj się",
                "  take [przedmiot]     - podnieś przedmiot (np. 'take woda')",
                "  drop [przedmiot]     - upuść przedmiot",
                "  inventory / inv      - pokaż ekwipunek",
                "  consume [przedmiot]  - zjedz/wypij przedmiot (np. 'consume monster')",
                "  talk [osoba]         - porozmawiaj z osobą (np. 'talk recepcjonistka')",
                "  buy [przedmiot]      - kup przedmiot",
                "  wear [przedmiot]     - ubierz przedmiot (np. 'wear stroj_sportowy')",
                "  money                - sprawdź ile masz pieniędzy",
                "  weights              - pokaż ekwipunek ciężarów",
                "  help                 - ta lista komend",
                "  quit                 - zakończ grę"
                ]
    let specific = case loc of
            Dom -> [ "Dostępne miejsca: parking" ]
            Parking -> [ "Dostępne miejsca: dom, recepcja"
                       , "Możesz porozmawiać z 'podejrzany typ'."]
            Recepcja -> [ "Dostępne miejsca: parking, szatnia meska, szatnia damska"
                        , "Możesz porozmawiać z 'recepcjonistka'."
                        , "Możesz 'komplementuj' (komplementuj recepcjonistke)"]
            SzatniaMeska -> [ "Dostępne miejsca: recepcja, nieczynny prysznic, strefa wolnych ciezarow, strefa cardio, strefa maszyn, lazienka"
                            , "Możesz 'wear stroj sportowy'."]
            StrefaWolnychCiezarow ->
                            [ "Dostępne miejsca: szatnia meska, szatnia damska"
                            , "Komendy związane z ławką (gdy ją zajmujesz 'take_bench'):"
                            , "  left_add [waga]    - nałóż ciężar na lewą stronę sztangi (np. 'left_add 10')"
                            , "  right_add [waga]   - nałóż ciężar na prawą stronę sztangi (np. 'right_add 10')"
                            , "  remove_weights     - zdejmij wszystkie ciężary ze sztangi"
                            , "  check_bench        - sprawdź obciążenie na sztandze"
                            , "  do_bench_press     - wykonaj wyciskanie sztangi"
                            ]
            _ -> ["Specyficzne akcje dla tej lokacji zostaną dodane wkrótce..."] 
    return $ base ++ [""] ++ specific