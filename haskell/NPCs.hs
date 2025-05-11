module NPCs where

import DataTypes
import GameData
import Actions
import Utils
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)


handleTalk :: String -> GameAction
handleTalk npcNameStr = do
    gs <- get
    let currentLoc = currentLocation gs
    let pState = playerState gs

    case stringToNPC npcNameStr of
        Nothing -> return ["Nie ma tu kogoś takiego jak '" ++ npcNameStr ++ "'."]
        Just npc ->
            if not (S.member npc (activeNPCs gs) && M.lookup npc (npcLocations gs) == Just currentLoc)
            then return ["Nie widzisz tutaj osoby: " ++ npcDisplayName npc ++ "."]
            else
                case npc of
                    Recepcjonistka -> interactRecepcjonistka
                    PodejrzanyTyp  -> interactPodejrzanyTyp
                    ChudySzczur    -> interactChudySzczur
                    Brunetka       -> interactBrunetka
                    CzlowiekSzczuply -> interactCzlowiekSzczuply
                    SzczurBojowy   -> interactSzczurBojowy
                    DuzyChlop      -> interactDuzyChlop
                    WielkiChlop    -> interactWielkiChlop
                    Swiezak        -> interactSwiezak

interactRecepcjonistka :: GameAction
interactRecepcjonistka = return
    [ "Recepcjonistka:"
    , "Witaj na siłowni! Czy czegoś ci potrzeba?"
    , "Możesz tutaj kupić: "
    , " - monster (buy monster) za 10zł"
    , " - przedtreningowka (buy przedtreningowka) za 20zł"
    , " - karnet (buy karnet) za 40zł."
    ]

interactPodejrzanyTyp :: GameAction
interactPodejrzanyTyp = return
    [ "Podejrzany typ:"
    , "Witaj, czyżbyś szedł na trening? Może chcesz coś mocniejszego, co na pewno poprawi twoje wyniki? Moje produkty są całkowicie bezpieczne!"
    , "Możesz tutaj kupić: "
    , " - mala strzykawka (buy mala_strzykawka) za 30zł"
    , " - duza strzykawka (buy duza_strzykawka) za 50zł."
    ]

interactChudySzczur :: GameAction
interactChudySzczur = do
    gs <- get
    if chudySzczurTalked gs
      then return
        [ "Chudy szczur: Już ci dałem moje talerze! Daj mi spokój, dobrze?"
        , "Ty: Spoko, nie przeszkadzam więcej."
        ]
      else do
        msgs <- addWeightsToPlayer [5, 5]
        gs_new <- get
        put gs_new { chudySzczurTalked = True }
        return
            ([ "Ty: Ej, mały, potrzebuję tych talerzy 5kg, mogę je zabrać?"
             , "Chudy szczur: (piszczy nerwowo) Tylko... nie bij mnie! Bierz, co chcesz, i znikaj!"
             , "Ty: Spoko, luz. Tylko mi je podaj, i będę się zwijał."
             , "Chudy szczur: (podaje talerze) Masz, i już mnie nie zaczepiaj!"
             ] ++ msgs ++ ["Dodałeś do swojego ekwipunku ciężary: 5kg i 5kg."]
            )


interactBrunetka :: GameAction
interactBrunetka = return
    [ "Ty: Cześć, przepraszam cię najmocniej, że przeszkadzam, ale czy będziesz jeszcze używać tych 10kg?"
    , "Brunetka: Zostaw mnie, mam chłopaka!"
    , "Ty: Źle zrozumiałaś, ja tylko chciałem..."
    , "Brunetka: Mam wezwać ochronę? Spadaj!"
    ]

interactCzlowiekSzczuply :: GameAction
interactCzlowiekSzczuply = do
    gs <- get
    if czlowiekSzczuplyTalked gs
      then return
        [ "Człowiek szczupły: Już ci pożyczyłem talerze! Oddaj je, jak skończysz."
        , "Ty: Jasne, dzięki jeszcze raz!"
        ]
      else do
        msgs <- addWeightsToPlayer [15, 15]
        gs_new <- get
        put gs_new { czlowiekSzczuplyTalked = True }
        return
            ([ "Ty: Stary, te talerze 15kg... mogę je pożyczyć? Na chwilę?"
            , "Człowiek szczupły: (wzrusza ramionami) No dobra, ale szybko oddaj. Ja tu jeszcze muszę poćwiczyć."
            , "Ty: Jasne, jasne, tylko je wezmę. Dzięki!"
            , "Człowiek szczupły: Tylko ich nie zgub, bo będziesz miał ze mną do czynienia!"
            ] ++ msgs ++ ["Dodałeś do swojego ekwipunku ciężary: 15kg i 15kg."]
            )

interactSzczurBojowy :: GameAction
interactSzczurBojowy = do
    gs <- get
    if szczurBojowyTalked gs
      then return
        [ "Szczur bojowy: Już oddałem ci swoje talerze. Teraz muszę odpocząć!"
        , "Ty: Dzięki jeszcze raz!"
        ]
      else do
        msgs <- addWeightsToPlayer [20, 20]
        gs_new <- get
        put gs_new { szczurBojowyTalked = True }
        return
            ([ "Ty: Ej, byczku, te 20 kilo... mogę je zabrać?"
            , "Szczur bojowy: Jasne, stary, bierz. I tak mi się już nie przydadzą."
            , "Ty: Dzięki, jesteś wielki!"
            , "Szczur bojowy: Tylko uważaj, żebyś sobie krzywdy nie zrobił. To ciężkie żelastwo."
            ] ++ msgs ++ ["Dodałeś do swojego ekwipunku ciężary: 20kg i 20kg."]
            )




interactDuzyChlop :: GameAction
interactDuzyChlop = do
    gs <- get
    if duzyChlopTalked gs
      then return
        [ "Duży chłop: Już ci dałem talerze 25kg. Nie mam więcej!"
        , "Ty: Spoko, dzięki!"
        ]
      else do
        msgs <- addWeightsToPlayer [25, 25]
        gs_new <- get
        put gs_new { duzyChlopTalked = True }
        return
            ([ "Ty: Mogę zabrać talerze 25 kg?"
            , "Duży chłop: Spoko, nie ma sprawy, już ich nie używam!"
            ] ++ msgs ++ ["Dodałeś do swojego ekwipunku ciężary: 25kg i 25kg."]
            )

interactWielkiChlop :: GameAction
interactWielkiChlop = return
    [ "Ty: Mogę zabrać talerze..."
    , "Wielki chłop: Spadaj szczurze!"
    ]

interactSwiezak :: GameAction
interactSwiezak = do
    gs <- get
    let pState = playerState gs
    let currentStage = gameStage gs

    if currentStage == 2 then
        if S.member CzerwonyBidon (inventory pState) then do
            let newPState = pState { inventory = S.delete CzerwonyBidon (inventory pState) }
            put gs { playerState = newPState, gameStage = 3 }

            stage3Msgs <- startStage 3
            return $
                [ "Ty: Cześć, stary, znalazłem twój czerwony bidon w łazience."
                , "Swiezak: Dzięki, stary! Nie wiem co bym bez ciebie zrobił!"
                , "Ty: Nie ma sprawy!"
                ] ++ stage3Msgs
        else
            return
                [ "Swiezak: I jak znalazłeś mój bidon?"
                , "Ty: Niestety, jeszcze go nie znalazłem."
                , "Swiezak: Powodzenia!"
                ]
    else if currentStage >= 3 then 
        return ["Swiezak: Dzięki jeszcze raz za pomoc z bidonem! Jak trening idzie?"]
    else 
        return ["Swiezak patrzy na ciebie pytająco."]