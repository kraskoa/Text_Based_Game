Object subclass: #GymAdventureGame
    instanceVariableNames:
        'currentLocation strength money liftedWeights gameStage score weightInventory benchLeftWeights benchRightWeights heldItems wearing npcInteractionCounts worldItems npcLocations prysznicSprawdzony szatniaWejscie gameOver gameWon randomGenerator paths itemPrices buyableAtLocation allNpcs'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'Games-TextAdventure'

!GymAdventureGame methodsFor: 'initialization' stamp: 'AI 6/2/2025 10:30'!
initialize
    randomGenerator := Random new.
    gameOver := false.
    gameWon := false.

    self initializeStaticData.
    self initializeGameState.
    self initializeWorldItemsAndNPCs.
    self initializeInteractionCounts.
!

initializeStaticData
    allNpcs := #(#recepcjonistka #podejrzany_typ #szczur_bojowy #brunetka #duzy_chlop #wielki_chlop #czlowiek_szczuply #chudy_szczur #swiezak) asSet.

    paths := Dictionary new.
    paths at: #dom put: (#(#parking) asSet).
    paths at: #parking put: (#(#dom #recepcja) asSet).
    paths at: #recepcja put: (#(#parking #szatnia_meska #szatnia_damska) asSet).
    paths at: #szatnia_meska put: (#(#recepcja #nieczynny_prysznic #strefa_wolnych_ciezarow #strefa_cardio #strefa_maszyn #lazienka) asSet).
    paths at: #nieczynny_prysznic put: (#(#szatnia_meska) asSet). "This leads to a special action, not a direct move"
    paths at: #szatnia_damska put: (#(#recepcja #strefa_maszyn #strefa_cardio #strefa_wolnych_ciezarow) asSet).
    paths at: #strefa_wolnych_ciezarow put: (#(#szatnia_meska #szatnia_damska) asSet).
    paths at: #strefa_cardio put: (#(#szatnia_meska #szatnia_damska #strefa_maszyn) asSet).
    paths at: #strefa_maszyn put: (#(#szatnia_meska #szatnia_damska #strefa_cardio) asSet).
    paths at: #lazienka put: (#(#szatnia_meska) asSet).

    itemPrices := Dictionary new.
    itemPrices at: #monster put: 10.
    itemPrices at: #przedtreningowka put: 20.
    itemPrices at: #karnet put: 40.
    itemPrices at: #mala_strzykawka put: 30.
    itemPrices at: #duza_strzykawka put: 50.

    buyableAtLocation := Dictionary new.
    buyableAtLocation at: #recepcja put: (#(#monster #przedtreningowka #karnet) asSet).
    buyableAtLocation at: #parking put: (#(#mala_strzykawka #duza_strzykawka) asSet).
!

initializeGameState
    currentLocation := #dom.
    strength := 50 + ((randomGenerator next * 51) floor). "50..100"
    money := (randomGenerator next * 151) floor. "0..150"
    liftedWeights := Dictionary new.
    liftedWeights at: #klatka_piersiowa put: 0.
    liftedWeights at: #barki put: 0.
    liftedWeights at: #biceps put: 0.
    gameStage := 0.
    score := 0. "Used to indicate win/loss state at the end"
    weightInventory := Dictionary new.
    benchLeftWeights := OrderedCollection new.
    benchRightWeights := OrderedCollection new.
    heldItems := Set new.
    wearing := nil.
    prysznicSprawdzony := false.
    szatniaWejscie := false.
!

initializeWorldItemsAndNPCs
    worldItems := Dictionary new.
    npcLocations := Dictionary new. "Initialize before use!"
    allNpcs do: [:npc | npcLocations at: npc put: nil]. "Initialize all NPCs as not being anywhere specific yet unless placed"
    #(#dom #parking #recepcja #szatnia_meska #nieczynny_prysznic #szatnia_damska #strefa_wolnych_ciezarow #strefa_cardio #strefa_maszyn #lazienka) do: [ :loc |
        worldItems at: loc put: Set new.
    ].

    (worldItems at: #dom) addAll: #(#stroj_sportowy #woda #karnet).
    (worldItems at: #lazienka) add: #czerwony_bidon.
    "Lawka is part of the location, not a takeable item in the Prolog sense of at/2"
    " (worldItems at: #strefa_wolnych_ciezarow) add: #lawka. "

    npcLocations at: #recepcjonistka put: #recepcja.
    npcLocations at: #podejrzany_typ put: #parking.
    npcLocations at: #chudy_szczur put: #strefa_wolnych_ciezarow.
    npcLocations at: #brunetka put: #strefa_cardio.
    npcLocations at: #wielki_chlop put: #strefa_wolnych_ciezarow.
    npcLocations at: #czlowiek_szczuply put: #strefa_maszyn.
    npcLocations at: #duzy_chlop put: #strefa_maszyn.
    npcLocations at: #szczur_bojowy put: #strefa_maszyn.
    "Swiezak is added dynamically in stage 2"
!

initializeInteractionCounts
    npcInteractionCounts := Dictionary new.
    allNpcs do: [:npc | npcInteractionCounts at: npc put: 0].
!

startGame
    Transcript show: 'Twoim celem jest odbycie treningu na siłowni.'; cr.
    Transcript show: 'Zbierz potrzebny ekwipunek z domu, wejdź na siłownię i wykonaj ćwiczenia!'; cr.
    self look.
    self gameLoop.
!

gameLoop
    | commandParts action argument1 |
    [gameOver not] whileTrue: [
        Transcript show: '> '.
        commandParts := (FileStream stdin nextLine substrings).
        commandParts isEmpty ifFalse: [
            action := commandParts first asSymbol.
            argument1 := commandParts size > 1
                ifTrue: [
                    | argString |
                    argString := commandParts second.
                    "Try to convert to number if it's a weight, else symbol"
                    (action = #left_add_weight_bench or: [action = #right_add_weight_bench])
                        ifTrue: [argString asNumber]
                        ifFalse: [argString asSymbol]
                ]
                ifFalse: [nil].

            (self canUnderstand: action with: argument1)
                ifTrue: [self perform: action withEnoughArguments: (Array with: argument1)]
                ifFalse: [Transcript show: 'Nie rozumiem polecenia: '; show: commandParts first; cr].

            (gameOver and: [gameWon not]) ifTrue: [self finishGame. ^self].
            (gameOver and: [gameWon]) ifTrue: [self finishGame. ^self].
        ].
    ].
!

canUnderstand: selectorSymbol with: arg
    arg isNil
        ifTrue: [^(self respondsTo: selectorSymbol) notNil]
        ifFalse: [^(self respondsTo: (selectorSymbol, ':') asSymbol) notNil].
!

perform: selectorSymbol withEnoughArguments: args
    | actualSelector |
    args first isNil
        ifTrue: [
            actualSelector := selectorSymbol.
            (self respondsTo: actualSelector)
                ifTrue: [self perform: actualSelector]
                ifFalse: [Transcript show: 'Błąd wewnętrzny - zła liczba argumentów dla '; show: actualSelector; cr].
        ]
        ifFalse: [
            actualSelector := (selectorSymbol, ':') asSymbol.
             (self respondsTo: actualSelector)
                ifTrue: [self perform: actualSelector with: args first]
                ifFalse: [Transcript show: 'Błąd wewnętrzny - zła liczba argumentów dla '; show: actualSelector; cr].
        ].
!

finishGame
    gameOver := true.
    gameWon ifTrue: [
        Transcript show: 'Gratulacje! Wygrałeś!'; cr.
        Transcript show: 'Twój wynik to: '; show: (liftedWeights at: #klatka_piersiowa) printString; cr.
    ] ifFalse: [
        Transcript show: 'Przegrałeś!'; cr.
    ].
    Transcript cr; show: 'Dziękujemy za grę!'; cr.
!

die
    | response |
    Transcript show: 'Czy chcesz oszukać koniec gry? (tak / nie )'; cr.
    response := FileStream stdin nextLine asLowercase.
    (response = 'tak') ifTrue: [
        Transcript show: 'Miałeś niesamowite szczęście.'; cr.
        ^ self "Continue game"
    ].
    gameOver := true.
    gameWon := false.
!

dieSteroid
    Transcript show: 'To był twój ostatni trening. Zmarłeś na skutek przedawkowania sterydów.'; cr.
    self die.
    gameOver ifFalse: [ "If player chose not to end game via 'tak' in die"
         Transcript show: 'Obok przechodził student WUM idący na siłkę i Cię wskrzesił'; cr.
    ].
! !

!GymAdventureGame methodsFor: 'player actions' stamp: 'AI 6/2/2025 10:30'!
look
    Transcript cr.
    self describeLocation: currentLocation.
    Transcript cr.
    self noticeObjectsAt: currentLocation.
    self noticeNPCsAt: currentLocation.
    Transcript cr.
!

describeLocation: aPlace
    aPlace = #dom ifTrue: [Transcript show: 'Jesteś w domu. Musisz zebrać ekwipunek na siłownię!'; cr. ^self].
    aPlace = #parking ifTrue: [Transcript show: 'Jesteś na parkingu przed siłownią. Możesz iść do siłowni!'; cr. ^self].
    aPlace = #recepcja ifTrue: [Transcript show: 'Jesteś w recepcji. Możesz kupić suplementy i karnet!'; cr. ^self].
    aPlace = #szatnia_meska ifTrue: [
        Transcript show: 'Jesteś w męskiej szatni. Możesz udać się do stref treningowych!'; cr.
        Transcript show: 'Widzisz, że jeden z pryszniców jest nieczynny, może chcesz sprawdzić co jest w środku? (go nieczynny_prysznic)'; cr.
        ^self
    ].
    aPlace = #szatnia_damska ifTrue: [Transcript show: 'Jesteś w damskiej szatni. Możesz udać się do stref treningowych!'; cr. ^self].
    aPlace = #strefa_maszyn ifTrue: [Transcript show: 'Jesteś w strefie maszyn.'; cr. ^self].
    aPlace = #strefa_wolnych_ciezarow ifTrue: [Transcript show: 'Jesteś w strefie wolnych ciężarów.'; cr. ^self].
    aPlace = #strefa_cardio ifTrue: [Transcript show: 'Jesteś w strefie cardio.'; cr. ^self].
    aPlace = #lazienka ifTrue: [Transcript show: 'Jesteś w łazience.'; cr. ^self].
    Transcript show: 'Jesteś w nieznanym miejscu: '; show: aPlace; cr.
!

noticeObjectsAt: aPlace
    | itemsHere |
    itemsHere := worldItems at: aPlace ifAbsent: [Set new].
    itemsHere isEmpty ifFalse: [
        itemsHere do: [:item | Transcript show: 'Tutaj znajduje się '; show: item; show: '.'; cr].
    ].
!

noticeNPCsAt: aPlace
    npcLocations keysAndValuesDo: [:npc :loc |
        loc = aPlace ifTrue: [
            Transcript show: 'Widzisz tutaj '; show: npc; show: '.'; cr.
        ].
    ].
!

go: destinationSymbol
    | newLocation pathsFromHere |
    newLocation := destinationSymbol.
    pathsFromHere := paths at: currentLocation ifAbsent: [Set new].

    (pathsFromHere includes: newLocation) ifFalse: [
        "Special case for nieczynny_prysznic which is an action not a simple path"
        (currentLocation = #szatnia_meska and: [newLocation = #nieczynny_prysznic]) ifTrue: [
            self idzDoPrysznica.
            ^self
        ].
        Transcript show: 'Nie możesz tam pójść!'; cr.
        ^self
    ].

    newLocation = #szatnia_meska ifTrue: [
        szatniaWejscie ifTrue: [
            Transcript show: 'Wchodzisz do szatni męskiej!'; cr.
        ] ifFalse: [
            (heldItems includes: #karnet) ifTrue: [
                Transcript show: 'Wchodzisz do szatni męskiej!'; cr.
                szatniaWejscie := true.
            ] ifFalse: [
                | response |
                Transcript show: 'Nie masz karnetu! Może chcesz sprawdzić czy skomplementowanie recepcjonistki coś da? (tak/nie)'; cr.
                response := FileStream stdin nextLine asLowercase.
                response = 'tak' ifTrue: [
                    self komplementujRecepcjonistke. "This method might change location"
                    ^self "komplementujRecepcjonistke handles location change and look"
                ] ifFalse: [
                    Transcript show: 'Recepcjonistka patrzy na ciebie z dezaprobatą.'; cr.
                    ^self
                ].
            ].
        ].
    ].

    newLocation = #szatnia_damska ifTrue: [
        Transcript show: 'Podglądacze nie są tolerowani! Zostałeś wyrzucony z siłowni! Koniec gry.'; cr.
        self die.
        gameOver ifFalse: [ "If cheated death"
            Transcript show: 'Tak naprawdę nie wszedłeś do szatni damskiej, tylko do pomieszczenia dla personelu nazwanego szatnia_damska!'; cr.
        ].
        ^self
    ].

    newLocation = #strefa_cardio ifTrue: [
        gameStage = 4 ifTrue: [
            gameStage := 5.
            Transcript show: 'Przechodzisz do strefy cardio. Zapatrujesz się na ćwiczące się osoby.'; cr.
            Transcript show: 'Nawet nie zauważasz kiedy mija czas na kolejną serię!'; cr.
            currentLocation := newLocation.
            self look.
            ^self
        ].
    ].

    (#(#strefa_wolnych_ciezarow #strefa_cardio #strefa_maszyn) includes: newLocation) ifTrue: [
        wearing = #stroj_sportowy ifFalse: [
            Transcript show: 'Nie możesz iść na trening bez stroju sportowego! Przebierz się!'; cr.
            ^self
        ].
    ].

    currentLocation := newLocation.
    self look.
!

idzDoPrysznica
    currentLocation = #szatnia_meska ifFalse: [
        Transcript show: 'Nie możesz tego tutaj zrobić!'; cr.
        ^self
    ].
    prysznicSprawdzony ifTrue: [
        Transcript show: 'Prysznic jest już pusty. Nie ma tam nic więcej.'; cr.
        ^self
    ].
    Transcript show: 'Podchodzisz do nieczynnego prysznica...'; cr.
    Transcript show: 'Nagle znajdujesz tam darmowego Dzika (napój energetyczny)!'; cr.
    heldItems add: #dzik.
    Transcript show: 'Wypij zdobyty napój i poczuj przypływ siły!'; cr.
    Transcript show: 'Automatycznie wracasz do szatni.'; cr.
    currentLocation := #szatnia_meska. "Redundant if already there, but matches Prolog."
    prysznicSprawdzony := true.
    self look.
!

take: itemNameSymbol
    | itemsInCurrentLocation |
    (allNpcs includes: itemNameSymbol) ifTrue: [
        Transcript show: 'Nie możesz podnieść '; show: itemNameSymbol; show: '! To osoba!'; cr.
        ^self
    ].
    (heldItems includes: itemNameSymbol) ifTrue: [
        Transcript show: 'Masz już '; show: itemNameSymbol; show: '!'; cr.
        ^self
    ].
    ((buyableAtLocation at: currentLocation ifAbsent: [Set new]) includes: itemNameSymbol) ifTrue: [
        Transcript show: 'Nie możesz podnieść '; show: itemNameSymbol; show: '! To przedmiot do kupienia!'; cr.
        ^self
    ].

    itemsInCurrentLocation := worldItems at: currentLocation ifAbsent: [Set new].

    (itemsInCurrentLocation includes: itemNameSymbol) ifTrue: [
        itemsInCurrentLocation remove: itemNameSymbol.
        heldItems add: itemNameSymbol.
        Transcript show: 'Podniosłeś '; show: itemNameSymbol; show: '.'; cr.
        ^self
    ].
    Transcript show: 'Nie widzę tego tutaj.'; cr.
!

drop: itemNameSymbol
    (heldItems includes: itemNameSymbol) ifFalse: [
        Transcript show: 'Nie masz tego przedmiotu!'; cr.
        ^self
    ].
    heldItems remove: itemNameSymbol.
    (worldItems at: currentLocation) add: itemNameSymbol.
    Transcript show: 'Odłożyłeś '; show: itemNameSymbol; show: '.'; cr.
!

inventory
    heldItems isEmpty
        ifTrue: [Transcript show: 'Twój ekwipunek jest pusty!'; cr]
        ifFalse: [
            Transcript show: 'Twój ekwipunek: '.
            heldItems do: [:item | Transcript show: item; space].
            Transcript cr.
        ].
    self show_weight_inventory.
!

consume: itemNameSymbol
    | strengthGain |
    (heldItems includes: itemNameSymbol) ifFalse: [
        Transcript show: 'Nie masz tego przedmiotu!'; cr.
        ^self
    ].

    strengthGain := 0.
    itemNameSymbol = #monster ifTrue: [strengthGain := 3].
    itemNameSymbol = #dzik ifTrue: [strengthGain := 5]. "Prolog has dzik giving 5, but comment suggests 10 in shower"
    itemNameSymbol = #przedtreningowka ifTrue: [strengthGain := 10].
    itemNameSymbol = #mala_strzykawka ifTrue: [strengthGain := 30].
    itemNameSymbol = #duza_strzykawka ifTrue: [strengthGain := 60].

    strengthGain = 0 ifTrue: [
         Transcript show: 'Nie możesz tego spożyć!'; cr.
        ^self
    ].

    heldItems remove: itemNameSymbol.
    Transcript show: 'Spożywasz '; show: itemNameSymbol; show: '.'; cr.
    self increaseStrength: strengthGain.
    Transcript show: 'Twoja siła wzrosła o '; show: strengthGain; show: '!'; cr.

    itemNameSymbol = #mala_strzykawka ifTrue: [
        (randomGenerator nextIntegerBetween: 1 and: 4) = 1 ifTrue: [self dieSteroid].
        gameOver ifTrue: [^self].
    ].
    itemNameSymbol = #duza_strzykawka ifTrue: [
        "Prolog random_between(1,1,R) means R is always 1"
        self dieSteroid.
        gameOver ifTrue: [^self].
    ].
!

wear: itemNameSymbol
    itemNameSymbol = #stroj_sportowy ifFalse: [
        Transcript show: 'Nie masz stroju sportowego lub próbujesz założyć coś innego!'; cr.
        ^self
    ].
    currentLocation = #szatnia_meska ifFalse: [
        Transcript show: 'Możesz się przebrać tylko w szatni męskiej.'; cr.
        ^self
    ].
    (heldItems includes: #stroj_sportowy) ifFalse: [
        Transcript show: 'Nie masz stroju sportowego w ekwipunku!'; cr.
        ^self
    ].

    heldItems remove: #stroj_sportowy. "Prolog implies it's removed from general holding"
    wearing := #stroj_sportowy.
    Transcript show: 'Przebrałeś się w strój sportowy!'; cr.
!

komplementujRecepcjonistke
    currentLocation = #recepcja ifFalse: [
        Transcript show: 'Nie możesz tego tutaj zrobić!'; cr.
        ^self
    ].
    Transcript show: 'Mówisz recepcjonistce, że ma piękny uśmiech.'; cr.
    Transcript show: 'Recepcjonistka przewraca oczami ale mimo to jednak pozwala ci wejść.'; cr.
    currentLocation := #szatnia_meska.
    szatniaWejscie := true. "Allows entry without ticket from now on"
    self look.
!

check_money
    Transcript show: 'Masz '; show: money asString; show: ' zł na koncie.'; cr.
!

buy: itemNameSymbol
    | price itemBuyableLocations |
    itemBuyableLocations := buyableAtLocation at: currentLocation ifAbsent: [Set new].
    (itemBuyableLocations includes: itemNameSymbol) ifFalse: [
        Transcript show: 'Nie ma tutaj takiego przedmiotu do kupienia lub ten przedmiot nie jest na sprzedaż!'; cr.
        ^self
    ].

    price := itemPrices at: itemNameSymbol ifAbsent: [nil].
    price isNil ifTrue: [ "Should not happen if buyableAtLocation is correct"
        Transcript show: 'Ten przedmiot nie jest na sprzedaż (brak ceny)!'; cr.
        ^self
    ].

    money >= price ifTrue: [
        money := money - price.
        heldItems add: itemNameSymbol.
        Transcript show: 'Kupiono '; show: (itemNameSymbol asString copyReplaceAll: '_' with: ' '); show: '.'; cr.
        (itemNameSymbol = #mala_strzykawka or: [itemNameSymbol = #duza_strzykawka]) ifTrue: [
            Transcript show: 'Zastanów się dwa razy czy na pewno chcesz jej użyć!'; cr.
        ].
        Transcript show: 'Pozostało Ci '; show: money asString; show: ' zł.'; cr.
    ] ifFalse: [
        Transcript show: 'Nie masz wystarczająco pieniędzy!'; cr.
    ].
! !

!GymAdventureGame methodsFor: 'npc interaction' stamp: 'AI 6/2/2025 10:30'!
talk: npcSymbol
    "Special case for Swiezak based on stage, who might not be in npcLocations initially"
    npcSymbol = #swiezak ifTrue: [
        currentLocation = #strefa_wolnych_ciezarow ifTrue: [
            gameStage = 2 ifTrue: [
                (heldItems includes: #czerwony_bidon) ifTrue: [
                    Transcript show: 'Ty: Cześć, stary, znalazłem twój czerwony bidon w łazience.'; cr.
                    Transcript show: 'Swiezak: Dzięki, stary! Nie wiem co bym bez ciebie zrobił!'; cr.
                    heldItems remove: #czerwony_bidon ifAbsent: [].
                    Transcript show: 'Ty: Nie ma sprawy!'; cr.
                    gameStage := 3.
                    self startStage: 3.
                ] ifFalse: [
                    Transcript show: 'Swiezak: Ktoś widział mój czerwony bidon?'; cr.
                    Transcript show: 'Ty: ...'; cr.
                ].
                ^self
            ].
            (gameStage = 3 or: [gameStage = 4]) ifTrue: [
                 Transcript show: 'Swiezak: Cześć! Jak tam trening?'; cr.
                 ^self
            ].
        ].
        Transcript show: 'Nie widzisz tutaj swiezak.'; cr.
        ^self
    ].

    (npcLocations at: npcSymbol ifAbsent: [nil]) ~= currentLocation ifTrue: [
        Transcript show: 'Nie widzisz tutaj '; show: npcSymbol; show: '.'; cr.
        ^self
    ].

    (gameStage = 0 and: [(#(#recepcjonistka #podejrzany_typ) includes: npcSymbol) not]) ifTrue: [
        Transcript show: 'Nie czas na rozmowy, zajmij ławkę (take_bench) w strefie wolnych ciężarów.'; cr.
        ^self
    ].

    self interactWith: npcSymbol.
!

incrementInteractionCountFor: npcSymbol
    | currentCount |
    currentCount := npcInteractionCounts at: npcSymbol ifAbsent: [0].
    npcInteractionCounts at: npcSymbol put: currentCount + 1.
!

decrementInteractionCountFor: npcSymbol
    | currentCount |
    currentCount := npcInteractionCounts at: npcSymbol ifAbsent: [0].
    currentCount > 0 ifTrue: [
        npcInteractionCounts at: npcSymbol put: currentCount - 1.
    ].
!

interactWith: npcSymbol
    npcSymbol = #recepcjonistka ifTrue: [
        Transcript show: 'Recepcjonistka:'; cr.
        Transcript show: 'Witaj na siłowni! Czy czegoś ci potrzeba?'; cr.
        Transcript show: 'Możesz tutaj kupić: '; cr.
        Transcript show: ' - monster (buy monster) za 10zł'; cr.
        Transcript show: ' - przedtreningowka (buy przedtreningowka) za 20zł'; cr.
        Transcript show: ' - karnet (buy karnet) za 40zł.'; cr.
        ^self
    ].
    npcSymbol = #podejrzany_typ ifTrue: [
        Transcript show: 'Podejrzany typ:'; cr.
        Transcript show: 'Witaj, czyżbyś szedł na trening? Może chcesz coś mocniejszego, co na pewno poprawi twoje wyniki? Moje produkty są całkowicie bezpieczne!'; cr.
        Transcript show: 'Możesz tutaj kupić: '; cr.
        Transcript show: ' - mala_strzykawka (buy mala_strzykawka) za 30zł'; cr.
        Transcript show: ' - duza_strzykawka (buy duza_strzykawka) za 50zł.'; cr.
        ^self
    ].
    npcSymbol = #chudy_szczur ifTrue: [
        | count |
        self incrementInteractionCountFor: npcSymbol.
        count := npcInteractionCounts at: npcSymbol.
        count = 1 ifTrue: [
            Transcript show: 'Ty: Ej, mały, potrzebuję tych talerzy 5kg, mogę je zabrać?'; cr.
            Transcript show: 'Chudy szczur: (piszczy nerwowo) Tylko... nie bij mnie! Bierz, co chcesz, i znikaj!'; cr.
            Transcript show: 'Ty: Spoko, luz. Tylko mi je podaj, i będę się zwijał.'; cr.
            Transcript show: 'Chudy szczur: (podaje talerze) Masz, i już mnie nie zaczepiaj!'; cr.
            self addWeights: #(5 5).
            Transcript show: 'Dodałeś do swojego ekwipunku ciężary: 5 kg i 5 kg.'; cr.
        ] ifFalse: [count = 2 ifTrue: [
            strength >= 100 ifTrue: [
                Transcript show: 'Chudy szczur: (przestraszony) Czego ode mnie chcesz?! Pieniądzy?'; cr.
                Transcript show: 'Chudy szczur wyciąga 50 zł z etui do telefonu'; cr.
                Transcript show: 'Nie chcesz brać pieniędzy od szczura, ale patrząc na niego, obawiasz się jak może zareagować jak nie przyjmiesz pieniędzy.'; cr.
                money := money + 50.
                Transcript show: 'Dostałeś 50 zł.'; cr.
            ] ifFalse: [
                Transcript show: 'Zostaw mnie w spokoju!'; cr.
                self decrementInteractionCountFor: npcSymbol.
            ].
        ] ifFalse: [ "count >= 3"
            Transcript show: 'Postanawiasz jednak nie podchodzić do chudego szura, obawiając się o jego zdrowie'; cr.
            self decrementInteractionCountFor: npcSymbol.
        ]].
        ^self
    ].
    npcSymbol = #brunetka ifTrue: [
        Transcript show: 'Ty: Cześć, przepraszam cię najmocniej, że przeszkadzam, ale czy będziesz jeszcze używać tych 10kg?'; cr.
        Transcript show: 'Brunetka: Zostaw mnie, mam chłopaka!'; cr.
        Transcript show: 'Ty: Źle zrozumiałaś, ja tylko chciałem...'; cr.
        Transcript show: 'Brunetka: Mam wezwać ochronę? Spadaj!'; cr.
        ^self
    ].
    npcSymbol = #czlowiek_szczuply ifTrue: [
        | count |
        self incrementInteractionCountFor: npcSymbol.
        count := npcInteractionCounts at: npcSymbol.
        count = 1 ifTrue: [
            Transcript show: 'Ty: Stary, te talerze 15kg... mogę je pożyczyć? Na chwilę?'; cr.
            Transcript show: 'Człowiek szczupły: (wzrusza ramionami) No dobra, ale szybko oddaję. Ja tu jeszcze muszę poćwiczyć.'; cr.
            Transcript show: 'Ty: Jasne, jasne, tylko je wezmę. Dzięki!'; cr.
            Transcript show: 'Człowiek szczupły: Tylko je odłóż na miejsce, bo będziesz miał ze mną do czynienia!'; cr.
            self addWeights: #(15 15).
            Transcript show: 'Dodałeś do swojego ekwipunku ciężary: 15 kg i 15 kg.'; cr.
        ] ifFalse: [
            Transcript show: 'Człowiek szczupły: Odłożyłeś talerze już na miejsce?.'; cr.
            Transcript show: 'Ty: Jeszcze nie skończyłem '; cr.
        ].
        ^self
    ].
    npcSymbol = #szczur_bojowy ifTrue: [
        | count |
        self incrementInteractionCountFor: npcSymbol.
        count := npcInteractionCounts at: npcSymbol.
        count = 1 ifTrue: [
            Transcript show: 'Ty: Ej, byczku, te 20 kilo... mogę je zabrać?'; cr.
            Transcript show: 'Szczur bojowy: Jasne, stary, bierz. I tak mi się już nie przydadzą.'; cr.
            Transcript show: 'Ty: Dzięki, jesteś wielki!'; cr.
            Transcript show: 'Szczur bojowy: Tylko uważaj, żebyś sobie krzywdy nie zrobił. To ciężkie żelastwo.'; cr.
            self addWeights: #(20 20).
            Transcript show: 'Dodałeś do swojego ekwipunku ciężary: 20 kg i 20 kg.'; cr.
        ] ifFalse: [
            Transcript show: 'Szczur bojowy: Czego?.'; cr.
            Transcript show: 'Ty: ...'; cr.
        ].
        ^self
    ].
    npcSymbol = #duzy_chlop ifTrue: [
        | count |
        self incrementInteractionCountFor: npcSymbol.
        count := npcInteractionCounts at: npcSymbol.
        count = 1 ifTrue: [
            Transcript show: 'Ty: Mogę zabrać talerze 25 kg?'; cr.
            Transcript show: 'Duży chłop: Spoko, nie ma sprawy, już ich nie używam!'; cr.
            self addWeights: #(25 25).
            Transcript show: 'Dodałeś do swojego ekwipunku ciężary: 25 kg i 25 kg.'; cr.
        ] ifFalse: [
            Transcript show: 'Nie chcesz zawaracać głowy dużemu chłopowi bez powodu'; cr.
        ].
        ^self
    ].
    npcSymbol = #wielki_chlop ifTrue: [
        | count |
        self incrementInteractionCountFor: npcSymbol. "Prolog starts count at 0, then 1, then 2. Here, 1, 2, 3"
        count := npcInteractionCounts at: npcSymbol.
        count = 1 ifTrue: [
            Transcript show: 'Ty: Mogę zabrać talerze...'; cr.
            Transcript show: 'Wielki chłop: Spadaj szczurze!'; cr.
        ] ifFalse: [count = 2 ifTrue: [
            Transcript show: 'Ty: Ej, serio, potrzebuję tych talerzy. Możesz mi je dać?'; cr.
            Transcript show: 'Wielki chłop: Jeszcze jedno słowo i pożałujesz!'; cr.
        ] ifFalse: [count = 3 ifTrue: [
            Transcript show: 'Próbujesz zagadać do wielkiego chłopa jeszcze raz.'; cr.
            Transcript show: 'To był błąd, za nim zdążysz się zorientować, dostajesz od niego cios w brzuch.'; cr.
            Transcript show: 'Nie masz szans przetrwać następnych uderzeń.'; cr.
            self die.
            gameOver ifFalse: [ "If cheated death"
                Transcript cr; show: 'Przyszła właśnie jego dziewczyna i odwróciło to jego uwagę od ciebie.'; cr.
                Transcript show: 'Udało ci się przeżyć!'; cr.
            ].
        ] ifFalse: [ "count >= 4"
            Transcript show: 'Postanawiasz nie zaczepiać wielkiego chłopa więcej, obawiając się o swoje zdrowie.'; cr.
        ]]].
        ^self
    ].
! !

!GymAdventureGame methodsFor: 'weight management' stamp: 'AI 6/2/2025 10:30'!
addWeights: weightsCollection
    weightsCollection do: [:weight |
        weightInventory at: weight put: (weightInventory at: weight ifAbsent: [0]) + 1.
    ].
!

removeWeights: weightsCollection
    weightsCollection do: [:weight |
        | currentCount |
        currentCount := weightInventory at: weight ifAbsent: [0].
        currentCount = 1 ifTrue: [
            weightInventory removeKey: weight ifAbsent: [].
        ] ifFalse: [currentCount > 1 ifTrue: [
            weightInventory at: weight put: currentCount - 1.
        ] ifFalse: [
            "Transcript show: 'Próba usunięcia ciężaru '; show: weight; show: ' kg, którego nie masz w ekwipunku.'; cr."
            "Prolog version allows this to fail silently in subtract_weights if not found"
        ]].
    ].
!

show_weight_inventory
    weightInventory isEmpty
        ifTrue: [Transcript show: 'Ekwipunek ciężarów jest pusty.'; cr]
        ifFalse: [
            Transcript show: 'Ciężary w twoim ekwipunku: '; cr.
            weightInventory keys sorted do: [:weight |
                Transcript show: (weightInventory at: weight) printString; show: 'x '; show: weight printString; show: 'kg'; cr.
            ].
        ].
!

left_add_weight_bench: weight
    currentLocation = #strefa_wolnych_ciezarow ifFalse: [Transcript show: 'Nie jesteś w strefie wolnych ciężarów!'; cr. ^self].
    (#(1 3 5) includes: gameStage) ifFalse: [Transcript show: 'Nie można wykonać takiej akcji teraz!'; cr. ^self].

    (weightInventory at: weight ifAbsent: [0]) > 0 ifFalse: [
        Transcript show: 'Nie masz ciężaru '; show: weight printString; show: ' kg w ekwipunku!'; cr.
        ^self
    ].

    self removeWeights: (Array with: weight).
    benchLeftWeights add: weight.
    Transcript show: 'Dodałeś ciężar '; show: weight printString; show: ' kg na lewą stronę sztangi.'; cr.
    self check_bench.
!

right_add_weight_bench: weight
    currentLocation = #strefa_wolnych_ciezarow ifFalse: [Transcript show: 'Nie jesteś w strefie wolnych ciężarów!'; cr. ^self].
    (#(1 3 5) includes: gameStage) ifFalse: [Transcript show: 'Nie można wykonać takiej akcji teraz!'; cr. ^self].

    (weightInventory at: weight ifAbsent: [0]) > 0 ifFalse: [
        Transcript show: 'Nie masz ciężaru '; show: weight printString; show: ' kg w ekwipunku!'; cr.
        ^self
    ].

    self removeWeights: (Array with: weight).
    benchRightWeights add: weight.
    Transcript show: 'Dodałeś ciężar '; show: weight printString; show: ' kg na prawą stronę sztangi.'; cr.
    self check_bench.
!

remove_all_weight_bench
    | allWeightsOnBench |
    currentLocation = #strefa_wolnych_ciezarow ifFalse: [Transcript show: 'Nie jesteś w strefie wolnych ciężarów!'; cr. ^self].
    (#(1 3 5) includes: gameStage) ifFalse: [Transcript show: 'Nie można wykonać takiej akcji teraz!'; cr. ^self].

    allWeightsOnBench := OrderedCollection new.
    allWeightsOnBench addAll: benchLeftWeights.
    allWeightsOnBench addAll: benchRightWeights.
    self addWeights: allWeightsOnBench.

    benchLeftWeights := OrderedCollection new.
    benchRightWeights := OrderedCollection new.
    Transcript show: 'Zdjąłeś wszystkie ciężary ze sztangi!'; cr.
!

check_bench
    | totalLeft totalRight totalWeight diff |
    currentLocation = #strefa_wolnych_ciezarow ifFalse: [Transcript show: 'Nie jesteś w strefie wolnych ciężarów!'; cr. ^self].
    (#(1 3 5) includes: gameStage) ifFalse: [Transcript show: 'Nie można wykonać takiej akcji teraz!'; cr. ^self].

    totalLeft := 0.
    benchLeftWeights do: [:weight | totalLeft := totalLeft + weight].
    totalRight := 0.
    benchRightWeights do: [:weight | totalRight := totalRight + weight].
    totalWeight := totalLeft + totalRight + 20. "20kg for the bar itself"

    Transcript show: 'Obciążenie na sztandze wynosi: '; show: totalWeight printString; show: ' kg.'; cr.

    totalLeft = totalRight ifTrue: [
        Transcript show: 'Obciążenie na sztandze jest równo rozłożone!'; cr.
    ] ifFalse: [
        diff := (totalLeft - totalRight) abs.
        diff > 53 ifTrue: [
            Transcript show: 'Sztanga się przewaliła! Wszyscy się teraz z ciebie śmieją. Uciekasz z siłowni'; cr.
            self die.
            gameOver ifFalse: [ "Cheated death"
                Transcript show: 'Duży chłop powstrzymuje cię przed ucieczką!'; cr.
                Transcript show: 'I pomaga ci podnieść ciężary!'; cr.
                self remove_all_weight_bench. "Weights go back to inventory"
                Transcript show: '(ciężary teraz w twoim ekwipunku)'; cr.
            ].
        ] ifFalse: [
            Transcript show: 'Obciążenie na sztandze jest nierówno rozłożone, ale w granicach bezpieczeństwa.'; cr.
            Transcript show: 'Różnica w obciążeniu wynosi: '; show: diff printString; show: ' kg.'; cr.
            Transcript show: 'Ciężary na lewej stronie: '; show: benchLeftWeights printString; cr.
            Transcript show: 'Ciężary na prawej stronie: '; show: benchRightWeights printString; cr.
        ].
    ].
!

do_bench_press
    | totalLeft totalRight totalWeight currentLifted newLifted |
    currentLocation = #strefa_wolnych_ciezarow ifFalse: [Transcript show: 'Nie jesteś w strefie wolnych ciężarów!'; cr. ^self].
    (#(1 3 5) includes: gameStage) ifFalse: [Transcript show: 'Nie można wykonać takiej akcji teraz!'; cr. ^self].

    totalLeft := 0.
    benchLeftWeights do: [:weight | totalLeft := totalLeft + weight].
    totalRight := 0.
    benchRightWeights do: [:weight | totalRight := totalRight + weight].

    totalLeft ~= totalRight ifTrue: [
        Transcript show: 'Obciążenie na sztandze jest nierówno rozłożone!'; cr.
        Transcript show: 'Nie możesz podnieść takiej sztangi!'; cr.
        ^self
    ].

    totalWeight := totalLeft + totalRight + 20. "Barbell weight"
    Transcript show: 'Podnosisz sztangę o obciążeniu '; show: totalWeight printString; show: ' kg.'; cr.

    totalWeight <= strength ifTrue: [
        gameStage := gameStage + 1.
        currentLifted := liftedWeights at: #klatka_piersiowa.
        newLifted := currentLifted + totalWeight.
        liftedWeights at: #klatka_piersiowa put: newLifted.
        self startStage: gameStage.
    ] ifFalse: [
        Transcript show: 'Obciążenie cię przygniata to będzie twój koniec!'; cr.
        self die.
        gameOver ifFalse: [ "Cheated death"
            Transcript show: 'Przechodzi obok ciebie wielki chłop i jedną ręką podnosi sztangę ratując cię przed marnym końcem!'; cr.
            Transcript show: 'Nie udało ci się wykonać ćwiczenia!'; cr.
        ].
    ].
!

take_bench
    currentLocation = #strefa_wolnych_ciezarow ifFalse: [
        Transcript show: 'Nie jesteś w strefie wolnych ciężarów!'; cr.
        ^self
    ].
    Transcript show: 'Current stage: '; show: gameStage printString; cr.
    gameStage = 0 ifTrue: [
        gameStage := 1.
        self startStage: 1.
        ^self
    ].
    (#(1 3 5) includes: gameStage) ifTrue: [
        Transcript show: 'Wykonujesz to ćwiczenie teraz!'; cr.
        ^self
    ].
    Transcript show: 'Nie można wykonać takiej akcji teraz!'; cr.
! !

!GymAdventureGame methodsFor: 'game stages and helpers' stamp: 'AI 6/2/2025 10:30'!
increaseStrength: value
    strength := strength + value.
!

startStage: stageNumber
    stageNumber = 1 ifTrue: [
        Transcript show: 'Rozpoczynasz trening na klatę! Rozgladasz się obok sztangi, ale nie ma obok niej żadnych ciężarów.'; cr.
        Transcript show: 'Pora zebrać ciężary!'; cr.
        Transcript show: 'Rozejrzyj się po siłowni i przynieś ciężary, a następnie je nałóż!'; cr.
        Transcript show: 'Jeżeli nie znajdziesz nieużywanego sprzętu, możesz spróbować zapytać o niego innych ćwiczących'; cr.
        Transcript show: 'Pamiętaj, że z dwóch stron trzeba mieć tyle samo na sztandze!'; cr.
        Transcript show: 'Im więcej podniesiesz, tym lepszy wynik zdobędziesz!'; cr.
        Transcript show: 'Powodzenia!'; cr.
    ].
    stageNumber = 2 ifTrue: [
        "Add Swiezak NPC"
        npcLocations at: #swiezak put: #strefa_wolnych_ciezarow.
        Transcript show: 'Gratulacje udało ci się wykonać pierwszą serię!'; cr.
        Transcript show: 'Kiedy odpoczywasz po pierwszej serii, podchodzi do ciebie jakiś przeciętnie zbudowany chłopak z pytaniem:'; cr.
        Transcript show: ' Swiezak: Hej, ile zostało ci serii?'; cr.
        Transcript show: ' Ty: Jeszcze dwie.'; cr.
        Transcript show: ' Swiezak: Mogę w takim razie się dołączyć?'; cr.
        Transcript show: ' Ty: Jasne'; cr.
        Transcript show: ' Swiezak: Tak w ogóle nie widziałeś przypadkiem gdzieś na siłowni czerwonego bidonu, musiałem go wczoraj zostawić w szatni. Znalazcy z pewnością się odwdzięczę'; cr.
        (heldItems includes: #czerwony_bidon) ifTrue: [
            Transcript show: 'Przypominasz sobie, że podniosłeś taki bidon w łazience'; cr.
            Transcript show: 'Podajesz mu bidon'; cr.
            Transcript show: ' Swiezak: O, dzięki! Nie wiem co bym bez ciebie zrobił!'; cr.
            heldItems remove: #czerwony_bidon ifAbsent: [].
            Transcript show: ' Ty: Nie ma sprawy!'; cr.
            gameStage := 3. "Advance stage"
            self startStage: 3. "Call recursively for stage 3 message"
        ] ifFalse: [
            Transcript show: ' Ty: Niestety, nie widziałem go. Ale i tak teraz odpoczywam pójdę go poszukać (Powiedziałeś już myśląc o tym jaka nagroda cię czeka)'; cr.
            Transcript show: ' Swiezak: Powodzenia!'; cr.
        ].
    ].
    stageNumber = 3 ifTrue: [
        Transcript show: ' Swiezak: W nagrodę trzymaj tą przedtreningówę'; cr.
        Transcript show: ' Ty: Dzięki!'; cr.
        Transcript cr.
        Transcript show: 'Spojrzałeś na datę ważności przedtreningówki i okazało się, że jest przeterminowana'; cr.
        Transcript show: 'Nie chcąc się narzucać, postanowiłeś nie mówić o tym swojemu nowemu znajomemu'; cr.
        Transcript show: 'Jednak wiesz, że nie możesz jej użyć'; cr.
        "Player receives nothing usable, but the interaction happened."
        heldItems add: #przeterminowana_przedtreningowka. "Can't be consumed with effect"
        Transcript cr.
        Transcript show: 'Po odnalezieniu bidonu, wracasz do strefy wolnych ciężarów i szykujesz się na drugą serię.'; cr.
    ].
    stageNumber = 4 ifTrue: [
        Transcript show: 'Gratulacje udało ci się wykonać drugą serię!'; cr.
        Transcript show: 'Podczas odpoczynku możesz przejść się po siłowni i porozmawiać z innymi ćwiczącymi.'; cr.
    ].
    stageNumber = 5 ifTrue: [
        Transcript cr. "Quiet stage, likely preparation for last set"
    ].
    stageNumber = 6 ifTrue: [
        Transcript show: 'Gratulacje udało ci się wykonać trening!'; cr.
        score := 1. "Set score to 1 for win"
        gameWon := true.
        gameOver := true.
        "finishGame will be called by the game loop"
    ].
!

instructions
    Transcript show: 'Możesz wykonać następujące akcje:'; cr.
    currentLocation = #dom ifTrue: [
        Transcript show: '- Zabrać przedmioty (take <przedmiot>)'; cr.
        Transcript show: '- Sprawdzić ekwipunek (inventory)'; cr.
        Transcript show: '- Pójść na parking przed siłownię (go parking)'; cr.
        Transcript show: '- Sprawdzić pieniądze (check_money)'; cr.
        Transcript show: '- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'; cr.
    ].
    currentLocation = #parking ifTrue: [
        Transcript show: '- Porozmawiać z podejrzanym typem (talk podejrzany_typ)'; cr.
        Transcript show: '- Wejść do siłowni (go recepcja)'; cr.
        Transcript show: '- Wrócić do domu (go dom)'; cr.
        Transcript show: '- Sprawdzić ekwipunek (inventory)'; cr.
        Transcript show: '- Sprawdzić pieniądze (check_money)'; cr.
        Transcript show: '- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'; cr.
        Transcript show: '- Kupić przedmioty (buy <przedmiot>)'; cr.
        Transcript show: '- Spożyć przedmioty (consume <przedmiot>)'; cr.
    ].
    currentLocation = #recepcja ifTrue: [
        Transcript show: '- Porozmawiać z recepcjonistką (talk recepcjonistka)'; cr.
        Transcript show: '- Kupić przedmioty (buy <przedmiot>)'; cr.
        Transcript show: '- Wejść do szatni (go szatnia_meska lub go szatnia_damska)'; cr.
        Transcript show: '- Wyjść na parking przed siłownię (go parking)'; cr.
        Transcript show: '- Sprawdzić ekwipunek (inventory)'; cr.
        Transcript show: '- Sprawdzić pieniądze (check_money)'; cr.
        Transcript show: '- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'; cr.
        Transcript show: '- Spożyć przedmioty (consume <przedmiot>)'; cr.
        Transcript show: '- Skomplementować recepcjonistkę (komplementujRecepcjonistke)'; cr.
    ].
    currentLocation = #szatnia_meska ifTrue: [
        Transcript show: '- Udać się do strefy treningowej (go strefa_wolnych_ciezarow / strefa_cardio / strefa_maszyn)'; cr.
        Transcript show: '- Udać się do łazienki (go lazienka)'; cr.
        Transcript show: '- Wrócić na recepcję (go recepcja)'; cr.
        Transcript show: '- Sprawdzić nieczynny prysznic (go nieczynny_prysznic)'; cr.
        Transcript show: '- Przebrać się w strój sportowy (wear stroj_sportowy)'; cr.
        Transcript show: '- Sprawdzić ekwipunek (inventory)'; cr.
        Transcript show: '- Sprawdzić pieniądze (check_money)'; cr.
        Transcript show: '- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'; cr.
    ].
    currentLocation = #strefa_wolnych_ciezarow ifTrue: [
        Transcript show: '- Zająć ławkę (take_bench)'; cr.
        Transcript show: '- Wrócić do szatni (go szatnia_meska)'; cr.
        Transcript show: '- Sprawdzić ekwipunek (inventory)'; cr.
        Transcript show: '- Sprawdzić pieniądze (check_money)'; cr.
        Transcript show: '- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'; cr.
        Transcript show: '- Dodać ciężar na lewą stronę sztangi (left_add_weight_bench <waga>)'; cr.
        Transcript show: '- Dodać ciężar na prawą stronę sztangi (right_add_weight_bench <waga>)'; cr.
        Transcript show: '- Zdjąć wszystkie ciężary ze sztangi (remove_all_weight_bench)'; cr.
        Transcript show: '- Sprawdzić obciążenie na sztandze (check_bench)'; cr.
        Transcript show: '- Wykonać trening (do_bench_press)'; cr.
        Transcript show: '- Sprawdzić jakie ciężary masz w ekwipunku (show_weight_inventory)'; cr.
        Transcript show: '- Porozmawiać z osobami w okolicy (talk <osoba>)'; cr.
        Transcript show: '- Spożyć przedmioty (consume <przedmiot>)'; cr.
    ].
    currentLocation = #strefa_maszyn ifTrue: [
        Transcript show: '- Wrócić do szatni (go szatnia_meska)'; cr.
        Transcript show: '- Sprawdzić ekwipunek (inventory)'; cr.
        Transcript show: '- Sprawdzić pieniądze (check_money)'; cr.
        Transcript show: '- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'; cr.
        Transcript show: '- Porozmawiać z osobami w okolicy (talk <osoba>)'; cr.
        Transcript show: '- Spożyć przedmioty (consume <przedmiot>)'; cr.
    ].
    currentLocation = #strefa_cardio ifTrue: [
        Transcript show: '- Wrócić do szatni (go szatnia_meska)'; cr.
        Transcript show: '- Sprawdzić ekwipunek (inventory)'; cr.
        Transcript show: '- Sprawdzić pieniądze (check_money)'; cr.
        Transcript show: '- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'; cr.
        Transcript show: '- Porozmawiać z osobami w okolicy (talk <osoba>)'; cr.
        Transcript show: '- Spożyć przedmioty (consume <przedmiot>)'; cr.
    ].
    currentLocation = #lazienka ifTrue: [
        Transcript show: '- Wrócić do szatni (go szatnia_meska)'; cr.
        Transcript show: '- Podnieść przedmioty (take <przedmiot>)'; cr.
        Transcript show: '- Sprawdzić ekwipunek (inventory)'; cr.
        Transcript show: '- Sprawdzić pieniądze (check_money)'; cr.
        Transcript show: '- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'; cr.
        Transcript show: '- Spożyć przedmioty (consume <przedmiot>)'; cr.
    ].
    Transcript show: '- Wyjść z gry (quit)'; cr.
    Transcript cr.
! !

!GymAdventureGame class methodsFor: 'instance creation' stamp: 'AI 6/2/2025 10:30'!
new
    ^super new initialize
!

play
    "Convenience method to start the game"
    | game |
    game := self new.
    game startGame.
! !

GymAdventureGame play.