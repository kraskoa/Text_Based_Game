/* "Klata, biceps, barki", by Mateusz Daszewski, Adam Kraś, Krzysztof Król */

:- dynamic i_am_at/1, at/2, holding/1, strength/1, lifted/2, money/1, stage/1, score/1, weight_inventory/1, bench_left/1, bench_right/1, npc/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)), retractall(strength(_)), retractall(lifted(_, _)), retractall(weight_inventory(_)), retractall(bench_left(_)), retractall(bench_right(_)).




/* Początkowa lokalizacja */
i_am_at(dom).

/* Początkowa siła, podniesione ciężary oraz pieniądze */
random_strength_and_money :-
        random(50, 151, S),  % Siła w zakresie 50-150 kg
        % do testów
        random(0, 151, M),
        assert(strength(S)),
        assert(money(M)),
        assert(lifted(klatka_piersiowa, 0)),
        assert(lifted(barki, 0)),
        assert(lifted(biceps, 0)),
        assert(stage(0)),
        assert(score(0)),
        assert(weight_inventory([])),
        assert(bench_left([])),
        assert(bench_right([])).

/* Postacie poboczne */
npc(recepcjonistka).
npc(podejrzany_typ).

/* Przedmioty do kupienia na recepcji */
item_price(monster, 10).
item_price(przedtreningowka, 20).
item_price(karnet, 40).

/* Przedmioty do kupienia u podejrzanego typa na parkingu */
item_price(mala_strzykawka, 30).
item_price(duza_strzykawka, 50).

/* Rozmowa z NPC */
talk(NPC) :-
        i_am_at(Place),
        at(NPC, Place),
        npc(NPC),
        interact(NPC),
        !.

talk(NPC) :-
        write('Nie widzisz tutaj '), write(NPC), write('.'), nl.

/* Definicje interakcji z NPC */
interact(recepcjonistka) :-
        write('Recepcjonistka:'), nl,
        write('Witaj na siłowni! Czy czegoś ci potrzeba?'), nl,
        write('Możesz tutaj kupić: '), nl,
        write(' - monster (buy(monster)) za 10zł'), nl,
        write(' - przedtreningowka (buy(przedtreningowka)) za 20zł'), nl,
        write(' - karnet (buy(karnet)) za 40zł.'), nl.

interact(podejrzany_typ) :-
        write('Podejrzany typ:'), nl,
        write('Witaj, czyżbyś szedł na trening? Może chcesz coś mocniejszego, co na pewno poprawi twoje wyniki? Moje produkty są całkowicie bezpieczne!'), nl,
        write('Możesz tutaj kupić: '), nl,
        write(' - mala_strzykawka (buy(mala_strzykawka)) za 30zł'), nl,
        write(' - duza_strzykawka (buy(duza_strzykawka)) za 50zł.'), nl.

interact(szur_bojowy) :-
        write('Podchodzisz i się pytasz:'), nl,
        write('Mogę zabrać 15 kg i 5 kg?'), nl,
        write('Szur bojowy:'), nl,
        write('XD, nie ma sprawy!'), nl,
        weight_inventory(WI),
        append(WI, [15, 5], NewWI),
        retract(weight_inventory(WI)),
        assert(weight_inventory(NewWI)),
        write('Dodałeś do swojego ekwipunku ciężary: 15 kg i 5 kg.'), nl.

interact(brunetka) :-
        write('Podchodzisz i się pytasz:'), nl,
        write('Mogę zabrać 10 kg i 20 kg?'), nl,
        write('Brunetka:'), nl,
        write('XD, nie ma sprawy!'), nl,
        weight_inventory(WI),
        append(WI, [10, 20], NewWI),
        retract(weight_inventory(WI)),
        assert(weight_inventory(NewWI)),
        write('Dodałeś do swojego ekwipunku ciężary: 10 kg i 20 kg.'), nl.

interact(duzy_chlop) :-
        write('Podchodzisz i się pytasz:'), nl,
        write('Mogę zabrać 25 kg i 30 kg?'), nl,
        write('Duży chłop:'), nl,
        write('XD, nie ma sprawy!'), nl,
        weight_inventory(WI),
        append(WI, [25, 30], NewWI),
        retract(weight_inventory(WI)),
        assert(weight_inventory(NewWI)),
        write('Dodałeś do swojego ekwipunku ciężary: 25 kg i 30 kg.'), nl.

interact(czlowiek_szczuply) :-
        write('Nic jeszcze nie napisane'), nl.




/* Zbieranie pieniędzy z początkową wartością */
check_money :-
        money(M),
        write('Masz '), write(M), write(' zł na koncie.'), nl.

weight_inventory(Weights) :-
        findall(Weight, holding_weight(Weight), Weights).

holding_weight(Weight) :-
        holding(X),
        (X = 5 ; X = 10 ; X = 15 ; X = 20 ; X = 25 ; X = 30),
        Weight = X.

/* Zakup przedmiotów */
buy(Item) :-
        i_am_at(Place),
        buy_at(Item, Place),
        item_price(Item, Price),
        money(M),
        (M >= Price ->
            NewMoney is M - Price,
            retract(money(M)),
            assert(money(NewMoney)),
            assert(holding(Item)),
            write('Kupiono '), atomic_list_concat(SplitName, '_', Item), atomic_list_concat(SplitName, ' ', ItemName), write(ItemName), write('.'), nl,
            (Item = mala_strzykawka ; Item = duza_strzykawka ->
                write('Zastanów się dwa razy czy na pewno chcesz jej użyć!'), nl
            ; true),
            write('Pozostało Ci '), write(NewMoney), write(' zł.'), nl
        ;
            write('Nie masz wystarczająco pieniędzy!'), nl
        ), !.

buy(Item) :-
        i_am_at(Place),
        buy_at(Item, Place),
        \+ item_price(Item, _),
        write('Ten przedmiot nie jest na sprzedaż!'), nl, !.

buy(Item) :-
        i_am_at(Place),
        \+ buy_at(Item, Place),
        write('Nie ma tutaj takiego przedmiotu do kupienia!'), nl, !.

/* Zwiększanie siły */
increase_strength(Value) :-
        strength(S),
        NewStrength is S + Value,
        retract(strength(S)),
        assert(strength(NewStrength)).

/* Lokacje */

/* Ścieżki między lokacjami (dwustronne) */

path(parking, dom).
path(dom, parking).
path(parking, recepcja).
path(recepcja, parking).
path(recepcja, szatnia_meska).
path(szatnia_meska, recepcja).
path(szatnia_meska, nieczynny_prysznic).
path(nieczynny_prysznic, szatnia_meska).
path(recepcja, szatnia_damska).
path(szatnia_damska, recepcja).
path(szatnia_meska, strefa_wolnych_ciezarow).
path(strefa_wolnych_ciezarow, szatnia_meska).
path(szatnia_meska, strefa_cardio).
path(strefa_cardio, szatnia_meska).
path(szatnia_damska, strefa_maszyn).
path(strefa_maszyn, szatnia_damska).
path(szatnia_damska, strefa_wolnych_ciezarow).
path(strefa_wolnych_ciezarow, szatnia_damska).
path(szatnia_damska, strefa_cardio).
path(strefa_cardio, szatnia_damska).
path(szatnia_meska, lazienka).
path(lazienka, szatnia_meska).

/* Przedmioty */
at(stroj_sportowy, dom).
at(woda, dom).
at(karnet, dom).
at(recepcjonistka, recepcja).
at(podejrzany_typ, parking).
at(czerwony_bidon, lazienka).
% at(hantle, strefa_wolnych_ciezarow).
% at(sztanga, strefa_wolnych_ciezarow).
% at(kettlebell, strefa_wolnych_ciezarow).

/* Przedmioty do kupienia */
buy_at(monster, recepcja).
buy_at(przedtreningowka, recepcja).
buy_at(mala_strzykawka, parking).
buy_at(duza_strzykawka, parking).
buy_at(karnet, recepcja).

/* Podnoszenie przedmiotów */

take(X) :-
        npc(X),
        write('Nie możesz podnieść '), write(X), write('! To osoba!'), nl, !.

take(X) :-
        holding(X),
        write('Masz już '), write(X), write('!'), nl, !.

take(X) :-
        item_price(X, _),
        i_am_at(Place),
        buy_at(X, Place),
        write('Nie możesz podnieść '), write(X), write('! To przedmiot do kupienia!'), nl, !.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('Podniosłeś '), write(X), write('.'), nl, !.

take(_) :-
        write('Nie widzę tego tutaj.'), nl.

/* Upuszczanie przedmiotów */
drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('Odłożyłeś '), write(X), write('.'), nl, !.

drop(_) :-
        write('Nie masz tego przedmiotu!'), nl.

inventory :-
        findall(Item, holding(Item), Items),
        weight_inventory(Weights),
        (Items \= [] ->
                write('Twój ekwipunek: '), write(Items), nl
        ;
                write('Twój ekwipunek jest pusty!'), nl),
        (Weights \= [] ->
                write('Ciężary w twoim ekwipunku: '), write(Weights), nl
        ;
                write('Nie masz żadnych ciężarów w ekwipunku!'), nl).

/* Spożywanie suplementów */
consume(X) :-
        holding(X),
        retract(holding(X)),
        write('Spożywasz '), write(X), write('.'), nl,
        (X = monster -> increase_strength(3)
        ; X = dzik -> increase_strength(5)
        ; X = przedtreningowka -> increase_strength(10)
        ; X = mala_strzykawka -> (random(1, 5, R), (R =:= 1 -> die_steroid ; increase_strength(30)))
        ; X = duza_strzykawka -> (random(1, 3, R), (R =:= 1 -> die_steroid ; increase_strength(60)))
        ; true),
        (X = monster -> write('Twoja siła wzrosła o 3!')
        ; X = przedtreningowka -> write('Twoja siła wzrosła o 10!')
        ; X = mala_strzykawka -> write('Twoja siła wzrosła o 30!')
        ; X = duza_strzykawka -> write('Twoja siła wzrosła o 60!')
        ; true),
        nl,
        !.

consume(_) :-
        write('Nie masz tego przedmiotu!'), nl.

consume(X) :-
        holding(X),
        write('Nie możesz tego spożyć!'), nl,
        !.

komplementuj_recepcjonistke :-
        i_am_at(recepcja),
        write('Mówisz recepcjonistce, że ma piękny uśmiech.'), nl,
        write('Recepcjonistka przewraca oczami ale mimo to jednak pozwala ci wejść.'), nl,
        retract(i_am_at(recepcja)),
        assert(i_am_at(szatnia_meska)),
        look, !.

komplementuj_recepcjonistke :-
        write('Nie możesz tego tutaj zrobić!'), nl.

/* Przebieranie się */
:- dynamic wearing/1.

wear(stroj_sportowy) :-
        i_am_at(szatnia_meska),
        holding(stroj_sportowy),
        retract(holding(stroj_sportowy)),
        assert(wearing(stroj_sportowy)),
        write('Przebrałeś się w strój sportowy!'), nl, !.

wear(_) :-
        write('Nie masz stroju sportowego!'), nl, !.

/* Ruch */
:- dynamic szatnia_wejscie/0.

go(Direction) :-
    i_am_at(Here),
    path(Here, Direction),
    (Direction = szatnia_meska ->
        (szatnia_wejscie ->
            write('Wchodzisz do szatni męskiej!'), nl,
            retract(i_am_at(Here)),
            assert(i_am_at(Direction)),
            look
        ; holding(karnet) ->
            write('Wchodzisz do szatni męskiej!'), nl,
            retract(i_am_at(Here)),
            assert(i_am_at(Direction)),
            assert(szatnia_wejscie),
            look
        ;
            write('Nie masz karnetu! Może chcesz sprawdzić czy skomplementowanie recepcjonistki coś da? (tak/nie)'), nl,
            read(Odpowiedz),
            (Odpowiedz = tak ->
                komplementuj_recepcjonistke,
                assert(szatnia_wejscie)
            ;
                write('Recepcjonistka patrzy na ciebie z dezaprobatą.'), nl
            )
        )
    ; Direction = szatnia_damska ->
        (write('Podglądacze nie są tolerowani! Zostałeś wyrzucony z siłowni! Koniec gry.'), nl, finish)
    ; Direction = nieczynny_prysznic ->
        idz_do_prysznica
    ; member(Direction, [strefa_wolnych_ciezarow, strefa_cardio]) ->
        (wearing(stroj_sportowy) ->
            retract(i_am_at(Here)),
            assert(i_am_at(Direction)),
            look
        ;
            write('Nie możesz iść na trening bez stroju sportowego! Przebierz się!'), nl
        )
    ;
        retract(i_am_at(Here)),
        assert(i_am_at(Direction)),
        look
    ), !.


go(_) :-
        write('Nie możesz tam pójść!'), nl.


/* Idź do nieczynnego prysznica */
idz_do_prysznica :-
        i_am_at(szatnia_meska),
        prysznic_sprawdzony,
        write('Prysznic jest już pusty. Nie ma tam nic więcej.'), nl, !.

idz_do_prysznica :-
        i_am_at(szatnia_meska),
        write('Podchodzisz do nieczynnego prysznica...'), nl,
        write('Nagle znajdujesz tam darmowego Dzika (napój energetyczny)!'), nl,
        assert(holding(dzik)),
        % increase_strength(10),
        write('Wypij monstera i poczuj przypływ siły!'), nl,
        write('Automatycznie wracasz do szatni.'), nl,
        retract(i_am_at(_)),
        assert(i_am_at(szatnia_meska)),
        assert(prysznic_sprawdzony),
        look, !.

idz_do_prysznica :-
        write('Nie możesz tego tutaj zrobić!'), nl.

/* Deklaracja dynamicznego faktu */
:- dynamic prysznic_sprawdzony/0.


% /* Stary Trening
% train(Partia) :-
%         strength(S),
%         holding(X),
%         weight(X, W),
%         (W =< S -> train_success(Partia, W) ; die), !.

% train(_) :-
%         write('Nie masz odpowiedniego obciążenia do tego ćwiczenia!'), nl.

% train_success(Partia, W) :-
%         lifted(Partia, L),
%         NewL is L + W,
%         retract(lifted(Partia, L)),
%         assert(lifted(Partia, NewL)),
%         write('Podniosłeś '), write(W), write(' kg na '), write(Partia), write('. Łącznie: '), write(NewL), write(' kg.'), nl,
%         check_goal.

% /* Cele gry
% check_goal :-
%         lifted(klatka_piersiowa, P),
%         lifted(barki, B),
%         lifted(biceps, C),
%         (P >= 500, B >= 300, C >= 200 -> write('Gratulacje! Ukończyłeś trening!'), nl, finish ; true).

% /* Wagi sprzętu
% weight(sztanga, 100).
% weight(hantle, 50).
% weight(kettlebell, 30).
% */

/* Nowy Trening */
take_bench :-
        i_am_at(strefa_wolnych_ciezarow),
        stage(CurrentStage),
        write('Current stage: '), write(CurrentStage), nl,
        (CurrentStage =:= 0 ->
            retract(stage(CurrentStage)),
            assert(stage(1)),
            start_stage(1)
        ;
        (member(CurrentStage, [1, 3, 5]) ->
                write('Wykonujesz to ćwiczenie teraz!'), nl
        ;
                write('Nie można wykonać takiej akcji!'), nl
        )
        ),
        !.


take_bench :-
        i_am_at(Place),
        \+ Place = strefa_wolnych_ciezarow,
        write('Nie jesteś w strefie wolnych ciężarów!'), nl.

left_add_weight_bench(X) :-
        i_am_at(strefa_wolnych_ciezarow),
        stage(CurrentStage),
        (member(CurrentStage, [1, 3, 5]) ->
                weight_inventory(WI),
                member(X, WI),
                select(X, WI, NewWI),
                retract(weight_inventory(WI)),
                assert(weight_inventory(NewWI)),
                bench_left(BL),
                append(BL, [X], NewBL),
                retract(bench_left(BL)),
                assert(bench_left(NewBL)),
                write('Dodałeś ciężar '), write(X), write(' kg na lewą stronę sztangi.'), nl,
                check_bench
        ;
                write('Nie można wykonać takiej akcji!'), nl
        ),
        !.

left_add_weight_bench(_) :-
        i_am_at(Place),
        \+ Place = strefa_wolnych_ciezarow,
        write('Nie jesteś w strefie wolnych ciężarów!'), nl.

left_add_weight_bench(X) :-
        i_am_at(strefa_wolnych_ciezarow),
        stage(CurrentStage),
        (member(CurrentStage, [1, 3, 5]) ->
                weight_inventory(WI),
                \+ member(X, WI),
                write('Nie masz takiego ciężaru!'), nl
        ;
                write('Nie można wykonać takiej akcji!'), nl
        ),
        !.

right_add_weight_bench(X) :-
        i_am_at(strefa_wolnych_ciezarow),
        stage(CurrentStage),
        (member(CurrentStage, [1, 3, 5]) ->
                weight_inventory(WI),
                member(X, WI),
                select(X, WI, NewWI),
                retract(weight_inventory(WI)),
                assert(weight_inventory(NewWI)),
                bench_right(BR),
                append(BR, [X], NewBR),
                retract(bench_right(BR)),
                assert(bench_right(NewBR)),
                write('Dodałeś ciężar '), write(X), write(' kg na prawą stronę sztangi.'), nl,
                check_bench
        ;
                write('Nie można wykonać takiej akcji!'), nl
        ),
        !.

right_add_weight_bench(_) :-
        i_am_at(Place),
        \+ Place = strefa_wolnych_ciezarow,
        write('Nie jesteś w strefie wolnych ciężarów!'), nl.

right_add_weight_bench(X) :-
        i_am_at(strefa_wolnych_ciezarow),
        stage(CurrentStage),
        (member(CurrentStage, [1, 3, 5]) ->
                weight_inventory(WI),
                \+ member(X, WI),
                write('Nie masz takiego ciężaru!'), nl
        ;
                write('Nie można wykonać takiej akcji!'), nl
        ),
        !.

check_bench :-
        i_am_at(strefa_wolnych_ciezarow),
        stage(CurrentStage),
        (   member(CurrentStage, [1, 3, 5]) ->
                bench_left(BL),
                bench_right(BR),
                sum_list(BL, WBL),
                sum_list(BR, WBR),
                TotalWeight is WBL + WBR + 20,
                write('Obciążenie na sztandze wynosi: '), write(TotalWeight), write(' kg.'), nl,

                (   WBL =:= WBR ->
                        write('Obciążenie na sztandze jest równo rozłożone!'), nl
                ;
                        Diff is abs(WBL - WBR),
                        (   Diff > 70 ->
                                write('Sztanga się przewaliła! Wszyscy się teraz z ciebie śmieją. Ze wstydu szybko uciekłeś z siłowni'), nl,
                                finish(0)
                        ;
                                write('Obciążenie na sztandze jest nierówno rozłożone, ale w granicach bezpieczeństwa.'), nl,
                                write('Różnica w obciążeniu wynosi: '), write(Diff), write(' kg.'), nl,
                                write('Ciężary na lewej stronie: '), write(BL), nl,
                                write('Ciężary na prawej stronie: '), write(BR), nl
                        )
                )
        ;
                write('Nie można wykonać takiej akcji!'), nl
        ),
        !.


remove_all_weight_bench :-
        i_am_at(strefa_wolnych_ciezarow),
        stage(CurrentStage),
        (member(CurrentStage, [1, 3, 5]) ->
                bench_left(BL),
                bench_right(BR),
                append(BL, BR, AllWeights),
                weight_inventory(WI),
                append(WI, AllWeights, NewWI),
                retract(weight_inventory(WI)),
                assert(weight_inventory(NewWI)),
                retract(bench_left(_)),
                retract(bench_right(_)),
                assert(bench_left([])),
                assert(bench_right([])),
                write('Zdjąłeś wszystkie ciężary ze sztangi!'), nl
        ;
                write('Nie można wykonać takiej akcji!'), nl
        ),
        !.


/* Podnoszenie ciężarów */
do_bench_press :-
        i_am_at(strefa_wolnych_ciezarow),
        stage(CurrentStage),
        (CurrentStage =:= 1 ->
                bench_left(BL),
                bench_right(BR),
                sum_list(BL, WBL),
                sum_list(BR, WBR),
                (WBL =\= WBR ->
                        write('Obciążenie na sztandze jest nierówno rozłożone!'), nl,
                        write('Nie możesz podnieść sztangi!'), nl,
                        !
                ;
                        true
                ),
                write('Podnosisz sztangę o obciążeniu '), write(WBL + WBR + 20), write(' kg.'), nl,
                strength(S),
                (WBL + WBR + 20 =< S ->
                        retract(stage(CurrentStage)),
                        assert(stage(CurrentStage + 1)),
                        retract(lifted(klatka_piersiowa, L)),
                        NewL is L + (WBL + WBR + 20),
                        assert(lifted(klatka_piersiowa, NewL)),
                        write('Podniosłeś sztangę o wadze'), write(WBL + WBR + 20), write(' kg!'), nl,
                        %check_goal
                        start_stage(CurrentStage + 1)
                ;
                        die
                )
        ;
                write('Nie można wykonać takiej akcji!'), nl
        ),
        !.


% STAGES

start_stage(X) :-
        X =:= 1 -> (
                assert(at(szczur_bojowy, strefa_wolnych_ciezarow)),
                assert(at(brunetka, strefa_maszyn)),
                assert(at(duzy_chlop, strefa_wolnych_ciezarow)),
                assert(npc(szczur_bojowy)),
                assert(npc(brunetka)),
                assert(npc(duzy_chlop)),
                write('Rozpoczynasz trening na klatę! Rozgladasz się obok sztangi, ale nie ma obok niej żadnych ciężarów.'), nl,
                write('Pora zebrać cięzary!'), nl,
                write('Rozejrzyj się po siłowni i przynieś ciężary, a następnie je nałóż!'), nl,
                write('Pamiętaj, że z dwóch stron trzeba mieć tyle samo na sztandze!'), nl,
                write('Im więcej podniesiesz, tym lepszy wynik zdobędziesz!'), nl,
                write('Powodzenia!'), nl
        );
        X =:= 2 -> (
                assert(npc(czlowiek_szczuply)),
                assert(at(czlowiek_szczuply, strefa_wolnych_ciezarow)),
                write('Gratulacje udało ci się wykonać pierwszą serię!'), nl,
                write('Kiedy opoczywasz po pierwszej serii, podchodzi do ciebie czlowiek_szczuply i z pytaniem:'), nl,
                write(' - Hej, ile zostało ci serii?'), nl,
                write(' - Jeszcze dwie.'), nl,
                write(' - Mogę w takim razie się dołączyć?'), nl,
                write(' - Jasne'), nl,
                write(' - Tak w ogóle nie widziałeś przypadkiem gdzieś na siłowni czerwonego bidonu, musiałem go wczoraj zostawić w szatni. Znalazcy z pewnością się odwdzięczę'), nl,
                (holding(czerwony_bidon) ->
                    write('Przypominasz sobie, że podniosłeś taki bidon w łazience'), nl,
                    write('Możesz teraz go oddać (give(czerwony_bidon, czlowiek_szczuply))'), nl
                ;
                    write(' - Niestety, nie widziałem go. Ale i tak teraz odpoczywam pójdę go poszukać (Powiedziałeś już myśląc o tym jaka nagroda cię czeka)'), nl,
                    write(' - Powodzenia!'), nl
                )
        );
        X =:= 3 -> (
                write('Gratulacje udało ci się wykonać drugą serię!'), nl,
                write('Wiesz jednak, że bez magnezji nie uda ci się wykonać kolejnej serii.'), nl,
                write('Musisz znaleźć magnezję!'), nl,
                write("Może ktoś na siłowni ci pomoże?"), nl
        ).


/* Śmierć */
die :-
        write('Podjąłeś próbę podniesienia zbyt dużego ciężaru i odniosłeś kontuzję. Koniec gry.'), nl,
        finish(0).

die_steroid :-
        write('To był twój ostatni trening. Zmarłeś na skutek przedawkowania sterydów.'), nl,
        finish(0).

/* Wyświetlanie otoczenia */
look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.

describe(dom) :-
        write('Jesteś w domu. Musisz zebrać ekwipunek na siłownię!'), nl.

describe(parking) :-
        write('Jesteś na parkingu przed siłownią. Możesz iść do siłowni!'), nl.

describe(recepcja) :-
        write('Jesteś w recepcji. Możesz kupić suplementy i karnet!'), nl.

describe(szatnia_meska) :-
        write('Jesteś w męskiej szatni. Możesz udać się do stref treningowych!'), nl,
        write('Widzisz, że jeden z pryszniców jest nieczynny, może chcesz sprawdzić co jest w środku?'), nl.

describe(szatnia_damska) :-
        write('Jesteś w damskiej szatni. Możesz udać się do stref treningowych!'), nl.

describe(strefa_maszyn) :-
        write('Jesteś w strefie maszyn.'), nl.

describe(strefa_wolnych_ciezarow) :-
        write('Jesteś w strefie wolnych ciężarów.'), nl.

describe(strefa_cardio) :-
        write('Jesteś w strefie cardio.'), nl.

describe(lazienka) :-
        write('Jesteś w łazience.'), nl.

notice_objects_at(Place) :-
        at(X, Place),
        write('Tutaj znajduje się '), write(X), write('.'), nl,
        fail.
notice_objects_at(_).

/* Pomoc */
instructions :-
    i_am_at(Place),
    write('Możesz wykonać następujące akcje:'), nl,
    (Place = dom ->
        write('- Zabrać przedmioty (take(X))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Pójść na parking przed siłownię (go(parking))'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl
    ; Place = parking ->
        write('- Porozmawiać z podejrzanym typem (talk(podejrzany_typ))'), nl,
        write('- Wejść do siłowni (go(recepcja))'), nl,
        write('- Wrócić do domu (go(dom))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl
    ; Place = recepcja ->
        write('- Porozmawiać z recepcjonistką (talk(recepcjonistka))'), nl,
        write('- Kupić przedmioty (buy(X))'), nl,
        write('- Wejść do szatni (go(szatnia_meska) lub go(szatnia_damska))'), nl,
        write('- Wyjść na parking przed siłownię (go(parking))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl

    ; Place = szatnia_meska ->
        write('- Udać się do strefy treningowej (go(strefa_wolnych_ciezarow), go(strefa_cardio))'), nl,
        write('- Wrócić na recepcję (go(recepcja))'), nl,
        write('- Sprawdzić nieczynny prysznic (go(nieczynny_prysznic))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl

    ; Place = strefa_wolnych_ciezarow ->
        write('- Podnieść ciężary (take(X))'), nl,
        write('- Trenować (train(Partia))'), nl,
        write('- Wrócić do szatni (go(szatnia_meska))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl

    ; Place = strefa_cardio ->
        write('- Wrócić do szatni (go(szatnia_meska))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl
    ), nl.


/* Start gry */
start :-
        write('Twoim celem jest odbycie treningu na siłowni.'), nl,
        write('Zbierz potrzebny ekwipunek z domu, wejdź na siłownię i wykonaj ćwiczenia!'), nl,
        random_strength_and_money,
        look.

finish :-
        write('Koniec gry! Dziękujemy za udział.'), nl,
        halt.

/* Zakończenie gry */
finish(score) :-
        (score =:= 1 -> write('Gratulacje! Wygrałeś!') ; write('Przegrałeś!')), nl,
        write('Dziękujemy za grę!'), nl,
        halt.

/* Debugging paths */
debug_paths :-
    findall((A, B), path(A, B), Paths),
    write('Available paths: '), write(Paths), nl.
