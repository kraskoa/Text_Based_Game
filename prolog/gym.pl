/* "Klata, biceps, barki", by Mateusz Daszewski, Adam Kraś, Krzysztof Król */

:- dynamic i_am_at/1, at/2, holding/1, strength/1, lifted/2, money/1, stage/1, score/1, weight_inventory/1, bench_left/1, bench_right/1, npc/1, interaction_count/2.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)), retractall(strength(_)), retractall(lifted(_, _)), retractall(weight_inventory(_)), retractall(bench_left(_)), retractall(bench_right(_)).
:- discontiguous talk/1.

/* Początkowa lokalizacja */
i_am_at(dom).

/* Początkowa siła, podniesione ciężary oraz pieniądze */
random_strength_and_money :-
        random(50, 101, S),  % Siła w zakresie 50-150 kg
        random(0, 151, M),
        assert(strength(S)),
        assert(money(M)),
        assert(lifted(klatka_piersiowa, 0)),
        assert(lifted(barki, 0)),
        assert(lifted(biceps, 0)),
        assert(stage(0)),
        assert(score(0)),
        assert(weight_inventory(_{})),
        assert(bench_left([])),
        assert(bench_right([])).


/* Dodawanie ciężarów do ekwipunku */
add_weights(WeightsToAdd) :-
    retract(weight_inventory(CurrentWeights)),
    update_weights(CurrentWeights, WeightsToAdd, NewWeights),
    assert(weight_inventory(NewWeights)).

update_weights(CurrentWeights, [], CurrentWeights).
update_weights(CurrentWeights, [Weight | Rest], NewWeights) :-
    (get_dict(Weight, CurrentWeights, Count) ->
        NewCount is Count + 1,
        put_dict(Weight, CurrentWeights, NewCount, TempWeights)
    ;
        put_dict(Weight, CurrentWeights, 1, TempWeights)
    ),
    update_weights(TempWeights, Rest, NewWeights).

/* Usuwanie ciężarów z ekwipunku */
remove_weights(WeightsToRemove) :-
    retract(weight_inventory(CurrentWeights)),
    subtract_weights(CurrentWeights, WeightsToRemove, NewWeights),
    assert(weight_inventory(NewWeights)).

subtract_weights(CurrentWeights, [], CurrentWeights).
subtract_weights(CurrentWeights, [Weight | Rest], NewWeights) :-
    (get_dict(Weight, CurrentWeights, Count) ->
        (Count = 1 ->
            del_dict(Weight, CurrentWeights, _, TempWeights)
        ;
            NewCount is Count - 1,
            put_dict(Weight, CurrentWeights, NewCount, TempWeights)
        )
    ;
        TempWeights = CurrentWeights
    ),
    subtract_weights(TempWeights, Rest, NewWeights).

/* Wyświetlanie ekwipunku ciężarów */
show_weight_inventory :-
    weight_inventory(Weights),
    (Weights = _{} ->
        write('Ekwipunek ciężarów jest pusty.'), nl
    ;
        write('Ciężary w twoim ekwipunku: '), nl,
        show_weights(Weights)
    ).

show_weights(Weights) :-
    dict_pairs(Weights, _, Pairs),
    show_pairs(Pairs).

show_pairs([]).
show_pairs([Weight-Count | Rest]) :-
    write(Count), write('x '), write(Weight), write('kg'), nl,
    show_pairs(Rest).


/* Postacie poboczne */
npc(recepcjonistka).
npc(podejrzany_typ).
npc(szczur_bojowy).
npc(brunetka).
npc(duzy_chlop).
npc(wielki_chlop).
npc(czlowiek_szczuply).
npc(chudy_szczur).

/* Przedmioty do kupienia na recepcji */
item_price(monster, 10).
item_price(przedtreningowka, 20).
item_price(karnet, 40).

/* Przedmioty do kupienia u podejrzanego typa na parkingu */
item_price(mala_strzykawka, 30).
item_price(duza_strzykawka, 50).

/* Rozmowa z NPC */
talk(swiezak) :-
    i_am_at(strefa_wolnych_ciezarow),
    stage(CurrentStage),
    (CurrentStage =:= 2 ->
        (holding(czerwony_bidon) ->
            write('Ty: Cześć, stary, znalazłem twój czerwony bidon w łazience.'), nl,
            write('Swiezak: Dzięki, stary! Nie wiem co bym bez ciebie zrobił!'), nl,
            retract(holding(czerwony_bidon)),
            write('Ty: Nie ma sprawy!'), nl,
            retract(stage(2)),
            assert(stage(3)),
            start_stage(3)
        ;
            write('Swiezak: Ktoś widział mój czerwony bidon?'), nl,
            write('Ty: ...'), nl
        )
    ;
    CurrentStage =:= 3 ; CurrentStage =:= 4 ->
        write('Swiezak: Cześć! Jak tam trening?'), nl
    ;
        write('Nie widzisz tutaj swiezak.'), nl
    ), !.

talk(NPC) :-
    i_am_at(Place),
    at(NPC, Place),
    npc(NPC),
    (stage(0), \+ member(NPC, [recepcjonistka, podejrzany_typ]) ->
        write('Nie czas na rozmowy, zajmij ławkę (take_bench.) w strefie wolnych ciężarów.'), nl
    ;
        interact(NPC)
    ),
    !.

talk(NPC) :-
    write('Nie widzisz tutaj '), write(NPC), write('.'), nl.



% Initialize interaction count for all NPCs
initialize_interaction_counts :-
    forall(npc(NPC), assert(interaction_count(NPC, 0))).

% Increment interaction count for an NPC
increment_interaction_count(NPC) :-
    interaction_count(NPC, Count),
    NewCount is Count + 1,
    retract(interaction_count(NPC, Count)),
    assert(interaction_count(NPC, NewCount)).

decrement_interaction_count(NPC) :-
    interaction_count(NPC, Count),
    NewCount is Count - 1,
    retract(interaction_count(NPC, Count)),
    assert(interaction_count(NPC, NewCount)).


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

interact(chudy_szczur) :-
    increment_interaction_count(chudy_szczur),
    interaction_count(chudy_szczur, Count),

    (Count =:= 1 ->
        write('Ty: Ej, mały, potrzebuję tych talerzy 5kg, mogę je zabrać?'), nl,
        write('Chudy szczur: (piszczy nerwowo) Tylko... nie bij mnie! Bierz, co chcesz, i znikaj!'), nl,
        write('Ty: Spoko, luz. Tylko mi je podaj, i będę się zwijał.'), nl,
        write('Chudy szczur: (podaje talerze) Masz, i już mnie nie zaczepiaj!'), nl,
        add_weights([5, 5]),
        write('Dodałeś do swojego ekwipunku ciężary: 5 kg i 5 kg.'), nl
    ; Count =:= 2 ->
        strength(S),
        (S >= 100 ->
            write('Chudy szczur: (przestraszony) Czego ode mnie chcesz?! Pieniądzy?'), nl,
            write('Chudy szczur wyciąga 50 zł z etui do telefonu'), nl,
            write('Nie chcesz brać pieniędzy od szczura, ale patrząc na niego, obawiasz się jak może zareagować jak nie przyjmiesz pieniędzy.'), nl,
            NewMoney is M + 50,
            retract(money(M)),
            assert(money(NewMoney))
        ;
            write('Zostaw mnie w spokoju!'), nl,
            decrement_interaction_count(chudy_szczur)
        )
    ; Count >= 3 ->
        write('Postanawiasz jednak nie podchodzić do chudego szura, obawiając się o jego zdrowie'), nl,
        decrement_interaction_count(chudy_szczur)
    ).

interact(brunetka) :-
    write('Ty: Cześć, przepraszam cię najmocniej, że przeszkadzam, ale czy będziesz jeszcze używać tych 10kg?'), nl,
    write('Brunetka: Zostaw mnie, mam chłopaka!'), nl,
    write('Ty: Źle zrozumiałaś, ja tylko chciałem...'), nl,
    write('Brunetka: Mam wezwać ochronę? Spadaj!'), nl.

interact(czlowiek_szczuply) :-
    increment_interaction_count(czlowiek_szczuply),
    interaction_count(czlowiek_szczuply, Count),

    (Count =:= 1 ->
        write('Ty: Stary, te talerze 15kg... mogę je pożyczyć? Na chwilę?'), nl,
        write('Człowiek szczupły: (wzrusza ramionami) No dobra, ale szybko oddaję. Ja tu jeszcze muszę poćwiczyć.'), nl,
        write('Ty: Jasne, jasne, tylko je wezmę. Dzięki!'), nl,
        write('Człowiek szczupły: Tylko je odłóż na miejsce, bo będziesz miał ze mną do czynienia!'), nl,
        add_weights([15, 15]),
            write('Dodałeś do swojego ekwipunku ciężary: 15 kg i 15 kg.'), nl
        ;
            write('Człowiek szczupły: Odłożyłeś talerze już na miejsce?.'), nl,
            write('Ty: Jeszcze nie skończyłem '), nl).

interact(szczur_bojowy) :-
    increment_interaction_count(szczur_bojowy),
    interaction_count(szczur_bojowy, Count),

    (Count =:= 1 ->
        write('Ty: Ej, byczku, te 20 kilo... mogę je zabrać?'), nl,
        write('Szczur bojowy: Jasne, stary, bierz. I tak mi się już nie przydadzą.'), nl,
        write('Ty: Dzięki, jesteś wielki!'), nl,
        write('Szczur bojowy: Tylko uważaj, żebyś sobie krzywdy nie zrobił. To ciężkie żelastwo.'), nl,
        add_weights([20, 20]),
        write('Dodałeś do swojego ekwipunku ciężary: 20 kg i 20 kg.'), nl
        ;
            write('Szczur bojowy: Czego?.'), nl,
            write('Ty: ...'), nl).

interact(duzy_chlop) :-
    increment_interaction_count(duzy_chlop),
    interaction_count(duzy_chlop, Count),

    (Count =:= 1 ->
        write('Ty: Mogę zabrać talerze 25 kg?'), nl,
        write('Duży chłop: Spoko, nie ma sprawy, już ich nie używam!'), nl,
        add_weights([25, 25]),
        write('Dodałeś do swojego ekwipunku ciężary: 25 kg i 25 kg.'), nl
        ;
            write('Nie chcesz zawaracać głowy dużemu chłopowi bez powodu'), nl).



interact(wielki_chlop) :-
    increment_interaction_count(wielki_chlop),
    interaction_count(wielki_chlop, Count),

    (Count =:= 0 ->
        write('Ty: Mogę zabrać talerze...'), nl,
        write('Wielki chłop: Spadaj szczurze!'), nl
    ; Count =:= 1 ->
        write('Ty: Ej, serio, potrzebuję tych talerzy. Możesz mi je dać?'), nl,
        write('Wielki chłop: Jeszcze jedno słowo i pożałujesz!'), nl
    ; Count =:= 2 ->
        write('Próbujesz zagadać do wielkiego chłopa jeszcze raz.'), nl,
        write('To był błąd, za nim zdążysz się zorientować, dostajesz od niego cios w brzuch.'), nl,
        write('Nie masz szans przetrwać następnych uderzeń.'), nl,
        die,
        nl,
        write('Przyszła właśnie jego dziewczyna i odwróciło to jego uwagę od ciebie.'), nl,
        write('Udało ci się przeżyć!'), nl
    ; Count >= 3 ->
        write('Postanawiasz nie zaczepiać wielkiego chłopa więcej, obawiając się o swoje zdrowie.'), nl
    ).



/* Zbieranie pieniędzy z początkową wartością */
check_money :-
        money(M),
        write('Masz '), write(M), write(' zł na koncie.'), nl.

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
path(szatnia_meska, strefa_maszyn).
path(strefa_cardio, szatnia_meska).
path(strefa_maszyn, szatnia_meska).
path(szatnia_damska, strefa_maszyn).
path(szatnia_damska, strefa_cardio).
path(strefa_cardio, strefa_maszyn).
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
at(chudy_szczur, strefa_wolnych_ciezarow).
at(brunetka, strefa_cardio).
at(wielki_chlop, strefa_wolnych_ciezarow).
at(czlowiek_szczuply, strefa_maszyn).
at(duzy_chlop, strefa_maszyn).
at(szczur_bojowy, strefa_maszyn).
at(lawka, strefa_wolnych_ciezarow).

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
    (Items \= [] ->
        write('Twój ekwipunek: '), write(Items), nl
    ;
        write('Twój ekwipunek jest pusty!'), nl
    ),
    show_weight_inventory.

/* Spożywanie suplementów */
consume(X) :-
        holding(X),
        retract(holding(X)),
        write('Spożywasz '), write(X), write('.'), nl,
        (X = monster -> increase_strength(3)
        ; X = dzik -> increase_strength(5)
        ; X = przedtreningowka -> increase_strength(10)
        ; X = mala_strzykawka -> (increase_strength(30), random(1, 5, R), (R =:= 1 -> die_steroid; true))
        ; X = duza_strzykawka -> (increase_strength(60), random_between(1, 1, R), (R =:= 1 -> die_steroid ; true))
        ; true),
        (X = monster -> write('Twoja siła wzrosła o 3!')
        ; X = dzik -> write('Twoja siła wzrosła o 5!')
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
                (write('Podglądacze nie są tolerowani! Zostałeś wyrzucony z siłowni! Koniec gry.'), nl, die, write('Tak naprawdę nie wszedłeś do szatni damskiej, tylko do pomieszczenia dla personelu nazwanego szatnia_damska!'), nl)
        ; Direction = strefa_cardio ->
                stage(CurrentStage),
                (CurrentStage =:= 4 ->
                        retract(stage(CurrentStage)),
                        assert(stage(5)),
                        write('Przechodzisz do strefy cardio. Zapatrujesz się na ćwiczące się osoby.'), nl,
                        write('Nawet nie zauważasz kiedy mija czas na kolejną serię!'), nl,
                        retract(i_am_at(Here)),
                        assert(i_am_at(Direction)),
                        look
                ;
                        retract(i_am_at(Here)),
                        assert(i_am_at(Direction)),
                        look
                )
    ; Direction = nieczynny_prysznic ->
        idz_do_prysznica
    ; member(Direction, [strefa_wolnych_ciezarow, strefa_cardio, strefa_maszyn]) ->
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
        write('Wypij zdobyty napój i poczuj przypływ siły!'), nl,
        write('Automatycznie wracasz do szatni.'), nl,
        retract(i_am_at(_)),
        assert(i_am_at(szatnia_meska)),
        assert(prysznic_sprawdzony),
        look, !.

idz_do_prysznica :-
        write('Nie możesz tego tutaj zrobić!'), nl.

/* Deklaracja dynamicznego faktu */
:- dynamic prysznic_sprawdzony/0.

/* Używanie nowego systemu ciężarów */
left_add_weight_bench(X) :-
    i_am_at(strefa_wolnych_ciezarow),
    stage(CurrentStage),
    (member(CurrentStage, [1, 3, 5]) ->
        remove_weights([X]),
        bench_left(BL),
        append(BL, [X], NewBL),
        retract(bench_left(BL)),
        assert(bench_left(NewBL)),
        write('Dodałeś ciężar '), write(X), write(' kg na lewą stronę sztangi.'), nl,
        check_bench
    ;
        write('Nie można wykonać takiej akcji!'), nl
    ), !.

right_add_weight_bench(X) :-
    i_am_at(strefa_wolnych_ciezarow),
    stage(CurrentStage),
    (member(CurrentStage, [1, 3, 5]) ->
        remove_weights([X]),
        bench_right(BR),
        append(BR, [X], NewBR),
        retract(bench_right(BR)),
        assert(bench_right(NewBR)),
        write('Dodałeś ciężar '), write(X), write(' kg na prawą stronę sztangi.'), nl,
        check_bench
    ;
        write('Nie można wykonać takiej akcji!'), nl
    ), !.

remove_all_weight_bench :-
    i_am_at(strefa_wolnych_ciezarow),
    stage(CurrentStage),
    (member(CurrentStage, [1, 3, 5]) ->
        bench_left(BL),
        bench_right(BR),
        append(BL, BR, AllWeights),
        add_weights(AllWeights),
        retract(bench_left(_)),
        retract(bench_right(_)),
        assert(bench_left([])),
        assert(bench_right([])),
        write('Zdjąłeś wszystkie ciężary ze sztangi!'), nl
    ;
        write('Nie można wykonać takiej akcji!'), nl
    ), !.

/* Pozostałe predykaty */
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
    ), !.

take_bench :-
    i_am_at(Place),
    \+ Place = strefa_wolnych_ciezarow,
    write('Nie jesteś w strefie wolnych ciężarów!'), nl.

check_bench :-
    i_am_at(strefa_wolnych_ciezarow),
    stage(CurrentStage),
    ( member(CurrentStage, [1, 3, 5]) ->
        bench_left(BL),
        bench_right(BR),
        sum_list(BL, WBL),
        sum_list(BR, WBR),
        TotalWeight is WBL + WBR + 20,
        write('Obciążenie na sztandze wynosi: '), write(TotalWeight), write(' kg.'), nl,

        ( WBL =:= WBR ->
            write('Obciążenie na sztandze jest równo rozłożone!'), nl
        ;
            Diff is abs(WBL - WBR),
            ( Diff > 53 ->
                write('Sztanga się przewaliła! Wszyscy się teraz z ciebie śmieją. Uciekasz z siłowni'), nl,
                die,
                write('Duży chłop powstrzymuje cię przed ucięciem!'), nl,
                write('I pomaga ci podnieść ciężary!'), nl,
                remove_all_weight_bench,
                write('(ciężary  teraz w twoim ekwipunku)'), nl
            ;
                write('Obciążenie na sztandze jest nierówno rozłożone, ale w granicach bezpieczeństwa.'), nl,
                write('Różnica w obciążeniu wynosi: '), write(Diff), write(' kg.'), nl,
                write('Ciężary na lewej stronie: '), write(BL), nl,
                write('Ciężary na prawej stronie: '), write(BR), nl
            )
        )
    ;
        write('Nie można wykonać takiej akcji!'), nl
    ), !.

/* Podnoszenie ciężarów */
do_bench_press :-
    i_am_at(strefa_wolnych_ciezarow),
    stage(CurrentStage),
    member(CurrentStage, [1, 3, 5]) -> (
        bench_left(BL),
        bench_right(BR),
        sum_list(BL, WBL),
        sum_list(BR, WBR),
        (WBL =\= WBR ->
            write('Obciążenie na sztandze jest nierówno rozłożone!'), nl,
            write('Nie możesz podnieść takiej sztangi!'), nl
        ;
            write('Podnosisz sztangę o obciążeniu '), write(WBL + WBR + 20), write(' kg.'), nl,
            strength(S),
            (WBL + WBR + 20 =< S ->
                retract(stage(CurrentStage)),
                NextStage is CurrentStage + 1,
                assert(stage(NextStage)),
                retract(lifted(klatka_piersiowa, L)),
                NewL is L + (WBL + WBR + 20),
                assert(lifted(klatka_piersiowa, NewL)),
                start_stage(NextStage)
            ;
                write('Obciążenie cię przygniata to będzie twój koniec!'), nl,
                die,
                write('Przechodzi obok ciebie wielki chłop i jedną ręką podnosi sztangę ratując cię przed marnym końcem!'), nl,
                write('Nie udało ci się wykonać ćwiczenia!'), nl
            )
        )
    ) ; (
        write('Nie można wykonać takiej akcji!'), nl
    ), !.

/* Magnezja */
has_magnesium :-
    holding(magnesium).


% STAGES
start_stage(X) :-
        X =:= 1 -> (
                write('Rozpoczynasz trening na klatę! Rozgladasz się obok sztangi, ale nie ma obok niej żadnych ciężarów.'), nl,
                write('Pora zebrać cięzary!'), nl,
                write('Rozejrzyj się po siłowni i przynieś ciężary, a następnie je nałóż!'), nl,
                write('Jeżeli nie znajdziesz nieużywanego sprzętu, możesz spróbować zapytać o niego innych ćwiczących'), nl,
                write('Pamiętaj, że z dwóch stron trzeba mieć tyle samo na sztandze!'), nl,
                write('Im więcej podniesiesz, tym lepszy wynik zdobędziesz!'), nl,
                write('Powodzenia!'), nl
        );
        X =:= 2 -> (
                assert(npc(swiezak)),
                assert(at(swiezak, strefa_wolnych_ciezarow)),
                write('Gratulacje udało ci się wykonać pierwszą serię!'), nl,
                write('Kiedy odpoczywasz po pierwszej serii, podchodzi do ciebie jakiś przeciętnie zbudowany chłopak z pytaniem:'), nl,
                write(' Swiezak: Hej, ile zostało ci serii?'), nl,
                write(' Ty: Jeszcze dwie.'), nl,
                write(' Swiezak: Mogę w takim razie się dołączyć?'), nl,
                write(' Ty: Jasne'), nl,
                write(' Swiezak: Tak w ogóle nie widziałeś przypadkiem gdzieś na siłowni czerwonego bidonu, musiałem go wczoraj zostawić w szatni. Znalazcy z pewnością się odwdzięczę'), nl,
                (holding(czerwony_bidon) ->
                write('Przypominasz sobie, że podniosłeś taki bidon w łazience'), nl,
                write('Podajesz mu bidon'), nl,
                        write(' Swiezak: O, dzięki! Nie wiem co bym bez ciebie zrobił!'), nl,
                        retract(holding(czerwony_bidon)), nl,
                        write(' Ty: Nie ma sprawy!'), nl,
                        retract(stage(2)),
                        assert(stage(3)),
                        start_stage(3)
                ;
                write(' Ty: Niestety, nie widziałem go. Ale i tak teraz odpoczywam pójdę go poszukać (Powiedziałeś już myśląc o tym jaka nagroda cię czeka)'), nl,
                write(' Swiezak: Powodzenia!'), nl
                )
        );
        X =:= 3 -> (
                write(' Swiezak: W nagrodę trzymaj tą przedtreningówę'), nl,
                write(' Ty: Dzięki!'), nl,
                nl,
                write('Spojrzałeś na datę ważności przedtreningówki i okazało się, że jest przeterminowana'), nl,
                write('Nie chcąć się narzucać, postanowiłeś nie mówić o tym swojemu nowemu znajomemu'), nl,
                write('Jednak wiesz, że nie możesz jej użyć'), nl,
                nl,
                write('Po odnalezieniu bidonu, wracasz do strefy wolnych ciężarów i szykujesz się na drugą serię.'), nl
        );
        X =:= 4 -> (
                write('Gratulacje udało ci się wykonać drugą serię!'), nl,
                write('Podczas odopoczynku możesz przejść się po siłowni i porozmawiać z innymi ćwiczącymi.'), nl
        );
        X =:= 5 -> (
                nl
        );
        X =:= 6 -> (
                write('Gratulacje udało ci się wykonać trening!'), nl,
                retractall(score(_)),
                assert(score(1)),
                finish
        ).


/* Śmierć */

die :-
    write('Czy chcesz oszukać koniec gry? (tak. / nie. )'), nl,
    read(Odpowiedz),
    (Odpowiedz = tak ->
        write('Miałeś niesamowite szczęście.'), nl
    ;
        finish
    ).

die_steroid :-
        write('To był twój ostatni trening. Zmarłeś na skutek przedawkowania sterydów.'), nl,
        die,
        write("Obok przechodził sudent WUM idący na siłkę i Cię wskrzesił"), nl.

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
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl,
        write('- Kupić przedmioty (buy(X))'), nl,
        write('- Spożyć przedmioty (consume(X))'), nl
    ; Place = recepcja ->
        write('- Porozmawiać z recepcjonistką (talk(recepcjonistka))'), nl,
        write('- Kupić przedmioty (buy(X))'), nl,
        write('- Wejść do szatni (go(szatnia_meska) lub go(szatnia_damska))'), nl,
        write('- Wyjść na parking przed siłownię (go(parking))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl,
        write('- Spożyć przedmioty (consume(X))'), nl
    ; Place = szatnia_meska ->
        write('- Udać się do strefy treningowej (go(strefa_wolnych_ciezarow), go(strefa_cardio), go(strefa_maszyn))'), nl,
        write('- Udać się do łazienki (go(lazienka))'), nl,
        write('- Wrócić na recepcję (go(recepcja))'), nl,
        write('- Sprawdzić nieczynny prysznic (go(nieczynny_prysznic))'), nl,
        write('- Przebrać się w strój sportowy (wear(stroj_sportowy))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl,
        write('- Kupić przedmioty (buy(X))'), nl
    ; Place = strefa_wolnych_ciezarow ->
        write('- Podnieść ciężary (take(X))'), nl,
        write('- Trenować na ławce (take_bench)'), nl,
        write('- Wrócić do szatni (go(szatnia_meska))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl,
        write('- Dodać ciężar na lewą stronę sztangi (left_add_weight_bench(X))'), nl,
        write('- Dodać ciężar na prawą stronę sztangi (right_add_weight_bench(X))'), nl,
        write('- Zdjąć wszystkie ciężary ze sztangi (remove_all_weight_bench)'), nl,
        write('- Sprawdzić obciążenie na sztandze (check_bench)'), nl,
        write('- Wykonać trening (do_bench_press)'), nl,
        write('- Sprawdzić jakie ciężary masz w ekwipunku (weight_inventory)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl,
        write('- Porozmawiać z osobami w okolicy (talk(X))'), nl,
        write('- Spożyć przedmioty (consume(X))'), nl
    ; Place = strefa_maszyn ->
        write('- Podnieść ciężary (take(X))'), nl,
        write('- Wrócić do szatni (go(szatnia_meska))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl,
        write('- Porozmawiać z osobami w okolicy (talk(X))'), nl,
        write('- Spożyć przedmioty (consume(X))'), nl
    ; Place = strefa_cardio ->
        write('- Wrócić do szatni (go(szatnia_meska))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl,
        write('- Porozmawiać z osobami w okolicy (talk(X))'), nl,
        write('- Spożyć przedmioty (consume(X))'), nl
    ; Place = lazienka ->
        write('- Wrócić do szatni (go(szatnia_meska))'), nl,
        write('- Podnieść przedmioty (take(X))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl,
        write('- Spożyć przedmioty (consume(X))'), nl
    ), nl.


/* Start gry */
start :-
        write('Twoim celem jest odbycie treningu na siłowni.'), nl,
        write('Zbierz potrzebny ekwipunek z domu, wejdź na siłownię i wykonaj ćwiczenia!'), nl,
        random_strength_and_money,
        initialize_interaction_counts,
        look.

/* Zakończenie gry */
finish :-
    score(Value),
    (Value =:= 1 ->
        write('Gratulacje! Wygrałeś!'), nl,
        lifted(klatka_piersiowa, L),
        write('Twój wynik to: '), write(L), nl
    ;
        write('Przegrałeś!')
    ),
    nl,
    write('Dziękujemy za grę!'), nl,
    halt.

/* Debugging paths */
debug_paths :-
    findall((A, B), path(A, B), Paths),
    write('Available paths: '), write(Paths), nl.
