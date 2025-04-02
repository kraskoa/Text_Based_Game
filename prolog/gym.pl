/* "Klata, biceps, barki", by Mateusz Daszewski, Adam Kraś, Krzysztof Król */

:- dynamic i_am_at/1, at/2, holding/1, strength/1, lifted/2, money/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)), retractall(strength(_)), retractall(lifted(_, _)).

/* Początkowa lokalizacja */
i_am_at(dom).

/* Początkowa siła, podniesione ciężary oraz pieniądze */
random_strength_and_money :-
        random(50, 151, S),  % Siła w zakresie 50-150 kg
        random(0, 51, M), % Pieniądze w zakresie 0 - 50 zł
        assert(strength(S)),
        assert(money(M)),
        assert(lifted(klatka_piersiowa, 0)),
        assert(lifted(barki, 0)),
        assert(lifted(biceps, 0)).

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


/* Zbieranie pieniędzy z początkową wartością */
check_money :-
        money(M),
        write('Masz '), write(M), write(' zł na koncie.'), nl.

/* Zakup przedmiotów */
buy(monster) :-
        money(M),
        item_price(monster, Price),
        M >= Price,
        NewMoney is M - Price,
        retract(money(M)),
        assert(money(NewMoney)),
        assert(holding(monster)),
        write('Kupiono Monster.'), nl,
        write('Pozostało Ci '), write(NewMoney), write(' zł.'), nl.

buy(przedtreningowka) :-
        money(M),
        item_price(przedtreningowka, Price),
        M >= Price,
        NewMoney is M - Price,
        retract(money(M)),
        assert(money(NewMoney)),
        assert(holding(przedtreningowka)),
        write('Kupiono przedtreningówkę.'), nl,
        write('Pozostało Ci '), write(NewMoney), write(' zł.'), nl.

buy(karnet) :-
        money(M),
        item_price(karnet, Price),
        M >= Price,
        NewMoney is M - Price,
        retract(money(M)),
        assert(money(NewMoney)),
        assert(holding(karnet)),
        write('Kupiono karnet.'), nl,
        write('Pozostało Ci '), write(NewMoney), write(' zł.'), nl.

buy(mala_strzykawka) :-
        money(M),
        item_price(mala_strzykawka, Price),
        M >= Price,
        NewMoney is M - Price,
        retract(money(M)),
        assert(money(NewMoney)),
        assert(holding(mala_strzykawka)),
        write('Kupiono małą strzykawkę.'), nl,
        write('Zastanów się dwa razy czy na pewno chcesz jej użyć!'), nl,
        write('Pozostało Ci '), write(NewMoney), write(' zł.'), nl.

buy(duza_strzykawka) :-
        money(M),
        item_price(duza_strzykawka, Price),
        M >= Price,
        NewMoney is M - Price,
        retract(money(M)),
        assert(money(NewMoney)),
        assert(holding(duza_strzykawka)),
        write('Kupiono dużą strzykawkę.'), nl,
        write('Zastanów się dwa razy czy na pewno chcesz jej użyć!'), nl,
        write('Pozostało Ci '), write(NewMoney), write(' zł.'), nl.

buy(_) :-
        write('Nie masz wystarczająco pieniędzy!'), nl.

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

/* Przedmioty */
at(strój_sportowy, dom).
at(woda, dom).
at(karnet, dom).
at(recepcjonistka, recepcja).
at(podejrzany_typ, parking).
at(hantle, strefa_wolnych_ciezarow).
at(sztanga, strefa_wolnych_ciezarow).
at(kettlebell, strefa_wolnych_ciezarow).

/* Podnoszenie przedmiotów */

take(X) :-
        npc(X),
        write('Nie możesz podnieść '), write(X), write('! To osoba!'), nl, !.

take(X) :-
        holding(X),
        write('Masz już '), write(X), write('!'), nl, !.

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
        write('Twój ekwipunek jest pusty!'), nl).

/* Spożywanie suplementów */
consume(X) :-
        holding(X),
        retract(holding(X)),
        write('Spożywasz '), write(X), write('.'), nl,
        (X = monster -> increase_strength(3) 
        ; X = przedtreningowka -> increase_strength(10) 
        ; X = mala_strzykawka -> (random(1, 5, R), (R =:= 1 -> die ; increase_strength(30))) 
        ; X = duza_strzykawka -> (random(1, 5, R), (R =:= 1 -> die ; increase_strength(60))) 
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


/* Ruch */
go(Direction) :-
        i_am_at(Here),
        path(Here, Direction),
        (Direction = szatnia_meska ->
            (holding(karnet) ->
                write('Wchodzisz do szatni męskiej!'), nl,
                retract(i_am_at(Here)),
                assert(i_am_at(Direction)),
                look
            ;
                write('Nie masz karnetu!'), nl
            )
        ; Direction = szatnia_damska ->
            (write('Podglądacze nie są tolerowani! Zostałeś wyrzucony z siłowni! Koniec gry.'), nl, finish)
        ;
            retract(i_am_at(Here)),
            assert(i_am_at(Direction)),
            look
        ), !.

go(_) :-
        write('Nie możesz tam pójść!'), nl.


/* Trening */
train(Partia) :-
        strength(S),
        holding(X),
        weight(X, W),
        (W =< S -> train_success(Partia, W) ; die), !.

train(_) :-
        write('Nie masz odpowiedniego obciążenia do tego ćwiczenia!'), nl.

train_success(Partia, W) :-
        lifted(Partia, L),
        NewL is L + W,
        retract(lifted(Partia, L)),
        assert(lifted(Partia, NewL)),
        write('Podniosłeś '), write(W), write(' kg na '), write(Partia), write('. Łącznie: '), write(NewL), write(' kg.'), nl,
        check_goal.

/* Cele gry */
check_goal :-
        lifted(klatka_piersiowa, P),
        lifted(barki, B),
        lifted(biceps, C),
        (P >= 500, B >= 300, C >= 200 -> write('Gratulacje! Ukończyłeś trening!'), nl, finish ; true).

/* Wagi sprzętu */
weight(sztanga, 100).
weight(hantle, 50).
weight(kettlebell, 30).

/* Śmierć */
die :-
        write('Podjąłeś próbę podniesienia zbyt dużego ciężaru i odniosłeś kontuzję. Koniec gry.'), nl,
        finish.

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
        write('Jesteś na ulicy. Możesz iść do siłowni!'), nl.

describe(szatnia_meska) :-
        write('Jesteś w męskiej szatni. Możesz udać się do stref treningowych!'), nl.

describe(szatnia_damska) :-
        write('Jesteś w damskiej szatni. Możesz udać się do stref treningowych!'), nl.

describe(strefa_maszyn) :-
        write('Jesteś w strefie maszyn.'), nl.

describe(strefa_wolnych_ciezarow) :-
        write('Jesteś w strefie wolnych ciężarów.'), nl.

describe(strefa_cardio) :-
        write('Jesteś w strefie cardio.'), nl.

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
        write('- Porozmawiać z podejrzanym typem'), nl,
        write('- Wejść do siłowni (go(recepcja))'), nl,
        write('- Wrócić do domu (go(dom))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl
    ; Place = recepcja ->
        write('- Porozmawiać z recepcjonistką'), nl,
        write('- Kupić przedmioty (buy(X))'), nl,
        write('- Wejść do szatni (go(szatnia_meska) lub go(szatnia_damska))'), nl,
        write('- Wyjść na parking przed siłownię (go(parking))'), nl,
        write('- Sprawdzić ekwipunek (inventory)'), nl,
        write('- Sprawdzić pieniądze (check_money)'), nl,
        write('- Sprawdzić jakie przedmioty i osoby znajdują się w okolicy (look)'), nl

    ; Place = szatnia_meska ->
        write('- Udać się do strefy treningowej (go(strefa_wolnych_ciezarow), go(strefa_cardio))'), nl,
        write('- Wrócić na recepcję (go(recepcja))'), nl,
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

/* Zakończenie gry */
finish(score) :-
        (score =:= 1 -> write('Gratulacje! Wygrałeś!') ; write('Przegrałeś!')), nl,
        write('Dziękujemy za grę!'), nl,
        halt.

/* Debugging paths */
debug_paths :-
    findall((A, B), path(A, B), Paths),
    write('Available paths: '), write(Paths), nl.
