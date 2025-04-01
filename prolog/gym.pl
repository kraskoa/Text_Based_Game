/* "Klata, biceps, barki", by Mateusz Daszewski, Adam Kraś, Krzysztof Król */

:- dynamic i_am_at/1, at/2, holding/1, strength/1, lifted/2, money/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)), retractall(strength(_)), retractall(lifted(_, _)).

/* Początkowa siła, podniesione ciężary oraz pieniądze */
random_strength_and_money :-
    random(50, 201, S),  % Siła w zakresie 50-200 kg
    random(0, 51, M), % Pieniądze w zakresie 100 - 1000 zł
    assert(strength(S)),
    assert(money(M)),
    assert(lifted(klatka_piersiowa, 0)),
    assert(lifted(barki, 0)),
    assert(lifted(biceps, 0)).

/* Przedmioty do kupienia na recepcji */
item_price(monster, 5).
item_price(przedtreningowka, 15).

/* Zbieranie pieniędzy z początkową wartością */
check_money :-
    money(M),
    write('Masz '), write(M), write(' zł na koncie.'), nl.

/* Zakup przedmiotów */
buy_item(monster) :-
    money(M),
    item_price(monster, Price),
    M >= Price,
    NewMoney is M - Price,
    retract(money(M)),
    assert(money(NewMoney)),
    increase_strength(3), % Zwiększamy siłę o 3
    write('Kupiono Monster. Siła wzrosła o 3!'), nl,
    write('Pozostało Ci '), write(NewMoney), write(' zł.'), nl.

buy_item(przedtreningowka) :-
    money(M),
    item_price(przedtreningowka, Price),
    M >= Price,
    NewMoney is M - Price,
    retract(money(M)),
    assert(money(NewMoney)),
    increase_strength(10), % Zwiększamy siłę o 10
    write('Kupiono przedtreningówkę. Siła wzrosła o 10!'), nl,
    write('Pozostało Ci '), write(NewMoney), write(' zł.'), nl.

buy_item(_) :-
    write('Nie masz wystarczająco pieniędzy!'), nl.

/* Zwiększanie siły */
increase_strength(Value) :-
    strength(S),
    NewStrength is S + Value,
    retract(strength(S)),
    assert(strength(NewStrength)),
    write('Twoja siła wzrosła do '), write(NewStrength), write(' kg!'), nl.

/* Lokacje */
i_am_at(dom).

/* Ścieżki między lokacjami (dwustronne) */
path(dom, ulica).
path(ulica, dom).
path(ulica, recepcja).
path(recepcja, ulica).
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

available_paths :-
    i_am_at(Here),
    findall(Direction, path(Here, Direction, _), Directions),
    (Directions \= [] -> 
        write('Możesz iść w kierunkach: '), write(Directions), nl
    ; 
        write('Nie ma żadnych dostępnych ścieżek z tej lokacji.'), nl).


/* Przedmioty */
at(strój_sportowy, dom).
at(woda, dom).
at(karnet, dom).
at(hantle, strefa_wolnych_ciezarow).
at(sztanga, strefa_wolnych_ciezarow).
at(kettlebell, strefa_wolnych_ciezarow).

/* Podnoszenie przedmiotów */
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


/* Ruch */
go(Direction) :-
        i_am_at(Here),
        path(Here, There),
        There == Direction,
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('Nie możesz tam pójść!'), nl.

/* Wejście na siłownię */
enter(szatnia_meska) :-
        holding(karnet),
        write('Wchodzisz do szatni męskiej!'), nl,
        retract(i_am_at(ulica)),
        assert(i_am_at(szatnia_meska)),
        look.

enter(szatnia_damska) :-
        write('Podglądacze nie są tolerowani! Zostałeś wyrzucony z siłowni! Koniec gry.'), nl,
        finish.

enter(_) :-
        write('Nie masz karnetu!'), nl.

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

describe(ulica) :-
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

/* Start gry */
start :-
        write('Twoim celem jest odbycie treningu na siłowni.'), nl,
        write('Zbierz potrzebny ekwipunek z domu, wejdź na siłownię i wykonaj ćwiczenia!'), nl,
        random_strength_and_money,
        look.

/* Zakończenie gry */
finish :-
        write('Koniec gry. Wpisz "halt." aby wyjść.'), nl.
