/* Trening na siłowni, by <Twoje imię> */

:- dynamic i_am_at/1, at/2, holding/1, strength/1, lifted/2.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(holding(_)), retractall(strength(_)), retractall(lifted(_, _)).

/* Lokacje */
i_am_at(dom).

path(dom, ulica).
path(ulica, szatnia_meska).
path(ulica, szatnia_damska).
path(szatnia_meska, strefa_maszyn).
path(szatnia_meska, strefa_wolnych_ciezarow).
path(szatnia_meska, strefa_cardio).
path(szatnia_damska, strefa_maszyn).
path(szatnia_damska, strefa_wolnych_ciezarow).
path(szatnia_damska, strefa_cardio).

/* Przedmioty */
at(strój_sportowy, dom).
at(woda, dom).
at(karnet, dom).
at(hantle, strefa_wolnych_ciezarow).
at(sztanga, strefa_wolnych_ciezarow).
at(kettlebell, strefa_wolnych_ciezarow).

/* Początkowa siła i podniesione ciężary */
random_strength :-
        random(50, 201, S),  % Siła w zakresie 50-200 kg
        assert(strength(S)),
        assert(lifted(klatka_piersiowa, 0)),
        assert(lifted(barki, 0)),
        assert(lifted(biceps, 0)).

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
        random_strength,
        look.

/* Zakończenie gry */
finish :-
        write('Koniec gry. Wpisz "halt." aby wyjść.'), nl.
