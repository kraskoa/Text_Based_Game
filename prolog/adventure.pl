/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, at/2, holding/1.
:- retractall(at(_, _)), retractall(i_am_at(_)), retractall(alive(_)).

i_am_at(someplace).

path(someplace, n, someplace).

at(thing, someplace).

/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


/* These rules define the direction letters as calls to go/1. */

n :- go(n).

s :- go(s).

e :- go(e).

w :- go(w).


/* This rule tells how to move in a given direction. */

go(Direction) :-
        i_am_at(Here),
        path(Here, Direction, There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        !, look.

go(_) :-
        write('You can''t go that way.').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        notice_objects_at(Place),
        nl.


/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        fail.

notice_objects_at(_).


/* This rule tells how to die. */

die :-
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look.


/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

describe(someplace) :- write('You are someplace.'), nl.


% Takie funkcje przydałpby się rozwinąć, część z nich już jest powyżej rozpisana
idz(Place).         % Move to a new location
rozmawiaj(Person).  % Talk to NPCs
podnies(Object).    % Pick up an item
odloz(Object).      % Drop an item
wypij(monsterek).   % Drink energy drink for a boost
ocen_sile.          % Check your strength

% Definiowanie lokacji
i_am_at(parking).
i_am_at(recepcja).
i_am_at(szatnia).
i_am_at(strefa_cardio).
i_am_at(wolne_ciezary).
i_am_at(maszyna_biceps).
i_am_at(sztanga).

% % Tutaj od czata jakieś cuda, można zaadaptować do naszej gry:
% % Define paths between locations
% path(parking, recepcja).
% path(recepcja, szatnia).
% path(recepcja, strefa_cardio).
% path(strefa_cardio, wolne_ciezary).
% path(szatnia, wolne_ciezary).
% path(wolne_ciezary, maszyna_biceps).
% path(wolne_ciezary, sztanga).

% idz(Place) :-
%         i_am_at(Current),
%         path(Current, Place),
%         retract(i_am_at(Current)),
%         assert(i_am_at(Place)),
%         look, !.

% idz(_) :-
%         write('Nie możesz tam iść.'), nl.

%         :- dynamic at/2, holding/1. % at(Item, Location), holding(Item)

% % Items in the game world
% at(karnet, recepcja).
% at(hantle_20kg, wolne_ciezary).
% at(ciężary_10kg, sztanga).
% at(monsterek, szatnia).

% :- dynamic carrying_weight/1.
% carrying_weight(0).

% max_weight(40). % Max inventory weight

% item_weight(hantle_20kg, 20).
% item_weight(ciężary_10kg, 10).
% item_weight(monsterek, 0). % No weight for drinks

% take(Item) :-
%     holding(Item),
%     write('Już masz to w rękach!'), nl, !.

% take(Item) :-
%     i_am_at(Place),
%     at(Item, Place),
%     carrying_weight(W),
%     item_weight(Item, IW),
%     max_weight(MaxW),
%     NewW is W + IW,
%     NewW =< MaxW,
%     retract(at(Item, Place)),
%     assert(holding(Item)),
%     retract(carrying_weight(W)),
%     assert(carrying_weight(NewW)),
%     write('Podniosłeś '), write(Item), write('.'), nl, !.

% take(Item) :-
%     carrying_weight(W),
%     item_weight(Item, IW),
%     max_weight(MaxW),
%     W + IW > MaxW,
%     write('Nie możesz unieść więcej!'), nl, !.

% take(_) :-
%     write('Nie widzę tego tutaj.'), nl.


% drop(Item) :-
%         holding(Item),
%         i_am_at(Place),
%         retract(holding(Item)),
%         assert(at(Item, Place)),
%         carrying_weight(W),
%         item_weight(Item, IW),
%         NewW is W - IW,
%         retract(carrying_weight(W)),
%         assert(carrying_weight(NewW)),
%         write('Odłożyłeś '), write(Item), write('.'), nl, !.

% drop(_) :-
%         write('Nie masz tego przedmiotu!'), nl.

% inventory :-
%         write('Masz przy sobie:'), nl,
%         holding(Item),
%         write('- '), write(Item), nl, fail.

% inventory :-
%         carrying_weight(W),
%         write('Łączna waga: '), write(W), write(' kg.'), nl, !.