woman(mia).
woman(judy).
woman(yolanda).
loves(vincent,mia).
loves(marcellus,mia).
loves(butch, jody).
loves(jody, butch).

team(mia, jody, butch).
team(vincent, marcellus, yolanda).

jealous(X,Y):- loves(X,Z), loves(Y,Z).
% #         write('You can see:'), nl,
% #         look_in(Place),
% #         nl.


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
