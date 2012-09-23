:- use_module(library(readln)).

:- dynamic current_room/1.
:- dynamic toggled/1.

:- prompt(_, 'Type a command (or ''help''): ').

strip_punctuation([], []).

strip_punctuation([Word|Tail], [Word|Result]) :-
    \+(member(Word, ['.', ',', '?', '!'])),
    strip_punctuation(Tail, Result).

strip_punctuation([_|Tail], Result) :-
    strip_punctuation(Tail, Result).

read_sentence(Input) :-
    readln(Input1, _, ".!?", "_0123456789", lowercase),
    strip_punctuation(Input1, Input).

play :-
    retractall(current_room(_)), retractall(toggled(_)),
    assertz(current_room(library)),
    print_location,
    get_input.

get_input :- read_sentence(Input), get_input(Input).
get_input([quit]).
get_input(Input) :-
    process_input(Input), print_location,
    read_sentence(Input1), get_input(Input1).

room(garden, 'Garden', 'You are in the Garden. The trees and shrubs appear...').
room(hallway, 'Hallway', 'You are in the Hallway. Dusty broken lamps and flower pots...').
room(kitchen, 'Kitchen', 'You are in the kitchen. Knives, pots, pans, ...').
room(library, 'Library', 'You are among many books in the Library...').
room(lair, 'Lair', 'You have found an apparently quite evil lair, of all things...').
connected(north, library, hallway).
connected(south, hallway, library).
connected(up, lair, library).
connected(west, library, garden).
connected(east, garden, library).
connected(west, hallway, kitchen).
connected(east, kitchen, hallway).

connected(down, library, lair) :- toggled(switch3).

toggle(switch1) :- assertz(toggled(switch1)).
toggle(switch2) :- toggled(switch1), assertz(toggled(switch2)).
toggle(switch3) :- toggled(switch2), assertz(toggled(switch3)).

print_location :-
    current_room(Current),
    room(Current, Name, Description),
    print(Name), nl, print(Description), nl.    

change_room(NewRoom) :-
    current_room(Current),
    retract(current_room(Current)),
    assertz(current_room(NewRoom)).

process_input([help]) :- print('Help...'), nl.

process_input([go, Direction]) :-
    current_room(Current),
    connected(Direction, Current, NewRoom),
    change_room(NewRoom).

process_input([go, _]) :-
    print('No exit that direction.'), nl.

process_input([toggle, Switch]) :-
    toggle(Switch),
    setof(X, toggled(X), Switches),
    print('Toggled switches: '), print(Switches), nl.

process_input([toggle, _]) :-
    print('Cannot toggle that switch.'), nl.

process_input([_]) :-
    print('Huh?'), nl, nl.
