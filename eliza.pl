:- use_module(library(readln)).

% From The Art of Prolog 2nd Ed. (Sterling & Shapiro)

strip_punctuation([], []).
strip_punctuation([Word|Tail], [Word|Result]) :-
    \+(member(Word, ['.', ',', '?', '!'])),
    strip_punctuation(Tail, Result).
strip_punctuation([_|Tail], Result) :-
    strip_punctuation(Tail, Result).

read_sentence(Input) :-
    readln(Input1, _, ".!?", "_0123456789", lowercase),
    strip_punctuation(Input1, Input).

eliza :- read_sentence(Input), eliza(Input), !.

eliza(['bye']) :- reply(['Goodbye. I hope I have helped you.']).

eliza(Input) :-
    pattern(Stimulus, Response),
    match(Stimulus, Dictionary, Input),
    match(Response, Dictionary, Output),
    reply(Output),
    read_sentence(Input1),
    !, eliza(Input1).

match([N|Pattern], Dictionary, Target) :-
    integer(N), lookup(N, Dictionary, LeftTarget),
    append(LeftTarget, RightTarget, Target),
    match(Pattern, Dictionary, RightTarget).

match([Word|Pattern], Dictionary, [Word|Target]) :-
    atom(Word), match(Pattern, Dictionary, Target).

match([], _, []).

lookup(Key, [(Key, Value)|_], Value).
lookup(Key, [(Key1, _)|Dictionary], Value) :-
    \=(Key, Key1), lookup(Key, Dictionary, Value).

add_memory(X, Predicate, Y) :-
    print(X), nl, print(Predicate), nl, print(Y),
    Fact =.. [Predicate, X, Y],
    asserta(Fact).

pattern([i, am, 1], ['How', long, have, you, been, 1, '?']) :-
    add_memory(i, am, X).
pattern([1, you, 2, me], ['What', makes, you, think, 'I', 2, you, '?']).
pattern([i, like, 1], ['Does', anyone, else, in, your, family, like, 1, '?']).
pattern([i, feel, 1], ['Do', you, often, feel, that, way, '?']).
pattern([1, X, 2], ['Please', tell, me, more, about, X, '.']) :- important(X).
%pattern([remember, X, Predicate, Y], ['I', will, remember, that, '.']) :-
%    add_memory(X, Predicate, Y).
%pattern([X, Predicate], ['Is', that, because, X, Predicate, Y, '?']) :-
%    call(Predicate, X, Y).
pattern([1], ['Please', go, on, '.']).

important(father).
important(mother).
important(sister).
important(brother).
important(son).
important(daughter).

reply([Head|Tail]) :- write(Head), write(' '), reply(Tail).
reply([]) :- nl.

