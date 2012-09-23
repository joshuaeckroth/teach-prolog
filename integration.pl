:- use_module(library(clpfd)).

constant(X) :- integer(X), X in -10..10.

simplified([], []).
simplified([-1, '*', X], ['-', S]) :- simplified(X, S).
simplified([X, '*', -1], ['-', S]) :- simplified(X, S).
simplified([1, '*', X], S) :- simplified(X, S).
simplified([X, '*', 1], S) :- simplified(X, S).
simplified([0, '+', X], S) :- simplified(X, S).
simplified([X, '+', 0], S) :- simplified(X, S).
simplified([0, '-', X], ['-', S]) :- simplified(X, S).
simplified([X, '-', X], [0]).
simplified([0, '*', _], [0]).
simplified([_, '*', 0], [0]).
simplified([X, '/', 1], S) :- simplified(X, S).
simplified([0, '/', _], [0]).
simplified([X, '^', 1], S) :- simplified(X, S).
simplified([_, '^', 0], [1]).
simplified([[X, '^', N1], '^', N2], [S, '^', N3]) :-
    constant(N1), constant(N2), constant(N3),
    N3 #= N1 ^ N2,
    simplified(X, S).
simplified([N1, '+', N2], [N3]) :-
    constant(N1), constant(N2), constant(N3),
    N3 #= N1 + N2.
simplified([N1, '-', N2], [N3]) :-
    constant(N1), constant(N2), constant(N3),
    N3 #= N1 - N2.
simplified([N1, '*', N2], [N3]) :-
    constant(N1), constant(N2), constant(N3),
    N3 #= N1 * N2.
simplified([N1, '/', N2], [N3]) :-
    constant(N1), constant(N2), constant(N3),
    N3 #= N1 / N2.
simplified([X, '+', Y], [SX, '+', SY]) :-
    simplified(X, SX), simplified(Y, SY).
simplified([X, '-', Y], [SX, '-', SY]) :-
    simplified(X, SX), simplified(Y, SY).
simplified([X, '*', Y], [SX, '*', SY]) :-
    simplified(X, SX), simplified(Y, SY).
simplified([X, '/', Y], [SX, '/', SY]) :-
    simplified(X, SX), simplified(Y, SY).
simplified([X, '^', Y], [SX, '^', SY]) :-
    simplified(X, SX), simplified(Y, SY).
simplified(X, X).

deriv(N, 0) :- constant(N).
deriv(x, 1).

deriv([cos, x], ['-', [sin, x]]).

deriv([sin, x], [cos, x]).

deriv([x, '^', N], S) :-
    constant(N),
    N2 #= N - 1,
    simplified([N, '*', [x, '^', N2]], S).

deriv([[x, '^', N], '/', N], S) :-
    constant(N),
    N2 #= N - 1,
    simplified([x, '^', N2], S).

deriv([X, '+', Y], S) :-
    deriv(X, X2),
    deriv(Y, Y2),
    simplified([X2, '+', Y2], S).

deriv([X, '-', Y], S) :-
    deriv(X, X2),
    deriv(Y, Y2),
    simplified([X2, '-', Y2], S).

deriv([X, '*', Y], S) :-
    deriv(X, X2),
    deriv(Y, Y2),
    simplified([[X2, '*', Y], '+', [X, '*', Y2]], S).

deriv([X, '/', Y], S) :-
    deriv(X, X2),
    deriv(Y, Y2),
    simplified([[[X2, '*', Y], '-', [X, '*', Y2]], '/', [Y, '^', 2]], S).

deriv([e, '^', x], [e, '^', x]).

do_deriv(X, Y) :- deriv(X, Y), !.

do_int(X, Y) :- deriv(Y, X), !.

