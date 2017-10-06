
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(clpfd)).
:- use_module(library(clpr)).
:- use_module(library(tabling)).

% TODO: add support for variables (and variable-assignment dictionary)

eval(R, S) :- expr(R, 0, S, []), !.

expr(R, _) --> num(R).
expr(R, N) --> { N in 0..3, N2 #= N + 1 }, expr(X, N2), "+", expr(Y, N2),
    { {R = X + Y} }. % clpr constraints require {}
expr(R, N) --> { N in 0..3, N2 #= N + 1 }, expr(X, N2), "-", expr(Y, N2),
    { {R = X - Y} }.
expr(R, N) --> { N in 0..3, N2 #= N + 1 }, expr(X, N2), "*", expr(Y, N2),
    { {R = X * Y} }.
expr(R, N) --> { N in 0..3, N2 #= N + 1 }, expr(X, N2), "/", expr(Y, N2),
    { {R = X / Y} }.

num(R) --> digitnonzero(D), { string_codes(S, [D]), number_string(R, S) }.
num(R) --> digitnonzero(D), digits(Ds, 3),
    { reverse([D|Ds], [LastDigit|_]), dif(LastDigit, '0'), % can't end in 0
      string_codes(S, [D|Ds]), number_string(R, S) }.

digitnonzero(D) --> [D], { char_type(D, digit), dif(D, '0') }.
digits([D|Ds], MaxLength) -->
    { MaxLength #> 0, RestLength #= MaxLength - 1 },
    [D], { char_type(D, digit) ; member(D, ['.']) }, digits(Ds, RestLength).
digits([], _) --> [].

