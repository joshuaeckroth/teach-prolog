
% keep state

:- set_prolog_flag(double_quotes, chars).
:- use_module(library(clpfd)).

s(Subject, Object, Action, 0) --> np(Subject), " ", vp(Object, Action).
s(Subject, Object, Action, N) --> np(Subject), " ", vp(Object, Action), " ", num(N), " times".

np(X) --> det, " ", n(X).

vp(Object, Action) --> v(Action), " ", np(Object).
vp(none, Action) --> v(Action).

det --> "the".
det --> "a".

n(woman) --> "woman".
n(man) --> "man".

v(shoots) --> "shoots".

num(N) --> digitnonzero(D), { number_codes(N, [D]) }.
num(N) --> digitnonzero(D), digits(Digits, 4), { dif(N, noissue), number_codes(N, [D|Digits]) }.
digitnonzero(D) --> [D], { char_type(D, digit), dif(D, '0') }.
digits([D|Ds], MaxLength) -->
    { MaxLength #> 0, RestLength #= MaxLength - 1 },
    [D], { char_type(D, digit) }, digits(Ds, RestLength).
digits([], _) --> [].


