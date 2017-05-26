
:- use_module(library(clpfd)).

generateRepeats(_, 0, []).
generateRepeats(X, N, [X|List]) :-
    N #>= 1, % only needed to prevent negative values on backtracking
    N2 #= N - 1,
    generateRepeats(X, N2, List).

pattern(Rows) --> rowSeq(Rows).

rowSeq([S]) --> stitchSeq(S), ".\n".
rowSeq(Rows) --> stitchSeq(S), ".\n", rowSeq(RowsRest), { append([S], RowsRest, Rows) }.

stitchSeq(S) --> stitch(S).
stitchSeq(SSeq) --> stitch(S), ", ", stitchSeq(SSeqRest), { append(S, SSeqRest, SSeq) }.

stitch(Knits) --> "k", repeatCount(N), { generateRepeats(k, N, Knits) }.
stitch(Purls) --> "p", repeatCount(N), { generateRepeats(p, N, Purls) }.

repeatCount(N) --> digitnonzero(D), digit(D2), { number_codes(N, [D,D2]), N in 1..20 }.
repeatCount(N) --> digit(D), { number_codes(N, [D]), N in 1..9 }.
digitnonzero(D) --> [D], { code_type(D, digit), D \= 48 }.
digit(D) --> [D], { code_type(D, digit) }.

