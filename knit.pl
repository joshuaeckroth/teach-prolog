
:- use_module(library(clpfd)).
:- use_module(library(tabling)).

constrainedLength([], _, 0).
constrainedLength([_|T], Max, Length) :-
    Length #=< Max,
    Length #>= 0,
    Length #= Length2 + 1,
    constrainedLength(T, Max, Length2).

generateRepeats(_, 0, []).
generateRepeats(X, N, [X|List]) :-
    N in 1..20,
    N2 #= N - 1,
    generateRepeats(X, N2, List).

reverseEvenRow(S, even, S2) :- reverse(S, S2).
reverseEvenRow(S, odd, S).

pattern(Rows) -->
  {
    RowCount in 1..10,
    constrainedLength(Rows, 10, RowCount)
  },
  rowSeq(Rows, RowCount, odd).

rowSeq([S], 1, EvenOdd) -->
  {
    StitchCount in 0..20,
    constrainedLength(S, 20, StitchCount),
    constrainedLength(STmp, 20, StitchCount),
    reverseEvenRow(STmp, EvenOdd, S)
  },
  stitchSeq(STmp, StitchCount), ".\n".

rowSeq(Rows, RowCount, EvenOdd) -->
  {
    RowCount in 2..10,
    RowCountRest #= RowCount - 1,
    StitchCount in 0..20,
    constrainedLength(S, 20, StitchCount),
    constrainedLength(STmp, 20, StitchCount),
    constrainedLength(Rows, 10, RowCount),
    constrainedLength(RowsRest, 10, RowCountRest),
    append([S], RowsRest, Rows),
    reverseEvenRow(STmp, EvenOdd, S),
    select(EvenOdd, [even, odd], [OddEven]) % alternate among even/odd
  },
  stitchSeq(STmp, StitchCount), ".\n", rowSeq(RowsRest, RowCountRest, OddEven).

:- table stitchSeq/4.

stitchSeq(S, StitchCount) --> stitchSeqNonrepeating(S, StitchCount).
stitchSeq(S, StitchCount) --> stitchSeqRepeating(S, StitchCount).
stitchSeq(S, StitchCount) -->
  {
    StitchCount in 0..20,
    StitchCountLeft in 0..20,
    StitchCountRight in 0..20,
    StitchCount #= StitchCountLeft + StitchCountRight,
    constrainedLength(S, 20, StitchCount),
    constrainedLength(Subseq, 20, StitchCountLeft),
    constrainedLength(RepeatedSeq, 20, StitchCountRight),
    append(Subseq, RepeatedSeq, S)
  },
  stitchSeqNonrepeating(Subseq, StitchCountLeft), ", ", stitchSeqRepeating(RepeatedSeq, StitchCountRight).

:- table stitchSeqNonrepeating/4.

stitchSeqNonrepeating([], 0).
stitchSeqNonrepeating(S, StitchCount) --> stitch(S, StitchCount).
stitchSeqNonrepeating(S, StitchCount) -->
  {
    StitchCount in 0..20,
    StitchCountLeft in 0..20,
    StitchCountRight in 0..20,
    StitchCount #= StitchCountLeft + StitchCountRight,
    constrainedLength(S, 20, StitchCount), % must come before append
    append([SFirst|SRest], [SFirst2|SRest2], S),
    select(SFirst, [k,p], [SFirst2]) % ensure alternation among k and p
  },
  stitch([SFirst|SRest], StitchCountLeft), ", ", stitchSeqNonrepeating([SFirst2|SRest2], StitchCountRight).

:- table stitchSeqRepeating/4.

stitchSeqRepeating(S, StitchCount) -->
  {
    StitchCount in 0..20,
    SubStitchCount in 0..20,
    SubStitchCount1 in 0..20,
    SubStitchCount2 in 0..20,
    N in 2..20, % "repeat 1 times" is not allowed
    SubStitchCount #= SubStitchCount1 + SubStitchCount2,
    StitchCount #= SubStitchCount * N,
    constrainedLength(S, 20, StitchCount),
    constrainedLength(Subseq, 20, SubStitchCount), % must come before append
    append([SFirst|SRest], [SFirst2|SRest2], Subseq),
    select(SFirst, [k,p], [SFirst2]) % ensure alternation among k and p
  },
  "* ", stitch([SFirst|SRest], SubStitchCount1), ", ", stitchSeqNonrepeating([SFirst2|SRest2], SubStitchCount2),
  "; rep from * ", repeatCount(N), " times",
  {
    generateRepeats(Subseq, N, SubseqRepeats),
    append(SubseqRepeats, S)
  }.

stitch(Knits, N) --> "k", repeatCount(N), { generateRepeats(k, N, Knits) }.
stitch(Purls, N) --> "p", repeatCount(N), { generateRepeats(p, N, Purls) }.

repeatCount(N) --> digitnonzero(D), digit(D2), { number_codes(N, [D,D2]), N in 1..20 }.
repeatCount(N) --> digit(D), { number_codes(N, [D]), N in 1..9 }.
digitnonzero(D) --> [D], { code_type(D, digit), D \= 48 }.
digit(D) --> [D], { code_type(D, digit) }.

%diagramFromPattern(Pattern, Diagram) :- pattern(Diagram, Pattern, []), !.

%compareLength(<, L1, L2) :- length(L1, Len1), length(L2, Len2), Len1 < Len2.
%compareLength(>, L1, L2) :- length(L1, Len1), length(L2, Len2), Len1 > Len2.
%compareLength(=, L1, L2) :- length(L1, Len1), length(L2, Len2), Len1 = Len2.

%shortestPatternFromDiagram(Diagram, Pattern) :-
%    findall(P, pattern(Diagram, P, []), Patterns),
%    predsort(compareLength, Patterns, PatternsSorted),
%    PatternsSorted = [Pattern|_], !.

% example usage:
% time(setof(X, pattern([[k,k,k,p,p,p]], X, []), Result)).
% 157,197 inferences, 0.018 CPU in 0.018 seconds
% Result = [[107, 51, 44, 32, 112, 51, 46, 10]]. (i.e., "k3, p3.\n")

% example usage:
% time(setof(X, pattern([[k,k,k,p,p,p,k,k,k,p,p,p]], X, []), Result)), format("~s~s~n", Result).
% 1,306,952 inferences, 0.156 CPU in 0.157 seconds
% * k3, p3; rep from * 2 times.
% k3, p3, k3, p3.

% example usage:
% time(setof(Rows, stitchSeq(Rows, 6, "p3, k3", []), Result)).
% 45,917 inferences, 0.005 CPU in 0.005 seconds
% Result = [[p, p, p, k, k, k]].

% example usage:
% pattern([[k,_,_,p,p,p]], X, []), format("~s~n", [X]).
% results:
% k1, p5.
% k1, p1, k1, p3.
% k2, p4.
% k3, p3.

% example usage:
% pattern(Rows, "p2, * k2, p2; rep from * 2 times.\n", []), print(Rows).
% [[p,p,k,k,p,p,k,k,p,p]]
% time((setof(Rows, pattern(Rows, "p2, * k2, p2; rep from * 2 times.\n", []), Result))).
% 42,049,767 inferences, 3.984 CPU in 4.009 seconds

% example usage:
% pattern(Rows, "k4, p6.\nk1, p1, * k2, p2; rep from * 2 times.\nk7, p3.\nk9, p1.\n", []), print(Rows).
% [[k,k,k,k,p,p,p,p,p,p],[p,p,k,k,p,p,k,k,p,k],[k,k,k,k,k,k,k,p,p,p],[p,k,k,k,k,k,k,k,k,k]]
% time((setof(Rows, pattern(Rows, "k4, p6.\nk1, p1, * k2, p2; rep from * 2 times.\nk7, p3.\nk9, p1.\n", []), Result))).
% 100,278,278 inferences, 9.256 CPU in 9.292 seconds

% example usage:
% time((findall(X, (pattern([[k,_,k,p,_,_,k,k,k,p,_,p]], X, []), format("~s", [X])), Result), length(Result, L))).
% k1, p1, k1, p1, k1, p1, k3, p3.
% k1, p1, k1, p1, k1, p1, k3, p1, k1, p1.
% ...
% k3, p2, k3, * k1, p1; rep from * 2 times.
% k3, p3, k2, * k1, p1; rep from * 2 times.
% 3,767,963 inferences, 0.441 CPU in 0.446 seconds
% L = 26.

% example usage:
% time((diagramFromPattern("k3, p3, k3, p3.\n", D), print(D))).
% [[k,k,k,p,p,p,k,k,k,p,p,p]]
% 632,978 inferences, 0.060 CPU in 0.061 seconds

% example usage:
% time((shortestPatternFromDiagram([[k,k,k,p,p,p,k,k,k,p,p,p]], P), format("~s", [P]))).
% k3, p3, k3, p3.
% 1,313,544 inferences, 0.166 CPU in 0.176 seconds

