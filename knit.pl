
:- use_module(library(clpfd)).

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

pattern(Rows) --> rowSeq(Rows, _).

rowSeq([], 0).
rowSeq([S], 1) --> 
  {
    StitchCount in 0..20,
    constrainedLength(S, 20, StitchCount)
  },
  stitchSeq(S, StitchCount), ".\n".

rowSeq(Rows, RowCount) -->
  {
    RowCount in 0..10,
    RowCountRest #= RowCount - 1,
    StitchCount in 0..20,
    constrainedLength(S, 20, StitchCount),
    append([S], RowsRest, Rows),
    constrainedLength(Rows, 10, RowCount),
    constrainedLength(RowsRest, 10, RowCountRest)
  },
  stitchSeq(S, StitchCount), ".\n", rowSeq(RowsRest, RowCountRest).

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

diagramFromPattern(Pattern, Diagram) :- pattern(Diagram, Pattern, []), !.

compareLength(<, L1, L2) :- length(L1, Len1), length(L2, Len2), Len1 < Len2.
compareLength(>, L1, L2) :- length(L1, Len1), length(L2, Len2), Len1 > Len2.
compareLength(=, L1, L2) :- length(L1, Len1), length(L2, Len2), Len1 = Len2.

shortestPatternFromDiagram(Diagram, Pattern) :-
    findall(P, pattern(Diagram, P, []), Patterns),
    print(Patterns), nl,
    predsort(compareLength, Patterns, PatternsSorted),
    PatternsSorted = [Pattern|_], !.

% example usage:
% time(setof(X, pattern([[k,k,k,p,p,p]], X, []), Result)).
% 442,830 inferences, 0.051 CPU in 0.052 seconds
% Result = [[107, 51, 44, 32, 112, 51, 46, 10]]. (i.e., "k3, p3.\n")

% example usage:
% time(setof(X, pattern([[k,k,k,p,p,p,k,k,k,p,p,p]], X, []), Result)), format("~s~s~n", Result).
% 2,794,494 inferences, 0.328 CPU in 0.331 seconds (99% CPU, 8523567 Lips)
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
% 45,139,737 inferences, 4.242 CPU in 4.304 seconds

% example usage:
% time((findall(X, (pattern([[k,_,k,p,_,_,k,k,k,p,_,p]], X, []), format("~s", [X])), Result), length(Result, L))).
% k1, p1, k1, p1, k1, p1, k3, p3.
% k1, p1, k1, p1, k1, p1, k3, p1, k1, p1.
% ...
% k3, p2, k3, * k1, p1; rep from * 2 times.
% k3, p3, k2, * k1, p1; rep from * 2 times.
% 9,379,594 inferences, 1.093 CPU in 1.103 seconds (99% CPU, 8578531 Lips)
% L = 26.

% example usage:
% time((diagramFromPattern("k3, p3, k3, p3.\n", D), print(D))).
% [[k,k,k,p,p,p,k,k,k,p,p,p]]
% 632,978 inferences, 0.060 CPU in 0.061 seconds

% example usage:
% time((shortestPatternFromDiagram([[k,k,k,p,p,p,k,k,k,p,p,p]], P), format("~s", [P]))).
% k3, p3, k3, p3.
% 2,794,509 inferences, 0.357 CPU in 0.360 seconds

