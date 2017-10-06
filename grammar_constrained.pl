
s(CountAB) --> s(CountAB, CountAB).
s(CountA, CountB) --> "a", s(PriorCountA, CountB), { CountA is PriorCountA + 1 }.
s(CountA, CountB) --> "b", s(CountA, PriorCountB), { CountB is PriorCountB + 1 }.
s(0, 0) --> [].

% ?- s(CountA, CountB, "aa", []).
% CountA = 2,
% CountB = 0 .
%
% ?- s(CountA, CountB, "aabba", []).
% CountA = 3,
% CountB = 2 .
%
% ?- s(CountAB, "aabba", []).
% false.
%
% ?- s(CountAB, "aabbab", []).
% CountAB = 3 .

:- use_module(library(clpfd)).

s2(CountAB) -->
    { CountAB in 0..20 },
    s2(CountAB, CountAB).
s2(CountA, CountB) -->
    { PriorCountA in 0..10, CountA #= PriorCountA + 1 },
    "a", s2(PriorCountA, CountB).
s2(CountA, CountB) -->
    { PriorCountB in 0..10, CountB #= PriorCountB + 1 },
    "b", s2(CountA, PriorCountB).
s2(0, 0) --> [].

% ?- s2(CountAB, "aabbab", []).
% CountAB = 3 ;
% false.
%
% ?- s2(3, String, []), format("~s~n", [String]).
% aaabbb
% String = [97, 97, 97, 98, 98, 98] ;
% aababb
% String = [97, 97, 98, 97, 98, 98] ;
% etc.
%
% ?- setof(String, s2(3, String, []), AllStrings), length(AllStrings, L).
% L = 20.

s3(CountA, CountB) --> s3a(CountA, CountB).
s3a(CountA, CountB) -->
    { PriorCountA in 0..10, CountA #= PriorCountA + 1, CountB in 0..10, CountB #>= CountA },
    "a", s3a(PriorCountA, CountB), "a".
s3a(0, CountB) --> s3b(CountB).
s3a(0, 0) --> [].
s3b(CountB) -->
    { PriorCountB in 0..10, CountB #= PriorCountB + 1 },
    "b", s3b(PriorCountB).
s3b(0) --> [].

