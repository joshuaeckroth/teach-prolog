:- include('cf2010gms.pl').

played(Month, Day, Year, X, Score1, Y, Score2) :-
    game(Month, Day, Year, X, Score1, Y, Score2).
played(Month, Day, Year, X, Score1, Y, Score2) :-
    game(Month, Day, Year, Y, Score2, X, Score1).

opponent(X, Y) :- played(_, _, _, X, _, Y, _).

opponents(X, L) :- findall(Y, opponent(X, Y), L).

opponents_count(X, C) :- opponents(X, L), length(L, C).

earlier(X1, Y1, X2, Y2, Year) :-
    played(Month1, _, Year, X1, _, Y1, _),
    played(Month2, _, Year, X2, _, Y2, _),
    <(Month1, Month2).

earlier(X1, Y1, X2, Y2, Year) :-
    played(Month1, Day1, Year, X1, _, Y1, _),
    played(Month2, Day2, Year, X2, _, Y2, _),
    =(Month1, Month2),
    <(Day1, Day2).

won(X, Y, Year) :-
    played(_, _, Year, X, Score1, Y, Score2),
    >(Score1, Score2).

lost(X, Y, Year) :-
    played(_, _, Year, X, Score1, Y, Score2),
    <(Score1, Score2).

tied(X, Y, Year) :-
    played(_, _, Year, X, Score1, Y, Score2),
    =(Score1, Score2).

% not very interesting
won_after_lost(X, Year) :-
    lost(X, Y1, Year),
    won(X, Y2, Year),
    earlier(X, Y1, X, Y2, Year).

sumlist(L, Sum) :- sumlist(L, 0, Sum).
sumlist([], Sum, Sum).
sumlist([H|T], Count, Sum) :-
    NewCount is Count + H,
    sumlist(T, NewCount, Sum).

avg_score(X, Avg) :-
    findall(Score, played(_, _, _, X, Score, _, _), Scores),
    sumlist(Scores, Sum),
    opponents_count(X, C),
    Avg is Sum / C.

