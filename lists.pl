
head(X, [X|_]).

% member(X, [X]).
member(X, [X|_]).
member(X, [_|Tail]) :- member(X, Tail).

foobar(X, List) :- member(Y, List), X \= Y.

insertsort([],[]).
 
insertsort([X|Tail],Sorted):-
    insertsort(Tail,SortedTail),
    insert(X,SortedTail, Sorted).
 
insert(X,[Y|Sorted],[Y|Sorted1]):-
    X > Y,
    insert(X,Sorted,Sorted1).
 
insert(X,Sorted,[X|Sorted]).



perm([],[]).

perm(List, [H|Perm]) :-
    delete(H, List, Rest),
    perm(Rest, Perm).

delete(X, [X|T], T).
delete(X, [H|T], [H|NT]) :- delete(X, T, NT).
