family(10392,
       person(tom, fox, born(7, may, 1960), works(cnn, 152000)),
       person(ann, fox, born(19, april, 1961), works(nyu, 65000)),
       % here are the children...
       [person(pat, fox, born(5, october, 1983), unemployed),
        person(jim, fox, born(1, june, 1986), unemployed),
        person(amy, fox, born(17, december, 1990), unemployed)]).

family(38463, 
       person(susan, rothchild, born(13, september, 1972), works(osu, 75000)),
       person(jess, rothchild, born(20, july, 1975), works(nationwide, 123500)),
       % here are the children...
       [person(ace, rothchild, born(2, january, 2010), unemployed)]).

married(FirstName1, LastName1, FirstName2, LastName2) :-
    family(_, person(FirstName1, LastName1, _, _),
           person(FirstName2, LastName2, _, _), _).

married(FirstName1, LastName1, FirstName2, LastName2) :-
    family(_, person(FirstName2, LastName2, _, _),
           person(FirstName1, LastName1, _, _), _).

exists(Person) :- family(_, Person, _, _).
exists(Person) :- family(_, _, Person, _).
exists(Person) :- family(_, _, _, Children), member(Person, Children).

householdIncome(ID, Income) :-
    family(ID, person(_, _, _, works(_, Income1)),
           person(_, _, _, works(_, Income2)), _),
    Income is Income1 + Income2.

householdSize(ID, Size) :-
    family(ID, _, _, Children),
    length(Children, ChildrenCount),
    Size is 2 + ChildrenCount.

:- use_module(library(lists)). % load lists library for sum_list

average(List, Avg) :-
    sumlist(List, Sum),
    length(List, N),
    Avg is Sum / N.


% exists(person(FirstName, LastName, born(_, _, Year), Job)), Year > 1980, Job \= osu.

