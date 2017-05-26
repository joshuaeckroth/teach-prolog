:- use_module(library(clpfd)).

sameTable(a,b).
sameTable(e,f).
differentTables(b,c).
differentTables(a,c).

% important to check if person is unassigned (and therefore trivially satisfies constraint)
% so that we can avoid generating completed tables only then to discover they do not meet constraints
% ("fail early" approach)
sameTable(PersonA, _, Unassigned, _) :- member(PersonA, Unassigned).
sameTable(_, PersonB, Unassigned, _) :- member(PersonB, Unassigned).
sameTable(PersonA, PersonB, _, Tables) :-
    member(table(_, Persons), Tables),
    member(PersonA, Persons),
    member(PersonB, Persons).

differentTables(PersonA, _, Unassigned, _) :- member(PersonA, Unassigned).
differentTables(_, PersonB, Unassigned, _) :- member(PersonB, Unassigned).
differentTables(PersonA, PersonB, _, Tables) :-
    select(table(_, PersonsA), Tables, RemainingTables),
    member(table(_, PersonsB), RemainingTables),
    member(PersonA, PersonsA),
    member(PersonB, PersonsB).

seatingConstraintsMet(Unassigned, Tables) :-
    foreach(sameTable(PersonA, PersonB), sameTable(PersonA, PersonB, Unassigned, Tables)),
    foreach(differentTables(PersonA, PersonB), differentTables(PersonA, PersonB, Unassigned, Tables)).

% every table has at least two people
tablesSufficientlyOccupied([]).
tablesSufficientlyOccupied([table(_, Persons)|Tables]) :-
    length(Persons, PersonCount),
    PersonCount #>= 2,
    tablesSufficientlyOccupied(Tables).

addPerson(table(Size, Persons), Person, table(Size, [Person|Persons])) :-
    length(Persons, PersonCount),
    PersonCount #< Size.

sumTableSizes([], 0).
sumTableSizes([table(Size, [])|Tables], S) :-
    member(Size, [4, 6, 8]), % tables only come in sizes 4, 6, 8
    sumTableSizes(Tables, S2),
    S #= Size + S2.

generateTables(Unassigned, Tables) :-
    length(Unassigned, PersonCount),
    % need to constrain length of tables, otherwise sumTableSizes goes infinite
    TableCount in 1..6, % only room for 6 tables in the hall 
    indomain(TableCount), % need this to force instantiation before checking length
    % otherwise, length will generate arbitrary lists and then discover they are
    % not the right length when the clpfd var is figured out
    length(Tables, TableCount),
    sumTableSizes(Tables, TableCapacity),
    % table capacity (in sum) has room for all the people with no more than 6 empty seats
    TableCapacity #>= PersonCount,
    TableCapacity #=< PersonCount + 6.

findSeats(Unassigned, Tables, RemainingUnassigned, UpdatedTables) :-
    select(Person, Unassigned, RemainingUnassigned),
    select(Table, Tables, UnchangedTables),
    addPerson(Table, Person, UpdatedTable),
    UpdatedTables = [UpdatedTable|UnchangedTables],
    seatingConstraintsMet(RemainingUnassigned, UpdatedTables).

findSeats([], Tables, Tables).
findSeats(Unassigned, Tables, FinalTables) :-
    findSeats(Unassigned, Tables, UpdatedUnassigned, UpdatedTables),
    findSeats(UpdatedUnassigned, UpdatedTables, FinalTables),
    tablesSufficientlyOccupied(FinalTables).

findSeats(Unassigned, FinalTables) :-
    generateTables(Unassigned, EmptyTables),
    findSeats(Unassigned, EmptyTables, FinalTables).

% example:
% time(findSeats([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p], Tables)). -> 0.001s
% time((setof(Tables, findSeats([a,b,c,d,e,f], Tables), Results), length(Results, L))). -> 2.733s

