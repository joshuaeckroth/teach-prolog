% OSU Electrical and Computer Engineering (Undergrad)
% Computer Engineering Specialization (CES)
% http://ece.osu.edu/sites/default/files/uploads/undergrads/ugrad_handbook.pdf

:- dynamic taken/1.

credits(ece261, 3).
credits(ece265, 3).
credits(ece567, 2).
credits(ece206, 1).
credits(ece561, 3).
credits(ece662, 3).
credits(ece205, 3).
credits(ece301, 3).
credits(ece209, 2).
credits(ece323, 3).
credits(ece351, 3).
credits(ece352, 3).
credits(ece582, 3).
credits(ece682, 3).
credits(ece331, 3).

% assumed:
credits(cse221, 3).
credits(cse222, 3).
credits(math366, 3).
credits(cse321, 3).
credits(cse560, 3).
credits(ece265, 3).
credits(cse660, 3).
credits(stat427, 3).
credits(ece662, 3).

prereq(ece265, ece261).
prereq(ece206, ece261).
prereq(ece567, ece265).
prereq(ece567, ece206).
prereq(ece561, ece206).
prereq(ece662, ece265).
prereq(ece662, ece561).
prereq(ece301, ece205).
prereq(ece209, ece205).
prereq(ece351, ece205).
prereq(ece352, ece351).
prereq(ece323, ece301).
prereq(ece352, ece301).
prereq(ece582, ece323).
prereq(ece682, ece582).

prereq(cse222, cse221).
prereq(cse321, cse222).
prereq(cse560, cse321).
prereq(cse660, cse560).

coreq(ece206, ece265).
coreq(ece209, ece301).
coreq(ece561, ece323).
coreq(math366, cse321).
coreq(ece365, cse560).
coreq(stat427, cse660).
coreq(ece662, cse660).

coreq_pair(X, Y) :- coreq(Y, X).
coreq_pair(X, Y) :- coreq(X, Y).

taken(ece261).
taken(ece265).
taken(ece206).
taken(ece205).

prereqs_sat(X) :- forall(prereq(X, Y), taken(Y)).
prereqs_sat(X) :- \+(prereq(X, _)).

% You can always take no class.
can_take([]).

% You can take a class if it has prereqs satisfied and no coreqs.
can_take([H]) :-
    \+(taken(H)),
    prereqs_sat(H),
    \+(coreq_pair(_, H)).

% You can take a class if you have taken all the prereqs,
% and you are going to take concurrently or have already
% taken all the coreqs
can_take([H1|[H2|T]]) :-
    \+(taken(H1)),
    \+(taken(H2)),
    prereqs_sat(H1),
    coreq_pair(H1, H2),
    prereqs_sat(H2),
    can_take(T).


% not done: need to find combinations that can be taken
% when coreqs are required
one_quarter([], []).

one_quarter([C|PossibleCourses], [C|Courses]) :-
    can_take([C]),
    one_quarter(PossibleCourses, Courses).

one_quarter([CannotTake|PossibleCourses], Courses) :-
    one_quarter(PossibleCourses, Courses).
