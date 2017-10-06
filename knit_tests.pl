
% run as: swipl --traditional -q -s knit_tests.pl -t run_tests

:- use_module(library(test_cover)).

:- begin_tests(knit).
:- [knit].

test(patternToDiagram, [nondet]) :-
    diagramPattern(Rows, "p2, * k2, p2; rep from * 2 times.\n"),
    Rows = [[p,p,k,k,p,p,k,k,p,p]].

test(patternToDiagramAllResults) :-
    setof(Rows, diagramPattern(Rows, "k4, p6.\nk1, p1, * k2, p2; rep from * 2 times.\nk7, p3.\nk9, p1.\n"), Result),
    Result = [[[k,k,k,k,p,p,p,p,p,p],[p,p,k,k,p,p,k,k,p,k],[k,k,k,k,k,k,k,p,p,p],[p,k,k,k,k,k,k,k,k,k]]].

test(diagramToPattern, [nondet]) :-
    diagramPattern([[k,k,k,p,p,p]], Pattern),
    Pattern = "k3, p3.\n".

test(patternToDiagramAllResults) :-
    setof(X, diagramPattern([[k,k,k,p,p,p]], X), Result),
    Result = ["k3, p3.\n"].

test(partialDiagramToPattern, [nondet]) :-
    diagramPattern([[k,_,_,p,p,p]], "k1, p5.\n"),
    diagramPattern([[k,_,_,p,p,p]], "k1, p1, k1, p3.\n"),
    diagramPattern([[k,_,_,p,p,p]], "k2, p4.\n"),
    diagramPattern([[k,_,_,p,p,p]], "k3, p3.\n").

test(partialDiagramToPatternAllPossibilities) :-
    findall(X, diagramPattern([[k,_,k,p,_,_,k,k,k,p,_,p]], X), Result),
    length(Result, 26).

:- end_tests(knit).

