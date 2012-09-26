:- use_module(library(lists)). %% for member/2

cons([X, List], [X|List]).

car([X|_], X).

cdr([], []).
cdr([_|List], List).

listp([]).
listp([_|_]).
listp(nil).

nilp(nil).

atomp(X) :- \+listp(X).

cond([Pred, Clause|_], Env, NewEnv2, Result) :-
    eval(Pred, Env, NewEnv, t),
    eval(Clause, NewEnv, NewEnv2, Result).

cond([_, _|Rest], Env, NewEnv, Result) :-
    cond(Rest, Env, NewEnv, Result).

eval_each([], Env, Env, []).
eval_each([First|Rest], Env, NewEnv2, [FirstResult|RestResult]) :-
    %print('First: '), print(First), nl, print('Rest: '), print(Rest), nl,
    eval(First, Env, NewEnv, FirstResult),
    %print('FirstResult: '), print(FirstResult), nl,
    eval_each(Rest, NewEnv, NewEnv2, RestResult).
    %print('RestResult: '), print(RestResult), nl,
    %print('FinalResult: '), print([FirstResult|RestResult]), nl.

eval(t, Env, Env, t).
eval(nil, Env, Env, nil).
eval(car, Env, Env, car).
eval(cdr, Env, Env, cdr).
eval(cons, Env, Env, cons).
eval(eq, Env, Env, eq).
eval(cond, Env, Env, cond).
eval(listp, Env, Env, listp).
eval(atomp, Env, Env, atomp).
eval(nilp, Env, Env, nilp).
eval(+, Env, Env, +).
eval(-, Env, Env, -).
eval(/, Env, Env, /).
eval(*, Env, Env, *).

eval([lambda|Forms], Env, Env, [lambda|Forms]).

eval([quote|[Forms]], Env, Env, Forms).

eval([def,Sym|[Forms]], Env, NewEnv, [quote, Sym]) :-
    merge_env([Sym], [Forms], Env, NewEnv).

eval([Fn|Forms], Env, NewEnv3, Result) :-
    %print('Fn: '), print(Fn), nl, print('Forms: '), print(Forms), nl,
    eval(Fn, Env, NewEnv, FnEvaled),
    %print('FnEvaled: '), print(FnEvaled), nl,
    eval_each(Forms, NewEnv, NewEnv2, FormsEvaled),
    %print('FormsEvaled: '), print(FormsEvaled), nl,
    apply(FnEvaled, FormsEvaled, NewEnv2, NewEnv3, Result).

eval(Number, Env, Env, Number) :- number(Number).
eval(Atom, Env, Env, Result) :- atomp(Atom), lookup(Env, Atom, Result).

apply(car, [Forms], Env, Env, Result) :-
    car(Forms, Result).

apply(cdr, [Forms], Env, Env, Result) :-
    cdr(Forms, Result).

apply(cons, Forms, Env, Env, Result) :-
    cons(Forms, Result).

apply(eq, [X, X], Env, Env, t) :- atomp(X).
apply(eq, [[], []], Env, Env, t).
apply(eq, _, Env, Env, nil).

apply(listp, [Forms], Env, Env, t) :- listp(Forms).
apply(listp, _, Env, Env, nil).

apply(atomp, [Forms], Env, Env, t) :- atomp(Forms).
apply(atomp, _, Env, Env, nil).

apply(nilp, [Forms], Env, Env, t) :- nilp(Forms).
apply(nilp, _, Env, Env, nil).

apply(+, [X, Y], Env, Env, Z) :- Z is X + Y.
apply(-, [X, Y], Env, Env, Z) :- Z is X - Y.
apply(*, [X, Y], Env, Env, Z) :- Z is X * Y.
apply(/, [X, Y], Env, Env, Z) :- Z is X / Y.

% special form; don't eval the args
apply(cond, [Forms], Env, NewEnv, Result) :-
    %print('cond forms: '), print(Forms), nl,
    cond(Forms, Env, NewEnv, Result).

% lambda form; update environment first
apply([lambda, Params|[Body]], Vals, Env, NewEnv2, Result ) :-
    %print('lambda params: '), print(Params), nl,
    %print('lambda body: '), print(Body), nl,
    %print('env: '), print(Env), nl,
    %print('vals: '), print(Vals), nl,
    merge_env(Params, Vals, Env, NewEnv),
    %print('new env: '), print(NewEnv), nl,
    eval(Body, NewEnv, NewEnv2, Result).

lookup([[Sym, Val]|_], Sym, Val).
lookup([_|Env], Sym, Val) :- lookup(Env, Sym, Val).

merge_env([], [], Env, Env).
merge_env([S|Syms], [V|Vals], Env, [[S, V]|ResultEnv]) :-
    merge_env(Syms, Vals, Env, ResultEnv).


whitespace --> [W], {code_type(W, space)}, whitespace.
whitespace --> [].

sexp_many([E|Es]) --> whitespace, sexp(E), whitespace, sexp_many(Es).
sexp_many([]) --> [].

sexp(A) --> symbol(Cs), {atom_codes(A, Cs)}.
sexp(N) --> number(Cs), {number_codes(N, Cs)}.
sexp(List) --> "(", sexp_many(List), ")".
sexp([quote, Q]) --> "'", sexp(Q).

number([D|Ds]) --> digit(D), number(Ds).
number([D]) --> digit(D).

digit(D) --> [D], {code_type(D, digit)}.

% require symbols do not start with a number
symbol([A|As]) --> [A], {member(A, "+/-*<>=") ; code_type(A, alpha)}, symbolr(As).
symbolr([A|As]) --> [A], {member(A, "+/-*<>=") ; code_type(A, alnum)}, symbolr(As).
symbolr([]) --> [].

parse(S, P) :- phrase(sexp_many(P), S).

run(S, NewEnv, Result) :- parse(S, P), eval_each(P, [], NewEnv, Result).

tests :-
    !,
    print('t'), nl,
    eval(t, [], [], t),

    print('nil'), nl,
    eval(nil, [], [], nil),

    print('52'), nl,
    eval(52, [], [], 52),

    print('(nilp nil)'), nl,
    eval([nilp, nil], [], [], t),

    print('(nilp 52)'), nl,
    eval([nilp, 52], [], [], nil),

    print('(nilp t)'), nl,
    eval([nilp, t], [], [], nil),

    print('(atomp 42)'), nl,
    eval([atomp, 42], [], [], t),

    print('(atomp (quote ()))'), nl,
    eval([atomp, [quote, []]], [], [], nil),

    print('(atomp (quote (a b)))'), nl,
    eval([atomp, [quote, [a, b]]], [], [], nil),

    print('(listp 52)'), nl,
    eval([listp, 52], [], [], nil),

    print('(listp nil)'), nl,
    eval([listp, nil], [], [], t),

    print('(listp (quote ()))'), nl,
    eval([listp, [quote, []]], [], [], t),

    print('(listp (quote (a b)))'), nl,
    eval([listp, [quote, [a, b]]], [], [], t),

    print('(quote 52)'), nl,
    eval([quote, 52], [], [], 52),

    print('(quote a)'), nl,
    eval([quote, a], [], [], a),

    print('(car (quote (42 52)))'), nl,
    eval([car, [quote, [42, 52]]], [], [], 42),

    print('(car (quote (a b)))'), nl,
    eval([car, [quote, [a, b]]], [], [], a),

    print('(cdr (quote (42 52)))'), nl,
    eval([cdr, [quote, [42, 52]]], [], [], [52]),

    print('(cdr (quote (a b c d)))'), nl,
    eval([cdr, [quote, [a, b, c, d]]], [], [], [b, c, d]),

    print('(cdr (quote ()))'), nl,
    eval([cdr, [quote, []]], [], [], []),

    print('(car (cdr (quote (a b c))))'), nl,
    eval([car, [cdr, [quote, [a, b, c]]]], [], [], b),

    print('(cdr (cdr (cdr (quote (a b c d)))))'), nl,
    eval([cdr, [cdr, [cdr, [quote, [a, b, c, d]]]]], [], [], [d]),

    print('(eq 1 1)'), nl,
    eval([eq, 1, 1], [], [], t),

    print('(eq (quote a) (quote a))'), nl,
    eval([eq, [quote, a], [quote, a]], [], [], t),

    print('(eq 1 2)'), nl,
    eval([eq, 1, 2], [], [], nil),

    print('(eq a 2)'), nl,
    eval([eq, a, 2], [[a, 2]], [[a, 2]], t),

    print('(eq (quote (a b)) (quote ()))'), nl,
    eval([eq, [quote, [a, b]], [quote, []]], [], [], nil),
    run("(eq (quote (a b)) (quote ()))", [], [nil]),

    print('(eq (quote ()) (quote ()))'), nl,
    eval([eq, [quote, []], [quote, []]], [], [], t),
    run("(eq (quote ()) (quote ()))", [], [t]),

    print('(cond (quote (nil 42 t 52)))'), nl,
    eval([cond, [quote, [nil, 42, t, 52]]], [], [], 52),
    run("(cond (quote (nil 42 t 52)))", [], [52]),

    print('(cond (quote ((car (quote (nil t))) 42 t 52)))'), nl,
    eval([cond, [quote, [[car, [quote, [nil, t]]], 42, t, 52]]], [], [], 52),
    run("(cond (quote ((car (quote (nil t))) 42 t 52)))", [], [52]),

    print('(cond (quote ((car (quote (t nil))) 42 t 52)))'), nl,
    eval([cond, [quote, [[car, [quote, [t, nil]]], 42, t, 52]]], [], [], 42),
    run("(cond (quote ((car (quote (t nil))) 42 t 52)))", [], [42]),

    print('a (env=((a 52)))'), nl,
    eval(a, [[a, 52]], [[a, 52]], 52),

    print('(cond (quote ((car a), 42, t, 52))) (env=((a (t nil))))'), nl,
    eval([cond, [quote, [[car, a], 42, t, 52]]], [[a, [t, nil]]], [[a, [t, nil]]], 42),

    print('merge env () () ((b 2))'), nl,
    merge_env([], [], [[b, 2]], [[b, 2]]),

    print('merge env (a) (1) ((b 2))'), nl,
    merge_env([a], [1], [[b, 2]], [[a, 1], [b, 2]]),

    print('merge env (a d) (1 5) ((b 2) (a 3))'), nl,
    merge_env([a, d], [1, 5], [[b, 2], [a, 3]], Env), lookup(Env, a, 1),

    print('((lambda (x y) (car (cons x y))) (quote a) (quote (b c)))'), nl,
    eval([[lambda, [x, y], [car, [cons, x, y]]], [quote, a], [quote, [b, c]]], [], [], a),
    run("((lambda (x y) (car (cons x y))) (quote a) (quote (b c)))", [], [a]),

    print('((lambda (x) (cond (quote (x 42 t 52)))) t)'), nl,
    eval([[lambda, [x], [cond, [quote, [x, 42, t, 52]]]], t], [], [], 42),
    run("((lambda (x) (cond (quote (x 42 t 52)))) t)", [], [42]),

    print('((lambda (x y) (x y)) car (quote (a b)))'), nl,
    eval([[lambda, [x, y], [x, y]], car, [quote, [a, b]]], [], [], a),
    run("((lambda (x y) (x y)) car (quote (a b)))", [], [a]),

    print('y combinator for factorial'), nl.
    
% run("(((lambda (f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v)))))) (lambda (f) (lambda (n) (cond (= n 0) 1 (* n (f (- n 1))))))) 2)", [], [2]).
