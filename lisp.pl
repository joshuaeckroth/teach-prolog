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

cond([Pred, Clause|_], Env, Result) :-
    eval(Pred, Env, _, t),
    eval(Clause, Env, _, Result).

cond([_, _|Rest], Env, Result) :-
    cond(Rest, Env, Result).

eval_each([], Env, Env, []).
eval_each([First|Rest], Env, NewEnv2, [FirstResult|RestResult]) :-
    eval(First, Env, NewEnv, FirstResult),
    eval_each(Rest, NewEnv, NewEnv2, RestResult).

eval(Sym, Env, Env, Sym) :- member(Sym, [t, nil, car, cdr, cons, eq, cond, lisp,
                                         atomp, nilp, +, -, /, *, <, >, <=, >=]).

eval([lambda|Forms], Env, Env, [lambda|Forms]).

eval([[lambda|Forms]|Args], Env, Env, Result) :-
    apply([lambda|Forms], Args, Env, Result).

eval([quote|[Forms]], Env, Env, Forms).

eval([def,Sym|[Forms]], Env, NewEnv, [quote, Sym]) :-
    merge_env([Sym], [Forms], Env, NewEnv).

eval([Fn|Forms], Env, NewEnv2, Result) :-
    eval(Fn, Env, NewEnv, FnEvaled),
    eval_each(Forms, NewEnv, NewEnv2, FormsEvaled),
    apply(FnEvaled, FormsEvaled, NewEnv2, Result).

eval(Number, Env, Env, Number) :- number(Number).
eval(Atom, Env, Env, Result) :- atomp(Atom), lookup(Env, Atom, Result).

apply(car, [Forms], _, Result) :-
    car(Forms, Result).

apply(cdr, [Forms], _, Result) :-
    cdr(Forms, Result).

apply(cons, Forms, _, Result) :-
    cons(Forms, Result).

apply(eq, [X, X], _, t) :- atomp(X).
apply(eq, [[], []], _, t).
apply(eq, _, _, nil).

apply(listp, [Forms], _, t) :- listp(Forms).
apply(listp, _, _, nil).

apply(atomp, [Forms], _, t) :- atomp(Forms).
apply(atomp, _, _, nil).

apply(nilp, [Forms], _, t) :- nilp(Forms).
apply(nilp, _, _, nil).

apply(+, [X, Y], _, Z) :- Z is X + Y.
apply(-, [X, Y], _, Z) :- Z is X - Y.
apply(*, [X, Y], _, Z) :- Z is X * Y.
apply(/, [X, Y], _, Z) :- Z is X / Y.

apply(<, [X, Y], _, t) :- X < Y.
apply(<, [_, _], _, nil).

apply(>, [X, Y], _, t) :- X > Y.
apply(>, [_, _], _, nil).

apply(<=, [X, Y], _, t) :- X =< Y. % swi-prolog's form of <=
apply(<=, [_, _], _, nil).

apply(>=, [X, Y], _, t) :- X >= Y.
apply(>=, [_, _], _, nil).

apply(cond, [Forms], Env, Result) :-
    cond(Forms, Env, Result).

apply([lambda, Params|[Body]], Vals, Env, Result) :-
    term_rewrite(Body, Params, Vals, RewrittenBody),
    eval(RewrittenBody, Env, _, Result).

get_binding(Param, [Param|_], [Val|_], Val).
get_binding(Param, [_|Params], [_|Vals], Val) :-
    get_binding(Param, Params, Vals, Val).

remove_binding(Param, [Param|Params], [_|Vals], Params, Vals).
remove_binding(Param, [P|Params], [V|Vals], [P|NewParams], [V|NewVals]) :-
    remove_binding(Param, Params, Vals, NewParams, NewVals).
remove_binding(_, Params, Vals, Params, Vals).

remove_bindings([], Params, Vals, Params, Vals).
remove_bindings([Param|Rest], Params, Vals, NewParams2, NewVals2) :-
    remove_binding(Param, Params, Vals, NewParams, NewVals),
    remove_bindings(Rest, NewParams, NewVals, NewParams2, NewVals2).

term_rewrite([], _, _, []).

term_rewrite([quote|Body], _, _, [quote|Body]).

term_rewrite([lambda,LambdaParams|Body], Params, Vals, [lambda,LambdaParams|RewrittenBody]) :-
    remove_bindings(LambdaParams, Params, Vals, NewParams, NewVals),
    term_rewrite(Body, NewParams, NewVals, RewrittenBody).

term_rewrite([def, DefParam|Body], Params, Vals, [def,DefParam|RewrittenBody]) :-
    remove_binding(DefParam, Params, Vals, NewParams, NewVals),
    term_rewrite(Body, NewParams, NewVals, RewrittenBody).

term_rewrite([Forms|Rest], Params, Vals, [RewrittenForms|RewrittenRest]) :-
    listp(Forms),
    term_rewrite(Forms, Params, Vals, RewrittenForms),
    term_rewrite(Rest, Params, Vals, RewrittenRest).

term_rewrite([Atom|Rest], Params, Vals, [Binding|RewrittenRest]) :-
    get_binding(Atom, Params, Vals, Binding),
    term_rewrite(Rest, Params, Vals, RewrittenRest).

term_rewrite([Atom|Rest], Params, Vals, [Atom|RewrittenRest]) :-
    term_rewrite(Rest, Params, Vals, RewrittenRest).

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
symbol([A|As]) --> [A], {member(A, "+/-*<>=") ;
                         code_type(A, alpha)}, symbolr(As).
symbolr([A|As]) --> [A], {member(A, "+/-*<>=") ;
                          code_type(A, alnum)}, symbolr(As).
symbolr([]) --> [].

parse(S, P) :- phrase(sexp_many(P), S).

run(S, NewEnv, Result) :- parse(S, P), !, eval_each(P, [], NewEnv, Result).

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

    print('((lambda (a b) (+ a b)) 3 5)'), nl,
    run("((lambda (a b) (+ a b)) 3 5)", [], [8]),

    print('(((lambda (a) (lambda (b) (+ a b))) 3) 5)'), nl,
    run("(((lambda (a) (lambda (b) (+ a b))) 3) 5)", [], [8]),

    print('(def fact (lambda (n) (cond ''((eq n 0) 1 t (* n (fact (- n 1))))))) (fact 5)'), nl,
    run("(def fact (lambda (n) (cond '((eq n 0) 1 t (* n (fact (- n 1))))))) (fact 5)", _, [[quote, fact], 120]),

    print('(def a (lambda (x) (+ x y))) (def y 5) (a 6)'), nl,
    run("(def a (lambda (x) (+ x y))) (def y 5) (a 6)", _, [[quote, a], [quote, y], 11]),

    print('y combinator for factorial'), nl,
    run("(((lambda (f) ((lambda (x) (f (lambda (v) ((x x) v)))) (lambda (x) (f (lambda (v) ((x x) v)))))) (lambda (f) (lambda (n) (cond '((eq n 0) 1 t (* n (f (- n 1)))))))) 2)", [], [2]).
