
from graphviz import *

g1 = [ \
        "0 [label=\"f(a).\lf(b).\lg(a).\lg(b).\lh(b).\lk(X) :- f(X), g(X), h(X).\", shape=\"none\"];",
        "1 [label=\"?- k(Y)\", shape=\"box\"]; 1 -> 2 [label=\"Y = _G34\"];",
        "2 [label=\"?- f(_G34), g(_34), h(_34)\", shape=\"box\"]; 2 -> 3 [label=\"_G34 = a\"];",
        "3 [label=\"?- g(a), h(a)\", shape=\"box\"]; 3 -> 4;",
        "4 [label=\"?- h(a)\", shape=\"box\"]; 4 -> 5;",
        "5 [label=\":(\", shape=\"none\"]; 2 -> 6 [label=\"_G34 = b\"];",
        "6 [label=\"?- g(b), h(b)\", shape=\"box\"]; 6 -> 7;",
        "7 [label=\"?- h(b)\", shape=\"box\"]; 7 -> 8;",
        "8 [label=\":)\", shape=\"none\"];" ]

create_graph("ex1", g1)


g2 = [ \
        "0 [label=\"loves(vincent, mia).\lloves(marcellus, mia).\ljealous(A, B) :- loves(A, C), loves(B, C).\", shape=\"none\"];",
        "1 [label=\"?- jealous(X, Y)\", shape=\"box\"]; 1 -> 2 [label=\"X = _G5,\lY = _G7\"];",
        "2 [label=\"?- loves(_G5, _G6), loves(_G7, _G6)\", shape=\"box\"]; 2 -> 3 [label=\"_G5 = vincent,\l_G6 = mia\"];",
        "3 [label=\"?- loves(_G7, mia)\", shape=\"box\"]; 3 -> 4 [label=\"_G7 = vincent\"];",
        "4 [label=\":)\", shape=\"none\"]; 3 -> 5 [label=\"_G7 = marcellus\"];",
        "5 [label=\":)\", shape=\"none\"]; 2 -> 6 [label=\"_G5 = marcellus,\l_G6 = mia\"];",
        "6 [label=\"?- loves(_G7, mia)\", shape=\"box\"]; 6 -> 7 [label=\"_G7 = vincent\"];",
        "7 [label=\":)\", shape=\"none\"];" ]

create_graph("ex2", g2)


g3 = [ \
        "0 [label=\"member(X, [X|_]).\lmember(X, [_|T]) :- member(X, T).\", shape=\"none\"];",
        "1 [label=\"?- member(X, [a,b,c])\", shape=\"box\"]; 1 -> 2 [label=\"X = _G345\"];",
        "2 [label=\"?- member(_G345, [a,b,c])\", shape=\"box\"]; 2 -> 3 [label=\"_G345 = a\"];",
        "3 [label=\"?- member(a, [a,b,c])\", shape=\"box\"];",
        "4 [label=\"?- member(a, [a|_])\", shape=\"box\"]; 3 -> 4;",
        "5 [label=\":)\", shape=\"none\"]; 4 -> 5;" ]


create_graph("ex3", g3)

g4 = [ \
        "0 [label=\"member(X, [X|_]).\lmember(X, [_|T]) :- member(X, T).\", shape=\"none\"];",
        "1 [label=\"?- member(d, [a,b,c])\", shape=\"box\"]; 1 -> 2;",
        "2 [label=\"?- member(d, [a|_])\", shape=\"box\"]; 2 -> 3;",
        "3 [label=\":(\", shape=\"none\"]; 1 -> 4;",
        "4 [label=\"?- member(d, [_|[b,c]])\", shape=\"box\"]; 4 -> 5;",
        "5 [label=\"?- member(d, [b,c])\", shape=\"box\"]; 5 -> 6;",
        "6 [label=\"?- member(d, [b|_])\", shape=\"box\"]; 6 -> 7;",
        "7 [label=\":(\", shape=\"none\"]; 5 -> 8;",
        "8 [label=\"?- member(d, [_|[c]])\", shape=\"box\"]; 8 -> 9;",
        "9 [label=\"?- member(d, [c])\", shape=\"box\"]; 9 -> 10;",
        "10 [label=\"?- member(d, [c|_])\", shape=\"box\"]; 10 -> 11;",
        "11 [label=\":(\", shape=\"none\"]; 9 -> 12;",
        "12 [label=\"?- member(d, [_|[]])\", shape=\"box\"]; 12 -> 13;",
        "13 [label=\"?- member(d, [])\", shape=\"box\"]; 13 -> 14;",
        "14 [label=\":(\", shape=\"none\"];" ]

create_graph("ex4", g4)


g5 = [ \
        "0 [label=\"f(a).\lf(b).\lg(a, a).\lg(b, c).\lh(b).\lh(c).\lk(X, Y) :- f(x), g(X, Y), h(Y).\", shape=\"none\"];",
        "1 [label=\"?- k(X, c)\", shape=\"box\"]; 1 -> 2 [label=\"X = _G336\"];",
        "2 [label=\"?- k(_G336, c)\", shape=\"box\"]; 2 -> 3;",
        "3 [label=\"f(_G336), g(_G336, c), h(c)\", shape=\"box\"]; 3 -> 4 [label=\"_G336 = a\"];",
        "4 [label=\"f(a), g(a, c), h(c)\", shape=\"box\"]; 4 -> 5;",
        "5 [label=\"g(a, c), h(c)\", shape=\"box\"]; 5 -> 6;",
        "6 [label=\":(\", shape=\"none\"]; 3 -> 7 [label=\"_G336 = b\"];",
        "7 [label=\"f(b), g(b, c), h(c)\", shape=\"box\"]; 7 -> 8;",
        "8 [label=\"g(b, c), h(c)\", shape=\"box\"]; 8 -> 9;",
        "9 [label=\"h(c)\", shape=\"box\"]; 9 -> 10;",
        "10 [label=\":)\", shape=\"none\"];" ]

create_graph("ex5", g5)




