
% determine if a number is Fizz (multiple of 3) or Buzz (multiple of 5) or FizzBuzz (multiple of 3 and 5, i.e., 15)

% assume numbers 1..100

:- use_module(library(clpfd)).

fizzbuzz(N) :- N in 1..100, 0 #= N mod 15.
fizz(N) :- N in 1..100, 0 #= N mod 3, 0 #\= N mod 5.
buzz(N) :- N in 1..100, 0 #= N mod 5, 0 #\= N mod 3.

numToSymbol(N, fizzbuzz) :- fizzbuzz(N).
numToSymbol(N, fizz) :- fizz(N).
numToSymbol(N, buzz) :- buzz(N).
numToSymbol(N, N) :- integer(N).

fizzbuzzList([], []).
fizzbuzzList([N|Nums], [Sym|Result]) :-
    numToSymbol(N, Sym),
    fizzbuzzList(Nums, Result).


