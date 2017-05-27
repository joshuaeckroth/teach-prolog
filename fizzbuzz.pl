
% determine if a number is Fizz (multiple of 3) or Buzz (multiple of 5) or FizzBuzz (multiple of 3 and 5, i.e., 15)

% assume numbers 1..100

:- use_module(library(clpfd)).

fizz(N) :- N in 1..100, 0 #= N mod 3.
buzz(N) :- N in 1..100, 0 #= N mod 5.
fizzbuzz(N) :- fizz(N), buzz(N).


