
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(clpfd)).

printEach([]).
printEach([T|Terms]) :-
    format("~s~n", [T]),
    printEach(Terms).

ground_regex(Regex) :-
    regex(Terms, Regex, []), !,
    printEach(Terms).

regex(Terms) --> "/", regex_inner(Terms), "/".

regex_inner(Terms) --> "(", option_group(OptTerms), ")", regex_inner(RestTerms),
    { appendEach(OptTerms, RestTerms, Terms) }.
regex_inner(Terms) --> terms(LeftTerms), " ", regex_inner(RightTerms),
    { appendEach(LeftTerms, [" "], TermsSpace),
      appendEach(TermsSpace, RightTerms, Terms) }.
regex_inner(Terms) --> terms(Terms).
regex_inner([]).

option_group(Terms) --> regex_inner(Terms).
option_group(Terms) --> regex_inner(LeftTerms), "|", option_group(RightTerms),
    { append(LeftTerms, RightTerms, Terms) }.

terms([Term]) --> text(Term).
text([A|As]) --> [A], { char_type(A, alnum) }, text(As).
text([]) --> [].

num(N) --> digitnonzero(D), { number_codes(N, [D]) }.
num(N) --> digitnonzero(D), digits(Digits, 4), { number_codes(N, [D|Digits]) }.
digitnonzero(D) --> [D], { char_type(D, digit), dif(D, '0') }.
digits([D|Ds], MaxLength) -->
    { MaxLength #> 0, RestLength #= MaxLength - 1 },
    [D], { char_type(D, digit) }, digits(Ds, RestLength).
digits([], _) --> [].

appendEach([E|Elements], [], [E|Elements]).
appendEach([], _, []).
appendEach([Element|Elements], Lists, NewLists3) :-
    appendEachSingle(Element, Lists, NewLists),
    appendEach(Elements, Lists, NewLists2),
    append(NewLists, NewLists2, NewLists3).

appendEachSingle(_, [], []).
appendEachSingle(Element, [Term|Terms], [NewTerm|NewTerms]) :-
    append(Element, Term, NewTerm),
    appendEachSingle(Element, Terms, NewTerms).

