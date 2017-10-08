
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(clpfd)).

printEach([]).
printEach([T|Terms]) :-
    format("~s~n", [T]),
    printEach(Terms).

concatEach([], "").
concatEach([T|Terms], SNew) :-
    concatEach(Terms, SPrior),
    append(T, ['\n'], TNewline),
    append(TNewline, SPrior, SNew).

ground_regex(Regex) :-
    regex(Terms, Regex, []), !,
    printEach(Terms).

ground_regex(Regex, TermsString) :-
    regex(Terms, Regex, []), !,
    concatEach(Terms, TermsString).

regex(Terms) --> "/", regex_inner(Terms), "/".

regex_inner(Terms) --> terms(LeftTerms), "(", option_group(OptTerms), ")", regex_inner(RestTerms),
    { appendEach(LeftTerms, OptTerms, LeftOptTerms),
      appendEach(LeftOptTerms, RestTerms, Terms) }.
regex_inner(Terms) --> terms(LeftTerms), "(", option_group(OptTerms), ")?", regex_inner(RestTerms),
    { appendEach(LeftTerms, [""|OptTerms], LeftOptTerms),
      appendEach(LeftOptTerms, RestTerms, Terms) }.
regex_inner(Terms) --> terms(LeftTerms), " ", regex_inner(RightTerms),
    { appendEach(LeftTerms, [" "], TermsSpace),
      appendEach(TermsSpace, RightTerms, Terms) }.
regex_inner(Terms) --> terms(Terms).
regex_inner([]).

option_group(Terms) --> regex_inner(Terms).
option_group(Terms) --> regex_inner(LeftTerms), "|", option_group(RightTerms),
    { append(LeftTerms, RightTerms, Terms) }.

terms([Term]) --> text(Term).
terms(Terms) --> text(Term), [A], "?", terms(RightTerms),
    { append(Term, [A], TermA),
      appendEach([Term,TermA], RightTerms, Terms) }.
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


% listen on HTTP
%
% run as: swipl -s regexgrounder.pl -g 'server(10333)'

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
%:- use_module(library(http/http_log)).

server(Port) :- http_server(http_dispatch, [port(Port)]).

:- http_handler('/regexgrounder', handle, [time_limit(30)]).

read_term_from_atom([],[]). % need special case for empty list
read_term_from_atom(A,T) :- read_term_from_atom(A,T,[]).

term_to_dict([], [], Pairs, Dict) :-
    dict_pairs(Dict, _, Pairs).
term_to_dict([VarAtom|VarsAtoms], [Var|Vars], Pairs, Dict) :-
    text_to_string(Var, VarStr),
    term_to_dict(VarsAtoms, Vars, [VarAtom-VarStr|Pairs], Dict).

join_args([Arg], Arg).
join_args([Arg|Args], Out) :-
    join_args(Args, Rest),
    string_concat(Arg, ',', ArgComma),
    string_concat(ArgComma, Rest, Out), !.

extract_vnames([],[]).
extract_vnames([=(VName,_)|Rest], [VName|VNamesRest]) :-
    extract_vnames(Rest, VNamesRest).

vars_to_varnames(Args, VarNames) :-
    join_args(Args, Joined),
    term_string(_, Joined, [variable_names(VNames)]),
    extract_vnames(VNames, VarNames).

handle(Request) :-
    http_read_json(Request, JSONIn),
    json_to_prolog(JSONIn, [Pred|Args]),
    %http_log("~p~n", [[Pred|Args]]),
    maplist(read_term_from_atom, Args, ArgsTerms),
    member(Pred, [ground_regex]),
    term_variables(ArgsTerms, Vars),
    vars_to_varnames(Args, VarNames),
    aggregate_all(set(Dict), (Goal =.. [Pred|ArgsTerms],
                              call(Goal),
                              term_to_dict(VarNames, Vars, [], Dict)),
                  Results),
    reply_json_dict(Results).



