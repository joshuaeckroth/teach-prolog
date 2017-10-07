
% run as: swipl -q -s regexgrounder-tests.pl -t run_tests

:- use_module(library(test_cover)).

:- begin_tests(regexgrounder).
:- [regexgrounder].

test(simple) :-
    ground_regex("/abc/", "abc\n"),
    ground_regex("/abc xyz/", "abc xyz\n").

test(options) :-
    ground_regex("/(abc|def)/", "abc\ndef\n"),
    ground_regex("/abc (def|ghi)/", "abc def\nabc ghi\n"),
    ground_regex("/abc (def|ghi) xyz (tuv|wxy)/", "abc def xyz tuv\nabc def xyz wxy\nabc ghi xyz tuv\nabc ghi xyz wxy\n").

test(qmark) :-
    ground_regex("/abc?/", "ab\nabc\n"),
    ground_regex("/abc?xyz/", "abxyz\nabcxyz\n").

test(optqmark) :-
    ground_regex("/(abc|def)?xyz/", "xyz\nabcxyz\ndefxyz\n"),
    ground_regex("/abc(def|s?)xyz/", "abcdefxyz\nabcxyz\nabcsxyz\n"),
    ground_regex("/(abc|def)? xyz/", " xyz\nabc xyz\ndef xyz\n"),
    ground_regex("/abc (def|s?)xyz/", "abc defxyz\nabc xyz\nabc sxyz\n").

:- end_tests(regexgrounder).

