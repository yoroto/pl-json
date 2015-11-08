:- use_module(library(plunit)).

:- begin_tests(json).

%%:- ensure_loaded('../src/json.pl').
:- use_module('../src/json.pl').

test(small_positive_integer) :-
    json_parse("114", 114).

test(big_positive_integer) :-
    json_parse("2343534513", 2343534513).

test(zero) :-
    json_parse("0", 0).

test(small_negative_integer) :-
    json_parse("-8", -8).

test(big_negative_integer) :-
    json_parse("-803298238", -803298238).

test(empty_string) :-
    json_parse("""""", "").

test(string) :-
    json_parse("""this is a normal string""", "this is a normal string").

test(string_with_escape) :-
    json_parse("""this is a escaped\n string""", "this is a escaped\n string").

test(string_with_unicode_escape) :-
    json_parse("""this is a unicode escaped\x5C\u3AF6 string""", "this is a unicode escaped\x3AF6\ string").

:- end_tests(json).