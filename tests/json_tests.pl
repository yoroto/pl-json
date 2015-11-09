:- use_module(library(plunit)).
:- use_module('../src/json.pl').

:- begin_tests(json).

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

test(positive_small_decimal) :-
    json_parse("1234.1214", 1234.1214).

test(negative_big_decimal) :-
    json_parse("-3.141592653589793238462643383279", -3.141592653589793238462643383279). 

test(decimal_zero) :-
    json_parse("0.00", 0.00).

test(decimal_with_negative_exponent) :-
    json_parse("1.414e-6", 1.414E-6).

test(decimal_with_positive_exponent) :-
    json_parse("16.874874E+15", 1.6874874E+16).

test(empty_string) :-
    json_parse("""""", "").

test(string) :-
    json_parse("""this is a normal string""", "this is a normal string").

test(string_with_escape) :-
    json_parse("""this is a escaped\n string""", "this is a escaped\n string").

test(string_with_unicode_escape) :-
    json_parse("""this is a unicode escaped\x5C\u3AF6 string""", "this is a unicode escaped\x3AF6\ string").

test(string_with_consective_unicode_chars) :-
    json_parse("""three-in-row\\u9AA5\\u01B7\\uFFFFd""", "three-in-row\x9AA5\\x01B7\\xFFFF\d").

test(boolean_true) :-
    json_parse("true\t", true).

test(boolean_false) :-
    json_parse("    false", false).

test(empty_array) :-
    json_parse("[]", []).

test(number_array) :-
    json_parse("  [  1,2,     3, 4  , 5]\n", [1, 2, 3, 4, 5]).

test(mixed_array) :-
    json_parse(" [ 5, ""yoga"", { ""name"" : ""james"", ""age"" : 25 }, [[]] ]", [5, "yoga", obj([ pair(name, "james"), pair(age, 25)]), [[]]]).

test(empty_object) :-
    json_parse("{}", obj([])).

test(plain_object) :-
    json_parse("{\n\t""name"" : ""james"",\n\t""age"" : 25,\n\t""email"" : ""james.rodriguez@realmadrid.com"" }",
               obj([pair(name, "james"), pair(age, 25), pair(email, "james.rodriguez@realmadrid.com")])).

test(nested_object) :-
    json_parse("{\n\t""name"" : ""james rodr\\u00CDguez"",\n\t""age"" : 25.43E+1,\n\t""born"" : {\n\t\t""country"" : ""columbia"",\n\t\t""city"" : ""C\\u00DAcuta"",\n\t\t""Date"" : { ""Year"" : 1991,\n\t\t\t""month"" : ""July"",\n\t\t\t""Day"" : 12\n\t\t}\n\t}\n}",
               obj([pair(name, "james rodr\x00CD\guez"),pair(age,2.543E+2),pair(born,obj([pair(country,"columbia"),pair(city,"C\x00DA\cuta"),pair('Date',obj([pair('Year',1991),pair(month,"July"),pair('Day',12)]))]))])).

:- end_tests(json).