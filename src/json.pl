:- module( json, [ json_parse/2, json_parse/3 ] ).

json_parse( Chars, Json ) :-
    json_parse( Chars, Json, [] ).

json_parse( Chars, Json, _Options ) :-
    json(Json, Chars, _).

json(Json) -->
    spaces,
    json1(Json),
    spaces.

json1(null) --> "null".
json1(true) --> "true".
json1(false) --> "false".

json1(Number) --> number(Number).

json1(String) -->
    """",
    string(String).

json1(Array) -->
    "[",
    array(Array),
    "]".

json1(obj(Pairs)) -->
    "{",
    spaces,
    pairs(Pairs),
    spaces,
    "}".

number(Number) -->
    nm_token(NmCodes),
    { number_codes(Number, NmCodes) }.

nm_token([H|T]) -->
    [H],
    { minus(H);digit_table(H) },
    nm_token1(T).

nm_token1([0'\x2E\|T]) -->
    ".",!,
    nm_frac(T).

nm_token1([H|T]) -->
    [H],
    { digit_table(H) }, !,
    nm_token1(T).

nm_token1([]) --> [].


nm_frac([0'\x45\,H|T]) -->
    ("e";"E"),!, 
    [H], {minus(H);plus(H)},
    nm_exp(T).

nm_frac([H|T]) -->
    [H],
    { digit_table(H) }, !,
    nm_frac(T).

nm_frac([]) --> [].

nm_exp([H|T]) -->
    [H],
    { digit_table(H) }, !,
    nm_exp1(T).

nm_exp1([H|T]) -->
    [H],
    { digit_table(H) }, !,
    nm_exp1(T).

nm_exp1([]) --> [].

string([]) --> [0'\x22\], !.

string([EscapedChar|T]) -->
    [0'\x5C\],!,
    escape_char(EscapedChar),
    string(T).

string([H|T]) -->
    [H],
    string(T).

escape_char( 0'\x22\ ) --> [0'\x22\]. 
escape_char( 0'\x5C\ ) --> [0'\x5C\].
escape_char( 0'\x2F\ ) --> [0'\x2F\].
escape_char( 0'\x08\ ) --> [0'\x62\]. %b
escape_char( 0'\x0C\ ) --> [0'\x66\]. %f
escape_char( 0'\x0A\ ) --> [0'\x6E\]. %n
escape_char( 0'\x0D\ ) --> [0'\x72\]. %r
escape_char( 0'\x09\ ) --> [0'\x74\]. %t

escape_char( Code ) -->
        "u",
        hex_digit_char( H1 ),
        hex_digit_char( H2 ),
        hex_digit_char( H3 ),
        hex_digit_char( H4 ),
        { Code is (((H1 << 4 + H2) << 4 + H3) << 4 + H4) }.

hex_digit_char( 0 ) --> "0".
hex_digit_char( 1 ) --> "1".
hex_digit_char( 2 ) --> "2".
hex_digit_char( 3 ) --> "3".
hex_digit_char( 4 ) --> "4".
hex_digit_char( 5 ) --> "5".
hex_digit_char( 6 ) --> "6".
hex_digit_char( 7 ) --> "7".
hex_digit_char( 8 ) --> "8".
hex_digit_char( 9 ) --> "9".
hex_digit_char( 10 ) --> "A".
hex_digit_char( 11 ) --> "B".
hex_digit_char( 12 ) --> "C".
hex_digit_char( 13 ) --> "D".
hex_digit_char( 14 ) --> "E".
hex_digit_char( 15 ) --> "F".
hex_digit_char( 10 ) --> "a".
hex_digit_char( 11 ) --> "b".
hex_digit_char( 12 ) --> "c".
hex_digit_char( 13 ) --> "d".
hex_digit_char( 14 ) --> "e".
hex_digit_char( 15 ) --> "f".

array([]) --> [].
array([H|T]) -->
    ",", json(H), array(T).
array([H|T]) -->
    json(H), array(T).

pair(pair(Name, Value)) -->
    spaces, pair_name(Codes), ":", json(Value),
    { atom_codes(Name, Codes) }.

pair_name(Name) --> """", string(Name), spaces.

pairs([]) --> [].
pairs([H|T]) -->
    ",", !, pair(H), pairs(T).
pairs([H|T]) -->
    pair(H), pairs(T).

spaces( [], [] ).
spaces( [Char|Chars0], Chars1 ) :-
    ( Char =< 32 ->
        spaces( Chars0, Chars1 )
    ; otherwise ->
        Chars1 = [Char|Chars0]
    ).

minus( 0'- ).
plus( 0'+ ).
digit_table( 0'0 ).
digit_table( 0'1 ).
digit_table( 0'2 ).
digit_table( 0'3 ).
digit_table( 0'4 ).
digit_table( 0'5 ).
digit_table( 0'6 ).
digit_table( 0'7 ).
digit_table( 0'8 ).
digit_table( 0'9 ).
