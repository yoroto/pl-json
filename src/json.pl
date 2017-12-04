:- module( json, [ json_parse/2, json_parse/3,
                   json_parse_file/2
                 ] ).
% based on https://github.com/yoroto/pl-json
% have added line counting and error reporting, reading directly from file
% fixed a few issues with whitespace and avoid backtracking
% return strings using string(.) wrapper to avoid ambiguity with arrays of integers

% parse a JSON file
json_parse_file(File, Json) :-
   read_file(File,Codes),
   %format('Read:~n~s~n',[Codes]),
   json_parse(Codes,Json).

read_file(Filename,Codes) :-
    open(Filename,read,S,[encoding('UTF-8')]),
    read_codes(S,Codes),
    close(S).
read_codes(S,Codes) :-
    get_code(S,Code),
    ( Code < 0 ->
        Codes = []
    ; otherwise ->
        Codes = [Code|Rest],
        read_codes(S,Rest)).

% --------------

json_parse( Chars, Json ) :-
    json_parse( Chars, Json, [] ).

json_parse( Chars, Json, _Options ) :-
    reset_line_nr,
    json(Json, Chars, _).

json(Json) --> ws,!,json(Json).
json(Json) -->
    json1(Json),
    !,
    spaces.
json(Json) --> {var(Json)},check_string("?"), {fail}. % generate error message
%json(Json) --> {ground(Json)}, json(JSon2), {format('### Mismatch:~n ~w~n~w~n',[Json,Json2]),fail}.

% a version that does not print an error message
try_json(Json) --> ws,!,try_json(Json).
try_json(Json) -->
    json1(Json),
    !,
    spaces.

json1(null) --> "null",!.
json1(true) --> "true",!.
json1(false) --> "false",!.

json1(Number) --> number(Number),!.

json1(string(Codes)) -->
    """",!,
    %{print_info(start_string)},
    string(Codes). %, {atom_codes(String,Codes)}.

json1(Array) -->
    "[",!,
    %{print_info(start_array)},
    array(Array),
    check_string("]").

json1(obj(Pairs)) -->
    "{",!,
    %{print_info(start_object)},
    spaces,
    pairs(Pairs),
    spaces,
    check_string("}").

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

string(X) --> string2(X).

string2([]) --> [0'\x22\],
   !.
string2([EscapedChar|T]) -->
    [0'\x5C\],!,
    escape_char(EscapedChar),
    string2(T).
string2([H|T]) -->
    [H],
    string2(T).

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

array([H|T]) -->
    try_json(H), !,{print_info(first_array)},
    array1(T).
array([]) --> [], {print_info(end_array)}.

array1([H|T]) -->
    ",", !,
    json(H),!, {print_info(next_array)},
    array1(T).
array1([]) --> [], {print_info(empty_array)}.

pair(pair(Name, Value),Optional) -->
    spaces,
    ({Optional=optional} -> opt_pair_name(Codes) ; pair_name(Codes)),
    check_string(":"),
    { atom_codes(Name, Codes) },
    {print_info(pair_value_for(Name))},
    json(Value),
    {print_info(found_value_vor(Name,Value))}.

opt_pair_name(Name) --> """", string(Name), spaces.
pair_name(Name) --> check_string(""""), string(Name), spaces.


pairs(List) --> ws,!,pairs(List).
pairs([H|T]) -->
    pair(H,optional), !,
    pairs1(T).
pairs([]) --> [], {print_info(empty_pairs)}.

pairs1(List) --> ws,!,pairs1(List).
pairs1([H|T]) -->
    ",", !,
    pair(H,required),!,
    pairs1(T).
pairs1([]) --> [], {print_info(end_pairs)}.

check_string(List) --> ws,!,check_string(List).
check_string([Char]) --> [Char],!.
check_string(String, [Char|_],_) :-
   cur_line(LineNr),
   format(user_error,'! Error on line ~w: expecting ~s obtained ~s~n',[LineNr,String,[Char]]),
   %trace,
   fail.

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


spaces --> ws,!,spaces.
spaces --> [].

% whitespace
ws --> new_line,!.
ws --> " "; "\t" ; [10] ; [13].

new_line --> "\n",{inc_line_nr}.

% use a fact to keep track of line numbers
:- dynamic cur_line/1.
cur_line(1).
inc_line_nr :- retract(cur_line(N)), N1 is N+1, assert(cur_line(N1)).
reset_line_nr :- retract(cur_line(_)), assert(cur_line(1)).

print_info(_) :- !.
print_info(Error) :- print_error(Error).
print_error(Error) :-
    cur_line(LineNr),
    current_output(X),
    set_output(user_error),
    nl,
    write('! Line: '),write_term(LineNr,[]),nl,
    (var(Error)  -> print_message(error,'_')
    ;  write('! '),write_term(Error,[max_depth(20),numbervars(true)]),nl),
    %% flush_output(user_error), %%
    set_output(X).
