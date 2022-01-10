%
% Advent of Code 2021 - Day 15.
% 

:- use_module(library(lists)).
:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2021/prolog/day15/').

:- dynamic field/1.

store_field_row([]).
store_field_row([X | Tail]) :-
	assertz(field(X)),
	!,
	store_field_row(Tail).

read_input_line(Stream, RowNum) :- 
	read_line_to_string(Stream, String),
	String \= end_of_file,
	string_codes(String, Codes),
	maplist(plus(-48), Codes, Digits),
	findall(RowNum/Col/Value, nth1(Col, Digits, Value), Row),
	store_field_row(Row).

read_input_lines(Stream, RowNum) :-
	read_input_line(Stream, RowNum),
	!,
	N is RowNum + 1,
	read_input_lines(Stream, N).
read_input_lines(_,_).

read_input_data(FileName) :-
	retractall(field(_)),
	open(FileName, read, Stream),
	read_input_lines(Stream, 1),
	close(Stream).


conn(R/C1/V1, R/C2/V2) :-
	field(R/C1/V1),
	member(D, [-1,1]),
	C2 is C1 + D,
	field(R/C2/V2).
conn(R1/C/V1, R2/C/V2) :-
	field(R1/C/V1),
	member(D, [-1,1]),
	R2 is R1 + D,
	field(R2/C/V2).

