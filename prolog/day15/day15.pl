%
% Advent of Code 2021 - Day 15.
% 

:- use_module(library(lists)).
:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2021/prolog/day15/').

:- dynamic field/1.
:- dynamic goal/1.

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

myconn(R1/C1/G1, B) :-
	findall(A, conn(R1/C1/G1, A), L1),
	sort(2, @=<, L1, L2),
	member(B, L2).

% Paths and their costs. Path is in reverse order,
% cost is sum of field values for nodes entered along path.

path(Start, Start, [Start], 0).
path(Start, Finish, [Finish | Path], GPath) :-
	path(Start, OneButLast, Path, GTail),
	myconn(OneButLast, Finish),
	not(member(Finish, Path)),
	Finish = _/_/G,
	GPath is GTail + G.


% Best first graph search representation:
% State consists of the path (in reverse order) so far and its total cost.
% Goal is bottom right of grid.
% Successor s(A, B, Cost).
% Heuristic function h(State, Value).
/*
goal([Finish | _]/_) :-
	goal_node(Finish).

% s(A, B, StepCost).
s([LastButOne | Tail]/Cost1, [R/C/G, LastButOne | Tail]/Cost2, G) :-
	conn(LastButOne, R/C/G),
	not(member(R/C/G, Tail)),
	Cost2 is Cost1 + G.

% Heuristic - Manhattan distance.
h([R/C/_ | _]/_, H) :-
	goal_node(GR/GC/_),
	H is abs(GR - R) + abs(GC - C).
*/

s(A, R/C/G, G) :- conn(A, R/C/G).

% Heuristic - Manhattan distance.
h(R/C/_, H) :-
	goal(GR/GC/_),
	DR is GR - R,
	DC is GC - C,
	H is sqrt(DR * DR + DC * DC).

setup_goal :-
	retractall(goal(_)),
	findall(C1, field(1/C1/_), CL1),
	max_list(CL1, MC),
	findall(R1, field(R1/1/_), RL1),
	max_list(RL1, MR),
	field(MR/MC/MG),
	assertz(goal(MR/MC/MG)).

path_cost([_], 0).
path_cost([_/_/G | T], Cost) :-
	path_cost(T, TailCost),
	Cost is TailCost + G.

solve_for(FileName, Path, Cost) :-
	read_input_data(FileName),
	setup_goal,
	field(1/1/G1),
	bestfirst(1/1/G1, Path),
	path_cost(Path, Cost),
	format('Solution is ~w\nwith cost ~w\n', [Path, Cost]).


pixel(R, C, '#') :-	field(R/C/G), G > 5, !.
pixel(_, _, ' ').

pixel_row(R, String) :-
	goal(_/MC/_),
	findall(Ch, (between(1, MC, C), pixel(R, C, Ch)), CL1),
	string_chars(String, CL1).

display_pixels :-
	goal(NR/_/_),
	between(1, NR, R),
	pixel_row(R, String),
	writeln(String),
	fail.

day15_part1 :-
	writeln('Advent of Code 2021.\nDay 15, Part 1.'),
	writeln('Test run...'),
	solve_for('test15.txt', _, _),
	writeln('Full run...'),
	solve_for('input15.txt', _, _),
	writeln('All done.').
