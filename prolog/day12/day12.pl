%
% Advent of Code 2021 - Day 12 - Passage Pathing
%

:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2021/prolog/day12/').

:- dynamic connection/2.
:- dynamic solving_part2/0.

% Support routines - cave is big if a character from its name is in upper case.
cave_size(C, big) :- is_upper(C), !.
cave_size(_, small).

% Detect duplicates in a list.
has_duplicates([Head | Tail]) :-
    member(Head, Tail).
has_duplicates([_ | Tail]) :-
    has_duplicates(Tail).

% DCG Parser for input file - "JK-end" means large cave "JK" is connected to small cave "end"
%
% Parse to facts like:
%
% connection(cave("JK", big), cave("end", small)).
%

identifier_char(Char) -->
	[Char],
	{	char_type(Char, alpha)
	}.

identifier_chars([HChar | TChars]) -->
	identifier_char(HChar),
	identifier_chars(TChars).
identifier_chars([Char]) -->
	identifier_char(Char).

cave(cave(CaveName, CaveSize)) -->
    identifier_chars(IDChars),
    {   string_chars(CaveName, IDChars),
        IDChars = [Ch | _],
        cave_size(Ch, CaveSize)
    }.

connected(A, B) -->
    cave(A), ['-'], cave(B),
    !,
    {   assertz(connection(A, B))
    }.

read_input_line(Stream) :- 
	read_line_to_string(Stream, String),
	string_chars(String, Chars),
	phrase(connected(_, _), Chars, []).

read_input_lines(Stream) :-
	read_input_line(Stream),
	!,
	read_input_lines(Stream).
read_input_lines(_).

read_input_data(FileName) :-
	retractall(connection(_,_)),
	open(FileName, read, Stream),
	read_input_lines(Stream),
	close(Stream).

% Bidirectional connections.
s(A, B) :- connection(A, B).
s(B, A) :- connection(A, B).

% Define a goal:
goal(cave("end",small)).

% Conditions to allow a path to be extended.
allow_add_node(cave(_,big), _).
allow_add_node(cave(Name, small), Path) :-    
    not(member(cave(Name, small), Path)),!.

% Additional clause for part 2 (allow revisit one small cave once only).
allow_add_node(cave(Name1, small), Path) :-
    solving_part2,
    % No revisiting start
    Name1 \== "start",
    % No existing duplicates.
    findall(Name2, member(cave(Name2, small), Path), L1),
    setof(Name3, member(Name3, L1), L2),
    length(L1, N),
    length(L2, N).

% From Bratko:
%
% depthfirst(Path, Node, Solution)
%   extending the path [Node | Path] to a goal gives Solution.
%
depthfirst(Path, Node, [Node | Path]) :-
    goal(Node),!.

depthfirst(Path, Node, Sol) :-
    s(Node, Node1),
    allow_add_node(Node, Path),
    depthfirst([Node | Path], Node1, Sol).

write_solution(Solution) :-
    findall(Name, member(cave(Name,_), Solution), NL1),
    reverse(NL1, NL2),
    atomics_to_string(NL2, ',', String),
    writeln(String).

solve(Node, Solution) :-
    depthfirst([], Node, Solution).

solve_day12_for(FileName, N) :-
    read_input_data(FileName),
    aggregate_all(count, solve(cave("start", small), _), N).

day12_part1 :-
    writeln("Advent of Code 2021"),
    writeln("Day 12, Part 1"),
    retractall(solving_part2),
    solve_day12_for('day12.txt', N),
    write("Number of Paths = "),
    writeln(N).

day12_part2 :-
    writeln("Advent of Code 2021"),
    writeln("Day 12, Part 2"),
    assertz(solving_part2),
    solve_day12_for('day12.txt', N),
    write("Number of Paths = "),
    writeln(N).

