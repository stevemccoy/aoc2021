% Advent of Code 2021 - Day 18.
% (SnailFish Numbers).

:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2021/prolog/day18/').

% DCG Rules.

sfn(s(Left, Right)) -->	['['], sfn(Left), [','], sfn(Right), [']'].
sfn(Integer)		--> parse_integer(Integer).

parse_integer(I) --> digit(D0), digits(D),
        { number_codes(I, [D0|D])
        }.

digits([D|T]) 	--> digit(D), !, digits(T).
digits([])		--> [].

digit(D) --> [D],
        { code_type(D, digit)
        }.


% Parse input using the DCG.

read_input_line(Stream, S) :- 
	read_line_to_string(Stream, String),
	!,
	String \== end_of_file,
	string_chars(String, Chars),
	phrase(sfn(S), Chars, []).

read_input_lines(Stream, [X | Tail]) :-
	read_input_line(Stream, X),
	!,
	read_input_lines(Stream, Tail).
read_input_lines(_, []).

read_input_data(FileName, List) :-
	open(FileName, read, Stream),
	read_input_lines(Stream, List),
	close(Stream).


% Part 1 Problem.
day18_part1 :-
    writeln("Advent of Code 2021"),
    writeln("Day 18, Part 1"),
    solve_for('day18.txt', Sum, Magnitude),
    format("Sum = ~w\nMagnitude = ~w", [Sum, Magnitude]).

% Part 2 Problem.
day18_part2 :-
    writeln("Advent of Code 2021"),
    writeln("Day 18, Part 2"),
    read_input_data('day18.txt', List),
    max_binary_magnitude(List, Magnitude),
    format('Maximum magnitude for pairs of numbers from list = ~w\n', [Magnitude]).

max_binary_magnitude(List, Magnitude) :-
	findall([Tree, Mag], (
		member(T1, List),
		member(T2, List),
		T1 \== T2,
		add_and_reduce(T1, T2, Tree),
		magnitude(Tree, Mag)
	), SolutionList),
	sort(2, @>=, SolutionList, [[_, Magnitude] | _]).


solve_for(FileName, Sum, Mag) :-
	read_input_data(FileName, List),
	sum_tree_list(List, Sum),
	magnitude(Sum, Mag).

% Recursively sum the trees in the given list.
sum_tree_list([Tree1 | Rest], Sum) :-
	accumulate_tree_sum(Tree1, Rest, Sum).

accumulate_tree_sum(Tree, [], Tree).
accumulate_tree_sum(Accumulator, [T1 | Rest], NewTotal) :-
	add_and_reduce(Accumulator, T1, NT1),
	accumulate_tree_sum(NT1, Rest, NewTotal).

% Add trees and do necessary reductions.
add_and_reduce(TotalSoFar, Tree, NewTotal) :-
%	format('Add: ~w\n  +  ~w\n', [TotalSoFar, Tree]),
	add_trees(TotalSoFar, Tree, NT1),
	do_reductions(NT1, NewTotal).
%	format('  =  ~w\n', [NewTotal]).

% Magnitude of a tree.
magnitude(N, N) :-
	integer(N), !.
magnitude(s(A, B), M) :-
	magnitude(A, MA),
	magnitude(B, MB),
	M is 3 * MA + 2 * MB.

% Add two trees together.
add_trees(Tree1, Tree2, s(Tree1, Tree2)).
	

% Repeatedly look for 1. explosion, 2. split, until there are neither in the tree.
do_reductions(Tree, NewTree) :-
	explode(Tree, 4, NT1, _, _), !,
	do_reductions(NT1, NewTree).
do_reductions(Tree, NewTree) :-
	splits(Tree, NT1, yes), !,
	do_reductions(NT1, NewTree).
do_reductions(T, T).


% explode(Tree, DepthCount, NewTree, SpillLeft, SpillRight)
% There may be an explosion in Tree, resulting in NewTree after replacement.
% SpillLeft are the remaining units to be added to the nearest regular number to the left,
% SpillRight are to be added to the nearest neighbour to the right.
% Only the first (left-most) explosion should be activated in any time-step.


% Explode happens here!
explode(s(Left, Right), 0, 0, Left, Right) :- 
	integer(Left),
	integer(Right),
%	format('Explode: (~w, ~w)\n', [Left, Right]),
	!.
	
% Explode happens in left branch.
explode(s(Left, Right), Depth, s(NewLeft, NewRight), SpillLeft, 0) :-
	Depth > 0,
	NewDepth is Depth - 1,
	explode(Left, NewDepth, NewLeft, SpillLeft, SR1),
	!,
	spill_right(Right, SR1, NewRight).

% Explode happens in right branch.
explode(s(Left, Right), Depth, s(NewLeft, NewRight), 0, SpillRight) :-
	Depth > 0,
	NewDepth is Depth - 1,
	explode(Right, NewDepth, NewRight, SL1, SpillRight),
	spill_left(Left, SL1, NewLeft).


spill_left(Tree, Amount, NewTree) :-
	Amount > 0,
	add_rightmost(Tree, Amount, NewTree).
spill_left(Tree, 0, Tree).


add_rightmost(Tree, Amount, NewTree) :-
	integer(Tree),
	NewTree	is Tree + Amount.
add_rightmost(s(L, Tree), Amount, s(L, NewTree)) :-
	add_rightmost(Tree, Amount, NewTree).


spill_right(Tree, Amount, NewTree) :-
	add_leftmost(Tree, Amount, NewTree).
spill_right(Tree, 0, Tree).


add_leftmost(Tree, Amount, NewTree) :-
	integer(Tree),
	NewTree is Tree + Amount.
add_leftmost(s(Tree, R), Amount, s(NewTree, R)) :-
	add_leftmost(Tree, Amount, NewTree).


% Split any regular number that is 10 or above.

splits(N, s(M1, M2), yes) :-
	integer(N),
	N > 9,
	!,
	M1 is div(N, 2),
	M2 is N - M1.
%	format('Split: ~w --> (~w, ~w)\n', [N, M1, M2]).
splits(N, N, no) :-
	integer(N),
	!.
splits(s(L, R), s(NL, R), yes) :-
	splits(L, NL, yes),
	!.
splits(s(L, R), s(L, NR), yes) :-
	splits(R, NR, yes),
	!.
splits(s(L, R), s(L, R), no).
