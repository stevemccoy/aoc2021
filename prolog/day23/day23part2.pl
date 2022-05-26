/*
	Advent of Code 2021 - Day 23 (Amphipods) - Part 2.
*/

% Connections between locations in burrows and the hallway.
connection(h1,h2).
connection(h2,ha).
connection(ha,h3).
connection(h3,hb).
connection(hb,h4).
connection(h4,hc).
connection(hc,h5).
connection(h5,hd).
connection(hd,h6).
connection(h6,h7).

connection(ha,a1).
connection(a1,a2).
connection(a2,a3).
connection(a3,a4).

connection(hb,b1).
connection(b1,b2).
connection(b2,b3).
connection(b3,b4).

connection(hc,c1).
connection(c1,c2).
connection(c2,c3).
connection(c3,c4).

connection(hd,d1).
connection(d1,d2).
connection(d2,d3).
connection(d3,d4).

% Identify where each type needs to go (in order from hallway inwards).
home_for(a, [a1,a2,a3,a4]).
home_for(b, [b1,b2,b3,b4]).
home_for(c, [c1,c2,c3,c4]).
home_for(d, [d1,d2,d3,d4]).

% Bidirectional connection between locations.
adjacent(A, B) :-
	connection(A, B)
	;
	connection(B, A).

in_hallway(H) :- 
    member(H, [h1,h2,h3,h4,h5,h6,h7]).

% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% Reorder the elements of one list into another.	
permutation([], []).
permutation([Head | Tail], PermList) :-
	permutation(Tail, PermTail),
	del(Head, PermList, PermTail).

% Sequence of repeated values in a list.
replicate(N, X, Xs) :-
	length(Xs, N),
	maplist(=(X), Xs).

% path(A, B, Path) - Path is a path from location A to B, 
% with no cycles, through valid connections,
% but ignoring whether the path is open or not.
path(A, B, Path) :-
	path1(A, [B], Path).

path1(A, [A | Path1], [A | Path1]).
path1(A, [Y | Path1], Path) :-
	adjacent(X, Y),
	not(member(X, Path1)),			% No cycles in the path.
	path1(A, [X, Y | Path1], Path).

% Validate a candidate path portion can be travelled (all locations must be unoccupied)

path_open(_, []).
path_open(State, [H | T]) :-
	not(member(_/H, State)),
	path_open(State, T).

% Evaluate the cost of a possible move.
move_cost(Type, Path, Cost) :-
	member(Type/StepCost, [a/1, b/10, c/100, d/1000]),
	length(Path, Length),
	Steps is Length - 1,
	Cost is Steps * StepCost.

% picture(State, Locations, Types)
% Convert list of locations into the types at each location (use x for empty).
picture(_, [], []).
picture(State, [Loc1 | Locations], [Type1 | Types]) :-
	member(Type1/Loc1, State),
	!,
	picture(State, Locations, Types).
picture(State, [_ | Locations], [x | Types]) :-
	picture(State, Locations, Types).

% Match picture to sequence of [x* Type*]
picture_packed_for(Picture, Type) :-
	conc(L1, L2, Picture),
	replicate(_, x, L1),
	replicate(_, Type, L2).

% Test the given state to make sure it is OK to move an amphipod of the given
% type into its home burrow.
no_strangers_home(State, Type) :-
	home_for(Type, HL),
	picture(State, HL, Picture),
	picture_packed_for(Picture, Type).

% Move to get an amphipod home.
homing_move(Before, move(Type, From, To), Path, [Type/To | Inter1], Cost) :-
	del(Type/From, Before, Inter1),
	home_for(Type, HL),
	member(To, HL),
	path(From, To, Path),
	Path = [_ | Tail],
	path_open(Before, Tail),
	no_strangers_home(Before, Type),
	move_cost(Type, Path, Cost).

% Move an amphipod out into the hallway.
hallway_move(Before, move(Type, From, To), Path, [Type/To | Inter1], Cost) :-
	del(Type/From, Before, Inter1),
	not(in_hallway(From)),
	path(From, To, Path),
	in_hallway(To),
	Path = [_ | Tail],
	path_open(Before, Tail),
	move_cost(Type, Path, Cost).

% Successor of N is M with a cost of M:
s(N, M, C) :-
	homing_move(N, _, _, M, C)
	;
	hallway_move(N, _, _, M, C).

% Heuristic function.
% h(_, 0).

h(State, HCost) :-
	findall(Cost, (
		member(Type/Src, State),
		home_for(Type, [Dest | Tail]),
		not(member(Type/Src, Tail)),
		path(Src, Dest, Path),
		move_cost(Type, Path, Cost)
	), CL1),
	sum_list(CL1, HCost).

start_state([
	d/a1, d/a2, d/a3, d/a4, c/b1, c/b2, b/b3, c/b4,
	a/c1, b/c2, a/c3, b/c4,	b/d1, a/d2, c/d3, a/d4 ]).
goal_state([
	d/d1, d/d2, d/d3, d/d4, c/c1, c/c2, c/c3, c/c4,
	b/b1, b/b2, b/b3, b/b4, a/a1, a/a2, a/a3, a/a4 ]).

mid_state([
	d/h7, d/a2, d/a3, d/a4, c/h5, c/b2, b/b3, c/b4,
	a/c1, b/c2, a/c3, b/c4,	b/h3, a/d2, c/d3, a/d4 ]).

% Are the two given states the same?
matching_states([], []).
matching_states([H1 | Tail1], State2) :-
	del(H1, State2, State2Tail),
	matching_states(Tail1, State2Tail).

goal(State) :-
	goal_state(Goal),
	matching_states(State, Goal).


% depthfirst2(Node, Solution, MaxCost)
depthfirst2(Node, [Node], _) :-
	goal(Node).
depthfirst2(Node, [Node | Sol], MaxCost) :-
	MaxCost > 0,
	s(Node, Node1, Cost), 
	Max1 is MaxCost - Cost,
	depthfirst2(Node1, Sol, Max1).

solution_cost([_], 0) :- !.
solution_cost([A, B | Tail], Cost) :-
    solution_cost([B | Tail], Before),
    s(A, B, C1),
    Cost is Before + C1.

day23_part2 :-
    writeln('Advent of Code 2021 - Day 23, Part 2'),
    start_state(Start),
    write('Starting from '),
    writeln(Start),
    writeln('...'),
    !,
    depthfirst2(Start, Solution, 1000), % 47236),
    writeln('Done. Solution is:'),
    writeln(Solution),
    solution_cost(Solution, MinCost),
    write('Solution cost = '),
    writeln(MinCost).
