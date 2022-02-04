%
% Advent of Code 2021 - Day 23 - Amphipods
%

:- working_directory(_, 'C:/Users/stephen.mccoy/github/aoc2021/prolog/day23/').

:- dynamic connection/2.
:- dynamic path_cost_lookup/3.

% Definitions of locations in the 
hallway([h1,h2,ha,h3,hb,h4,hc,h5,hd,h6,h7]).
home_for(a, [a1,a2]).
home_for(b, [b1,b2]).
home_for(c, [c1,c2]).
home_for(d, [d1,d2]).

% Model for graph connections:
connected_sequence(L1) :- hallway(L1).
connected_sequence([ha,a1,a2]).
connected_sequence([hb,b1,b2]).
connected_sequence([hc,c1,c2]).
connected_sequence([hd,d1,d2]).

% Add the connection arcs to the graph for a single sequence of connected locations.
add_connections([_]).
add_connections([A, B | Tail]) :-
    assertz(connection(A, B)), !,
    add_connections([B | Tail]).

% Add the connections declared for a list of connected sequences.
make_connections([]).
make_connections([Sequence | Sequences]) :-
    add_connections(Sequence), !,
    make_connections(Sequences).

% Add the facts comprising the graph of connected locations in the problem.
make_graph :-
    retractall(connection(_,_)),
    findall(Seq, connected_sequence(Seq), Sequences),
    writeln(Sequences), !,
    make_connections(Sequences).

:- make_graph.

% Bidirectional connections.
bconn(A, B) :- connection(A, B).
bconn(B, A) :- connection(A, B).

% State gives the location of each of the 8 amphipods, like so:
%        A   A   B   B   C   C   D   D
% state(c1, d2, d1, c2, b1, b2, a1, a2).
%
% (all other locations are empty)
% 

% Rules:

% Never stop on the space immediately outside a room i.e. any of [ha,hb,hc,hd], keep going.
% Never move from hallway into a room unless it is its home and contains no different amphipods.
% Once amphipod has stopped in the hallway, it will stay there until it can move into a room.

start_state([c1, d2, d1, c2, b1, b2, a1, a2]).
test_state([h1, h2, h4, h5, h7, h3, a1, a2]).

goal([A1, A2, B1, B2, C1, C2, D1, D2]) :-
    home_for(a, AL), member(A1, AL), member(A2, AL),
    home_for(b, BL), member(B1, BL), member(B2, BL),
    home_for(c, CL), member(C1, CL), member(C2, CL),
    home_for(d, DL), member(D1, DL), member(D2, DL).

% Generic List Stuff...
% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

% Indexed access to a list element (indexes start at 1).
get_item(1, [Item | _], Item).
get_item(N, [_ | Tail], Item) :-
	N > 1,
	M is N - 1,
	get_item(M, Tail, Item).

index_of(Item, [Item | _], 1).
index_of(Item, [_ | Tail], N) :-
    index_of(Item, Tail, M),
    N is M + 1.

% set_item(Index, Before, Item, After)
set_item(1, [_ | Tail], Item, [Item | Tail]).
set_item(N, [Head | Tail], Item, [Head | Tail2]) :-
	N > 1,
	M is N - 1,
	set_item(M, Tail, Item, Tail2).

% Define moves.
%
% A move is a sequence of locations that an amphipod can occupy, between its start in one state
% and finish in another state.
% 
% move(State1, [Locations], State2, Cost).
%
% Locations must be linked by connections; starting location must be where an amphipod is in State1.

in_hallway(H) :- 
    member(H, [h1,h2,h3,h4,h5,h6,h7]).

% Never stop on the space immediately outside a room i.e. any of [ha,hb,hc,hd], keep going.
legal_path_end(S, F) :-
    home_for(_, L1),
    member(S, L1),
    !,
    in_hallway(F).
legal_path_end(S, F) :-
    in_hallway(S),
    !,
    home_for(_, [F, _]).

% path(A, B, Path) - Path is a path (in reverse order) from location A to B, through valid connections,
% but ignoring whether the path is open or not.
path(Node, Node, [Node]).
path(FirstNode, LastNode, [LastNode | Path]) :-
	path(FirstNode, OneButLast, Path),
	bconn(OneButLast, LastNode),
	not(member(LastNode, Path)).

% legal_path_rec(State, A, B, Path).
legal_path_rec(_, A, A, [A]) :- nonvar(A).
legal_path_rec(State, A, B, [B | Tail]) :-
    nonvar(A),
    nonvar(B),
    legal_path_rec(State, A, OneButLast, Tail),
    bconn(OneButLast, B),
    not(member(B, State)),      % Cannot visit occupied space.
    not(member(B, Tail)).       % No cycles in the path.

% Validate a candidate path (in reverse order) can be travelled.
path_open(State, [Start]) :-
    !,
    member(Start, State).               % Start from an occupied location.
path_open(State, [Head | Tail]) :-
    not(member(Head, State)),           % No other occupied locations in path.
    not(member(Head, Tail)),            % No cycles.
    path_open(State, Tail).

% Locations in a path must be connected and empty in the current state.
legal_path(State, A, B, Path) :-
    member(A, State),
    legal_path_end(A, B),
    once(path(A, B, Path)),
    path_open(State, Path).

% Types and step costs of respective amphipods in the state vector.
cost_lookup([a/1, a/1, b/10, b/10, c/100, c/100, d/1000, d/1000]).

% Zip the corresponding elements from two lists together.
zip_lists([], [], []).
zip_lists([Item1 | Tail1], [Item2 | Tail2], [[Item1, Item2] | ZippedTail]) :-
	length(Tail1, N),
	length(Tail2, N),
	zip_lists(Tail1, Tail2, ZippedTail).

no_strangers_home(State, Type) :-
    home_for(Type, [H1, H2]),
    cost_lookup(CL1),
    zip_lists(State, CL1, ZL1),
    % Positions in state for the other types.
    include(\=([_,Type/_]), ZL1, ZL2),
    % Do either home positions appear in remaining vector?
    not(member([H1,_], ZL2)),
    not(member([H2,_], ZL2)).

% Find out if the move is legal for the type of amphipod concerned.
% Never move from hallway into a room unless it is its home and contains no different amphipods.
legal_move_type(State, Start, Finish, Type) :-     % Homing move.
    in_hallway(Start),
    !,
    home_for(Type, HL),
    member(Finish, HL),
    no_strangers_home(State, Type).
legal_move_type(_, _, Finish, _) :-                 % Move out to the hallway.
    in_hallway(Finish).

% If the end of the move is to a home location, move in deeper if possible.
adjust_finish(State, Fin1, Fin2, Path, [Fin2 | Path]) :-
    home_for(_, [Fin1, Fin2]),
    not(member(Fin2, State)),
    !.
adjust_finish(_, Fin1, Fin1, Path, Path).

move(Before, Path, After, Cost) :-
    cost_lookup(L),
    % Find legal path.
    index_of(Start, Before, Index),
    legal_path(Before, Start, Fin1, P1),
    % Check type of move.
    once(get_item(Index, L, Type/StepCost)),
    legal_move_type(Before, Start, Fin1, Type),
    adjust_finish(Before, Fin1, Fin2, P1, Path),
    % Determine state afterwards.
    set_item(Index, Before, Fin2, After),
    % Find cost of move
    length(Path, PathLen),
    NumSteps is PathLen - 1,
    Cost is StepCost * NumSteps.

% Definitions needed to use bestfirst algorithm.

% Successor of N is M with a cost of M:
s(N, M, C) :-
    move(N, _, M, C).

% goal(N) is as shown above.

% (Admissible) heuristic function h(n) should be cost of restoring each amphipod to its home,
% ignoring obstacles.

add_lookup_costs([]).
add_lookup_costs([S/D/C | Tail]) :-
    assertz(path_cost_lookup(S, D, C)),
    add_lookup_costs(Tail).

create_path_cost_lookup :-
    retractall(path_cost_lookup(_, _, _)),
    cost_lookup(CL1),
    findall(X/T, (home_for(T, XL), member(X, XL)), XL1),
    findall(Y, (in_hallway(Y); member(Y/_, XL1)), YL1),
    findall(S/D/Cost, (
        member(S, YL1),
        member(D/T, XL1),
        once(path(S, D, P1)),
        length(P1, M),
        Dist is M - 1,
        once(member(T/StepCost, CL1)),
        Cost is Dist * StepCost
    ), List),
    add_lookup_costs(List).

:- create_path_cost_lookup.

% Simple cost of moving from A to B as a Type, ignoring any obstacles.
path_cost(A, B, Cost) :-
    path_cost_lookup(A, B, Cost).

h([A1loc, A2loc, B1loc, B2loc, C1loc, C2loc, D1loc, D2loc], Cost) :-
    path_cost(A1loc, a1, A1Cost),
    path_cost(A2loc, a1, A2Cost),
    path_cost(B1loc, b1, B1Cost),
    path_cost(B2loc, b1, B2Cost),
    path_cost(C1loc, c1, C1Cost),
    path_cost(C2loc, c1, C2Cost),
    path_cost(D1loc, d1, D1Cost),
    path_cost(D2loc, d1, D2Cost),
    Cost is A1Cost + A2Cost + B1Cost + B2Cost + C1Cost + C2Cost + D1Cost + D2Cost.

solution_cost([_], 0) :- !.
solution_cost([A, B | Tail], Cost) :-
    solution_cost([B | Tail], Before),
    s(A, B, C1),
    Cost is Before + C1.

day23_part1 :-
    writeln('Advent of Code 2021 - Day 23, Part 1'),
    start_state(Start),
    write('Starting from '),
    writeln(Start),
    writeln('...'),
    !,
    bestfirst(Start, Solution),
    writeln('Done. Solution is:'),
    writeln(Solution),
    solution_cost(Solution, MinCost),
    write('Solution cost = '),
    writeln(MinCost).
