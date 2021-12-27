%
% Advent of Code 2021 - Day 23 - Amphipods
%

:- working_directory(_, 'C:/Users/Steve/github/aoc2021/prolog/day23/').

:- dynamic connection/2.

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

goal([A1, A2, B1, B2, C1, C2, D1, D2]) :-
    home_for(a, AL), member(A1, AL), member(A2, AL),
    home_for(b, BL), member(B1, BL), member(B2, BL),
    home_for(c, CL), member(C1, CL), member(C2, CL),
    home_for(d, DL), member(D1, DL), member(D2, DL).

%
% Move is a sequence of locations that an amphipod can occupy, between its start in one state
% and finish in another state.
% 
% move(State1, [Locations], State2, Cost).
%
% Locations must be linked by connections; starting location must be where an amphipod is in State1.

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

% Never stop on the space immediately outside a room i.e. any of [ha,hb,hc,hd], keep going.
legal_path_end(E) :-
    not(member(E, [ha, hb, hc, hd])).

% Locations in a path must be connected and empty in the current state.
legal_path(_, [E]) :-
    legal_path_end(E).
legal_path(State, [A, B | Tail]) :-
    bconn(A, B),
    not(member(B, State)),
    legal_path(State, [B | Tail]).

% Types and step costs of respective amphipods in the state vector.
cost_lookup([a/1, a/1, b/10, b/10, c/100, c/100, d/1000, d/1000]).

in_hallway(H) :- 
    hallway(L),
    member(H, L).
% Remove any instances of a value from a list.
remove_all(_, [], []).
remove_all(V, [V | Tail], TailAfter) :-
	!,
	remove_all(V, Tail, TailAfter).
remove_all(V, [H | Tail], [H | TailAfter]) :-
	remove_all(V, Tail, TailAfter).

% Map list elements from one list through a named function(X,Y), producing a second list.
my_map_list([], _, []).
my_map_list([X | InTail], Function, [Y | OutTail]) :-
	Goal =.. [Function, X, Y],
	Goal,
	my_map_list(InTail, Function, OutTail).

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

move(Before, Path, After, Cost) :-
    % Is path legal?
    conc([Start | _], [Finish], Path),
    index_of(Start, Before, Index),
    legal_path(Before, Path),
    % Check type of move.
    cost_lookup(L),
    get_item(Index, L, Type/StepCost),
    legal_move_type(Before, Start, Finish, Type),
    % Determine state afterwards.
    set_item(Index, Before, Finish, After),
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


path(Node, Node, [Node]).
path(FirstNode, LastNode, [LastNode | Path]) :-
	path(FirstNode, OneButLast, Path),
	s(OneButLast, LastNode, _),
	not(member(LastNode, Path)).

% distance(A, B, Path, Distance)
distance(A, B, Path, N) :-
    path(A, B, Path),
    length(Path, M),
    N is M - 1.

path_cost(A, B, Type, Cost) :-
    distance(A, B, _, Dist),
    cost_lookup(CL1),
    member(Type/StepCost, CL1),
    Cost is Dist * StepCost.

h([A1loc, A2loc, B1loc, B2loc, C1loc, C2loc, D1loc, D2loc], Cost) :-
    path_cost(A1loc, a1, a, A1Cost),
    path_cost(A2loc, a1, a, A2Cost),
    path_cost(B1loc, b1, b, B1Cost),
    path_cost(B2loc, b1, b, B2Cost),
    path_cost(C1loc, c1, c, C1Cost),
    path_cost(C2loc, c1, c, C2Cost),
    path_cost(D1loc, d1, d, D1Cost),
    path_cost(D2loc, d1, d, D2Cost),
    Cost is A1Cost + A2Cost + B1Cost + B2Cost + C1Cost + C2Cost + D1Cost + D2Cost.

