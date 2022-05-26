/*
	Advent of Code 2021 - Day 23 (Amphipods)
*/

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

connection(ha,a2).
connection(a2,a1).
connection(hb,b2).
connection(b2,b1).
connection(hc,c2).
connection(c2,c1).
connection(hd,d2).
connection(d2,d1).

bconn(A, B) :- connection(A, B).
bconn(B, A) :- connection(A, B).

in_hallway(H) :- 
    member(H, [h1,h2,h3,h4,h5,h6,h7]).

% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).

% path(A, B, Path) - Path is a path (in reverse order) from location A to B, 
% with no cycles, through valid connections,
% but ignoring whether the path is open or not.
path(Node, Node, [Node]).
path(FirstNode, LastNode, [LastNode | Path]) :-
	path(FirstNode, OneButLast, Path),
	bconn(OneButLast, LastNode),
	not(member(LastNode, Path)).

% Validate a candidate path (in reverse order) can be travelled.
path_open(State, [Start]) :-
    !,
    member(_/Start, State).               % Start from an occupied location.
path_open(State, [Head | Tail]) :-
    not(member(_/Head, State)),           % No other occupied locations in path.
    not(member(_/Head, Tail)),            % No cycles.
    path_open(State, Tail).

% Delete an item from a list.
del(Item, [Item | List], List).
del(Item, [First | List1], [First | List2]) :-
	del(Item, List1, List2).

% Reorder the elements of one list into another.	
permutation([], []).
permutation([Head | Tail], PermList) :-
	permutation(Tail, PermTail),
	del(Head, PermList, PermTail).

% Evaluate the cost of a possible move.
move_cost(Type, Path, Cost) :-
	member(Type/StepCost, [a/1, b/10, c/100, d/1000]),
	length(Path, Length),
	Steps is Length - 1,
	Cost is Steps * StepCost.

% Move to get an amphipod home.

% deep_homing_move(Before, Path, After) :-
move(Before, move(Type, From, Home), Path, After, Cost) :-
	member(Type/Home, [a/a1, b/b1, c/c1, d/d1]),
	not(member(_/Home, Before)),
	del(Type/From, Before, Inter1),
	once(path(From, Home, Path)),
	path_open(Before, Path),
	del(Type/Home, After, Inter1),
	move_cost(Type, Path, Cost).

% shallow_homing_move(Before, Path, After) :-
move(Before, move(Type, From, Home2), Path, After, Cost) :-
	member(Type/Home2/Home1, [a/a2/a1, b/b2/b1, c/c2/c1, d/d2/d1]),
	member(Type/Home1, Before),			% Must have solved deep home problem first,
	not(member(_/Home2, Before)),		% and shallow one is empty.
	del(Type/From, Before, Inter1),
	From \== Home1,						% Must not move homed amphipod.
	once(path(From, Home2, Path)),
	path_open(Before, Path),
	del(Type/Home2, After, Inter1),
	move_cost(Type, Path, Cost).

% hallway_move1(Before, Path, After) :-
move(Before, move(Type, From, To), Path, After, Cost) :-
	member(Type/Home2/Home1, [a/a2/a1, b/b2/b1, c/c2/c1, d/d2/d1]),
	del(Type/From, Before, Inter1),
	not(in_hallway(From)),
	not(member(From, [Home2, Home1])),
	in_hallway(To),
	once(path(From, To, Path)),
	path_open(Before, Path),
	del(Type/To, After, Inter1),
	move_cost(Type, Path, Cost).

% hallway_move2(Before, Path, After) :-
move(Before, move(Type, Home2, Dest), Path, After, Cost) :-
	member(Type/Home2/Home1, [a/a2/a1, b/b2/b1, c/c2/c1, d/d2/d1]),
	del(Type/Home2, Before, Inter1),
	not(member(Type/Home1, Before)),		% Deep home not solved yet.
	in_hallway(Dest),
	once(path(Home2, Dest, Path)),
	path_open(Before, Path),
	del(Type/Dest, After, Inter1),
	move_cost(Type, Path, Cost).


% Planning operator(s).

can(State, move(Type, From, To), [Type/From]) :-
	member(Type, [a, b, c, d]),
	From \== To,
	move(State, move(Type, From, To), _, _, _).

add(move(Type, _, To), [Type/To]).
del(move(Type, From, _), [Type/From]).


% Means-end planner (from Bratko, 2nd Ed):

% plan(InitialState, Goals, Plan): Means end planner with goal regression.
plan(State, Goals, []) :-
	satisfied(State, Goals).

plan(State, Goals, Plan) :-
	writeln(Goals),
	conc(PrePlan, [Action], Plan),					% Divide plan achieving breadth-first effect.
	select(State, Goals, Goal),
	achieves(Action, Goal),
	can(State, Action, _),							% Ensure Action contains no variables. (?)
	preserves(Action, Goals),						% Protect Goals.
	regress(State, Goals, Action, RegressedGoals),	% Regress Goals through Action.
	plan(State, RegressedGoals, PrePlan).



% satisfied(State, Goals): Goals are satisfied in State.
satisfied(State, Goals) :- 
	delete(Goals, State, []).						% All Goals are in State.


select(_, Goals, Goal) :-
	member(Goal, Goals).


% achieves(Action, Goal): Goal is in the add-list of Action.
achieves(Action, Goal) :-
	add(Action, Goals),
	member(Goal, Goals).

% preserves(Action, Goal): Action does not destroy any one of the Goals.
preserves(Action, Goals) :-
	del(Action, Relations),
	not((member(Goal, Relations),
		member(Goal, Goals))).


regress(State, Goals, Action, RegressedGoals) :-
	add(Action, NewRelations),
	delete(Goals, NewRelations, RestGoals),
	can(State, Action, Condition),
	addnew(Condition, RestGoals, RegressedGoals).	% Add precondition, check impossible.


% addnew(NewGoals, OldGoals, AllGoals): AllGoals is the union of NewGoals and OldGoals, which must be compatible.
addnew([], L, L).
addnew([Goal | _], Goals, _) :-
	impossible(Goal, Goals),						% Goal incompatible with Goals.
	!,
	fail.
addnew([X | L1], L2, L3) :-
	member(X, L2), !,								% Ignore duplicate.
	addnew(L1, L2, L3).
addnew([X | L1], L2, [X | L3]) :-
	addnew(L1, L2, L3).


% impossible(Goal, Goals): Goal is incompatible with one or more of Goals.
impossible(Type/To, Goals) :-
	member(T2/To, Goals), T2 \== Type.				% Destination cannot be occupied twice.


% apply(State, Action, NewState): Action executed in State produces NewState.
apply(State, Action, NewState) :-
	del(Action, DelList),
	delete(State, DelList, State1), !, 
	add(Action, AddList),
	conc(AddList, State1, NewState).


% delete(L1, L2, Diff): Diff is set-difference of lists L1 and L2.
delete([], _, []).
delete([X | L1], L2, Diff) :-
	member(X, L2), !,
	delete(L1, L2, Diff).
delete([X | L1], L2, [X | Diff]) :-
	delete(L1, L2, Diff).


% Successor of N is M with a cost of M:
s(N, M, C) :- move(N, _, _, M, C).

% Heuristic function.
h(_, 0).

start_state(	[a/c1, a/d2, b/d1, b/c2, c/b1, c/b2, d/a1, d/a2]).
goal_state(		[d/d1, d/d2, c/c1, c/c2, b/b1, b/b2, a/a1, a/a2]).

goal(State) :-
	goal_state(Goal),
	permutation(State, Goal).

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
%    bestfirst(Start, Solution),
    writeln('Done. Solution is:'),
    writeln(Solution),
    solution_cost(Solution, MinCost),
    write('Solution cost = '),
    writeln(MinCost).
