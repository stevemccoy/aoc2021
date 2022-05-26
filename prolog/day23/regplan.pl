% Planning space definition for blocks world example.

start_state([on(a,1), on(c,a), on(b,3), clear(c), clear(2), clear(b), clear(4)]).
goal_state([on(c,2),on(b,c),on(a,b),clear(a),clear(1), clear(3), clear(4)]).

can(move(Block, From, To), [ clear(From), clear(To), on(Block, From) ]) :-
	block(Block),
	object(To),
	To \== Block,
	object(From),
	From \== To,
	Block \== From.

add(move(Block, From, To), [ on(Block, To), clear(From) ]).
del(move(Block, From, To), [ on(Block, From), clear(To) ]).

object(X) :-
	place(X)
	;
	block(X).

block(a).
block(b).
block(c).

place(1).
place(2).
place(3).
place(4).


% List concatenation.
conc([], L, L).
conc([H | Tail], L1, [H | L2]) :-
	conc(Tail, L1, L2).


% Means-end planner (from Bratko, 2nd Ed):

% plan(InitialState, Goals, Plan): Means end planner with goal regression.
plan(State, Goals, []) :-
	satisfied(State, Goals).

plan(State, Goals, Plan) :-
	conc(PrePlan, [Action], Plan),				% Divide plan achieving breadth-first effect.
	select(State, Goals, Goal),
	achieves(Action, Goal),
	can(Action, _),								% Ensure Action contains no variables. (?)
	preserves(Action, Goals),					% Protect Goals.
	regress(Goals, Action, RegressedGoals),		% Regress Goals through Action.
	plan(State, RegressedGoals, PrePlan).



% satisfied(State, Goals): Goals are satisfied in State.
satisfied(State, Goals) :- 
	delete(Goals, State, []).					% All Goals are in State.


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


regress(Goals, Action, RegressedGoals) :-
	add(Action, NewRelations),
	delete(Goals, NewRelations, RestGoals),
	can(Action, Condition),
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
impossible(on(X,X), _).
impossible(on(X,Y), Goals) :-
	member(clear(Y), Goals)
	;
	member(on(X,Y1), Goals), Y1 \== Y
	;
	member(on(X1,Y), Goals), X1 \== X.
impossible(clear(X), Goals) :-
	member(on(_, X), Goals).


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
