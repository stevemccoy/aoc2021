% solve(Node, Solution)
%	Solution is an acyclic path (in reverse order) between Node and a goal

solve(Node, Solution) :-
	depthfirst([], Node, Solution).

% depthfirst(Path, Node, Solution)
%	extending the path [Node | Path] to a goal gives Solution

depthfirst(Path, Node, [Node | Path]) :-
	goal(Node).

depthfirst(Path, Node, Sol) :-
	s(Node, Node1),
	not(member(Node1, Path)),	% Prevent cycles.
	depthfirst([Node | Path], Node1, Sol).



path(Node, Node, [Node]).
path(FirstNode, LastNode, [LastNode | Path]) :-
	path(FirstNode, OneButLast, Path),
	s(OneButLast, LastNode),
	not(member(LastNode, Path)).

depth_first_iterative_deepening(Node, Solution) :-
	path(Node, GoalNode, Solution),
	goal(GoalNode).
