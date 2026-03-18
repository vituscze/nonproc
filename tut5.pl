:- use_module(library(clpfd)).

%% Binary trees in Prolog %%

% We can represent any tree-like structure with compound terms.
% In the case of a binary tree, we need a way to represent an empty
% tree (say with the nil atom) and a node with two subtrees (say the
% compound term t(Left, Value, Right)).

%   2
%  / \
% 1   3

example(t(t(nil, 1, nil), 2, t(nil, 3, nil))).

% Working with binary trees recursively is very similar to lists, except
% we have two substructures to consider.

% size(?Tree, ?TreeSize)
size(nil, 0).
size(t(L, _, R), S) :- SL #>= 0, SR #>= 0, S #= 1 + SL + SR, size(L, SL), size(R, SR).
% SL #>= 0 and SR #>= 0 are crucial if we leave the tree unspecified! Otherwise it
% would try to generate trees of negative size.

%% Tree Height %%

% Consider binary trees represented as nil (the empty tree) or as t(L, X, R), where L and R are left and
% right subtrees. Write a predicate height(?T, ?H) that is true if H is the height of the binary tree T.
% Recall that the height of a tree is defined as the maximal distance from the root to any leaf. Your
% predicate should be able to generate all possible trees of any height.
height(nil, -1).
height(t(L, _, R), H) :- LH #>= -1, RH #>= -1, H #= 1 + max(LH, RH), height(L, LH), height(R, RH).

%% Tree Flatten %%

flatten_bad(nil, []).
flatten_bad(t(L, X, R), F) :- flatten_bad(L, FL), flatten_bad(R, FR), append([FL, [X], FR], F).
% Can be quadratic for degenerate trees!

flatten_good(nil, Acc, Acc).
flatten_good(t(L, X, R), Acc, Res) :- flatten_good(R, Acc, Acc2), flatten_good(L, [X|Acc2], Res).
% O(1) operations for each node! Guaranteed linear.
% flatten(T, L) :- flatten_good(T, [], L).

% We can also model binary search trees in the same way:
contains(X, t(L, Y, R)) :-
    ( X #< Y -> contains(X, L)
    ; X #= Y -> true
    ; X #> Y -> contains(X, R)
    ).

insert(X, nil, t(nil, X, nil)).
insert(X, t(L, Y, R), Res) :-
    ( X #< Y -> insert(X, L, NewL), Res = t(NewL, Y, R)
    ; X #> Y -> insert(X, R, NewR), Res = t(L, Y, NewR)
    ; X #= Y -> Res = t(L, X, R)
    ).

%% Graph Coloring %%

% Write a predicate colorable(+G, +N) that takes an undirected graph in adjacency list
% representation and a positive integer N. The predicate should succeed if the graph is
% N-colorable, i.e. it is possible to assign one of N colors to each vertex in such a way
% that no adjacent vertices have the same color.

% For simplicity we assume that the graph is connected. For unconnected graphs, you'd just
% check the colorability for each connected component.

example_graph([a -> [b, c], b -> [a, c], c -> [a, b]]).

colorable(G, N, R) :-
    G = [Start -> _ | _],
    visit(G, N, 0, Start, [], R).

visit(G, N, LastColor, Vertex, Assign, NewAssign) :-
    ( member(Vertex-Color, Assign) -> Color #\= LastColor, NewAssign = Assign
    ; between(1, N, Color),
      Color #\= LastColor,
      member(Vertex -> Neighbors, G),
      foldl(visit(G, N, Color), Neighbors, [Vertex-Color|Assign], NewAssign)
    ).

% Recall our "water jugs" puzzle model from the last tutorial:
start([8/8, 0/5, 0/3]).
goal([4/_, 4/_, 0/_]).

pour(F1/FC, T1/TC, F2/FC, T2/TC) :-
    Amount #= min(F1, TC - T1),
    Amount #> 0,
    F2 #= F1 - Amount,
    T2 #= T1 + Amount.

move(JugsIn, JugsOut) :-
    append([A,[Jug1],B,[Jug2],C], JugsIn),
    (pour(Jug1, Jug2, NJug1, NJug2); pour(Jug2, Jug1, NJug2, NJug1)),
    append([A,[NJug1],B,[NJug2],C], JugsOut).

% We solved the puzzle by applying very simple IDS, which had a couple of issues:
% a) it explored same states multiple times
% b) if there was no solution, it would keep trying forever even after exhausting the search space

% We can fix these by keep track of the states we've already visited!
bfs(Move, Start, Goal, Path) :-
    % Our queue contains *paths* from the start to the currently explored state!
    bfs_(Move, Goal, [[Start]], [Start], Path).

prepend(Xs, X, [X|Xs]).

% bfs_(Move, Goal, Queue, Visited, Path)
bfs_(_, Goal, [[State|Rest]|_], _, [State|Rest]) :- call(Goal, State). % The path ends in a goal state, solution found!
bfs_(Move, Goal, [[State|Rest]|Paths], Visited, Path) :-
    % We can't just do something like call(Move, State, Next) here because
    % we need to add *all* neighbors to the queue. Luckily, Prolog has
    % the findall predicate!

    % Find all states Next such that call(Move, State, Next) and Next wasn't visited already;
    % put the new states into the Neighbors list.
    findall(Next, (call(Move, State, Next), \+ member(Next, Visited)), Neighbors),
    append(Neighbors, Visited, NewVisited),
    % Construct the new paths
    maplist(prepend([State|Rest]), Neighbors, NewPaths),
    append(Paths, NewPaths, NewQueue),
    bfs_(Move, Goal, NewQueue, NewVisited, Path).
    % Note that the visited set and queue are very inefficient since lists aren't
    % exactly the ideal structure. Feel free to replace them with something better!
