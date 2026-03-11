:- use_module(library(clpfd)).
:- use_module(library(clpr)).

%% Evaluating a Polynomial %%

% We may represent a polynomial as a list of its coefficients. For example, the list [2.0, 3.0, 4.0] represents the polynomial 2x2 + 3x + 4.

% Write a predicate eval(P, X, Y) that succeeds if Y = P(X), where P is a polynomial represented as a list.

% We can use the Horner scheme but it's much easier to apply to a polynomial stored
% in reverse, i.e. with the highest degree last.
eval(P, X, Y) :- reverse(P, PR), eval_(PR, X, Y).

eval_([], _, 0).
eval_([C|Cs], X, Y) :- eval_(Cs, X, Y2), { Y = C + X * Y2 }.

%% Curve Fitting %%

% Continuing the previous exercise, we will represent a point (X, Y) using a structure p(X, Y).
% Write a predicate fit(P, N, L) that succeeds if P is a polynomial of degree N that passes through
% all points in the list L. Your predicate should be able to generate a polynomial of a given degree
% that passes through a given set of points.

fit(P, N, L) :-
    N2 #= N + 1,      % A degree N polynomial has N + 1 coefficients.
    length(P, N2),    % Generate a list of the appropriate length.
    fit_check(P, L).  % And finally check that all points match.

fit_check(_, []).
fit_check(P, [p(X, Y)|L]) :- eval(P, X, Y), fit_check(P, L).

%% All Same %%

% Write a predicate all_same(L) that is true if all elements of L are identical. Use maplist().

all_same([]).
all_same([X|Xs]) :- maplist(=(X), Xs).

%% Writing maplist() %%

% The predicate maplist(P, L, M) takes a predicate P of two arguments.
% Implement this version of maplist() using call().
maplist2(_, [], []).
maplist2(P, [X|Xs], [Y|Ys]) :- call(P, X, Y), maplist2(P, Xs, Ys).

%% Crosswords %%

% Suppose that we'd like to fill in a 3 x 3 grid with letters so that every row and column contains one of these words:

% AGE, AGO, CAN, CAR, NEW, RAN, ROW, WON

% Write a Prolog predicate that can find all possible solutions.

% If you can implement transpose, this solution can be simplified by a lot!

word(W) :- member(W, [[a,g,e],[a,g,o],[c,a,n],[c,a,r],[n,e,w],[r,a,n],[r,o,w],[w,o,n]]).

length_rev(L, M) :- length(M, L).

crosswords(M) :-
    length(M, 3),
    maplist(length_rev(3), M),
    maplist(word, M),
    transpose(M, MT),
    maplist(word, MT).

% We can also define our own transpose:

uncons([X|Xs], X, Xs).

split_col(M, C, R) :- maplist(uncons, M, C, R).

transpose2(M, []) :- maplist(=([]), M).
transpose2(M, [C|MM]) :- split_col(M, C, R), transpose2(R, MM).

%% State Space Searcher %%

%Write a higher-order predicate solve(+Move, +Start, +Goal) that can find the shortest path
% from a start state to an end state in any state space. Move(+S, ?T) should be a predicate
% that succeds if it's possible to move from state S to state T. Goal(+S) should be a predicate
% that's true if state S is a goal state.

solve(Move, Start, Goal, Path) :- length(Path, _), solve_(Move, Start, Goal, Path).

solve_(_, State, Goal, [State]) :- call(Goal, State).
solve_(Move, State, Goal, [State|Path]) :- call(Move, State, Next), solve_(Move, Next, Goal, Path).

%% Jugs %%

% Three jugs have capacity 8, 5 and 3 liters. Initially the first jug is full of water,
% and the others are empty.

% We can pour water between the jugs, but they have no markings of any sort, so we can
% pour between two jars only until one of them becomes full or empty.

% What sequence of moves can we make so that the jugs will hold 4, 4, and 0 liters of water,
% respectively?

% Write a Prolog program to find the shortest possible solution.

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
