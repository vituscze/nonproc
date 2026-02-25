:- use_module(library(clpfd)).

%% Triple List %%

% Write a predicate triple(L) that is true if L is a list with three elements, all of which are identical.

triple([X,X,X]).

%% Triple Diff %%

% Write a predicate triple_diff(L) that is true if L is a list with three elements, all of which are different.

triple_diff([X,Y,Z]) :- dif(X, Y), dif(X, Z), dif(Y, Z).

%% All Same %%

% Write a predicate all_same(L) that is true if all elements of L are the same.

% Base cases: trivially true for an empty list and a list with a single element
all_same([]).
all_same([_]).
% Recursive case: first two elements need to match and the rest of the list needs to be the same
all_same([X,X|Xs]) :- all_same([X|Xs]).

%% Same Length %%

% Write a predicate same_length(L, M) that is true if L and M have the same length. Do not use any integers in your solution.

same_length([], []).
same_length([_|Xs], [_|Ys]) :- same_length(Xs, Ys).

%% All Different %%

% Write a predicate all_diff(L) that is true if all elements of L are all different.

one_diff(_, []).
one_diff(X, [Y|Ys]) :- dif(X, Y), one_diff(X, Ys).

all_diff([]).
all_diff([X|Xs]) :- one_diff(X, Xs), all_diff(Xs).

%% Nth %%

% Write a predicate nth(N, L, X) that is true if the nth element of list L is X, where the first element has index 0.

nth(0, [X|_], X).
nth(N, [_|Xs], X) :- PN #= N - 1, nth(PN, Xs, X).

%% Prefix %%

% Write a predicate prefix(L, M) that is true if L is a prefix of list M.

prefix(L, M) :- append(L, _, M).

%% Product %%

% Write a predicate product(L, N) that is true if N is the product of the integers in L.

product([], 1).
product([X|Xs], N) :- N #= X * R, product(Xs, R).

%% Greatest Common Divisor %%

% Write a predicate gcd(I, J, K) that is true if the greatest common divisor of I and J is K.

gcd(I, 0, I).
gcd(I, J, K) :- R #= I mod J, gcd(J, R, K).

%% Reduce to Zero %%

% Write a predicate reduce_to_zero(L) that takes L, a list of integers. Given integers x1, ..., xn,
% the predicate should succeed if there is any sequence op1, ..., opn - 1 of arithmetic operations
% from the set (+, -, *, /) such that x1 op1 x2 op2 x3 ... xn = 0. Assume that all operators are
% right-associative. For example, reduce_to_zero([8, 8, 5, 4]) will succeed because (for example)
% 8 - (8 * (5 - 4)) = 0.

cond_all(X, R, V) :- X - R #= V; X + R #= V; X * R #= V; X div R #= V.
% If you want exact division, you can replace the last case with X #= V * R

reduce_to([X], X).
reduce_to([X|Xs], V) :-
    cond_all(X, R, V),
    reduce_to(Xs, R).

reduce_to_zero(L) :- reduce_to(L, 0).

%% Crosswords %%

% Suppose that we'd like to fill in a 3 x 3 grid with letters so that every row and column contains one of these words:

% AGE, AGO, CAN, CAR, NEW, RAN, ROW, WON

% Write a Prolog program that can find all possible solutions.

word(W) :- member(W, [[a,g,e],[a,g,o],[c,a,n],[c,a,r],[n,e,w],[r,a,n],[r,o,w],[w,o,n]]).

crosswords([[A,B,C],[D,E,F],[G,H,I]]) :-
    word([A,B,C]),
    word([D,E,F]),
    word([G,H,I]),
    word([A,D,G]),
    word([B,E,H]),
    word([C,F,I]).

% If you can implement transpose, this solution can be simplified by a lot!

%% Acyclic Directed Graph %%

% Consider this acyclic directed graph: [https://ksvi.mff.cuni.cz/~dingle/2024-5/npp/exercises_1/graph.svg]

% One way to represent a graph in Prolog is as a list of edges. For example, we can represent the graph above by this term:

% G = [ [a, b], [a, e], [b, c], [b, f], [e, d], [e, f], [f, c], [g, d], [g, h], [h, f] ].

% Write a predicate path(G, V, W, L) that is true if L is a list of vertices along a path from V to W in the graph G.

path(_, V, V, [V]).
path(G, V, W, [V|Path]) :- member([V, U], G), path(G, U, W, Path).
