:- use_module(library(clpfd)).
:- use_module(library(clpr)).

%% Select %%

% Write a predicate select(X, L, Y, M) that is true if L and M are identical except (possibly)
% for a single element, which is X in L and is Y in M.
select(X, L, Y, M) :-
    % Let C be the shared "core" of the lists, i.e. the elements that are in both.
    % Then L is obtained by adding X somewhere to C and likewise for M and Y.
    select(X, L, C),
    select(Y, M, C).

% select(X, [X|Xs], Xs).
% select(Y, [X|Xs], [X|Ys]) :- select(Y, Xs, Ys).

%% Line Segment %%

% Let segment(P, Q) represent a line segment from points P to Q, where points are represented as p(X, Y).

% Write a predicate on_segment(S, R) that is true if point R lies on the line segment S. For example:

% ?- on_segment(segment(p(0, 0), p(5, 5)), p(3, 3)).
% true.

% ?- on_segment(segment(p(0, 0), p(5, 5)), p(3, 4)).
% false.

% ?- on_segment(segment(p(0, 0), p(5, 5)), p(6, 6)).
% false.

on_segment(segment(p(PX, PY), p(QX, QY)), p(RX, RY)) :-
    {
        DX = QX - PX,
        DY = QY - PY,
        T >= 0, T =< 1,
        RX = PX + T * DX,
        RY = PY + T * DY
    }.

%% Line Segment Intersection %%

% Continuing the previous exercise, write a predicate intersect(S, T) that is true if the line segments S and T intersect. For example:

% ?- intersect(segment(p(0, 0), p(5, 5)), segment(p(1, 1), p(6, 6))).
% true.

% ?- intersect(segment(p(0, 0), p(5, 5)), segment(p(1, 0), p(6, 5))).
% false.
intersect(Seg1, Seg2) :-
    on_segment(Seg1, Pt),
    on_segment(Seg2, Pt).

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

%% Permutations %%

% Write a predicate permutation(L, M) that is true if the list L is a permutation of M.

permutation([], []).  % There is only a single permutation of the empty list.
permutation([X|Xs], Res) :- % For a nonempty list...
    permutation(Xs, P),     % First find a permutation of the sublist Xs.
    select(X, Res, P).      % And then try putting the element X in each position.

% ?- permutation([1,2,3], R).
% R = [1, 2, 3]; ...
%
% ?- permutation(R, [1,2,3]).
% R = [1, 2, 3]; ...
%
% Finds all then loops! It's trying to generate larger and largest lists which
% can obviously never lead to another answer. We can constrain it by adding
%
% ?- length(R, 3), permutation(R, [1,2,3]).
%
% Or by adding same_length somewhere in the definition of permutation.

%% Combinations %%

% Write a predicate combination(L, N, M) that is true if M is a combination of N elements
% of L, i.e. a subset of size N that has its elements in the same order as in L. Elements
% in M should appear in the same order as in L.

combination(_, 0, []). % There is only a single combination of length 0.
combination(L, N, [A|C]) :-
    N #> 0,
    append(_, [A|B], L),   % Pick A as the first element of the combination;
                           % the remaining N - 1 elements will come from B.
    N2 #= N - 1,
    combination(B, N2, C).  % Recursively generate a smaller combination from B
                            % and then simply prepend A.
