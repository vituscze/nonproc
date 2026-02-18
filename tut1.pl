% Nonprocedural Programming Tutorial %

% How to pass the tutorial
%
% * Attendance strongly recommended, but not mandatory
% * Homework
%   - A total of 12 standard assignments worth 120 points, some bonus assignments every now and then
%   - 5 Prolog assignments, 7 Haskell assignments
%   - 70% (i.e. 84 points) required to pass the tutorial
%   - You need to solve at least half the assignments for each language (i.e. 25 pts for Prolog, 35 for Haskell)
%   - Submitted via ReCodEx (https://recodex.mff.cuni.cz/)
%   - Do not copy code from other students or the internet
% * Semestral project
%   - Solve a nontrivial problem in Prolog or Haskell
%   - Topic ideas can be found at https://ksvi.mff.cuni.cz/~dingle/2024-5/npp/project_ideas.html
%     * Your own topics also welcome!
%   - Before you start working, submit a proposal and wait for confirmation
%     * Description of the problem and specification of the solution
%     * Doesn't have to be long, 1-2 paragraphs is usually enough
%     * Submit via ReCodEx
%   - To submit a project, you need to provide:
%     * Source code
%     * Test data (sample inputs and outputs, if applicable)
%     * User documentation (explain how to use the program to your users)
%     * Developer documentation (explain how your programs works to other developers)
%   - Use Git for the project
%   - Deadlines are on the course page (https://ksvi.mff.cuni.cz/~dingle/2025-6/npp/npp.html)

/*
Mortal Men

    Write Prolog clauses expressing the following facts:
            Socrates is a man.
            Plato is a man.
            All men are mortal.

    Write Prolog queries asking the following questions:
            Is Socrates a man?
            Is Plato mortal?
            Who is mortal?
            Who is a man and is also mortal?
            Do two different men exist?
*/

% First, a short recap: a Prolog program consists of predicates which talk about
% terms. A predicate can either be a fact (then it simply states that something is
% true) or it could be a rule (telling us that something is conditionally true).
%
% Facts are written as:

man(socrates).
man(plato).

% And rules as:

mortal(X) :- man(X). % Read as: X is mortal if X is a man.

% Prolog has many different kinds of terms but for now we only care about atoms
% and variables. Atoms begin with a lowercase letter and can be thought of as
% special constants (similar to e.g. the [] constant for an empty list in Python).
% We also have variables which begin with an uppercase letter. Think of them more
% as the mathematical variables rather than something in Python or C#.

% Once we define all the predicates, we can consult the file and ask questions (queries).
%
% ?- man(socrates).
% true.
%
% ?- mortal(plato).
% true.
%
% ?- mortal(X).
% X = socrates;  % If we aren't happy with this solution, we can press ; to ask Prolog to find another solution.
% X = plato.
%
% ?- man(X), mortal(X).
% (same as above)
%
% ?- dif(X, Y), man(X), man(Y).
% X = socrates, Y = plato;
% X = plato, Y = socrates;
% false.  % No more solutions.

/*
Dancing Pairs

Consider this Prolog program:
*/
male(hans).
male(charles).

female(elizabeth).
female(kate).

dance(P, Q) :- male(P), female(Q).
/*
What answers will these queries produce, and in what order?

        dance(charles, X).
        dance(jacob, radka).
        dance(kate, X).
        dance(X, elizabeth).
        dance(X, X).
        dance(hans, elizabeth).
        dance(X, john).
        dance(X, Y).
*/

% dance(charles, X) first tries male(charles), which succeeds and then tries female(X), producing
% two solution in the order in which they're written in the file.

% dance(jacob, radka) first tries male(jacob), which doesn't many any fact or a rule and thus fails;
% female(radka) isn't even attempted.

% Most of these are fairly straightforward, but let's take a detailed look at dance(X, X).
% The query immediatly turns into male(X), female(X). We can satisfy male(X) with X = hans, leaving us
% with only female(hans) to check. But unfortunately, that fails. We go back to male(X) and try to find
% another solution, this time yielding X = charles which then fails for the same reason. There are no
% more choices so dance(X, X) fails.

/*
Directed Graph

Consider this acyclic directed graph: [image here: https://ksvi.mff.cuni.cz/~dingle/2024-5/npp/exercises_1/graph.svg]

We can write a Prolog predicate representing adjacency between vertices:
*/

edge(a, b).
edge(a, e).
edge(b, c).
edge(b, f).
edge(e, d).
edge(e, f).
edge(f, c).
edge(g, d).
edge(g, h).
edge(h, f).

/*
    Write a predicate path(V, W) that is true if there is some path from V to W in the directed graph.

    Given your predicate, what answers will each of these queries produce?
            path(f, f).
            path(a, c).
            path(g, e).
            path(g, X).
            path(X, h).
            path(X, X).
            path(X, Y).
*/

% A path could either be trivial (e.g. from a to a):
path(X, X).
% Or, if we want to get from X to Y and there's no direct edge, we first move to Z along an edge and then try
% to find a path from Z to Y:
path(X, Y) :- edge(X, Z), path(Z, Y).

% ?- path(f, f).
% true ;  % expected behavior because it matches the fact
% false.  % ???
