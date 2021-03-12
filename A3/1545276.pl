


writeln(T) :- write(T), nl.


/* ---------------------------------------------------------
Question 1

setIntersect(+S1,+S2,-S3)

Compute the intersection of two sets S1, S2 and store
the result in S3

EX:
setIntersect([a,b,c,d,e,g],[b,a,c,e,f,q],S). -> [a,b,c,e]
--------------------------------------------------------- */
setIntersect([], _, []).

setIntersect([F|R], L, [F|R2]) :-
    member(F, L),
    !,
    setIntersect(R, L, R2).

setIntersect([_|R], L, R2) :-
    setIntersect(R, L, R2).


/* ---------------------------------------------------------
Question 2

swap(+L, -R)

Swap every two elements in a list L, and scores the result in R

EX:
swap([a,1,b,2], W). -> [1,a,2,b]
--------------------------------------------------------- */
swap([], []).
swap([X], [X]):- !.
swap([X,Y|R1], [Y,X|R2]) :-
    swap(R1, R2).


/* ---------------------------------------------------------
Question 3

filter(+L,+OP,+N,-L1)

Filter a list L by applying the provided operator to every atom 
in the list
--------------------------------------------------------- */
greaterThan(X, Y) :- X > Y.
equal(X, Y) :- X == Y.
lessThan(X, Y) :- X < Y.


filter(L1, F, N, L2) :-
    flatten(L1, L3),
    filt(L3, F, N, L2).

filt([], _, _, []).

filt([X|R1], F, N, [X|R]) :-
    number(X),
    call(F, X, N),
    filter(R1, F, N, R),
    !.

filt([X|R1], F, N, R) :-
    number(X),
    filter(R1, F, N, R),
    !.
        

/* ---------------------------------------------------------
Question 4

countAll(+L,-N)

Take a list L and create a list of pairs, of the form [A, N],
where A is a symbol and N is the number of times that symbol
appears int the list
--------------------------------------------------------- */

countAll(L1, N) :-
    sort(L1, L2),
    makeCounts(L1, L2, N1),
    !,
    sort(2,@=<,N1,N).

makeCounts(_, [], []).

makeCounts(L1, [X|R1], [[X, C]|N]) :-
    doCount(X, L1, C),
    makeCounts(L1, R1, N). 

doCount(_, [], 0).

doCount(X, [X|R1], C) :-
    doCount(X, R1, Z),
    !,
    C is Z + 1.

doCount(X, [K|R1], Z) :-
    X \= K,
    doCount(X, R1, Z).

/* ---------------------------------------------------------
Question 5

sub(+L,+S,-L1)
--------------------------------------------------------- */
sub([],_,[]).

sub([X|R],[[X,Y]|S],[Y|L1]) :-
    atom(X),
    !,
    sub(R,[[X,Y]|S], L1).

sub([X|R],S,[X|L1]) :-
    atom(X),
    !,
    sub(R,S,L1).

sub([X|R],S,[R2|L1]) :-
    sub(X,S,R2),
    !,
    sub(R,S,L1).

/* ---------------------------------------------------------
Question 6

allConnected(L)
--------------------------------------------------------- */
node(_).
edge(_,_).
clique(L) :-
    findall(X, node(X), Nodes),
    subset(Nodes,L).
    allConnected(L).

subset([], _).

subset([X|Xs], Set) :-
    append(_, [X|Set1], Set),
    subset(Xs, Set1).

allConnected(L) :-
    connects(S, L).

connects([], []).

connects([X|S], L) :-
    forall(member(Y,L), (edge(X,Y); edge(Y,X))),



/* ---------------------------------------------------------
Question Y

convert(+Term,-Result)
--------------------------------------------------------- */




% :- initialization main.

% main :-
%     node(a),
%     node(b),
%     node(c),
%     edge(a,b),
%     edge(b,c),
%     edge(c,a),
%     clique(L),
%     writeln(L),
%     halt.


:- begin_tests(a1).

test(setinter1, true(S == [a,b,c,e])) :-
    setIntersect([a,b,c,d,e,g],[b,a,c,e,f,q], S).

test(swap1, true(W == [1,a,2,b])) :-
    swap([a,1,b,2], W).

test(swap2, true(W == [1,a,b])) :-
    swap([a,1,b], W).

test(filter1, true(W2 == [4,5,7])) :-
    filter([3,4,[5,2],[1,7,3]],greaterThan,3,W2).

test(filter1, true(W2 == [3,3])) :-
    filter([3,4,[5,2],[1,7,3]],equal,3,W2).

test(filter1, true(W2 == [2,1])) :-
    filter([3,4,[5,2],[1,7,3]],lessThan,3,W2).

test(docount1, true(C == 2)) :-
    doCount(c, [a,b,e,c,c,b], C).

test(countAll1, true(N == [[a,1],[e,1],[b,2],[c, 2]])) :-
    countAll([a,b,e,c,c,b],N).

test(sub2, true(L == [2,2,d,e,2])) :-
    sub([a,a,d,e,a],[[a,2]],L).

test(sub1, true(L == [2,[2,d],[e,2]])) :-
    sub([a,[a,d],[e,a]],[[a,2]],L).

:- end_tests(a1).