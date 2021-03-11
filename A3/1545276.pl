


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
    filter(R1, F, N, R).

filt([X|R1], F, N, R) :-
    number(X),
    filter(R1, F, N, R).
        

/* ---------------------------------------------------------
Question 4

countAll(+L,-N)

Take a list L and create a list of pairs, of the form [A, N],
where A is a symbol and N is the number of times that symbol
appears int the list
--------------------------------------------------------- */

countAll([], []).

countAll()


% :- initialization main.

% main :-
%     filter([3,4,[5,2],[1,7,3]],greaterThan,3,W2),
%     writeln(W2),
%     halt.


:- begin_tests(a1).

test(setinter1, true(S == [a,b,c,e])) :-
    setIntersect([a,b,c,d,e,g],[b,a,c,e,f,q], S).

test(swap1, true(W == [1,a,2,b])) :-
    swap([a,1,b,2], W).

test(swap2, true(W == [1,a,b])) :-
    swap([a,1,b], W).

test(filter1, true(W2 = [4,5,7])) :-
    once(filter([3,4,[5,2],[1,7,3]],greaterThan,3,W2)).

test(filter1, true(W2 = [3,3])) :-
    once(filter([3,4,[5,2],[1,7,3]],equal,3,W2)).

test(filter1, true(W2 = [2,1])) :-
    once(filter([3,4,[5,2],[1,7,3]],lessThan,3,W2)).

:- end_tests(a1).