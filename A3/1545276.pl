
% UTILITIES

writeln(T) :- write(T), nl.

% REFERENCE:
% https://stackoverflow.com/questions/50189547/how-to-order-a-list-of-tuples-in-swi-prolog
pivoting(_,[],[],[]).
pivoting([A,B],[[C,D]|T],[[C,D]|L],G):-
    D>B,
    pivoting([A,B],T,L,G).
pivoting([A,B],[[C,D]|T],[[C,D]|L],G):- 
    D=B,
    pivoting([A,B],T,L,G).
pivoting([A,B],[[C,D]|T],L,[[C,D]|G]):-
    D<B,
    pivoting([A,B],T,L,G).
quick_sort(List,Sorted):-
    q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
    pivoting(H,T,L1,L2),
    q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted),
    !.

greaterThan(X, Y) :- X > Y.
equal(X, Y) :- X == Y.
lessThan(X, Y) :- X < Y.

memb(_, []) :- false,!.
memb(X, [F|_]) :-
    X == F,
    !.

memb(X, [_|L]) :-
    memb(X, L).


setify([], []).
setify([F|L], S) :-
    memb(F,L),
    !,
    setify(L,S).
setify([F|L], [F|S]) :-
    setify(L,S).

flatty([], []).
flatty([X|L],[X|Res]) :-
    atomic(X),
    flatty(L, Res),
    !.
flatty([X|L],Res) :-
    flatty(X, R1),
    flatty(L, R2),
    append(R1, R2, Res),
    !.

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
    memb(F, L),
    setIntersect(R, L, R2),
    !.

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


filter(L1, F, N, L2) :-
    flatty(L1, L3),
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
    setify(L1, L2),
    makeCounts(L1, L2, N1),
    !,
    quick_sort(N1, N).

makeCounts(_, [], []).

makeCounts(L1, [X|R1], [[X, C]|N]) :-
    doCount(X, L1, C),
    makeCounts(L1, R1, N). 

doCount(_, [], 0).

doCount(X, [X|R1], C) :-
    doCount(X, R1, Z),
    !,
    C is Z + 1.

doCount(X, [_|R1], Z) :-
    doCount(X, R1, Z).

/* ---------------------------------------------------------
Question 5

sub(+L,+S,-L1)
--------------------------------------------------------- */
sub([],_,[]).

sub([X|R],[[X,Y]|S],[Y|L1]) :-
    atomic(X),
    !,
    sub(R,[[X,Y]|S], L1).

sub([X|R],S,[X|L1]) :-
    atomic(X),
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
node(X) :- edge(X,X).
node(a).
node(b).
node(c).

edge(a,b).
edge(b,c).
edge(c,a).


clique(L) :-
    findall(X, node(X), Nodes),
    subs(L,Nodes),
    allConnected(L).

subs([], _).

subs([X|Xs], Set) :-
    append(_, [X|Set1], Set),
    subs(Xs, Set1).

allConnected(L) :-
    forall(memb(Y,L), connects(Y,L)).

connects(X, L) :-
    forall(memb(Y,L), (edge(X,Y); edge(Y,X))).

/* ---------------------------------------------------------
Question Y

convert(+Term,-Result)
--------------------------------------------------------- */

convert([], []) :- !.

convert([e|T], R) :-
    convert(T, R),
    !.

convert([q|T], R) :-
    find_matching(T, Rem, Chunk),
    NewC = [q|Chunk],
    append(NewC, R2, R), 
    convert(Rem, R2),
    !.


convert([q|T], [q|R]) :-
    convert(T, R),
    !.

convert([_|T], [c|R]) :-
    convert(T, R),
    !.

find_matching([], _, []) :- false, !.

find_matching([q|T], T, [q]) :-
    !.

find_matching([X|T], Left, [X|R]) :-
find_matching(T, Left, R).



:- initialization main.

main :-
    run_tests(a1),
    halt.


:- begin_tests(a1).

% HELPER TESTS
test(docount1, true(C == 2)) :-
    doCount(c, [a,b,e,c,c,b], C).

test(qsort1, true(R == [[a,2],[v,3],[n,5],[b,8]])) :-
    quick_sort([[b,8],[a,2],[n,5],[v,3]], R).

test(memb1, true(X)) :-
    (memb(a, [a,b,c]) -> X = true; X = false).

test(memb2, true(\+ X)) :-
    (memb(j, [a,b,c]) -> X = true; X = false).

test(setify1, true(S == [b,a,c])) :-
    setify([a,a,b,a,c,c],S).

test(flatty, true(R == [3,4,5,2,1,7,3])) :-
    flatty([3,4,[5,2],[1,7,3]],R).

% MAIN FUNC TESTS
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

test(countAll1, true(N == [[k ,1],[e,1],[c,2],[b, 2]])) :-
    countAll([k,b,e,c,c,b],N).

test(sub2, true(L == [2,2,d,e,2])) :-
    sub([a,a,d,e,a],[[a,2]],L).

test(sub1, true(L == [2,[2,d],[e,2]])) :-
    sub([a,[a,d],[e,a]],[[a,2]],L).

test(clique1, all(L == [[],[a], [a,b], [a,b,c], [a,c], [b], [b,c], [c]])) :-
    clique(L).

test(convert1, true(R == [c,c])) :-
    convert([e,e,a,e,b,e],R).

test(convert2, true(R == [q,c,c])) :-
    convert([e,q,a,b,e,e],R).

test(convert3, true(R == [c])) :-
    convert([e,a,e,e],R).

test(convert4, true(R == [q,a,e,b,q,c])) :-
    convert([e,q,a,e,b,q,e,a,e],R).

test(convert5, true(R == [c,q,e,l,q,c,q,c])) :-
    convert([a,q,e,l,q,r,e,q,b,e],R).

test(convert6, true(R == [q,e,q,c,q,e,l,q,c] )) :-
    convert([q,e,q,b,q,e,l,q,a,e],R).

:- end_tests(a1).