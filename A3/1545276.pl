
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

% used for the filter predicate
greaterThan(X, Y) :- X > Y.
equal(X, Y) :- X == Y.
lessThan(X, Y) :- X < Y.

% check if some X is a member of a list L
memb(_, []) :- false,!.
memb(X, [F|_]) :-
    X == F,
    !.
memb(X, [_|L]) :-
    memb(X, L).

% turn a list into a set (remove duplicates)
setify([], []).
setify([F|L], S) :-
    memb(F,L),
    !,
    setify(L,S).
setify([F|L], [F|S]) :-
    setify(L,S).

% flatten a list
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

% The case that F is a member of both lists and there
% belongs in the intersection
setIntersect([F|R], L, [F|R2]) :-
    memb(F, L),
    setIntersect(R, L, R2),
    !.

% The case that the front element in S1
% is not in S2 and therefore does not belong 
% in the intersection 
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

% There are an uneven count of elements in L, so do nothing with
% the last element
swap([X], [X]):- !.
swap([X,Y|R1], [Y,X|R2]) :-
    swap(R1, R2).


/* ---------------------------------------------------------
Question 3

filter(+L,+OP,+N,-L1)

Filter a list L by applying the provided operator to every atom 
in the list along with some number N. Remove the element from
the list if the comparizon fails.

filter([3,4,[5,2],[1,7,3]],greaterThan,3,W) -> [4,5,7]
^ remove all number not greater than 3
--------------------------------------------------------- */

% flatten the list first
filter(L1, F, N, L2) :-
    flatty(L1, L3),
    filt(L3, F, N, L2).

filt([], _, _, []).

% case that X belongs in the output
filt([X|R1], F, N, [X|R]) :-
    number(X),
    % apply the given boolean function F to X and N
    call(F, X, N),
    filter(R1, F, N, R),
    !.

% case that X does not belong in the output
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

countAll([a,b,e,c,c,b],N) ->  [[a,1],[e,1],[b,2],[c 2]]
--------------------------------------------------------- */

countAll(L1, N) :-
    % make a set of the elements in L1
    setify(L1, L2),
    % generate the count pairs
    makeCounts(L1, L2, N1),
    !,
    % sort the pairs
    quick_sort(N1, N).

makeCounts(_, [], []).

% go through every unique element from L1
% count the number of times it appears in L1
% and add a pair of the symbol and its count to
% the output
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

replace any symbol in L with its corresponding symbol in S if it exists

sub([a,[a,d],[e,a]],[[a,2]],L) -> [2,[2,d],[e,2]]
--------------------------------------------------------- */
sub([],_,[]).

% case where X exists in S, and is replaced
% with its value Y
sub([X|R],S,[Y|L1]) :-
    atomic(X),
    findx(X, S, Y),
    !,
    sub(R,[[X,Y]|S], L1).

% X is not in S, so remains the same
sub([X|R],S,[X|L1]) :-
    atomic(X),
    !,
    sub(R,S,L1).

% X is a list, so enter the list X
% before continuing
sub([X|R],S,[R2|L1]) :-
    sub(X,S,R2),
    !,
    sub(R,S,L1).

% Find if X exists in a set of pairs as the first element
% of some pair, if true, return the paired value Y.
findx(_, [], []) :- false, !.
findx(X, [[X,Y]|_], Y) :- !.
findx(X, [_|R], Res) :-
    findx(X, R, Res).


/* ---------------------------------------------------------
Question 6

clique(L)
allConnected(L)

Finds every clique given all nodes and edges currently 
present in the database

clique(L) ->
[] ;
[a] ;
[a,b] ;
[a,b,c] ;
[a,c] ;
[b] ;
[b,c] ;
[c]

Each time clique backtracks and generates a subset of Nodes
check if that subset is a clique, and if it is, it will be
returned
--------------------------------------------------------- */
% a node has an edge to itself so that
% a single node is considered a clique
node(X) :- edge(X,X).

% MY TEST DATA UNCOMMENT IF YOU WANT
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

% for all Y in L, check that it is connected to all other X in L
allConnected(L) :-
    forall(memb(Y,L), connects(Y,L)).

connects(X, L) :-
    forall(memb(Y,L), (edge(X,Y); edge(Y,X))).

/* ---------------------------------------------------------
Question Y

convert(+Term,-Result)

Given a string any symbol in between a matching pair of q's
will remain untouched, otherwise, an e will be removed, and 
any other character will become a c. Any q will also remain unchanged

convert([q,e,q,b,q,e,l,q,a,e],R) -> [q,e,q,c,q,e,l,q,c]
--------------------------------------------------------- */

convert([], []) :- !.

% an e outside of matching qs is removed
convert([e|T], R) :-
    convert(T, R),
    !.

% case where we have matching qs
% extract everything between the qs, and 
% add it to the result, and continue on whatever
% comes after the second q
convert([q|T], R) :-
    % Chunk is everything in between the qs
    % Rem is everything after the second q
    find_matching(T, Rem, Chunk),

    % add the first q back to the Chunk
    NewC = [q|Chunk],
    append(NewC, R2, R), 
    convert(Rem, R2),
    !.

% an unmatched q
convert([q|T], [q|R]) :-
    convert(T, R),
    !.

% any other char outside of matched qs becomes a c
convert([_|T], [c|R]) :-
    convert(T, R),
    !.

% exhausted the list without finding a matching q so return false
find_matching([], _, []) :- false, !.

% q is found, make everything afther the q
% what is left
find_matching([q|T], T, [q]) :-
    !.

% some other char, continue on
find_matching([X|T], Left, [X|R]) :-
    find_matching(T, Left, R).



% MY TEST SCRIPTS

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

test(sub3, true(L == [2,[2,3],[e,2]])) :-
    sub([a,[a,d],[e,a]],[[a,2],[d,3]],L).

test(sub3, true(L == [2,[2,3],[4,2]])) :-
    sub([a,[a,d],[e,a]],[[a,2],[d,3], [e,4]],L).

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