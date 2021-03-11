
parent(A, B) :-
    father(A, B).

parent(A, B) :-
    mother(A, B).

grandparent(A, C) :-
    parent(A, B),
    parent(B, C).

father(ken, mary).
mother(lily, mary).
mother(mary, john).

check(Goal, true) :-
Goal.
check(Goal, false) :-
\+ Goal.

:- initialization main.

writeln(T) :- write(T), nl.


main :-
% ( grandparent(ken, john) -> RES = true ; RES = false),
check(grandparent(ken, john), RES),
writeln(RES),
halt.

main :-
halt. 