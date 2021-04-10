#const n = 9.
#show color/2.

{ color(X,1..n) } = 1 :- vertex(X).
:- arc(X,Y), color(X,C), color(Y,C).
