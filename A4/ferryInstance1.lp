#const steps=12.

car(car1;car2;car3;car4).

location(l1;l2;l3;l4).

at(car1,l1,0).
at(car2,l2,0).
at(car3,l3,0).
at(car4,l4,0).

at(ferry,l3,0).

goal(T):-
at(car1,l2,T),
at(car2,l3,T),
at(car3,l4,T),
at(car4,l1,T).