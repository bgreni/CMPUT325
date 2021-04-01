/* ---------------------------------------------------------
Question 1

https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
--------------------------------------------------------- */



:- use_module(library(clpfd)).
:- use_module(library(lists)).


/* ---------------------------------------------------------
Question 2
--------------------------------------------------------- */

encrypt(W1, W2, W3) :-
    append(W1,W2,W),
    append(W,W3,L),
    list_to_set(L,Letters),
    [F1|_] = W1,
    [F2|_] = W2,
    [F3|_] = W3,
    !,
    Letters ins 0..9,
    all_distinct(Letters),
    word_sum(W1, S1),
    word_sum(W2, S2),
    word_sum(W3, S3),
    S1 + S2 #= S3,
    F1 #\= 0, F2 #\= 0, F3 #\= 0,
    label(Letters).


word_sum(W, S) :-
    length(W, L),
    W ins 0..9,
    reverse(W, W1),
    foldl(pow, W1, 0-0, S-L).

pow(D, N0-I0, N-I) :-
    N #= N0 + D*10^I0,
    I #= I0 + 1.
    

/* ---------------------------------------------------------
Question 3
--------------------------------------------------------- */

subsetsum(Set, G) :-
    subs(Sub, Set),
    sum(Sub, #=, G),
    writeln(Sub).

subs([], _).
subs([X|Xs], Set) :-
  append(_, [X|Set1], Set),
  subs(Xs, Set1).



/* ---------------------------------------------------------
Question 4
--------------------------------------------------------- */
:- dynamic([reviewer/3, paper/4, workLoadAtMost/1]).

paper(1,lily,xxx,ai).
paper(2,peter,john,database).
paper(3,ann,xxx,theory).
paper(4,ken,lily,network).
paper(5,kris,xxx,games).

reviewer(lily,theory,network).
reviewer(john,ai,theory).
reviewer(peter,database,network).
reviewer(ann,theory,network).
reviewer(kris,theory,games).
reviewer(ken,database,games).
reviewer(bill,database,ai).
reviewer(jim,theory,games).

workLoadAtMost(2).

assign(W1, W2) :-
    findall([X,Y,Z], reviewer(X,Y,Z), Reviewers),
    findall([I,D,A,E], paper(I,D,A,E), Papers),
    % RevC = {},
    workLoadAtMost(K),
    % writeln(Reviewers),
    % writeln(Papers)
    % do_papers(Papers, Reviewers, W1, W2),
    % writeln(W1),
    % length(Reviewers, L),
    % W1 in 1..L,
    % W2 in 1..L,
    !,
    reverse(Reviewers, Rev),
    append(Reviewers, Reviewers, Rev1),
    append(Rev, Rev, Rev2),
    perm(Rev1, W1),
    perm(Rev2, W2),
    constrain(W1,W2, Ps),
    % writeln(W1),
    % writeln(W2),
    !.
    % label(W1),
    % label(W2).

constrain([], [], []).

constrain([[R1,T11, T12]|Rest1], [[R2,T21, T22]|Rest2], [[ID,A1,A2,T]|RestP]) :-
    R1 \= R2,
    R1 \= A1, R1 \= A2,
    R2 \= A1, R2 \= A2,
    T11 = T ; T12 = T,
    T21 = T ; T22 = T,
    constrain(Rest1, Rest2, RestP).

takeout(X,[X|R],R).  
takeout(X,[F |R],[F|S]) :- takeout(X,R,S).

perm([X|Y],Z) :- perm(Y,W), takeout(X,Z,W).  
perm([],[]).
    


% :- initialization main.

% main :-
    % encrypt([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]),
%     % run_tests(a1),
%     halt.


:- begin_tests(a1).

% HELPER TESTS
test(wordsum1, true(S == 23)) :-
    W is 2,
    E is 3,
    word_sum([W,E],S).

:- end_tests(a1).


q1 :- subsetsum([-1, 1, 2, 3, 7,9,-2, -4],18).

q2 :- subsetsum([2062518654, 1772728182, 524053838, 3872006790, 4143885470,
                 473259798,  4214676334, 250497318, 217279934, 205333686],
                524053838).

q3 :- subsetsum([4029715910, 209644766, 137797718, 431825326, 873310310,
                 2232437246, 491614710, 644823502, 2747801862, 2767496478],
                1305135636).

q4 :- subsetsum([73126254, 682039078, 697267134, 2806596278, 2044349838,
           1818198982, 1432982238, 3234586198, 816273326, 312522342,
           4193226750, 4054437878, 138219470, 3100462854, 1779338014,
           411350934, 2579731950, 2628644262, 447849534, 3186898230],
                7450443154).

q5 :- subsetsum([3794350174, 3146379734, 1029552430, 355507686, 3586069886,
                 1203202934,3984660814, 1176600198, 2343794846, 2612878102,
                 1893648238, 859170086, 4200167870, 3342557366, 1435820942,
                 3808719302, 2191669470, 4008123478, 2469367214],
                25424705316).

q6 :- subsetsum([3829957942, 555555555,
                 802593294, 2471297606, 353176414, 1878866134,
                 1081248998, 2349383806,757580406, 380816462,
                 2374890398, 2600583702, 993315438,753136678,
                 1429919670, 160577166, 217518278, 2455720926,
                 3969905510, 3052126462, 4288070902, 937285838],
                 19333806124).

q7 :- subsetsum([1034291094, 4083820014, 2096020902, 469087294, 3829957942,
                 802593294, 2471297606, 353176414, 1878866134, 1369316398,
                 1081248998, 2349383806,757580406, 380816462, 2048523654,
                 2374890398, 2600583702, 993315438,753136678, 4173317310,
                 1429919670, 160577166, 217518278, 2455720926,1939383126,
                 950481070, 3969905510, 3052126462, 4288070902, 937285838],
                  19333806124).

q8 :- subsetsum([409222606, 1704015110, 2514618654, 4020645270, 3110790126,
                 3606213542, 1242055230, 3869592374, 3349575694, 1180457030,
                 1079621982, 2759630550,605755950, 1491305190, 4064120446,
                 3358574710, 702212686, 367170438, 3296256414, 2308466710,
                 807798894, 1905928742, 3159282366, 1124474294, 982160526,
                 3184584390, 2996863454, 1893632342, 4144279214, 2691918182,
                 621612798, 3342682870, 706639566, 808735238, 3306265118,
                 4254335638,2836401390, 3610001574, 3844686654, 4031402038],
                20334003460).