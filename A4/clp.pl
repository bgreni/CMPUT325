/* ---------------------------------------------------------
Question 1

Part 1:
1. In the top left corner box 1,3,9 are removed from the domain
2. In top left corner square 1,3,9,8,7,3,2,6 are removed from the domain
3. In the bottiom left square 8,7,9,2,5,1,3 are removed from the domain
4. In the bottom right square 5,1,3,9,8 are removed from the domain
5. In the top right square 3,2,6,1,4,8,9 are removed from the domain

Part 2: 5

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
    length(Set, L),
    length(Vars, L),
    !,
    Vars ins {0,1},
    scalar_product(Set, Vars, #=, G),
    label(Vars),
    getsol(Set, Vars, Res),
    writeln(Res).

getsol([],[],[]).
getsol([N|R1], [S|R2], [N|Res]) :-
    S =\= 0,
    !,
    getsol(R1,R2,Res).
getsol([N|R1], [S|R2], Res) :-
    !,
    getsol(R1,R2,Res).


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

% paper(1,lily,xxx,ai).
% paper(2,peter,john,database).
% paper(3,ann,xxx,theory).
% paper(4,ken,lily,network).
% paper(5,kris,xxx,games).
% paper(6,jim,xxx,games).
% paper(7,bill,xxx,theory).
% paper(8,bill,lily,ai).
% paper(9,peter,ann,games).

% reviewer(lily,theory,network).
% reviewer(john,ai,theory).
% reviewer(peter,database,network).
% reviewer(ann,theory,network).
% reviewer(kris,theory,games).
% reviewer(ken,database,games).
% reviewer(bill,database,ai).
% reviewer(jim,theory,games).
% reviewer(kevin,theory,games).
% reviewer(paul,ai,network).

workLoadAtMost(2).

assign(W1,W2) :-
    findall([X,Y,Z], reviewer(X,Y,Z), Reviewers),
    findall([I,D,A,E], paper(I,D,A,E), Papers),
    workLoadAtMost(K),
    length(Reviewers, L),
    RL is  L * 2,
    length(Vars, RL),
    % length(R1s,L),
    % length(R2s,L),
    rev_names(1, Reviewers, K, Names),
    !,
    global_cardinality(Vars, Names),
    Vars ins 1..L,
    % append(R1s, R2s, AllChoices),
    % constrain(R1s, R2s, Papers, Reviewers),
    constrain(1, L, Vars, Papers, Reviewers),
    label(Vars),
    !,
    % writeln(Vars),
    get_sol2(1, L, Vars, Reviewers, Papers, W1, W2).
    % writeln(W1),
    % writeln(W2).


get_sol2(_,_,_,_,[],[],[]).

get_sol2(I, L, Vars, Reviewers, [P|ResP], [Name1|W1], [Name2|W2]) :-
    I2 is I + L,
    nth1(I, Vars, R1),
    nth1(I2, Vars, R2),
    nth1(R1, Reviewers, [Name1, T11, T12]),
    nth1(R2, Reviewers, [Name2, T21, T22]),
    NextI is I+1,
    get_sol2(NextI, L, Vars, Reviewers, ResP, W1, W2).

rev_names(_, [], _, []).
rev_names(I, [[R1,T11,T12]|Rest], K, [I-K|Out]) :-
    NextI is I+1,
    rev_names(NextI, Rest, K, Out).

constrain(_, _, _, [], _).

constrain(I, L, Revs, [[ID,A1,A2,T]|RestP], Reviewers) :-
    I2 is I + L,
    % I2 < (L*2)-1,
    nth1(I, Revs, R1),
    nth1(I2, Revs, R2),
    nth1(R1, Reviewers, [Name1, T11, T12]),
    nth1(R2, Reviewers, [Name2, T21, T22]),
    R1 #\= R2,
    Name1 \= A1, Name1 \= A2,
    Name2 \= A1, Name2 \= A2,
    % writeln([T,T11,T12, T21,T22]),
    (T11 == T ; T12 == T),
    (T21 == T ; T22 == T),
    NextI is I+1,
    constrain(NextI, L, Revs, RestP, Reviewers).

% constrain(_, _, [], _).

% constrain([R1|Rest1], [R2|Rest2], [[ID,A1,A2,T]|RestP], Reviewers) :-
%     nth1(R1, Reviewers, [Name1, T11, T12]),
%     nth1(R2, Reviewers, [Name2, T21, T22]),
%     R1 #\= R2,
%     Name1 \= A1, Name1 \= A2,
%     Name2 \= A1, Name2 \= A2,
%     T11 == T ; T12 == T,
%     T21 == T ; T22 == T,
%     constrain(Rest1, Rest2, RestP, Reviewers).
    

e1 :- encrypt([S,E,N,D], [M,O,R,E], [M,O,N,E,Y]). 
e2 :- encrypt([I,T], [I,S], [M,E]). 

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