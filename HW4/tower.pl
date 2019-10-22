%using old implementation of SWI transpose
%resource taken from https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

%helper function to test uniqueness of row
generate_proto([], 0).
generate_proto([N|T], N) :-
    Next is N-1,
    length([N|T], N),
    generate_proto(T, Next).

%count_vis, count visible towers in a row
count_vis(_, 0, []).    
count_vis(Max, Count, [H|T]) :-
    H > Max,
    Updated is Count - 1,
    count_vis(H, Updated, T);
    H =< Max,
    count_vis(Max, Count, T).
    
%check_count, check correctness of visbility count
check_count([], _, _, _, _).
check_count([HT|TT], Proto, [HF|TF], [HB|TB], N) :-
    permutation(Proto, HT),
    between(1, N, HF),
    between(1, N, HB),
    count_vis(0, HF, HT),
    reverse(HT, RHT),
    count_vis(0, HB, RHT),
    check_count(TT, Proto, TF, TB, N).
    
    
%plain_tower, I do this first as suggested by piazza 
plain_tower(N, T, C) :-
    length(T, N),
    length(New_T, N),
    transpose(T, New_T),
    C = counts(U, B ,L, R),
%simple tests
    length(U, N),
    length(B, N),
    length(L, N),
    length(R, N),
%use helper function to check counts    
    generate_proto(Proto, N),
    check_count(T, Proto, L, R, N),
    check_count(New_T, Proto, U, B, N).

%count_vis_2, count visible towers in a row
count_vis_2(_, 0, []).
count_vis_2(Max, Count, [H|T]) :-
    H #> Max,
    Updated #= Count - 1,
    count_vis_2(H, Updated, T);
    H #=< Max,
    count_vis_2(Max, Count, T).

%check_counts_2, check correctness of visbility count
check_count_2([], _, _, _).
check_count_2([HT|TT], [HF|TF], [HB|TB], N) :-
    between(1, N, HF),
    between(1, N, HB),
    count_vis_2(0, HF, HT),
    reverse(HT, RHT),
    count_vis_2(0, HB, RHT),
    check_count_2(TT, TF, TB, N).
  
%constraint, setup all the constraints
constraint(_, []).
constraint(N, [H|T]) :-
  length(H, N),
  fd_domain(H, 1, N),
  constraint(N, T).

%tower, use fds to solve the problem
tower(N, T, C) :-
    length(T, N),
    length(New_T, N),
    transpose(T, New_T),
    C = counts(U, B ,L, R),
%simple tests
    length(U, N),
    length(B, N),
    length(L, N),
    length(R, N),
%set constraints
    constraint(N,T),
    maplist(fd_all_different,T),
    maplist(fd_all_different,New_T),
    check_count_2(T, L, R, N),
    check_count_2(New_T, U, B, N),
    maplist(fd_labeling, T).

%task 3
ambiguous(N,C,T1,T2):-
    tower(N,T1,C),
    tower(N,T2,C),
    T1 \= T2.

%speedup
tower_time(Time) :-
    statistics(cpu_time,[Time1|_]),
    tower(5,_,counts([2,2,2,1,5],[2,2,4,3,1],[3,3,2,1,2],[2,3,3,2,1])),
    statistics(cpu_time,[Time2|_]),
    Time is Time2-Time1.

plain_time(Time) :-
    statistics(cpu_time,[Time1|_]),
    plain_tower(5,_,counts([2,2,2,1,5],[2,2,4,3,1],[3,3,2,1,2],[2,3,3,2,1])),
    statistics(cpu_time,[Time2|_]),
    Time is Time2-Time1.

speedup(R) :-
    tower_time(TT),
    plain_time(TP),
    R is TP/TT.
