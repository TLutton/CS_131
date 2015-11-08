%
% Transpose functions allow for easier checking
%

transpose([], []).
transpose([F|Fs], Ts) :- transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :- lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%
% KenKen with finite domain predicates
%
% Statistics from 6x6 Given Test
% Memory               limit         in use            free
%
%   trail  stack      16383 Kb           15 Kb        16368 Kb
%   cstr   stack      16383 Kb           27 Kb        16356 Kb
%   global stack      32767 Kb            9 Kb        32758 Kb
%   local  stack      16383 Kb            7 Kb        16376 Kb
%   atom   table      32768 atoms      1786 atoms     30982 atoms
%
% Times              since start      since last
%
%   user   time       0.009 sec       0.009 sec
%   system time       0.006 sec       0.006 sec
%   cpu    time       0.015 sec       0.015 sec
%   real   time      60.083 sec      60.083 sec
%

% Recursively call given Fun on given list.
doCall(_, []).
doCall(Funct, [H|T]) :- call(Funct, H), doCall(Funct, T).

% get Val from Coord
checkBox(T, Coord, Val) :- 
	Coord = R-C, 
	nth(R, T, Row), 
	nth(C, Row, Val).

% Addition (Recursive. Allows for more than 2 linked boxes)
rec_add(_, [], Res, Res).
rec_add(T, [Head|Tail], Num, Res) :- 
	checkBox(T, Head, Val), 
	S #= Num + Val,
	rec_add(T, Tail, S, Res).

% Product (Recursive. Allows for more than 2 linked boxes)
rec_prod(_, [], Res, Res).
rec_prod(T, [Head|Tail], Num, Res) :- 
	checkBox(T, Head, Val),
	P #= Num * Val,
	rec_prod(T, Tail, P, Res).

% evaluate add
evaluate(T, +(Res, List)) :- 
	rec_add(T, List, 0, Res).

% evaluate product
evaluate(T, *(Res, List)) :- 
	rec_prod(T, List, 1, Res).

% evaluate diff
evaluate(T, -(Res, A, B)) :- 
	checkBox(T, A, C), checkBox(T, B, D),
	(Res #= C - D ; Res #= D - C).

% evaluate quotient
evaluate(T, /(Res, A, B)) :- 
	checkBox(T, A, C), checkBox(T, B, D),
	(Res * C #= D ; Res * D #= C).

rLength(N, List) :- length(List, N).

domainCheck(N, List) :- fd_domain(List, 1, N).

kenken(N, C, T) :- length(T, N), 
	doCall(rLength(N), T),
	doCall(domainCheck(N), T),
	doCall(fd_all_different, T),
	transpose(T, TM),
	doCall(fd_all_different, TM), % doing transpose allows you to check cols
	doCall(evaluate(T), C),
	doCall(fd_labeling, T).

%
% Plain KenKen solver. Reuses evaluate and doCall predicates
% 
% Statistics from 4x4 Test. 6x6 Test runs too long to measure. 
% Memory               limit         in use            free
%
%   trail  stack      16383 Kb            0 Kb        16383 Kb
%   cstr   stack      16384 Kb            0 Kb        16384 Kb
%   global stack      32767 Kb            8 Kb        32759 Kb
%   local  stack      16383 Kb           10 Kb        16373 Kb
%   atom   table      32768 atoms      1786 atoms     30982 atoms
%
% Times              since start      since last
%
%   user   time       0.816 sec       0.807 sec
%   system time       0.022 sec       0.016 sec
%   cpu    time       0.838 sec       0.823 sec
%   real   time     193.691 sec     133.608 sec
%

% manually create a row with a finite domain. 
plainFDRow(0, []).
plainFDRow(N, [N|T]) :- N > 0, M is N-1, plainFDRow(M, T).

% checks if generated FDRow is a permutation of list of manual finite domain
% if List is a permutation of FDRow, then it contains no duplicates
plainRowIsPerm(N, List) :- plainFDRow(N, FDRow), permutation(FDRow, List).

% checks if all rows in matrix are legal Kenken rows
plainRowsLegal([] , 0, _).
plainRowsLegal([Head|Tail], I, N) :- 
	I > 0, J is I-1, 
	plainRowIsPerm(N, Head), 
	plainRowsLegal(Tail, J, N).

plain_kenken(N, C, T) :-
	plainRowsLegal(T, N, N),
	transpose(T, TM),
	plainRowsLegal(TM, N, N),
	doCall(evaluate(T), C).
	

%
% No-op kenken API found in kenken.txt
%

/*
kenken_testcase(
  6,
  [
   +(11, [1-1, 2-1]),
   /(2, 1-2, 1-3),
   *(20, [1-4, 2-4]),
   *(6, [1-5, 1-6, 2-6, 3-6]),
   -(3, 2-2, 2-3),
   /(3, 2-5, 3-5),
   *(240, [3-1, 3-2, 4-1, 4-2]),
   *(6, [3-3, 3-4]),
   *(6, [4-3, 5-3]),
   +(7, [4-4, 5-4, 5-5]),
   *(30, [4-5, 4-6]),
   *(6, [5-1, 5-2]),
   +(9, [5-6, 6-6]),
   +(8, [6-1, 6-2, 6-3]),
   /(2, 6-4, 6-5)
  ]
).
*/

/*
plain_kenken_testcase(
  4,
  [
   +(6, [1-1, 1-2, 2-1]),
   *(96, [1-3, 1-4, 2-2, 2-3, 2-4]),
   -(1, 3-1, 3-2),
   -(1, 4-1, 4-2),
   +(8, [3-3, 4-3, 4-4]),
   *(2, [3-4])
  ]).
*/
