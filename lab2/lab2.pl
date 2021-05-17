

%Rolling sum of list
preCompute([], []).
preCompute([X], [X]).
preCompute([X, Y|L], [X|L1]) :- Z is X+Y, preCompute([Z| L], L1).


%Insert sub list information in a list of size K
insertS(_, 0, _, []).
insertS(T, K, [], [T]) :- K > 0.
insertS([Indexes, Sum], K, [[CIndexes, CSum]|XS], [[Indexes, Sum] | RestOfList]) :- 
    Sum =< CSum, 
    NewK is K-1, 
    insertS([CIndexes, CSum], NewK, XS, RestOfList).
insertS([Indexes, Sum], K, [[CIndexes, CSum]|XS], [[CIndexes, CSum] | RestOfList]) :- 
    Sum > CSum, 
    NewK is K-1, 
    insertS([Indexes, Sum], NewK, XS, RestOfList).


%findIndices2 with all start indices
findIndices([], _, _, _, L, L).
findIndices([X|Xs], [Pre|Pres], K, Start, L, Rec) :- 
    NewStart is Start+1, 
    findIndices2([Pre|Pres], K, X, Pre, Start, Start, L, FI2),
    findIndices(Xs, Pres, K, NewStart, FI2, Rec).

%Tries to insert with inserS for all end indices
findIndices2([], _, _, _, _, _, L, L).
findIndices2([X | Xs], K, StartVal, StartSum, Start, End, L, Rec) :- 
    Sum is X-StartSum+StartVal, 
    insertS([[Start, End], Sum], K, L, NewL), 
    NewEnd is End+1,
    findIndices2(Xs, K, StartVal, StartSum, Start, NewEnd, NewL, Rec).

%findIndices after precompute
compute([], _, []).
compute(L, K, Result) :- preCompute(L, Pre), findIndices(L, Pre, K, 1, [], Result).


%Creates the sublist with the specified indices from the original list
subList(_, _, _, [], []).
subList(Start, End, Count, [X|Xs], [X|Rec]) :- 
    Start =< Count,
    Count =< End,
    NewCount is Count + 1,
    subList(Start, End, NewCount, Xs, Rec).
subList(Start, End, Count, [_|Xs], Rec) :-
    (Start > Count ; Count > End),
    NewCount is Count + 1,
    subList(Start, End, NewCount, Xs, Rec).


%Prints one sublist
writeList([[Start, End], Size], L) :-
    write(Size), write('    '), write(Start), write('    '), write(End), write('    '), subList(Start, End, 1, L, L1), writeln(L1).


%Prints all sublists in the solution
writeSolution([], _).
writeSolution([X|Xs], L) :-
    writeList(X, L),
    writeSolution(Xs, L).


solve(L, K) :-
    write('Size'), write('   '), write('i'), write('    '), write('j'), write('    '), writeln('sublist'), 
    compute(L, K, Result),
    writeSolution(Result, L).















bigList(100, [100]).
bigList(Num, [X|Xs]) :-
    Num < 100,
    X is Num*(-1)^Num,
    NewNum is Num+1,
    bigList(NewNum, Xs).

testBig() :-
    bigList(1, L),
    solve(L, 15).

