/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <TO BE FILLED IN BEFORE THE GRADING> 
%    Student user id  : <TO BE FILLED IN BEFORE THE GRADING> 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
%:- ensure_loaded('play.pl').
:-ensure_loaded('stupid.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState, 1):-initBoard(InitialState).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State, Plyr) :- terminal(State), score(State, (Score1, Score2)), Score1 < Score2, Plyr is 1. 
winner(State, Plyr) :- terminal(State), score(State, (Score1, Score2)), Score1 > Score2, Plyr is 2. 


%score([], (0, 0)).
%score([X|Xs], (Score1, Score2)):- X == 1, score(Xs, (RScore1, Score2)), Score1 is RScore1 + 1. 
%score([X|Xs], (Score1, Score2)):- X == 2, score(Xs, (Score1, RScore2)), Score2 is RScore2 + 1.
%score([X|Xs], (Score1, Score2)):- score(Xs, (Score1, Score2)).

score([], (0, 0)).
score([[]|Rest], S) :- score(Rest, S).
score([[X|Xs]|Rest], (Score1, Score2)):- X == 1, score([Xs|Rest], (RScore1, Score2)), Score1 is RScore1 + 1, !. 
score([[X|Xs]|Rest], (Score1, Score2)):- X == 2, score([Xs|Rest], (Score1, RScore2)), Score2 is RScore2 + 1, !.
score([[X|Xs]|Rest], (Score1, Score2)):- score([Xs|Rest], (Score1, Score2)).


testInit(Res):- initBoard(X), score(X, Res).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :- terminal(State), score(State, (Score1, Score2)), Score1 == Score2.





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State):-moves(1, State, L1), moves(2, State, L2), L1 == [n], L2 == [n].



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

%moves(Plyr, State, MvList):- findall(Move, validmove(Plyr, State, Move), MvList).
moves(Plyr, State, MvList) :- moveFinder(Plyr, State, [0, 0], MvList), length(MvList, L), L > 0, !.
moves(Plyr, State, [n]).

moveFinder(Plyr, State, [X, _], []):- X > 5, !.
moveFinder(Plyr, State, [X, 5], L):- validmove(Plyr, State, [X, 5]), L = [[X, 5]|Rest], NewX is X + 1, moveFinder(Plyr, State, [NewX, 0], Rest), !.
moveFinder(Plyr, State, [X, 5], Rest):- NewX is X + 1, moveFinder(Plyr, State, [NewX, 0], Rest), !.
moveFinder(Plyr, State, [X, Y], L):- validmove(Plyr, State, [X, Y]), L = [[X, Y]|Rest], NewY is Y + 1, moveFinder(Plyr, State, [X, NewY], Rest), !.
moveFinder(Plyr, State, [X, Y], Rest):- NewY is Y + 1, moveFinder(Plyr, State, [X, NewY], Rest).

testMoves(Plyr, MvList):- initBoard(B), moves(Plyr, B, MvList).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%


nextState(Plyr, Move, State, State, NextPlyr):- Move == n, otherPlayer(Plyr, NextPlyr).
%nextState(Plyr, Move, State, NewState, NextPlyr):-set(State, NewState, Move, Plyr), otherPlayer(Plyr, NextPlyr).

nextState(Plyr, Move, State, NewState, NextPlyr):-tempState(Plyr, Move, State, Tmp), checkBrackets(Plyr, Tmp, [0,0], NewState), otherPlayer(Plyr, NextPlyr).


checkBrackets(Plyr, State, [X, _], State):- X > 5, !.
checkBrackets(Plyr, State, [X, 5], NewState):- checkBrackets2(Plyr, State, [X, 5], Tmp), checkBrackets(Plyr, Tmp, [X, 5], NewState),!.
checkBrackets(Plyr, State, [X, 5], NewState):- NewX is X + 1,  checkBrackets(Plyr, State, [NewX, 0], NewState),!.
checkBrackets(Plyr, State, [X, Y], NewState):- checkBrackets2(Plyr, State, [X, Y], Tmp), checkBrackets(Plyr, Tmp, [X, Y], NewState),!.
checkBrackets(Plyr, State, [X, Y], NewState):- NewY is Y + 1, checkBrackets(Plyr, State, [X, NewY], NewState).

checkBrackets2(Plyr, State, Move, NewState):- get(State, Move, Plyr), direction(D, Move, NextSquare), checkDirection(Plyr, State, Move, D), set(State, NewState, NextSquare, Plyr).

tempState(Plyr, Move, State, NewState):-set(State, NewState, Move, Plyr).

testCB2([X,Y], NewState) :- initBoard(B), set(B, Tmp, [3,1], 1), checkBrackets2(1, Tmp, [X,Y], NewState).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.


validmove(Plyr, State, Proposed):- get(State, Proposed, V), V \= 1, V \= 2, checkDirection(Plyr, State, Proposed, D).

checkDirection(Plyr, State, Coord, D):- direction(D, Coord, NewCoord), get(State, NewCoord, V), otherPlayer(Plyr, V), checkDirection2(Plyr, State, NewCoord, D).

checkDirection2(Plyr, State, Coord, D):- direction(D, Coord, NewCoord), get(State, NewCoord, V), otherPlayer(Plyr, V), checkDirection2(Plyr, State, NewCoord, D).
checkDirection2(Plyr, State, Coord, D):- direction(D, Coord, NewCoord), get(State, NewCoord, Plyr).

%inBound([X, Y]):- X < 6, X > -1, Y < 6, Y > -1.
otherPlayer(1, 2).
otherPlayer(2, 1).


direction(s, [X, Y], [NewX, Y]):- NewX is X + 1.
direction(n, [X, Y], [NewX, Y]):- NewX is X - 1.
direction(e, [X, Y], [X, NewY]):- NewY is Y + 1.
direction(w, [X, Y], [X, NewY]):- NewY is Y - 1.
direction(se, [X, Y], [NewX, NewY]):- NewX is X + 1, NewY is Y + 1.
direction(ne, [X, Y], [NewX, NewY]):- NewX is X - 1, NewY is Y + 1.
direction(sw, [X, Y], [NewX, NewY]):- NewX is X + 1, NewY is Y - 1.
direction(nw, [X, Y], [NewX, NewY]):- NewX is X - 1, NewY is Y - 1.

testValid([X, Y], Plyr):- initBoard(B), validmove(Plyr, B, [X,Y]).
getFromInit([X, Y], V):- initBoard(B), get(B, [X, Y], V).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State, 100):- winner(State, 2), !.
h(State, -100):- winner(State, 1), !.
%h(State, Val):- moves(2, State, MvList), length(MvList, Size), Val is Size. 
h(State, Val):- countEdges(State, 2, Count2), countEdges(State, 1, Count1), Val is Count2.
%h(State, Val):- countEdges(State, 2, Count2), score(State, (Score1, Score2)), Val is Count2-Score2.

countEdges(State, Plyr, Count):- findall(Coords, onEdge(State, Coords, Plyr), Bag), length(Bag, Count).
onEdge(State, [X, Y], Plyr):- get(State, [X,Y], Plyr), (X == 0; X == 5; Y == 0; Y == 5).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-101).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(101).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
