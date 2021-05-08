move(state(Robot, onRobot, BrassKey, Package), drop(steelKey), state(Robot, Robot, BrassKey, Package)).
move(state(Robot, SteelKey, onRobot, Package), drop(brassKey), state(Robot, SteelKey, Robot, Package)).
move(state(Robot, SteelKey, BrassKey, onRobot), drop(package), state(Robot, SteelKey, BrassKey, Robot)).

move(state(Robot, SteelKey, BrassKey, Package), pickup(steelKey), state(Robot, onRobot, BrassKey, Package)):-
    Robot == SteelKey,
    SteelKey \== onRobot,
    nand(BrassKey == onRobot, Package == onRobot).
move(state(Robot, SteelKey, BrassKey, Package), pickup(brassKey), state(Robot, SteelKey, onRobot, Package)):-
    Robot == BrassKey,
    BrassKey \== onRobot,
    nand(SteelKey == onRobot, Package == onRobot).
move(state(Robot, SteelKey, BrassKey, Package), pickup(package), state(Robot, SteelKey, BrassKey, onRobot)):-
    Robot == Package,
    Package \== onRobot,
    nand(BrassKey == onRobot, SteelKey == onRobot).

move(state(Robot, SteelKey, BrassKey, Package), toR1, state(r1, SteelKey, BrassKey, Package)):-
    ((Robot == r2, SteelKey == onRobot);(Robot == r3, BrassKey == onRobot)).
move(state(Robot, SteelKey, BrassKey, Package), toR2, state(r2, SteelKey, BrassKey, Package)):-
    Robot == r1, SteelKey == onRobot.
move(state(Robot, SteelKey, BrassKey, Package), toR3, state(r3, SteelKey, BrassKey, Package)):-
    Robot == r1, BrassKey == onRobot.

solveR(state(r2, _, _, onRobot), _, []).
solveR(State, N, Trace):-
    move(State, Move, NewState),
    N > 0,
    Trace = [Move|Rest],
    NewN is N-1,
    solveR(NewState, NewN, Rest).

nand(X, Y) :- not((X, Y)).
