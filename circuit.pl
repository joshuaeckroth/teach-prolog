
inputCount(and, 2).
inputCount(or, 2).
inputCount(not, 1).

evalGate(and, [1, 1], 1).
evalGate(and, [0, 1], 0).
evalGate(and, [1, 0], 0).
evalGate(and, [0, 0], 0).

evalGate(or, [1, 1], 1).
evalGate(or, [0, 1], 1).
evalGate(or, [1, 0], 1).
evalGate(or, [0, 0], 0).

evalGate(not, [1], 0).
evalGate(not, [0], 1).

getWireState(Wires, WireName, State) :- member((WireName, State), Wires), !.

evalCombinationalLogic(_, []).
evalCombinationalLogic(Wires, [(Gate, Inputs, Output)|Gates]) :-
    % ensure inputs list is right length
    inputCount(Gate, InputCount),
    length(Inputs, InputCount),
    % apply the gate's logic
    maplist(getWireState(Wires), Inputs, InputsWithState),
    evalGate(Gate, InputsWithState, OutputState),
    % ensure the wire's state is as computed
    getWireState(Wires, Output, OutputState),
    % look at rest of gates
    evalCombinationalLogic(Wires, Gates).

