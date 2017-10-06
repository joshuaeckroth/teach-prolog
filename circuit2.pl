    knownGates(
        [(and, 2, [([1,1],1), ([0,1],0), ([1,0],0), ([0,0],0)]),
         (or,  2, [([1,1],1), ([0,1],1), ([1,0],1), ([0,0],0)]),
         (not, 1, [([1],0), ([0],1)])]).

    getWireState(Wires, WireName, State) :- member((WireName, State), Wires).

    evalCombLogic(_, []). % base case, no gates left to evaluate
    evalCombLogic(Wires, [(Gate, Inputs, Output)|RestGates]) :-
        knownGates(KnownGates),
        % extract gate info
        member((Gate, InputCount, Table), KnownGates),
        % ensure inputs list is right length
        length(Inputs, InputCount),
        % extract current state of the wires involved
        maplist(getWireState(Wires), Inputs, InputsWithState),
        % eval gate with those wire states
        member((InputsWithState, OutputState), Table),
        % ensure the wire's state is as computed
        getWireState(Wires, Output, OutputState),
        % look at rest of gates
        evalCombLogic(Wires, RestGates).

