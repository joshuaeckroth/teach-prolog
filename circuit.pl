
knownGates(
    [[and, 2, [[[1,1],1],
               [[0,1],0],
               [[1,0],0],
               [[0,0],0]]],
     [or, 2, [[[1,1],1],
              [[0,1],1],
              [[1,0],1],
              [[0,0],0]]],
     [not, 1, [[[1],0],
               [[0],1]]]]).

getWireState(Wires, WireName, State) :- member((WireName, State), Wires).

evalCombinationalLogic(_, _, []).
evalCombinationalLogic(KnownGates, Wires, [(Gate, Inputs, Output)|RestGates]) :-
    % extract gate info
    member([Gate, InputCount, Table], KnownGates),
    % ensure inputs list is right length
    length(Inputs, InputCount),
    % extract current state of the wires involved
    maplist(getWireState(Wires), Inputs, InputsWithState),
    % eval gate with those wire states
    member([InputsWithState, OutputState], Table),
    % ensure the wire's state is as computed
    getWireState(Wires, Output, OutputState),
    % look at rest of gates
    evalCombinationalLogic(KnownGates, Wires, RestGates).

%evalMultipleExamples(_, [], _).
%evalMultipleExamples(KnownGates, [Wires|ListWires], Gates) :-
%    evalCombinationalLogic(KnownGates, Wires, Gates),
%    evalMultipleExamples(KnownGates, ListWires, Gates).

% example usage:
% knownGates(KG), evalCombinationalLogic(KG, [(a,AState),(b,BState),(c,CState),(d,DState),(e,EState)], [(and, [a, b], c), (not, [c], d), (or, [a, c], e)]).
% AState = BState, BState = CState, CState = EState, EState = 1,
% DState = 0 ;
% AState = CState, CState = EState, EState = 0,
% BState = DState, DState = 1 ;
% AState = DState, DState = EState, EState = 1,
% BState = CState, CState = 0 ;
% AState = BState, BState = CState, CState = EState, EState = 0,
% DState = 1.

% example usage:
% knownGates(KG), evalCombinationalLogic(KG, [(a,AState),(b,1),(c,CState),(d,DState),(e,0)], [(and, [a, b], c), (not, [c], d), (or, [d|OrInputs], e)]).
% AState = CState, CState = 1,
% DState = 0,
% OrInputs = [d] ;
% AState = CState, CState = 1,
% DState = 0,
% OrInputs = [e] ;
% false.

% example usage:
% knownGates(KG), evalCombinationalLogic(KG, [(a,0),(b,1),(c,CState),(d,DState),(e,1)], [(and, [a, b], c), (not, [c], d), (LastGate, [d|RestInputs], Output)]).
% CState = 0,
% DState = 1,
% LastGate = and,
% RestInputs = [a],
% Output = a ;
% CState = 0,
% DState = 1,
% LastGate = and,
% RestInputs = [a],
% Output = c ;
% CState = 0,
% DState = 1,
% LastGate = and,
% RestInputs = [b],
% Output = b ;
% CState = 0,
% DState = 1,
% LastGate = and,
% RestInputs = [b],
% Output = d 
% etc.

% doesn't work:
% example usage: derive XOR
% evalCombinationalLogic([(a,0),(b,1)|Wires], [(, [a, b], c), (not, [c], d), (LastGate, [d|RestInputs], Output)]).

