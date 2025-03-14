-module('0002.Add Two Numbers').
-export([add_two_numbers/2]).

%% @doc Add two numbers represented as linked lists
%% @param L1 First linked list (head of list)
%% @param L2 Second linked list (head of list)
%% @returns New linked list representing the sum
-spec add_two_numbers(L1, L2) -> Result when
    L1 :: {integer(), L1 | null},
    L2 :: {integer(), L2 | null},
    Result :: {integer(), Result | null}.
add_two_numbers(L1, L2) ->
    add_two_numbers(L1, L2, 0, null).

%% @doc Helper function with carry and result accumulator
-spec add_two_numbers(L1, L2, Carry, Result) -> NewResult when
    L1 :: {integer(), L1 | null} | null,
    L2 :: {integer(), L2 | null} | null,
    Carry :: integer(),
    Result :: {integer(), Result | null} | null,
    NewResult :: {integer(), NewResult | null}.
add_two_numbers(null, null, 0, Result) ->
    Result;
add_two_numbers(L1, L2, Carry, Result) ->
    % Extract values from nodes or use 0 if null
    Val1 = case L1 of
        {V1, _} -> V1;
        null -> 0
    end,
    
    Val2 = case L2 of
        {V2, _} -> V2;
        null -> 0
    end,
    
    % Calculate sum and new carry
    Sum = Val1 + Val2 + Carry,
    NewCarry = Sum div 10,
    NodeVal = Sum rem 10,
    
    % Get next nodes or null
    Next1 = case L1 of
        {_, N1} -> N1;
        null -> null
    end,
    
    Next2 = case L2 of
        {_, N2} -> N2;
        null -> null
    end,
    
    % Create new node and continue recursion
    NewNode = {NodeVal, Result},
    add_two_numbers(Next1, Next2, NewCarry, NewNode).