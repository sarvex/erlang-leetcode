-module('0001.Two Sum').
-export([two_sum/2]).

%% @doc Find indices of two numbers that add up to the target
%% @param Nums List of integers
%% @param Target Target sum
%% @returns List with two indices of numbers that sum to Target
two_sum(Nums, Target) ->
    two_sum(Nums, Target, #{}, 0).

%% @doc Helper function with accumulator for the map and current index
two_sum([X | Rest], Target, Map, Index) ->
    Y = Target - X,
    case maps:find(Y, Map) of
        {ok, PrevIndex} ->
            [PrevIndex, Index];
        error ->
            two_sum(Rest, Target, maps:put(X, Index, Map), Index + 1)
    end;
two_sum([], _Target, _Map, _Index) ->
    []. %% No solution found (though problem states there is always a solution)
