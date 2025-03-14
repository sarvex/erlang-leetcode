# Erlang LeetCode Solutions

A comprehensive collection of LeetCode problem solutions implemented in Erlang, demonstrating idiomatic Erlang patterns, functional programming techniques, and efficient algorithms.

## Project Overview

This repository contains Erlang solutions to LeetCode problems, showcasing:

- Functional programming approaches to algorithmic challenges
- Erlang-specific optimizations and idioms
- Type specifications using `-spec`
- Tail-recursive implementations for memory efficiency
- Pattern matching for elegant control flow
- Proper documentation with EDoc comments

## Code Structure

Each solution follows a consistent pattern:

- Module named after the problem (e.g., `'0001.Two Sum'`)
- Exported functions that solve the specific problem
- Type specifications for function signatures
- Comprehensive documentation comments
- Helper functions for recursive implementations
- Efficient data structures (maps, lists, tuples) appropriate for each problem

## Implementation Highlights

- **Tail Recursion**: Solutions use tail recursion with accumulators for memory efficiency
- **Pattern Matching**: Leveraging Erlang's pattern matching for control flow and data extraction
- **Maps and ETS**: Using maps for O(1) lookups where appropriate
- **Type Specifications**: Including `-spec` annotations for better documentation and dialyzer support
- **Immutable Data**: Embracing functional programming with immutable data structures

## Example Solutions

### Two Sum (Problem 1)
```erlang
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
```

### Add Two Numbers (Problem 2)
```erlang
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

%% Helper function implementation with carry and result accumulator
```

## Running the Solutions

To compile and run a solution:

```bash
$ erlc '0001.Two Sum.erl'
$ erl -noshell -s '0001.Two Sum' two_sum "[2,7,11,15]" "9" -s init stop
```

## Testing

Solutions can be tested using Erlang's EUnit framework or by manually verifying against LeetCode test cases.

## Contributing

Contributions are welcome! When adding new solutions, please follow these guidelines:

1. Use idiomatic Erlang patterns
2. Include proper type specifications
3. Add comprehensive documentation
4. Implement efficient algorithms with appropriate time/space complexity
5. Use tail recursion where applicable
6. Follow the existing naming and module structure

## License

This repository is available under the MIT License.
