# Working with the Erlang LeetCode Repository

## Code Execution
- Compile a single file: `erlc "problem_file.erl"`
- Run in Erlang shell: `erl -noshell -s module_name function_name arg1 arg2 -s init stop`
- Test a solution: `erl -noshell -eval "io:format(\"~p~n\", ['module_name':function_name(arg1, arg2)])" -s init stop`

## Code Style Guidelines
- **Module Names**: Match file name (e.g., `-module('0001.Two Sum').`)
- **Documentation**: Use `%% @doc`, `%% @param`, `%% @returns` for function docs
- **Type Specs**: Include detailed `-spec` declarations for all functions
- **Pattern Matching**: Prefer pattern matching over conditionals
- **Error Handling**: Use pattern matching with `case` expressions
- **Recursion**: Implement tail recursion with accumulators when appropriate
- **Data Structures**: Use Erlang maps for lookups, lists for sequences
- **Naming**: Use snake_case for functions and variables
- **Base Cases**: Always handle base cases explicitly (e.g., empty lists)