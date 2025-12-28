-module(forthic_math_module).
-export([new/0, register_words/1]).

new() ->
    forthic_module:new("math", "").

register_words(Module) ->
    % Arithmetic
    forthic_module:add_word(Module, "+", fun plus/1),
    forthic_module:add_word(Module, "ADD", fun plus/1),
    forthic_module:add_word(Module, "-", fun minus/1),
    forthic_module:add_word(Module, "SUBTRACT", fun minus/1),
    forthic_module:add_word(Module, "*", fun times/1),
    forthic_module:add_word(Module, "MULTIPLY", fun times/1),
    forthic_module:add_word(Module, "/", fun divide/1),
    forthic_module:add_word(Module, "DIVIDE", fun divide/1),
    forthic_module:add_word(Module, "MOD", fun mod_op/1),
    % Aggregates
    forthic_module:add_word(Module, "SUM", fun sum/1),
    forthic_module:add_word(Module, "MEAN", fun mean/1),
    forthic_module:add_word(Module, "MAX", fun max_op/1),
    forthic_module:add_word(Module, "MIN", fun min_op/1),
    % Conversions
    forthic_module:add_word(Module, ">INT", fun to_int/1),
    forthic_module:add_word(Module, ">FLOAT", fun to_float/1),
    forthic_module:add_word(Module, "ROUND", fun round_op/1),
    forthic_module:add_word(Module, ">FIXED", fun to_fixed/1),
    % Functions
    forthic_module:add_word(Module, "ABS", fun abs_op/1),
    forthic_module:add_word(Module, "SQRT", fun sqrt_op/1),
    forthic_module:add_word(Module, "FLOOR", fun floor_op/1),
    forthic_module:add_word(Module, "CEIL", fun ceil_op/1),
    forthic_module:add_word(Module, "CLAMP", fun clamp/1),
    % Special
    forthic_module:add_word(Module, "INFINITY", fun infinity/1),
    forthic_module:add_word(Module, "UNIFORM-RANDOM", fun uniform_random/1),
    Module.

plus(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    case is_list(B) of
        true ->
            Sum = lists:foldl(fun(X, Acc) -> Acc + to_number(X) end, 0, B),
            forthic_interpreter:stack_push(Interp, Sum);
        false ->
            A = forthic_interpreter:stack_pop(Interp),
            forthic_interpreter:stack_push(Interp, to_number(A) + to_number(B))
    end.

minus(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, to_number(A) - to_number(B)).

times(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, to_number(A) * to_number(B)).

divide(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    case to_number(B) of
        0 -> forthic_interpreter:stack_push(Interp, null);
        Divisor -> forthic_interpreter:stack_push(Interp, to_number(A) / Divisor)
    end.

mod_op(Interp) ->
    N = forthic_interpreter:stack_pop(Interp),
    M = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, trunc(to_number(M)) rem trunc(to_number(N))).

sum(Interp) ->
    Items = forthic_interpreter:stack_pop(Interp),
    Sum = lists:foldl(fun(X, Acc) -> Acc + to_number(X) end, 0, Items),
    forthic_interpreter:stack_push(Interp, Sum).

mean(Interp) ->
    Items = forthic_interpreter:stack_pop(Interp),
    case length(Items) of
        0 -> forthic_interpreter:stack_push(Interp, 0);
        Len ->
            Sum = lists:foldl(fun(X, Acc) -> Acc + to_number(X) end, 0, Items),
            forthic_interpreter:stack_push(Interp, Sum / Len)
    end.

max_op(Interp) ->
    Items = forthic_interpreter:stack_pop(Interp),
    case Items of
        [] -> forthic_interpreter:stack_push(Interp, null);
        _ -> forthic_interpreter:stack_push(Interp, lists:max(lists:map(fun to_number/1, Items)))
    end.

min_op(Interp) ->
    Items = forthic_interpreter:stack_pop(Interp),
    case Items of
        [] -> forthic_interpreter:stack_push(Interp, null);
        _ -> forthic_interpreter:stack_push(Interp, lists:min(lists:map(fun to_number/1, Items)))
    end.

to_int(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, trunc(to_number(Val))).

to_float(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, to_number(Val)).

round_op(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, round(to_number(Val))).

to_fixed(Interp) ->
    Decimals = forthic_interpreter:stack_pop(Interp),
    Val = forthic_interpreter:stack_pop(Interp),
    %% TODO: Format to fixed decimals
    forthic_interpreter:stack_push(Interp, float_to_list(to_number(Val), [{decimals, trunc(to_number(Decimals))}])).

abs_op(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, abs(to_number(Val))).

sqrt_op(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, math:sqrt(to_number(Val))).

floor_op(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, math:floor(to_number(Val))).

ceil_op(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, math:ceil(to_number(Val))).

clamp(Interp) ->
    Max = forthic_interpreter:stack_pop(Interp),
    Min = forthic_interpreter:stack_pop(Interp),
    Val = forthic_interpreter:stack_pop(Interp),
    Clamped = max(to_number(Min), min(to_number(Max), to_number(Val))),
    forthic_interpreter:stack_push(Interp, Clamped).

infinity(Interp) ->
    forthic_interpreter:stack_push(Interp, infinity).

uniform_random(Interp) ->
    Max = forthic_interpreter:stack_pop(Interp),
    Min = forthic_interpreter:stack_pop(Interp),
    MinNum = to_number(Min),
    MaxNum = to_number(Max),
    Result = MinNum + (rand:uniform() * (MaxNum - MinNum)),
    forthic_interpreter:stack_push(Interp, Result).

to_number(null) -> 0;
to_number(N) when is_number(N) -> N;
to_number(N) when is_integer(N) -> float(N);
to_number(_) -> 0.
