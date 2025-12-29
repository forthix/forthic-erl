-module(forthic_boolean_module).
-export([new/0, register_words/1]).

new() -> forthic_module:new("boolean", "").

register_words(Module) ->
    forthic_module:add_word(Module, "==", fun equal/1),
    forthic_module:add_word(Module, "!=", fun not_equal/1),
    forthic_module:add_word(Module, "<", fun less_than/1),
    forthic_module:add_word(Module, ">", fun greater_than/1),
    forthic_module:add_word(Module, "<=", fun less_or_equal/1),
    forthic_module:add_word(Module, ">=", fun greater_or_equal/1),
    forthic_module:add_word(Module, "AND", fun and_op/1),
    forthic_module:add_word(Module, "OR", fun or_op/1),
    forthic_module:add_word(Module, "NOT", fun not_op/1),
    forthic_module:add_word(Module, "TRUE", fun true_op/1),
    forthic_module:add_word(Module, "FALSE", fun false_op/1),
    forthic_module:add_word(Module, "NULL", fun null_op/1),
    forthic_module:add_word(Module, "IN", fun in_op/1),
    Module.

equal(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, A =:= B).

not_equal(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, A =/= B).

less_than(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, A < B).

greater_than(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, A > B).

less_or_equal(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, A =< B).

greater_or_equal(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, A >= B).

and_op(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, is_truthy(A) andalso is_truthy(B)).

or_op(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, is_truthy(A) orelse is_truthy(B)).

not_op(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, not is_truthy(Val)).

true_op(Interp) -> forthic_interpreter:stack_push(Interp, true).
false_op(Interp) -> forthic_interpreter:stack_push(Interp, false).
null_op(Interp) -> forthic_interpreter:stack_push(Interp, null).

in_op(Interp) ->
    Container = forthic_interpreter:stack_pop(Interp),
    Item = forthic_interpreter:stack_pop(Interp),
    Result = case Container of
        L when is_list(L) -> lists:member(Item, L);
        M when is_map(M) -> maps:is_key(Item, M);
        _ -> false
    end,
    forthic_interpreter:stack_push(Interp, Result).

is_truthy(null) -> false;
is_truthy(false) -> false;
is_truthy(0) -> false;
is_truthy("") -> false;
is_truthy(_) -> true.
