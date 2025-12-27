-module(forthic_interpreter_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Interpreter Tests
%% ============================================================================

initial_state_test() ->
    Interp = forthic_interpreter:new(),
    Stack = forthic_interpreter:get_stack(Interp),
    ?assertEqual(0, forthic_stack:length(Stack)),
    Module = forthic_interpreter:cur_module(Interp),
    ?assertEqual("", forthic_module:get_name(Module)).

push_string_test() ->
    Interp = forthic_interpreter:new(),
    {ok, Interp2} = forthic_interpreter:run(Interp, "\"hello\""),
    {ok, Val, _Interp3} = forthic_interpreter:stack_pop(Interp2),
    ?assertEqual("hello", Val).

comment_test() ->
    Interp = forthic_interpreter:new(),
    {ok, Interp2} = forthic_interpreter:run(Interp, "# This is a comment"),
    Stack = forthic_interpreter:get_stack(Interp2),
    ?assertEqual(0, forthic_stack:length(Stack)).

push_integers_test() ->
    Interp = forthic_interpreter:new(),
    {ok, Interp2} = forthic_interpreter:run(Interp, "1 2 3"),
    Stack = forthic_interpreter:get_stack(Interp2),
    ?assertEqual(3, forthic_stack:length(Stack)),
    List = forthic_stack:to_list(Stack),
    ?assertEqual([1, 2, 3], List).

push_float_test() ->
    Interp = forthic_interpreter:new(),
    {ok, Interp2} = forthic_interpreter:run(Interp, "3.14"),
    {ok, Val, _Interp3} = forthic_interpreter:stack_pop(Interp2),
    ?assert(abs(Val - 3.14) < 0.001).

push_bool_test() ->
    Interp = forthic_interpreter:new(),
    {ok, Interp2} = forthic_interpreter:run(Interp, "TRUE FALSE"),
    {ok, Val1, Interp3} = forthic_interpreter:stack_pop(Interp2),
    {ok, Val2, _Interp4} = forthic_interpreter:stack_pop(Interp3),
    ?assertEqual(false, Val1),
    ?assertEqual(true, Val2).

stack_underflow_test() ->
    Interp = forthic_interpreter:new(),
    Result = forthic_interpreter:stack_pop(Interp),
    ?assertMatch({error, _}, Result).
