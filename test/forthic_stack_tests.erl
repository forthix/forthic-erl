-module(forthic_stack_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Stack Tests
%% ============================================================================

push_and_pop_test() ->
    Stack = forthic_stack:new(),
    Stack2 = forthic_stack:push(Stack, 42),
    ?assertEqual(1, forthic_stack:length(Stack2)),
    {ok, Val, Stack3} = forthic_stack:pop(Stack2),
    ?assertEqual(42, Val),
    ?assertEqual(0, forthic_stack:length(Stack3)).

underflow_test() ->
    Stack = forthic_stack:new(),
    Result = forthic_stack:pop(Stack),
    ?assertEqual({error, stack_underflow}, Result).

peek_test() ->
    Stack = forthic_stack:new(),
    Stack2 = forthic_stack:push(Stack, 42),
    {ok, Val} = forthic_stack:peek(Stack2),
    ?assertEqual(42, Val),
    ?assertEqual(1, forthic_stack:length(Stack2)).  % Still on stack

multiple_items_test() ->
    Stack = forthic_stack:new(),
    Stack2 = forthic_stack:push(Stack, 1),
    Stack3 = forthic_stack:push(Stack2, 2),
    Stack4 = forthic_stack:push(Stack3, 3),
    ?assertEqual(3, forthic_stack:length(Stack4)),
    {ok, Val3, Stack5} = forthic_stack:pop(Stack4),
    {ok, Val2, Stack6} = forthic_stack:pop(Stack5),
    {ok, Val1, Stack7} = forthic_stack:pop(Stack6),
    ?assertEqual(3, Val3),
    ?assertEqual(2, Val2),
    ?assertEqual(1, Val1),
    ?assertEqual(0, forthic_stack:length(Stack7)).

clear_test() ->
    Stack = forthic_stack:new(),
    Stack2 = forthic_stack:push(Stack, 1),
    Stack3 = forthic_stack:push(Stack2, 2),
    Stack4 = forthic_stack:push(Stack3, 3),
    Stack5 = forthic_stack:clear(Stack4),
    ?assertEqual(0, forthic_stack:length(Stack5)).

at_test() ->
    Stack = forthic_stack:new(),
    Stack2 = forthic_stack:push(Stack, "bottom"),
    Stack3 = forthic_stack:push(Stack2, "middle"),
    Stack4 = forthic_stack:push(Stack3, "top"),
    {ok, Bottom} = forthic_stack:at(Stack4, 0),
    {ok, Middle} = forthic_stack:at(Stack4, 1),
    {ok, Top} = forthic_stack:at(Stack4, 2),
    ?assertEqual("bottom", Bottom),
    ?assertEqual("middle", Middle),
    ?assertEqual("top", Top).

set_test() ->
    Stack = forthic_stack:new(),
    Stack2 = forthic_stack:push(Stack, 1),
    Stack3 = forthic_stack:push(Stack2, 2),
    Stack4 = forthic_stack:push(Stack3, 3),
    {ok, Stack5} = forthic_stack:set(Stack4, 1, 99),
    {ok, Val} = forthic_stack:at(Stack5, 1),
    ?assertEqual(99, Val).

to_list_test() ->
    Stack = forthic_stack:new(),
    Stack2 = forthic_stack:push(Stack, 1),
    Stack3 = forthic_stack:push(Stack2, 2),
    Stack4 = forthic_stack:push(Stack3, 3),
    List = forthic_stack:to_list(Stack4),
    ?assertEqual([1, 2, 3], List).  % Bottom to top order
