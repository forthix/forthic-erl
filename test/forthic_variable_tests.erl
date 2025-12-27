-module(forthic_variable_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Variable Tests
%% ============================================================================

create_test() ->
    Var = forthic_variable:new("my_var", 42),
    ?assertEqual("my_var", forthic_variable:get_name(Var)),
    ?assertEqual(42, forthic_variable:get_value(Var)).

set_value_test() ->
    Var = forthic_variable:new("my_var", 42),
    Var2 = forthic_variable:set_value(Var, 99),
    ?assertEqual(99, forthic_variable:get_value(Var2)),
    ?assertEqual("my_var", forthic_variable:get_name(Var2)).

immutability_test() ->
    Var = forthic_variable:new("my_var", 42),
    Var2 = forthic_variable:set_value(Var, 99),
    % Original variable unchanged
    ?assertEqual(42, forthic_variable:get_value(Var)),
    % New variable has new value
    ?assertEqual(99, forthic_variable:get_value(Var2)).
