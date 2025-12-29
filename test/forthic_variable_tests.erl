-module(forthic_variable_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Variable Tests
%% ============================================================================

create_test() ->
    Interp = forthic_interpreter:new(),
    Var = forthic_variable:new("my_var", "app"),
    ?assertEqual("my_var", forthic_variable:get_name(Var)),
    % Initial value should be null
    ?assertEqual(null, forthic_variable:get_value(Var, Interp)).

set_value_test() ->
    Interp = forthic_interpreter:new(),
    Var = forthic_variable:new("my_var", "app"),
    forthic_variable:set_value(Var, 99, Interp),
    ?assertEqual(99, forthic_variable:get_value(Var, Interp)),
    ?assertEqual("my_var", forthic_variable:get_name(Var)).

immutability_test() ->
    Interp = forthic_interpreter:new(),
    Var = forthic_variable:new("my_var", "app"),
    forthic_variable:set_value(Var, 42, Interp),
    ?assertEqual(42, forthic_variable:get_value(Var, Interp)),
    % Set new value
    forthic_variable:set_value(Var, 99, Interp),
    % Variable now has new value (values are mutable in ETS)
    ?assertEqual(99, forthic_variable:get_value(Var, Interp)).
