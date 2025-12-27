-module(forthic_word_options_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% WordOptions Tests
%% ============================================================================

create_from_flat_array_test() ->
    {ok, Opts} = forthic_word_options:from_array(["depth", 2, "with_key", true]),
    ?assert(forthic_word_options:has(Opts, "depth")),
    ?assert(forthic_word_options:has(Opts, "with_key")),
    ?assertEqual(2, forthic_word_options:get(Opts, "depth")),
    ?assertEqual(true, forthic_word_options:get(Opts, "with_key")).

requires_even_length_test() ->
    Result = forthic_word_options:from_array(["depth", 2, "odd"]),
    ?assertMatch({error, {invalid_format, _}}, Result).

requires_string_keys_test() ->
    Result = forthic_word_options:from_array([123, 2, "key", true]),
    ?assertMatch({error, {invalid_format, _}}, Result).

default_values_test() ->
    {ok, Opts} = forthic_word_options:from_array(["key1", "value1"]),
    ?assertEqual("value1", forthic_word_options:get_with_default(Opts, "key1", "default")),
    ?assertEqual("default", forthic_word_options:get_with_default(Opts, "missing", "default")).

has_method_test() ->
    {ok, Opts} = forthic_word_options:from_array(["key1", "value1"]),
    ?assert(forthic_word_options:has(Opts, "key1")),
    ?assertNot(forthic_word_options:has(Opts, "missing")).

to_record_test() ->
    {ok, Opts} = forthic_word_options:from_array(["key1", "value1", "key2", 42]),
    Record = forthic_word_options:to_record(Opts),
    ?assert(is_map(Record)),
    ?assertEqual("value1", maps:get("key1", Record)),
    ?assertEqual(42, maps:get("key2", Record)).

keys_test() ->
    {ok, Opts} = forthic_word_options:from_array(["key1", "value1", "key2", "value2"]),
    Keys = forthic_word_options:keys(Opts),
    ?assertEqual(2, length(Keys)),
    ?assert(lists:member("key1", Keys)),
    ?assert(lists:member("key2", Keys)).

override_behavior_test() ->
    {ok, Opts} = forthic_word_options:from_array(["key", "first", "key", "second"]),
    ?assertEqual("second", forthic_word_options:get(Opts, "key")).

empty_options_test() ->
    {ok, Opts} = forthic_word_options:from_array([]),
    ?assertNot(forthic_word_options:has(Opts, "anything")),
    ?assertEqual(0, forthic_word_options:count(Opts)).

count_test() ->
    {ok, Opts} = forthic_word_options:from_array(["key1", 1, "key2", 2, "key3", 3]),
    ?assertEqual(3, forthic_word_options:count(Opts)).
