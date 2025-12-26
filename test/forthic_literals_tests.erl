-module(forthic_literals_tests).
-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Boolean Tests
%% ============================================================================

to_bool_true_test() ->
    ?assertEqual({ok, true}, forthic_literals:to_bool("TRUE")).

to_bool_false_test() ->
    ?assertEqual({ok, false}, forthic_literals:to_bool("FALSE")).

to_bool_invalid_test() ->
    ?assertEqual(nil, forthic_literals:to_bool("true")),
    ?assertEqual(nil, forthic_literals:to_bool("True")),
    ?assertEqual(nil, forthic_literals:to_bool("invalid")).

%% ============================================================================
%% Integer Tests
%% ============================================================================

to_int_positive_test() ->
    ?assertEqual({ok, 42}, forthic_literals:to_int("42")).

to_int_negative_test() ->
    ?assertEqual({ok, -10}, forthic_literals:to_int("-10")).

to_int_zero_test() ->
    ?assertEqual({ok, 0}, forthic_literals:to_int("0")).

to_int_large_test() ->
    ?assertEqual({ok, 1000000}, forthic_literals:to_int("1000000")).

to_int_float_should_fail_test() ->
    ?assertEqual(nil, forthic_literals:to_int("3.14")).

to_int_invalid_test() ->
    ?assertEqual(nil, forthic_literals:to_int("abc")),
    ?assertEqual(nil, forthic_literals:to_int("42abc")).

%% ============================================================================
%% Float Tests
%% ============================================================================

to_float_simple_test() ->
    {ok, Result} = forthic_literals:to_float("3.14"),
    ?assert(abs(Result - 3.14) < 0.0001).

to_float_negative_test() ->
    {ok, Result} = forthic_literals:to_float("-2.5"),
    ?assert(abs(Result - (-2.5)) < 0.0001).

to_float_zero_test() ->
    {ok, Result} = forthic_literals:to_float("0.0"),
    ?assert(abs(Result - 0.0) < 0.0001).

to_float_no_decimal_should_fail_test() ->
    ?assertEqual(nil, forthic_literals:to_float("42")).

to_float_invalid_test() ->
    ?assertEqual(nil, forthic_literals:to_float("abc")).

%% ============================================================================
%% Time Tests
%% ============================================================================

to_time_simple_test() ->
    {ok, {{0, 1, 1}, {9, 0, 0}}} = forthic_literals:to_time("9:00").

to_time_afternoon_test() ->
    {ok, {{0, 1, 1}, {14, 30, 0}}} = forthic_literals:to_time("14:30").

to_time_pm_test() ->
    {ok, {{0, 1, 1}, {14, 30, 0}}} = forthic_literals:to_time("2:30 PM").

to_time_am_test() ->
    {ok, {{0, 1, 1}, {9, 0, 0}}} = forthic_literals:to_time("9:00 AM").

to_time_noon_test() ->
    {ok, {{0, 1, 1}, {12, 0, 0}}} = forthic_literals:to_time("12:00 PM").

to_time_midnight_test() ->
    {ok, {{0, 1, 1}, {0, 0, 0}}} = forthic_literals:to_time("12:00 AM").

to_time_invalid_test() ->
    ?assertEqual(nil, forthic_literals:to_time("25:00")).

%% ============================================================================
%% Date Tests
%% ============================================================================

to_literal_date_valid_test() ->
    Handler = forthic_literals:to_literal_date("UTC"),
    {ok, {{2020, 6, 5}, {0, 0, 0}}} = Handler("2020-06-05").

to_literal_date_year_wildcard_test() ->
    Handler = forthic_literals:to_literal_date("UTC"),
    {ok, {{_, 6, 5}, {0, 0, 0}}} = Handler("YYYY-06-05").

to_literal_date_month_wildcard_test() ->
    Handler = forthic_literals:to_literal_date("UTC"),
    {ok, {{2020, _, 5}, {0, 0, 0}}} = Handler("2020-MM-05").

to_literal_date_day_wildcard_test() ->
    Handler = forthic_literals:to_literal_date("UTC"),
    {ok, {{2020, 6, _}, {0, 0, 0}}} = Handler("2020-06-DD").

to_literal_date_all_wildcards_test() ->
    Handler = forthic_literals:to_literal_date("UTC"),
    {ok, {{_, _, _}, {0, 0, 0}}} = Handler("YYYY-MM-DD").

to_literal_date_invalid_test() ->
    Handler = forthic_literals:to_literal_date("UTC"),
    ?assertEqual(nil, Handler("2020/06/05")),
    ?assertEqual(nil, Handler("not-a-date")).

%% ============================================================================
%% ZonedDateTime Tests
%% ============================================================================

to_zoned_datetime_utc_test() ->
    Handler = forthic_literals:to_zoned_datetime("UTC"),
    {ok, {{{2025, 5, 24}, {10, 15, 0}}, _}} = Handler("2025-05-24T10:15:00Z").

to_zoned_datetime_offset_test() ->
    Handler = forthic_literals:to_zoned_datetime("UTC"),
    {ok, {{{2025, 5, 24}, {10, 15, 0}}, _}} = Handler("2025-05-24T10:15:00-05:00").

to_zoned_datetime_plain_test() ->
    Handler = forthic_literals:to_zoned_datetime("UTC"),
    {ok, {{{2025, 5, 24}, {10, 15, 0}}, _}} = Handler("2025-05-24T10:15:00").

to_zoned_datetime_iana_test() ->
    Handler = forthic_literals:to_zoned_datetime("UTC"),
    {ok, {{{2025, 5, 20}, {8, 0, 0}}, "America/Los_Angeles"}} = Handler("2025-05-20T08:00:00[America/Los_Angeles]").

to_zoned_datetime_offset_with_iana_test() ->
    Handler = forthic_literals:to_zoned_datetime("UTC"),
    {ok, {{{2025, 5, 20}, {8, 0, 0}}, "America/Los_Angeles"}} = Handler("2025-05-20T08:00:00-07:00[America/Los_Angeles]").

to_zoned_datetime_invalid_test() ->
    Handler = forthic_literals:to_zoned_datetime("UTC"),
    ?assertEqual(nil, Handler("not-a-datetime")),
    ?assertEqual(nil, Handler("2025-05-24 10:15:00")).
