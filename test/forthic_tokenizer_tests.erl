-module(forthic_tokenizer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/forthic_tokenizer.hrl").

%% ============================================================================
%% Basic Token Tests
%% ============================================================================

single_word_test() ->
    T = forthic_tokenizer:new("WORD"),
    {ok, Token, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(word, Token#token.type),
    ?assertEqual("WORD", Token#token.string).

multiple_words_test() ->
    T = forthic_tokenizer:new("WORD1 WORD2 WORD3"),
    {ok, T1, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(word, T1#token.type),
    {ok, T2, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(word, T2#token.type),
    {ok, T3, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(word, T3#token.type).

array_tokens_test() ->
    T = forthic_tokenizer:new("[ 1 2 3 ]"),
    {ok, T1, T_} = forthic_tokenizer:next_token(T),
    ?assertEqual(start_array, T1#token.type),

    {ok, T2, T__} = forthic_tokenizer:next_token(T_),
    ?assertEqual(word, T2#token.type),
    ?assertEqual("1", T2#token.string),

    {ok, T3, T___} = forthic_tokenizer:next_token(T__),
    ?assertEqual(word, T3#token.type),

    {ok, T4, T____} = forthic_tokenizer:next_token(T___),
    ?assertEqual(word, T4#token.type),

    {ok, T5, _} = forthic_tokenizer:next_token(T____),
    ?assertEqual(end_array, T5#token.type).

module_tokens_test() ->
    T = forthic_tokenizer:new("{module}"),
    {ok, T1, T_} = forthic_tokenizer:next_token(T),
    ?assertEqual(start_module, T1#token.type),
    ?assertEqual("module", T1#token.string),

    {ok, T2, _} = forthic_tokenizer:next_token(T_),
    ?assertEqual(end_module, T2#token.type).

definition_tokens_test() ->
    T = forthic_tokenizer:new(": DOUBLE 2 * ;"),
    {ok, T1, T_} = forthic_tokenizer:next_token(T),
    ?assertEqual(start_def, T1#token.type),
    ?assertEqual("DOUBLE", T1#token.string),

    {ok, T2, T__} = forthic_tokenizer:next_token(T_),
    ?assertEqual(word, T2#token.type),

    {ok, T3, T___} = forthic_tokenizer:next_token(T__),
    ?assertEqual(word, T3#token.type),

    {ok, T4, _} = forthic_tokenizer:next_token(T___),
    ?assertEqual(end_def, T4#token.type).

%% ============================================================================
%% String Tests
%% ============================================================================

double_quote_string_test() ->
    T = forthic_tokenizer:new("\"hello world\""),
    {ok, Token, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(string, Token#token.type),
    ?assertEqual("hello world", Token#token.string).

single_quote_string_test() ->
    T = forthic_tokenizer:new("'hello world'"),
    {ok, Token, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(string, Token#token.type),
    ?assertEqual("hello world", Token#token.string).

triple_quote_string_test() ->
    T = forthic_tokenizer:new("\"\"\"multi\nline\nstring\"\"\""),
    {ok, Token, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(string, Token#token.type),
    ?assertEqual("multi\nline\nstring", Token#token.string).

empty_string_test() ->
    T = forthic_tokenizer:new("\"\""),
    {ok, Token, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(string, Token#token.type),
    ?assertEqual("", Token#token.string).

%% ============================================================================
%% Comment Tests
%% ============================================================================

comment_test() ->
    T = forthic_tokenizer:new("WORD1 # this is a comment\nWORD2"),
    {ok, T1, T_} = forthic_tokenizer:next_token(T),
    ?assertEqual(word, T1#token.type),
    ?assertEqual("WORD1", T1#token.string),

    {ok, T2, T__} = forthic_tokenizer:next_token(T_),
    ?assertEqual(comment, T2#token.type),
    ?assert(string:str(T2#token.string, "this is a comment") > 0),

    {ok, T3, _} = forthic_tokenizer:next_token(T__),
    ?assertEqual(word, T3#token.type),
    ?assertEqual("WORD2", T3#token.string).

%% ============================================================================
%% Dot Symbol Tests
%% ============================================================================

dot_symbol_test() ->
    T = forthic_tokenizer:new(".field"),
    {ok, Token, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(dot_symbol, Token#token.type),
    ?assertEqual("field", Token#token.string).

dot_symbol_with_hyphen_test() ->
    T = forthic_tokenizer:new(".field-name"),
    {ok, Token, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(dot_symbol, Token#token.type),
    ?assertEqual("field-name", Token#token.string).

lone_dot_is_word_test() ->
    T = forthic_tokenizer:new("."),
    {ok, Token, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(word, Token#token.type),
    ?assertEqual(".", Token#token.string).

%% ============================================================================
%% Memo Tests
%% ============================================================================

memo_test() ->
    T = forthic_tokenizer:new("@: MEMOIZED 2 * ;"),
    {ok, T1, T_} = forthic_tokenizer:next_token(T),
    ?assertEqual(start_memo, T1#token.type),
    ?assertEqual("MEMOIZED", T1#token.string),

    {ok, T2, _} = forthic_tokenizer:next_token(T_),
    ?assertEqual(word, T2#token.type),
    ?assertEqual("2", T2#token.string).

%% ============================================================================
%% RFC 9557 DateTime Tests
%% ============================================================================

rfc9557_datetime_test() ->
    T = forthic_tokenizer:new("2025-05-20T08:00:00[America/Los_Angeles]"),
    {ok, Token, _} = forthic_tokenizer:next_token(T),
    ?assertEqual(word, Token#token.type),
    ?assertEqual("2025-05-20T08:00:00[America/Los_Angeles]", Token#token.string).

%% ============================================================================
%% Whitespace Tests
%% ============================================================================

whitespace_test() ->
    T = forthic_tokenizer:new("WORD1\t\tWORD2\n\nWORD3"),
    {ok, T1, T_} = forthic_tokenizer:next_token(T),
    ?assertEqual("WORD1", T1#token.string),

    {ok, T2, T__} = forthic_tokenizer:next_token(T_),
    ?assertEqual("WORD2", T2#token.string),

    {ok, T3, _} = forthic_tokenizer:next_token(T__),
    ?assertEqual("WORD3", T3#token.string).
