-module(forthic_word_options).

-export([
    from_array/1,
    new/0,
    get/2,
    get_with_default/3,
    has/2,
    keys/1,
    to_record/1,
    count/1
]).

%% ============================================================================
%% WordOptions - Container for word optional parameters
%% ============================================================================

%% WordOptions is represented as a map in Erlang
-type word_options() :: #{string() => term()}.

-export_type([word_options/0]).

%% ============================================================================
%% Construction
%% ============================================================================

%% Create WordOptions from flat array of alternating keys and values
%% Example: from_array(["depth", 2, "with_key", true])
-spec from_array(list()) -> {ok, word_options()} | {error, term()}.
from_array(FlatArray) when is_list(FlatArray) ->
    case length(FlatArray) rem 2 of
        0 ->
            try
                Options = parse_flat_array(FlatArray, #{}),
                {ok, Options}
            catch
                error:Reason -> {error, Reason}
            end;
        _ ->
            {error, {invalid_format, "Options must be key-value pairs (even length)"}}
    end;
from_array(_) ->
    {error, {invalid_format, "Options must be an array"}}.

%% Create empty WordOptions
-spec new() -> word_options().
new() ->
    #{}.

%% ============================================================================
%% Access Methods
%% ============================================================================

%% Get option value, returns undefined if not found
-spec get(word_options(), string()) -> term() | undefined.
get(Options, Key) ->
    maps:get(Key, Options, undefined).

%% Get option value with default
-spec get_with_default(word_options(), string(), term()) -> term().
get_with_default(Options, Key, Default) ->
    maps:get(Key, Options, Default).

%% Check if key exists
-spec has(word_options(), string()) -> boolean().
has(Options, Key) ->
    maps:is_key(Key, Options).

%% Get all keys
-spec keys(word_options()) -> list(string()).
keys(Options) ->
    maps:keys(Options).

%% Convert to record (returns same map in Erlang)
-spec to_record(word_options()) -> word_options().
to_record(Options) ->
    Options.

%% Get number of options
-spec count(word_options()) -> non_neg_integer().
count(Options) ->
    maps:size(Options).

%% ============================================================================
%% Private Helper Functions
%% ============================================================================

%% Parse flat array into map
parse_flat_array([], Acc) ->
    Acc;
parse_flat_array([Key, Value | Rest], Acc) when is_list(Key) ->
    parse_flat_array(Rest, maps:put(Key, Value, Acc));
parse_flat_array([Key, _Value | _Rest], _Acc) ->
    error({invalid_format, io_lib:format("Option key must be a string (dot-symbol). Got: ~p", [Key])}).
