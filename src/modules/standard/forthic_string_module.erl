-module(forthic_string_module).
-export([new/0, register_words/1]).

new() -> forthic_module:new("string", "").

register_words(Module) ->
    forthic_module:add_word(Module, ">STR", fun to_str/1),
    forthic_module:add_word(Module, "CONCAT", fun concat/1),
    forthic_module:add_word(Module, "SPLIT", fun split/1),
    forthic_module:add_word(Module, "JOIN", fun join/1),
    forthic_module:add_word(Module, "/N", fun slash_n/1),
    forthic_module:add_word(Module, "/R", fun slash_r/1),
    forthic_module:add_word(Module, "/T", fun slash_t/1),
    forthic_module:add_word(Module, "LOWERCASE", fun lowercase/1),
    forthic_module:add_word(Module, "UPPERCASE", fun uppercase/1),
    forthic_module:add_word(Module, "ASCII", fun ascii/1),
    forthic_module:add_word(Module, "STRIP", fun strip/1),
    forthic_module:add_word(Module, "REPLACE", fun replace/1),
    forthic_module:add_word(Module, "RE-MATCH", fun re_match/1),
    forthic_module:add_word(Module, "RE-MATCH-ALL", fun re_match_all/1),
    forthic_module:add_word(Module, "RE-MATCH-GROUP", fun re_match_group/1),
    forthic_module:add_word(Module, "URL-ENCODE", fun url_encode/1),
    forthic_module:add_word(Module, "URL-DECODE", fun url_decode/1),
    Module.

to_str(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, to_string(Val)).

concat(Interp) ->
    B = forthic_interpreter:stack_pop(Interp),
    A = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, to_string(A) ++ to_string(B)).

split(Interp) ->
    Delim = forthic_interpreter:stack_pop(Interp),
    Str = forthic_interpreter:stack_pop(Interp),
    Parts = string:split(to_string(Str), to_string(Delim), all),
    forthic_interpreter:stack_push(Interp, Parts).

join(Interp) ->
    Delim = forthic_interpreter:stack_pop(Interp),
    Items = forthic_interpreter:stack_pop(Interp),
    Strs = lists:map(fun to_string/1, Items),
    forthic_interpreter:stack_push(Interp, string:join(Strs, to_string(Delim))).

slash_n(Interp) -> forthic_interpreter:stack_push(Interp, "\n").
slash_r(Interp) -> forthic_interpreter:stack_push(Interp, "\r").
slash_t(Interp) -> forthic_interpreter:stack_push(Interp, "\t").

lowercase(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, string:lowercase(to_string(Val))).

uppercase(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, string:uppercase(to_string(Val))).

ascii(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    Str = to_string(Val),
    case Str of
        [C|_] -> forthic_interpreter:stack_push(Interp, C);
        _ -> forthic_interpreter:stack_push(Interp, 0)
    end.

strip(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, string:trim(to_string(Val))).

replace(Interp) ->
    Replace = forthic_interpreter:stack_pop(Interp),
    Pattern = forthic_interpreter:stack_pop(Interp),
    Str = forthic_interpreter:stack_pop(Interp),
    case re:replace(to_string(Str), to_string(Pattern), to_string(Replace), [global, {return, list}]) of
        {ok, Result} -> forthic_interpreter:stack_push(Interp, Result);
        _ -> forthic_interpreter:stack_push(Interp, Str)
    end.

re_match(Interp) ->
    Pattern = forthic_interpreter:stack_pop(Interp),
    Str = forthic_interpreter:stack_pop(Interp),
    S = to_string(Str),
    P = to_string(Pattern),
    case re:run(S, P, [{capture, first, list}]) of
        {match, [Match]} -> forthic_interpreter:stack_push(Interp, Match);
        _ -> forthic_interpreter:stack_push(Interp, null)
    end.

re_match_all(Interp) ->
    Pattern = forthic_interpreter:stack_pop(Interp),
    Str = forthic_interpreter:stack_pop(Interp),
    S = to_string(Str),
    P = to_string(Pattern),
    case re:run(S, P, [global, {capture, first, list}]) of
        {match, Matches} ->
            Result = [M || [M] <- Matches],
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, [])
    end.

re_match_group(Interp) ->
    GroupIdx = forthic_interpreter:stack_pop(Interp),
    Pattern = forthic_interpreter:stack_pop(Interp),
    Str = forthic_interpreter:stack_pop(Interp),
    S = to_string(Str),
    P = to_string(Pattern),
    Idx = case is_integer(GroupIdx) of true -> GroupIdx; _ -> 0 end,
    case re:run(S, P, [{capture, all, list}]) of
        {match, Groups} when Idx >= 0, Idx < length(Groups) ->
            Match = lists:nth(Idx + 1, Groups),
            forthic_interpreter:stack_push(Interp, Match);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

url_encode(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    Str = to_string(Val),
    Encoded = http_uri:encode(Str),
    forthic_interpreter:stack_push(Interp, Encoded).

url_decode(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    Str = to_string(Val),
    Decoded = http_uri:decode(Str),
    forthic_interpreter:stack_push(Interp, Decoded).

to_string(null) -> "";
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_list(V) -> V;
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_float(V) -> float_to_list(V);
to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(_) -> "".
