-module(forthic_record_module).
-export([new/0, register_words/1]).

new() -> forthic_module:new("record", "").

register_words(Module) ->
    Words = [
        {"REC", fun create_record/1}, {"<REC!", fun set_record_value/1},
        {"REC@", fun get_record_value/1}, {"|REC@", fun pipe_rec_at/1},
        {"KEYS", fun keys/1}, {"VALUES", fun values/1},
        {"RELABEL", fun relabel/1}, {"INVERT-KEYS", fun invert_keys/1},
        {"REC-DEFAULTS", fun rec_defaults/1}, {"<DEL", fun del/1}
    ],
    lists:foreach(fun({Name, Fun}) -> forthic_module:add_word(Module, Name, Fun) end, Words),
    Module.

create_record(Interp) ->
    Pairs = forthic_interpreter:stack_pop(Interp),
    Rec = lists:foldl(fun([K, V], Acc) -> maps:put(K, V, Acc) end, #{}, Pairs),
    forthic_interpreter:stack_push(Interp, Rec).

set_record_value(Interp) ->
    Field = forthic_interpreter:stack_pop(Interp),
    Value = forthic_interpreter:stack_pop(Interp),
    Rec = forthic_interpreter:stack_pop(Interp),
    NewRec = maps:put(Field, Value, Rec),
    forthic_interpreter:stack_push(Interp, NewRec).

get_record_value(Interp) ->
    Field = forthic_interpreter:stack_pop(Interp),
    Rec = forthic_interpreter:stack_pop(Interp),
    Val = maps:get(Field, Rec, null),
    forthic_interpreter:stack_push(Interp, Val).

pipe_rec_at(Interp) ->
    Field = forthic_interpreter:stack_pop(Interp),
    Recs = forthic_interpreter:stack_pop(Interp),
    Vals = lists:map(fun(R) -> maps:get(Field, R, null) end, Recs),
    forthic_interpreter:stack_push(Interp, Vals).

keys(Interp) ->
    Rec = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, maps:keys(Rec)).

values(Interp) ->
    Rec = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, maps:values(Rec)).

relabel(Interp) ->
    NewKeys = forthic_interpreter:stack_pop(Interp),
    OldKeys = forthic_interpreter:stack_pop(Interp),
    Container = forthic_interpreter:stack_pop(Interp),
    case {is_map(Container), is_list(OldKeys), is_list(NewKeys)} of
        {true, true, true} when length(OldKeys) =:= length(NewKeys) ->
            Mappings = lists:zip(OldKeys, NewKeys),
            Result = maps:fold(fun(K, V, Acc) ->
                NewKey = case lists:keyfind(K, 1, Mappings) of
                    {_, NK} -> NK;
                    false -> K
                end,
                maps:put(NewKey, V, Acc)
            end, #{}, Container),
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, Container)
    end.

invert_keys(Interp) ->
    Rec = forthic_interpreter:stack_pop(Interp),
    case is_map(Rec) of
        true ->
            %% Invert two-level nested record
            %% {a => {x => 1, y => 2}, b => {x => 3, y => 4}}
            %% becomes {x => {a => 1, b => 3}, y => {a => 2, b => 4}}
            Result = maps:fold(fun(FirstKey, FirstVal, Acc1) ->
                case is_map(FirstVal) of
                    true ->
                        maps:fold(fun(SecondKey, SecondVal, Acc2) ->
                            Inner = maps:get(SecondKey, Acc2, #{}),
                            NewInner = maps:put(FirstKey, SecondVal, Inner),
                            maps:put(SecondKey, NewInner, Acc2)
                        end, Acc1, FirstVal);
                    false -> Acc1
                end
            end, #{}, Rec),
            forthic_interpreter:stack_push(Interp, Result);
        false ->
            forthic_interpreter:stack_push(Interp, #{})
    end.

rec_defaults(Interp) ->
    KeyVals = forthic_interpreter:stack_pop(Interp),
    Rec = forthic_interpreter:stack_pop(Interp),
    BaseRec = case is_map(Rec) of true -> Rec; false -> #{} end,
    case is_list(KeyVals) of
        true ->
            Result = lists:foldl(fun(Pair, Acc) ->
                case Pair of
                    [Key, Val] when is_list(Key) orelse is_binary(Key) ->
                        KeyStr = case Key of
                            K when is_binary(K) -> binary_to_list(K);
                            K when is_list(K) -> K
                        end,
                        case maps:is_key(KeyStr, Acc) of
                            false -> maps:put(KeyStr, Val, Acc);
                            true -> Acc
                        end;
                    _ -> Acc
                end
            end, BaseRec, KeyVals),
            forthic_interpreter:stack_push(Interp, Result);
        false ->
            forthic_interpreter:stack_push(Interp, BaseRec)
    end.

del(Interp) ->
    Key = forthic_interpreter:stack_pop(Interp),
    Container = forthic_interpreter:stack_pop(Interp),
    case is_map(Container) of
        true -> forthic_interpreter:stack_push(Interp, maps:remove(Key, Container));
        false -> forthic_interpreter:stack_push(Interp, Container)
    end.
