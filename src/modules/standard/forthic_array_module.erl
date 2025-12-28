-module(forthic_array_module).
-export([new/0, register_words/1]).

new() -> forthic_module:new("array", "").

register_words(Module) ->
    Words = [
        {"APPEND", fun append/1}, {"REVERSE", fun reverse/1}, {"UNIQUE", fun unique/1},
        {"LENGTH", fun length_op/1}, {"NTH", fun nth/1}, {"LAST", fun last/1},
        {"SLICE", fun slice/1}, {"TAKE", fun take/1}, {"DROP", fun drop/1},
        {"DIFFERENCE", fun difference/1}, {"INTERSECTION", fun intersection/1}, {"UNION", fun union/1},
        {"SORT", fun sort/1}, {"SHUFFLE", fun shuffle/1}, {"ROTATE", fun rotate/1},
        {"ZIP", fun zip/1}, {"ZIP-WITH", fun zip_with/1}, {"FLATTEN", fun flatten/1},
        {"MAP", fun map/1}, {"SELECT", fun select/1}, {"REDUCE", fun reduce/1},
        {"INDEX", fun index/1}, {"BY-FIELD", fun by_field/1},
        {"GROUP-BY-FIELD", fun group_by_field/1}, {"GROUP-BY", fun group_by/1}, {"GROUPS-OF", fun groups_of/1},
        {"FOREACH", fun foreach/1}, {"<REPEAT", fun repeat/1}, {"UNPACK", fun unpack/1}, {"KEY-OF", fun key_of/1}
    ],
    lists:foreach(fun({Name, Fun}) -> forthic_module:add_word(Module, Name, Fun) end, Words),
    Module.

append(Interp) ->
    Item = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, Arr ++ [Item]).

reverse(Interp) ->
    Arr = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, lists:reverse(Arr)).

unique(Interp) ->
    Arr = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, lists:usort(Arr)).

length_op(Interp) ->
    Arr = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, length(Arr)).

nth(Interp) ->
    Idx = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    Val = lists:nth(Idx + 1, Arr),  % Erlang lists are 1-indexed
    forthic_interpreter:stack_push(Interp, Val).

last(Interp) ->
    Arr = forthic_interpreter:stack_pop(Interp),
    Val = case Arr of [] -> null; _ -> lists:last(Arr) end,
    forthic_interpreter:stack_push(Interp, Val).

slice(Interp) ->
    End = forthic_interpreter:stack_pop(Interp),
    Start = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    Result = lists:sublist(Arr, Start + 1, End - Start),
    forthic_interpreter:stack_push(Interp, Result).

take(Interp) ->
    N = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, lists:sublist(Arr, N)).

drop(Interp) ->
    N = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, lists:nthtail(N, Arr)).

difference(Interp) ->
    Arr2 = forthic_interpreter:stack_pop(Interp),
    Arr1 = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, Arr1 -- Arr2).

intersection(Interp) ->
    Arr2 = forthic_interpreter:stack_pop(Interp),
    Arr1 = forthic_interpreter:stack_pop(Interp),
    Set2 = sets:from_list(Arr2),
    Result = lists:filter(fun(X) -> sets:is_element(X, Set2) end, Arr1),
    forthic_interpreter:stack_push(Interp, Result).

union(Interp) ->
    Arr2 = forthic_interpreter:stack_pop(Interp),
    Arr1 = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, lists:usort(Arr1 ++ Arr2)).

sort(Interp) ->
    Arr = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, lists:sort(Arr)).

shuffle(Interp) ->
    Arr = forthic_interpreter:stack_pop(Interp),
    Shuffled = [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- Arr])],
    forthic_interpreter:stack_push(Interp, Shuffled).

rotate(Interp) ->
    N = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    Len = length(Arr),
    Rot = (N rem Len + Len) rem Len,
    {Left, Right} = lists:split(Rot, Arr),
    forthic_interpreter:stack_push(Interp, Right ++ Left).

zip(Interp) ->
    Arr2 = forthic_interpreter:stack_pop(Interp),
    Arr1 = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, lists:zip(Arr1, Arr2)).

zip_with(Interp) ->
    Code = forthic_interpreter:stack_pop(Interp),
    Arr2 = forthic_interpreter:stack_pop(Interp),
    Arr1 = forthic_interpreter:stack_pop(Interp),
    case {is_list(Arr1), is_list(Arr2), is_binary(Code) orelse is_list(Code)} of
        {true, true, true} ->
            MinLen = min(length(Arr1), length(Arr2)),
            Pairs = lists:zip(lists:sublist(Arr1, MinLen), lists:sublist(Arr2, MinLen)),
            Result = lists:map(fun({A, B}) ->
                forthic_interpreter:stack_push(Interp, A),
                forthic_interpreter:stack_push(Interp, B),
                forthic_interpreter:run(Interp, to_string(Code)),
                forthic_interpreter:stack_pop(Interp)
            end, Pairs),
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, [])
    end.

flatten(Interp) ->
    Arr = forthic_interpreter:stack_pop(Interp),
    forthic_interpreter:stack_push(Interp, lists:flatten(Arr)).

map(Interp) ->
    Code = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case {is_list(Arr), is_binary(Code) orelse is_list(Code)} of
        {true, true} ->
            Result = lists:map(fun(Item) ->
                forthic_interpreter:stack_push(Interp, Item),
                forthic_interpreter:run(Interp, to_string(Code)),
                forthic_interpreter:stack_pop(Interp)
            end, Arr),
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, [])
    end.

select(Interp) ->
    Code = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case {is_list(Arr), is_binary(Code) orelse is_list(Code)} of
        {true, true} ->
            Result = lists:filter(fun(Item) ->
                forthic_interpreter:stack_push(Interp, Item),
                forthic_interpreter:run(Interp, to_string(Code)),
                Val = forthic_interpreter:stack_pop(Interp),
                is_truthy(Val)
            end, Arr),
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, [])
    end.

reduce(Interp) ->
    Code = forthic_interpreter:stack_pop(Interp),
    Initial = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case {is_list(Arr), is_binary(Code) orelse is_list(Code)} of
        {true, true} ->
            Result = lists:foldl(fun(Item, Acc) ->
                forthic_interpreter:stack_push(Interp, Acc),
                forthic_interpreter:stack_push(Interp, Item),
                forthic_interpreter:run(Interp, to_string(Code)),
                forthic_interpreter:stack_pop(Interp)
            end, Initial, Arr),
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, Initial)
    end.

index(Interp) ->
    Code = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case {is_list(Arr), is_binary(Code) orelse is_list(Code)} of
        {true, true} ->
            Result = lists:foldl(fun(Item, Acc) ->
                forthic_interpreter:stack_push(Interp, Item),
                forthic_interpreter:run(Interp, to_string(Code)),
                Key = to_string(forthic_interpreter:stack_pop(Interp)),
                maps:put(Key, Item, Acc)
            end, #{}, Arr),
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, #{})
    end.

by_field(Interp) ->
    Field = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case is_list(Arr) of
        true ->
            FieldName = to_string(Field),
            Result = lists:foldl(fun(Item, Acc) ->
                case is_map(Item) of
                    true ->
                        case maps:get(FieldName, Item, undefined) of
                            undefined -> Acc;
                            Val ->
                                Key = to_string(Val),
                                maps:put(Key, Item, Acc)
                        end;
                    false -> Acc
                end
            end, #{}, Arr),
            forthic_interpreter:stack_push(Interp, Result);
        false ->
            forthic_interpreter:stack_push(Interp, #{})
    end.

group_by_field(Interp) ->
    Field = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case is_list(Arr) of
        true ->
            FieldName = to_string(Field),
            Result = lists:foldl(fun(Item, Acc) ->
                case is_map(Item) of
                    true ->
                        case maps:get(FieldName, Item, undefined) of
                            undefined -> Acc;
                            Val ->
                                Key = to_string(Val),
                                Current = maps:get(Key, Acc, []),
                                maps:put(Key, Current ++ [Item], Acc)
                        end;
                    false -> Acc
                end
            end, #{}, Arr),
            forthic_interpreter:stack_push(Interp, Result);
        false ->
            forthic_interpreter:stack_push(Interp, #{})
    end.

group_by(Interp) ->
    Code = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case {is_list(Arr), is_binary(Code) orelse is_list(Code)} of
        {true, true} ->
            Result = lists:foldl(fun(Item, Acc) ->
                forthic_interpreter:stack_push(Interp, Item),
                forthic_interpreter:run(Interp, to_string(Code)),
                Key = to_string(forthic_interpreter:stack_pop(Interp)),
                Current = maps:get(Key, Acc, []),
                maps:put(Key, Current ++ [Item], Acc)
            end, #{}, Arr),
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, #{})
    end.

groups_of(Interp) ->
    N = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case is_list(Arr) of
        true when is_integer(N), N > 0 ->
            Result = group_into_chunks(Arr, N),
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, [])
    end.

foreach(Interp) ->
    Code = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case {is_list(Arr), is_binary(Code) orelse is_list(Code)} of
        {true, true} ->
            lists:foreach(fun(Item) ->
                forthic_interpreter:stack_push(Interp, Item),
                forthic_interpreter:run(Interp, to_string(Code)),
                _ = forthic_interpreter:stack_pop(Interp)
            end, Arr);
        _ ->
            ok
    end.

repeat(Interp) ->
    N = forthic_interpreter:stack_pop(Interp),
    Item = forthic_interpreter:stack_pop(Interp),
    case is_integer(N) of
        true when N > 0 ->
            Result = lists:duplicate(N, Item),
            forthic_interpreter:stack_push(Interp, Result);
        _ ->
            forthic_interpreter:stack_push(Interp, [])
    end.

unpack(Interp) ->
    Container = forthic_interpreter:stack_pop(Interp),
    case Container of
        null -> ok;
        _ when is_list(Container) ->
            lists:foreach(fun(Item) ->
                forthic_interpreter:stack_push(Interp, Item)
            end, Container);
        _ when is_map(Container) ->
            Keys = lists:sort(maps:keys(Container)),
            lists:foreach(fun(Key) ->
                forthic_interpreter:stack_push(Interp, maps:get(Key, Container))
            end, Keys);
        _ -> ok
    end.

key_of(Interp) ->
    Item = forthic_interpreter:stack_pop(Interp),
    Arr = forthic_interpreter:stack_pop(Interp),
    case is_list(Arr) of
        true ->
            case find_index(Item, Arr, 0) of
                not_found -> forthic_interpreter:stack_push(Interp, null);
                Index -> forthic_interpreter:stack_push(Interp, Index)
            end;
        false ->
            forthic_interpreter:stack_push(Interp, null)
    end.

%% Helper functions
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_list(V) -> V;
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_float(V) -> float_to_list(V);
to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(_) -> "".

is_truthy(null) -> false;
is_truthy(false) -> false;
is_truthy(0) -> false;
is_truthy(0.0) -> false;
is_truthy("") -> false;
is_truthy(_) -> true.

group_into_chunks([], _) -> [];
group_into_chunks(List, N) ->
    {Chunk, Rest} = lists:split(min(N, length(List)), List),
    [Chunk | group_into_chunks(Rest, N)].

find_index(_, [], _) -> not_found;
find_index(Item, [Item|_], Index) -> Index;
find_index(Item, [_|Rest], Index) -> find_index(Item, Rest, Index + 1).
