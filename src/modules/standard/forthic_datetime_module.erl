-module(forthic_datetime_module).
-export([new/0, register_words/1]).

new() -> forthic_module:new("datetime", "").

register_words(Module) ->
    Words = [
        {">DATE", fun to_date/1}, {">DATETIME", fun to_datetime/1},
        {"DATE>STR", fun date_to_str/1}, {"DATETIME>STR", fun datetime_to_str/1},
        {"ADD-DAYS", fun add_days/1}, {"ADD-HOURS", fun add_hours/1},
        {"ADD-MINUTES", fun add_minutes/1}, {"ADD-SECONDS", fun add_seconds/1},
        {"DIFF-DAYS", fun diff_days/1}, {"DIFF-HOURS", fun diff_hours/1},
        {"DIFF-MINUTES", fun diff_minutes/1}, {"DIFF-SECONDS", fun diff_seconds/1},
        {"NOW", fun now_op/1}, {"TODAY", fun today/1},
        {"YEAR", fun year/1}, {"MONTH", fun month/1}, {"DAY", fun day/1},
        {"HOUR", fun hour/1}, {"MINUTE", fun minute/1}, {"SECOND", fun second/1}
    ],
    lists:foreach(fun({Name, Fun}) -> forthic_module:add_word(Module, Name, Fun) end, Words),
    Module.

to_date(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    Str = to_string(Val),
    case parse_datetime(Str) of
        {ok, DateTime} ->
            {{Y, M, D}, _} = DateTime,
            DateOnly = {{Y, M, D}, {0, 0, 0}},
            forthic_interpreter:stack_push(Interp, DateOnly);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

to_datetime(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    Str = to_string(Val),
    case parse_datetime(Str) of
        {ok, DateTime} -> forthic_interpreter:stack_push(Interp, DateTime);
        _ -> forthic_interpreter:stack_push(Interp, null)
    end.

date_to_str(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {{Y, M, D}, _} ->
            Str = io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D]),
            forthic_interpreter:stack_push(Interp, lists:flatten(Str));
        _ ->
            forthic_interpreter:stack_push(Interp, "")
    end.

datetime_to_str(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {{Y, M, D}, {H, Min, S}} ->
            Str = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
                                [Y, M, D, H, Min, S]),
            forthic_interpreter:stack_push(Interp, lists:flatten(Str));
        _ ->
            forthic_interpreter:stack_push(Interp, "")
    end.

add_days(Interp) ->
    Days = forthic_interpreter:stack_pop(Interp),
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {{_, _, _}, {_, _, _}} = DateTime ->
            Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
            DaysNum = to_number(Days),
            NewSeconds = Seconds + trunc(DaysNum * 86400),
            NewDateTime = calendar:gregorian_seconds_to_datetime(NewSeconds),
            forthic_interpreter:stack_push(Interp, NewDateTime);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

add_hours(Interp) ->
    Hours = forthic_interpreter:stack_pop(Interp),
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {{_, _, _}, {_, _, _}} = DateTime ->
            Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
            HoursNum = to_number(Hours),
            NewSeconds = Seconds + trunc(HoursNum * 3600),
            NewDateTime = calendar:gregorian_seconds_to_datetime(NewSeconds),
            forthic_interpreter:stack_push(Interp, NewDateTime);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

add_minutes(Interp) ->
    Minutes = forthic_interpreter:stack_pop(Interp),
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {{_, _, _}, {_, _, _}} = DateTime ->
            Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
            MinutesNum = to_number(Minutes),
            NewSeconds = Seconds + trunc(MinutesNum * 60),
            NewDateTime = calendar:gregorian_seconds_to_datetime(NewSeconds),
            forthic_interpreter:stack_push(Interp, NewDateTime);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

add_seconds(Interp) ->
    Secs = forthic_interpreter:stack_pop(Interp),
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {{_, _, _}, {_, _, _}} = DateTime ->
            Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
            SecsNum = to_number(Secs),
            NewSeconds = Seconds + trunc(SecsNum),
            NewDateTime = calendar:gregorian_seconds_to_datetime(NewSeconds),
            forthic_interpreter:stack_push(Interp, NewDateTime);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

diff_days(Interp) ->
    End = forthic_interpreter:stack_pop(Interp),
    Start = forthic_interpreter:stack_pop(Interp),
    case {Start, End} of
        {{{_, _, _}, {_, _, _}} = DT1, {{_, _, _}, {_, _, _}} = DT2} ->
            Sec1 = calendar:datetime_to_gregorian_seconds(DT1),
            Sec2 = calendar:datetime_to_gregorian_seconds(DT2),
            Diff = (Sec2 - Sec1) / 86400.0,
            forthic_interpreter:stack_push(Interp, Diff);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

diff_hours(Interp) ->
    End = forthic_interpreter:stack_pop(Interp),
    Start = forthic_interpreter:stack_pop(Interp),
    case {Start, End} of
        {{{_, _, _}, {_, _, _}} = DT1, {{_, _, _}, {_, _, _}} = DT2} ->
            Sec1 = calendar:datetime_to_gregorian_seconds(DT1),
            Sec2 = calendar:datetime_to_gregorian_seconds(DT2),
            Diff = (Sec2 - Sec1) / 3600.0,
            forthic_interpreter:stack_push(Interp, Diff);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

diff_minutes(Interp) ->
    End = forthic_interpreter:stack_pop(Interp),
    Start = forthic_interpreter:stack_pop(Interp),
    case {Start, End} of
        {{{_, _, _}, {_, _, _}} = DT1, {{_, _, _}, {_, _, _}} = DT2} ->
            Sec1 = calendar:datetime_to_gregorian_seconds(DT1),
            Sec2 = calendar:datetime_to_gregorian_seconds(DT2),
            Diff = (Sec2 - Sec1) / 60.0,
            forthic_interpreter:stack_push(Interp, Diff);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

diff_seconds(Interp) ->
    End = forthic_interpreter:stack_pop(Interp),
    Start = forthic_interpreter:stack_pop(Interp),
    case {Start, End} of
        {{{_, _, _}, {_, _, _}} = DT1, {{_, _, _}, {_, _, _}} = DT2} ->
            Sec1 = calendar:datetime_to_gregorian_seconds(DT1),
            Sec2 = calendar:datetime_to_gregorian_seconds(DT2),
            Diff = float(Sec2 - Sec1),
            forthic_interpreter:stack_push(Interp, Diff);
        _ ->
            forthic_interpreter:stack_push(Interp, null)
    end.

now_op(Interp) ->
    DateTime = calendar:now_to_datetime(erlang:timestamp()),
    forthic_interpreter:stack_push(Interp, DateTime).

today(Interp) ->
    {{Y, M, D}, _} = calendar:now_to_datetime(erlang:timestamp()),
    Today = {{Y, M, D}, {0, 0, 0}},
    forthic_interpreter:stack_push(Interp, Today).

year(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {{Y, _, _}, _} -> forthic_interpreter:stack_push(Interp, Y);
        _ -> forthic_interpreter:stack_push(Interp, null)
    end.

month(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {{_, M, _}, _} -> forthic_interpreter:stack_push(Interp, M);
        _ -> forthic_interpreter:stack_push(Interp, null)
    end.

day(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {{_, _, D}, _} -> forthic_interpreter:stack_push(Interp, D);
        _ -> forthic_interpreter:stack_push(Interp, null)
    end.

hour(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {_, {H, _, _}} -> forthic_interpreter:stack_push(Interp, H);
        _ -> forthic_interpreter:stack_push(Interp, null)
    end.

minute(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {_, {_, M, _}} -> forthic_interpreter:stack_push(Interp, M);
        _ -> forthic_interpreter:stack_push(Interp, null)
    end.

second(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    case Val of
        {_, {_, _, S}} -> forthic_interpreter:stack_push(Interp, S);
        _ -> forthic_interpreter:stack_push(Interp, null)
    end.

%% Helper functions
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_list(V) -> V;
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_float(V) -> float_to_list(V);
to_string(_) -> "".

to_number(V) when is_number(V) -> float(V);
to_number(V) when is_integer(V) -> float(V);
to_number(_) -> 0.0.

parse_datetime(Str) ->
    %% Simple ISO8601-like parsing: "2024-12-26" or "2024-12-26 10:30:00"
    case string:split(Str, " ", all) of
        [DateStr] ->
            parse_date_part(DateStr);
        [DateStr, TimeStr] ->
            case {parse_date_part(DateStr), parse_time_part(TimeStr)} of
                {{ok, Date}, {ok, Time}} -> {ok, {Date, Time}};
                _ -> error
            end;
        _ -> error
    end.

parse_date_part(Str) ->
    case string:split(Str, "-", all) of
        [YStr, MStr, DStr] ->
            try
                Y = list_to_integer(YStr),
                M = list_to_integer(MStr),
                D = list_to_integer(DStr),
                {ok, {Y, M, D}}
            catch _:_ -> error
            end;
        _ -> error
    end.

parse_time_part(Str) ->
    case string:split(Str, ":", all) of
        [HStr, MinStr, SStr] ->
            try
                H = list_to_integer(HStr),
                Min = list_to_integer(MinStr),
                S = list_to_integer(SStr),
                {ok, {H, Min, S}}
            catch _:_ -> error
            end;
        _ -> error
    end.
