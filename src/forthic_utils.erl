-module(forthic_utils).

-export([
    % Type conversion
    to_int/1,
    to_float/1,
    to_string/1,

    % String utilities
    trim/1,
    split/2,
    join/2,
    replace/3,

    % Date/time utilities
    parse_date/1,
    parse_time/1,
    format_date/1,
    format_time/1,
    format_datetime/1,
    parse_datetime/1,
    date_to_int/1,
    int_to_date/1
]).

%% ============================================================================
%% Type Conversion Utilities
%% ============================================================================

to_int(V) when is_integer(V) -> {ok, V};
to_int(V) when is_float(V) -> {ok, trunc(V)};
to_int(V) when is_binary(V) ->
    case string:to_integer(binary_to_list(V)) of
        {Int, ""} -> {ok, Int};
        {error, _} -> {error, "cannot convert to int"}
    end;
to_int(V) when is_list(V) ->
    case string:to_integer(V) of
        {Int, ""} -> {ok, Int};
        {error, _} -> {error, "cannot convert to int"}
    end;
to_int(_) -> {error, "cannot convert to int"}.

to_float(V) when is_float(V) -> {ok, V};
to_float(V) when is_integer(V) -> {ok, float(V)};
to_float(V) when is_binary(V) ->
    case string:to_float(binary_to_list(V)) of
        {Float, ""} -> {ok, Float};
        {error, _} ->
            % Try as integer first
            case to_int(V) of
                {ok, Int} -> {ok, float(Int)};
                {error, _} -> {error, "cannot convert to float"}
            end
    end;
to_float(V) when is_list(V) ->
    case string:to_float(V) of
        {Float, ""} -> {ok, Float};
        {error, _} ->
            % Try as integer first
            case to_int(V) of
                {ok, Int} -> {ok, float(Int)};
                {error, _} -> {error, "cannot convert to float"}
            end
    end;
to_float(_) -> {error, "cannot convert to float"}.

to_string(null) -> "null";
to_string(undefined) -> "null";
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_list(V) -> V;
to_string(true) -> "true";
to_string(false) -> "false";
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_float(V) -> float_to_list(V);
to_string(V) -> io_lib:format("~p", [V]).

%% ============================================================================
%% String Utilities
%% ============================================================================

trim(S) when is_binary(S) ->
    string:trim(S);
trim(S) when is_list(S) ->
    string:trim(S);
trim(S) -> S.

split(S, "") when is_binary(S) ->
    % Split into individual characters
    [<<C>> || <<C>> <= S];
split(S, "") when is_list(S) ->
    % Split into individual characters
    [[C] || C <- S];
split(S, Sep) when is_binary(S), is_binary(Sep) ->
    binary:split(S, Sep, [global]);
split(S, Sep) when is_binary(S), is_list(Sep) ->
    binary:split(S, list_to_binary(Sep), [global]);
split(S, Sep) when is_list(S), is_list(Sep) ->
    string:split(S, Sep, all);
split(S, Sep) when is_list(S), is_binary(Sep) ->
    string:split(S, binary_to_list(Sep), all).

join(Parts, Sep) when is_list(Parts), is_binary(Sep) ->
    lists:join(Sep, Parts);
join(Parts, Sep) when is_list(Parts), is_list(Sep) ->
    lists:join(Sep, Parts).

replace(S, Old, New) when is_binary(S), is_binary(Old), is_binary(New) ->
    binary:replace(S, Old, New, [global]);
replace(S, Old, New) when is_list(S), is_list(Old), is_list(New) ->
    string:replace(S, Old, New, all);
replace(S, Old, New) when is_binary(S) ->
    replace(S, ensure_binary(Old), ensure_binary(New));
replace(S, Old, New) when is_list(S) ->
    replace(S, ensure_list(Old), ensure_list(New)).

%% ============================================================================
%% Date/Time Utilities
%% ============================================================================

% Parse date in YYYY-MM-DD format
% Supports wildcards: YYYY-**-**, ****-MM-**, ****-**-DD
parse_date(S) when is_binary(S) ->
    parse_date(binary_to_list(S));
parse_date(S) when is_list(S) ->
    case string:find(S, "*") of
        nomatch ->
            % Standard parsing
            case string:split(S, "-", all) of
                [Y, M, D] ->
                    try
                        Year = list_to_integer(Y),
                        Month = list_to_integer(M),
                        Day = list_to_integer(D),
                        {ok, {{Year, Month, Day}, {0, 0, 0}}}
                    catch
                        _:_ -> {error, "invalid date format"}
                    end;
                _ -> {error, "invalid date format"}
            end;
        _ ->
            % Parse with wildcards
            parse_date_with_wildcards(S)
    end.

parse_date_with_wildcards(S) ->
    {{NowYear, NowMonth, NowDay}, _} = calendar:universal_time(),
    case string:split(S, "-", all) of
        [Y, M, D] ->
            try
                Year = case Y of
                    "****" -> NowYear;
                    _ -> list_to_integer(Y)
                end,
                Month = case M of
                    "**" -> NowMonth;
                    _ -> list_to_integer(M)
                end,
                Day = case D of
                    "**" -> NowDay;
                    _ -> list_to_integer(D)
                end,
                {ok, {{Year, Month, Day}, {0, 0, 0}}}
            catch
                _:_ -> {error, "invalid date format"}
            end;
        _ -> {error, "invalid date format"}
    end.

% Parse time in HH:MM or HH:MM:SS format
% Also supports 12-hour format with AM/PM (e.g., "2:30 PM")
parse_time(S) when is_binary(S) ->
    parse_time(binary_to_list(S));
parse_time(S) when is_list(S) ->
    Trimmed = string:trim(S),
    % Check for AM/PM format
    case re:run(Trimmed, "^(\\d{1,2}):(\\d{2})\\s*(AM|PM)$", [{capture, all_but_first, list}]) of
        {match, [HourStr, MinuteStr, Meridiem]} ->
            Hour = list_to_integer(HourStr),
            Minute = list_to_integer(MinuteStr),
            Hour24 = case Meridiem of
                "PM" when Hour < 12 -> Hour + 12;
                "AM" when Hour =:= 12 -> 0;
                _ -> Hour
            end,
            {ok, {{0, 1, 1}, {Hour24, Minute, 0}}};
        nomatch ->
            % Try HH:MM:SS or HH:MM format
            case string:split(Trimmed, ":", all) of
                [H, M, S] ->
                    try
                        Hour = list_to_integer(H),
                        Minute = list_to_integer(M),
                        Second = list_to_integer(S),
                        {ok, {{0, 1, 1}, {Hour, Minute, Second}}}
                    catch
                        _:_ -> {error, "invalid time format"}
                    end;
                [H, M] ->
                    try
                        Hour = list_to_integer(H),
                        Minute = list_to_integer(M),
                        {ok, {{0, 1, 1}, {Hour, Minute, 0}}}
                    catch
                        _:_ -> {error, "invalid time format"}
                    end;
                _ -> {error, "invalid time format"}
            end
    end.

% Format date as YYYY-MM-DD
format_date({{Year, Month, Day}, _}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day]).

% Format time as HH:MM
format_time({_, {Hour, Minute, _}}) ->
    io_lib:format("~2..0w:~2..0w", [Hour, Minute]).

% Format datetime as RFC3339
format_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
                  [Year, Month, Day, Hour, Minute, Second]).

% Parse RFC3339 datetime string
parse_datetime(S) when is_binary(S) ->
    parse_datetime(binary_to_list(S));
parse_datetime(S) when is_list(S) ->
    % Simple RFC3339 parsing (YYYY-MM-DDTHH:MM:SSZ)
    case re:run(S, "^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})Z?$",
                [{capture, all_but_first, list}]) of
        {match, [Y, M, D, H, Min, Sec]} ->
            try
                Year = list_to_integer(Y),
                Month = list_to_integer(M),
                Day = list_to_integer(D),
                Hour = list_to_integer(H),
                Minute = list_to_integer(Min),
                Second = list_to_integer(Sec),
                {ok, {{Year, Month, Day}, {Hour, Minute, Second}}}
            catch
                _:_ -> {error, "invalid datetime format"}
            end;
        nomatch -> {error, "invalid datetime format"}
    end.

% Convert date to YYYYMMDD integer format
date_to_int({{Year, Month, Day}, _}) ->
    Year * 10000 + Month * 100 + Day.

% Convert YYYYMMDD integer to date
int_to_date(N) when is_integer(N) ->
    Year = N div 10000,
    Month = (N rem 10000) div 100,
    Day = N rem 100,
    {{Year, Month, Day}, {0, 0, 0}}.

%% ============================================================================
%% Private Helper Functions
%% ============================================================================

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_list(V) -> list_to_binary(V);
ensure_binary(V) -> term_to_binary(V).

ensure_list(V) when is_list(V) -> V;
ensure_list(V) when is_binary(V) -> binary_to_list(V);
ensure_list(V) -> to_string(V).
