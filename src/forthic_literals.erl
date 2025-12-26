-module(forthic_literals).

-export([
    to_bool/1,
    to_float/1,
    to_int/1,
    to_time/1,
    to_literal_date/1,
    to_zoned_datetime/1
]).

%% ============================================================================
%% Literal Handler Type
%% ============================================================================

%% Literal handler: takes string, returns {ok, Value} or {error, Reason} or nil

%% ============================================================================
%% Boolean Literals
%% ============================================================================

%% Parse boolean literals: TRUE, FALSE
to_bool("TRUE") -> {ok, true};
to_bool("FALSE") -> {ok, false};
to_bool(_) -> nil.

%% ============================================================================
%% Numeric Literals
%% ============================================================================

%% Parse float literals: 3.14, -2.5, 0.0
%% Must contain a decimal point
to_float(Str) ->
    case string:str(Str, ".") of
        0 -> nil;
        _ ->
            case string:to_float(Str) of
                {Float, ""} -> {ok, Float};
                {error, _} -> nil;
                _ -> nil
            end
    end.

%% Parse integer literals: 42, -10, 0
%% Must not contain a decimal point
to_int(Str) ->
    case string:str(Str, ".") of
        0 ->
            case string:to_integer(Str) of
                {Int, ""} ->
                    % Verify it's actually an integer string (not "42abc")
                    case integer_to_list(Int) of
                        Str -> {ok, Int};
                        _ -> nil
                    end;
                {error, _} -> nil;
                _ -> nil
            end;
        _ -> nil
    end.

%% ============================================================================
%% Time Literals
%% ============================================================================

%% Parse time literals: 9:00, 11:30 PM, 22:15
to_time(Str) ->
    % Pattern: HH:MM or HH:MM AM/PM
    case re:run(Str, "^(\\d{1,2}):(\\d{2})(?:\\s*(AM|PM))?$", [{capture, all_but_first, list}]) of
        {match, [HourStr, MinuteStr]} ->
            parse_time_24hour(HourStr, MinuteStr);
        {match, [HourStr, MinuteStr, Meridiem]} ->
            parse_time_12hour(HourStr, MinuteStr, Meridiem);
        nomatch -> nil
    end.

parse_time_24hour(HourStr, MinuteStr) ->
    Hours = list_to_integer(HourStr),
    Minutes = list_to_integer(MinuteStr),
    case Hours > 23 orelse Minutes >= 60 of
        true -> nil;
        false ->
            % Return a time tuple: {{Year, Month, Day}, {Hour, Minute, Second}}
            {ok, {{0, 1, 1}, {Hours, Minutes, 0}}}
    end.

parse_time_12hour(HourStr, MinuteStr, Meridiem) ->
    Hours0 = list_to_integer(HourStr),
    Minutes = list_to_integer(MinuteStr),

    % Adjust for AM/PM
    Hours = case Meridiem of
        "PM" when Hours0 < 12 -> Hours0 + 12;
        "AM" when Hours0 =:= 12 -> 0;
        "AM" when Hours0 > 12 -> Hours0 - 12;
        _ -> Hours0
    end,

    case Hours > 23 orelse Minutes >= 60 of
        true -> nil;
        false ->
            % Return a time tuple
            {ok, {{0, 1, 1}, {Hours, Minutes, 0}}}
    end.

%% ============================================================================
%% Date Literals
%% ============================================================================

%% Create a date literal handler
%% Parses: 2020-06-05, YYYY-MM-DD (with wildcards)
to_literal_date(Timezone) ->
    fun(Str) ->
        % Pattern: YYYY-MM-DD or wildcards (YYYY, MM, DD)
        case re:run(Str, "^(\\d{4}|YYYY)-(\\d{2}|MM)-(\\d{2}|DD)$", [{capture, all_but_first, list}]) of
            {match, [YearStr, MonthStr, DayStr]} ->
                {{NowYear, NowMonth, NowDay}, _} = calendar:universal_time(),

                Year = case YearStr of
                    "YYYY" -> NowYear;
                    _ -> list_to_integer(YearStr)
                end,

                Month = case MonthStr of
                    "MM" -> NowMonth;
                    _ -> list_to_integer(MonthStr)
                end,

                Day = case DayStr of
                    "DD" -> NowDay;
                    _ -> list_to_integer(DayStr)
                end,

                % Return a date tuple
                {ok, {{Year, Month, Day}, {0, 0, 0}}};
            nomatch -> nil
        end
    end.

%% ============================================================================
%% ZonedDateTime Literals
%% ============================================================================

%% Create a zoned datetime literal handler
%% Parses:
%% - 2025-05-24T10:15:00[America/Los_Angeles] (IANA named timezone, RFC 9557)
%% - 2025-05-24T10:15:00-07:00[America/Los_Angeles] (offset + IANA timezone)
%% - 2025-05-24T10:15:00Z (UTC)
%% - 2025-05-24T10:15:00-05:00 (offset timezone)
%% - 2025-05-24T10:15:00 (uses interpreter's timezone)
to_zoned_datetime(Timezone) ->
    fun(Str) ->
        case string:str(Str, "T") of
            0 -> nil;
            _ ->
                % Handle IANA named timezone in bracket notation (RFC 9557)
                case string:str(Str, "[") of
                    0 ->
                        % No bracket notation
                        parse_datetime_no_bracket(Str, Timezone);
                    BracketStart ->
                        % Has bracket notation
                        parse_datetime_with_bracket(Str, BracketStart)
                end
        end
    end.

parse_datetime_with_bracket(Str, BracketStart) ->
    % Extract timezone name from brackets
    case string:str(Str, "]") of
        0 -> nil;
        BracketEnd ->
            TzName = string:substr(Str, BracketStart + 1, BracketEnd - BracketStart - 1),
            DtStr = string:substr(Str, 1, BracketStart - 1),

            % Try parsing the datetime part
            case parse_iso8601_datetime(DtStr) of
                {ok, DateTime} -> {ok, {DateTime, TzName}};
                nil -> nil
            end
    end.

parse_datetime_no_bracket(Str, Timezone) ->
    % Handle explicit UTC (Z suffix)
    case lists:last(Str) of
        $Z ->
            DtStr = string:substr(Str, 1, length(Str) - 1),
            case parse_iso8601_datetime(DtStr) of
                {ok, DateTime} -> {ok, {DateTime, "UTC"}};
                nil -> nil
            end;
        _ ->
            % Handle explicit timezone offset or no timezone
            case re:run(Str, "[+-]\\d{2}:\\d{2}$", []) of
                {match, _} ->
                    % Has offset
                    case parse_iso8601_datetime(Str) of
                        {ok, DateTime} -> {ok, {DateTime, "UTC"}};
                        nil -> nil
                    end;
                nomatch ->
                    % No timezone specified, use interpreter's timezone
                    case parse_iso8601_datetime(Str) of
                        {ok, DateTime} -> {ok, {DateTime, Timezone}};
                        nil -> nil
                    end
            end
    end.

parse_iso8601_datetime(Str) ->
    % Extract just the datetime portion: YYYY-MM-DDTHH:MM:SS
    % This ignores anything after (offset, brackets, etc.)
    case re:run(Str, "^(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2})",
                [{capture, all_but_first, list}]) of
        {match, [DtStr]} ->
            % Now parse the extracted datetime string
            case re:run(DtStr, "^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})",
                        [{capture, all_but_first, list}]) of
                {match, [YearStr, MonthStr, DayStr, HourStr, MinuteStr, SecondStr]} ->
                    Year = list_to_integer(YearStr),
                    Month = list_to_integer(MonthStr),
                    Day = list_to_integer(DayStr),
                    Hour = list_to_integer(HourStr),
                    Minute = list_to_integer(MinuteStr),
                    Second = list_to_integer(SecondStr),
                    {ok, {{Year, Month, Day}, {Hour, Minute, Second}}};
                nomatch -> nil
            end;
        nomatch -> nil
    end.
