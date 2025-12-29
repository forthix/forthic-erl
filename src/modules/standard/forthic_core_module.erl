-module(forthic_core_module).
-compile({no_auto_import,[get/1]}).
-export([new/0, register_words/1]).

new() ->
    forthic_module:new("core", "").

register_words(Module) ->
    % Stack operations
    M1 = forthic_module:add_word(Module, forthic_word:new_module_word("POP", fun pop/1, undefined)),
    M2 = forthic_module:add_word(M1, forthic_word:new_module_word("DUP", fun dup/1, undefined)),
    M3 = forthic_module:add_word(M2, forthic_word:new_module_word("SWAP", fun swap/1, undefined)),

    % Variable operations
    M4 = forthic_module:add_word(M3, forthic_word:new_module_word("VARIABLES", fun variables/1, undefined)),
    M5 = forthic_module:add_word(M4, forthic_word:new_module_word("!", fun set/1, undefined)),
    M6 = forthic_module:add_word(M5, forthic_word:new_module_word("@", fun get/1, undefined)),
    M7 = forthic_module:add_word(M6, forthic_word:new_module_word("!@", fun set_get/1, undefined)),

    % Module operations
    M8 = forthic_module:add_word(M7, forthic_word:new_module_word("EXPORT", fun export_word/1, undefined)),
    M9 = forthic_module:add_word(M8, forthic_word:new_module_word("USE-MODULES", fun use_modules/1, undefined)),

    % Execution
    M10 = forthic_module:add_word(M9, forthic_word:new_module_word("INTERPRET", fun interpret/1, undefined)),

    % Control flow
    M11 = forthic_module:add_word(M10, forthic_word:new_module_word("IDENTITY", fun identity/1, undefined)),
    M12 = forthic_module:add_word(M11, forthic_word:new_module_word("NOP", fun nop/1, undefined)),
    M13 = forthic_module:add_word(M12, forthic_word:new_module_word("NULL", fun null_word/1, undefined)),
    M14 = forthic_module:add_word(M13, forthic_word:new_module_word("ARRAY?", fun array_check/1, undefined)),
    M15 = forthic_module:add_word(M14, forthic_word:new_module_word("DEFAULT", fun default_word/1, undefined)),
    M16 = forthic_module:add_word(M15, forthic_word:new_module_word("*DEFAULT", fun default_star/1, undefined)),

    % Options
    M17 = forthic_module:add_word(M16, forthic_word:new_module_word("~>", fun to_options/1, undefined)),

    % Profiling (placeholders)
    M18 = forthic_module:add_word(M17, forthic_word:new_module_word("PROFILE-START", fun profile_start/1, undefined)),
    M19 = forthic_module:add_word(M18, forthic_word:new_module_word("PROFILE-END", fun profile_end/1, undefined)),
    M20 = forthic_module:add_word(M19, forthic_word:new_module_word("PROFILE-TIMESTAMP", fun profile_timestamp/1, undefined)),
    M21 = forthic_module:add_word(M20, forthic_word:new_module_word("PROFILE-DATA", fun profile_data/1, undefined)),

    % Logging (placeholders)
    M22 = forthic_module:add_word(M21, forthic_word:new_module_word("START-LOG", fun start_log/1, undefined)),
    M23 = forthic_module:add_word(M22, forthic_word:new_module_word("END-LOG", fun end_log/1, undefined)),

    % String operations
    M24 = forthic_module:add_word(M23, forthic_word:new_module_word("INTERPOLATE", fun interpolate/1, undefined)),
    M25 = forthic_module:add_word(M24, forthic_word:new_module_word("PRINT", fun print/1, undefined)),

    % Debug operations
    M26 = forthic_module:add_word(M25, forthic_word:new_module_word("PEEK!", fun peek/1, undefined)),
    M27 = forthic_module:add_word(M26, forthic_word:new_module_word("STACK!", fun stack_debug/1, undefined)),

    M27.

%% ========================================
%% Helper Functions
%% ========================================

%% Check if value is a Variable record
is_variable_record(Value) ->
    is_tuple(Value) andalso tuple_size(Value) == 4 andalso element(1, Value) == variable.

%% Get or create a variable, validating the name (ETS-based)
%% Returns {ok, UpdatedInterp} or {error, Reason}
get_or_create_variable(Interp, Name) when is_binary(Name) ->
    get_or_create_variable(Interp, binary_to_list(Name));
get_or_create_variable(Interp, Name) when is_list(Name) ->
    % Validate variable name - no __ prefix allowed
    case string:prefix(Name, "__") of
        nomatch ->
            CurModule = forthic_interpreter:cur_module(Interp),
            ModuleName = forthic_module:get_name(CurModule),
            % Check if variable exists in module
            case forthic_module:get_variable(CurModule, Name) of
                {ok, _Variable} ->
                    % Variable already exists in module
                    {ok, Interp};
                not_found ->
                    % Create the variable with null value in ETS
                    forthic_interpreter:create_variable(Interp, ModuleName, Name, null),
                    % Also add to module's variables map
                    UpdatedModule = forthic_module:add_variable(CurModule, Name, null),
                    % Update interpreter with new module
                    UpdatedInterp = forthic_interpreter:set_app_module(Interp, UpdatedModule),
                    {ok, UpdatedInterp}
            end;
        _ ->
            {error, {invalid_variable_name, Name}}
    end.

%% ========================================
%% Stack Operations
%% ========================================

pop(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, _Value, Interp2} -> {ok, Interp2};
        {error, _} = Error -> Error
    end.

dup(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, Value, Interp2} ->
            {ok, Interp3} = forthic_interpreter:stack_push(Interp2, Value),
            forthic_interpreter:stack_push(Interp3, Value);
        {error, _} = Error ->
            Error
    end.

swap(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, B, Interp2} ->
            case forthic_interpreter:stack_pop(Interp2) of
                {ok, A, Interp3} ->
                    {ok, Interp4} = forthic_interpreter:stack_push(Interp3, B),
                    forthic_interpreter:stack_push(Interp4, A);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% ========================================
%% Variable Operations
%% ========================================

variables(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, VarNames, Interp2} when is_list(VarNames) ->
            CurModule = forthic_interpreter:cur_module(Interp2),
            ModuleName = forthic_module:get_name(CurModule),
            % Create each variable in ETS and module
            UpdatedModule = lists:foldl(fun(VarName, ModAcc) ->
                Name = to_string(VarName),
                % Validate variable name
                case string:prefix(Name, "__") of
                    nomatch ->
                        % Create variable in ETS
                        forthic_interpreter:create_variable(Interp2, ModuleName, Name, null),
                        % Also add to module's variables map
                        forthic_module:add_variable(ModAcc, Name, null);
                    _ ->
                        % Invalid name, skip
                        ModAcc
                end
            end, CurModule, VarNames),
            % Update the interpreter's app module
            Interp3 = forthic_interpreter:set_app_module(Interp2, UpdatedModule),
            {ok, Interp3};
        {ok, _, Interp2} ->
            {ok, Interp2};
        {error, _} = Error ->
            Error
    end.

set(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, Variable, Interp2} ->
            case forthic_interpreter:stack_pop(Interp2) of
                {ok, Value, Interp3} ->
                    % Check if Variable is a record or string
                    case is_variable_record(Variable) of
                        true ->
                            % Variable record: use forthic_variable:set_value/3
                            forthic_variable:set_value(Variable, Value, Interp3),
                            {ok, Interp3};
                        false ->
                            % String name: auto-create variable
                            VarName = to_string(Variable),
                            case get_or_create_variable(Interp3, VarName) of
                                {ok, Interp4} ->
                                    CurModule = forthic_interpreter:cur_module(Interp4),
                                    ModuleName = forthic_module:get_name(CurModule),
                                    % Set variable value in ETS
                                    forthic_interpreter:set_variable(Interp4, ModuleName, VarName, Value),
                                    {ok, Interp4};
                                {error, _} = Error ->
                                    Error
                            end
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

get(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, Variable, Interp2} ->
            % Check if Variable is a record or string
            case is_variable_record(Variable) of
                true ->
                    % Variable record: use forthic_variable:get_value/2
                    Value = forthic_variable:get_value(Variable, Interp2),
                    forthic_interpreter:stack_push(Interp2, Value);
                false ->
                    % String name: auto-create variable
                    VarName = to_string(Variable),
                    case get_or_create_variable(Interp2, VarName) of
                        {ok, Interp3} ->
                            CurModule = forthic_interpreter:cur_module(Interp3),
                            ModuleName = forthic_module:get_name(CurModule),
                            % Get variable value from ETS
                            case forthic_interpreter:get_variable(Interp3, ModuleName, VarName) of
                                {ok, Value} ->
                                    forthic_interpreter:stack_push(Interp3, Value);
                                not_found ->
                                    % Should not happen since we auto-create, but return null
                                    forthic_interpreter:stack_push(Interp3, null)
                            end;
                        {error, _} = Error ->
                            Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.

set_get(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, Variable, Interp2} ->
            case forthic_interpreter:stack_pop(Interp2) of
                {ok, Value, Interp3} ->
                    % Check if Variable is a record or string
                    case is_variable_record(Variable) of
                        true ->
                            % Variable record: use forthic_variable:set_value/3
                            forthic_variable:set_value(Variable, Value, Interp3),
                            % Push value back to stack
                            forthic_interpreter:stack_push(Interp3, Value);
                        false ->
                            % String name: auto-create variable
                            VarName = to_string(Variable),
                            case get_or_create_variable(Interp3, VarName) of
                                {ok, Interp4} ->
                                    CurModule = forthic_interpreter:cur_module(Interp4),
                                    ModuleName = forthic_module:get_name(CurModule),
                                    % Set variable value in ETS
                                    forthic_interpreter:set_variable(Interp4, ModuleName, VarName, Value),
                                    % Push value back to stack
                                    forthic_interpreter:stack_push(Interp4, Value);
                                {error, _} = Error ->
                                    Error
                            end
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% ========================================
%% Module Operations
%% ========================================

export_word(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, Names, Interp2} when is_list(Names) ->
            StrNames = lists:map(fun to_string/1, Names),
            CurModule = forthic_interpreter:cur_module(Interp2),
            _UpdatedModule = forthic_module:add_exportable(CurModule, StrNames),
            % Note: Need to update module in interpreter
            {ok, Interp2};
        {ok, _, Interp2} ->
            {ok, Interp2};
        {error, _} = Error ->
            Error
    end.

use_modules(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, null, Interp2} ->
            {ok, Interp2};
        {ok, Names, Interp2} when is_list(Names) ->
            % TODO: Implement module import logic
            {ok, Interp2};
        {ok, _, Interp2} ->
            {ok, Interp2};
        {error, _} = Error ->
            Error
    end.

%% ========================================
%% Execution
%% ========================================

interpret(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, null, Interp2} ->
            {ok, Interp2};
        {ok, Code, Interp2} when is_list(Code); is_binary(Code) ->
            CodeStr = to_string(Code),
            forthic_interpreter:run(Interp2, CodeStr);
        {ok, _, Interp2} ->
            {ok, Interp2};
        {error, _} = Error ->
            Error
    end.

%% ========================================
%% Control Flow
%% ========================================

identity(Interp) ->
    {ok, Interp}.

nop(Interp) ->
    {ok, Interp}.

null_word(Interp) ->
    forthic_interpreter:stack_push(Interp, null).

array_check(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, Value, Interp2} ->
            IsArray = is_list(Value),
            forthic_interpreter:stack_push(Interp2, IsArray);
        {error, _} = Error ->
            Error
    end.

default_word(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, DefaultValue, Interp2} ->
            case forthic_interpreter:stack_pop(Interp2) of
                {ok, Value, Interp3} ->
                    Result = case Value of
                        null -> DefaultValue;
                        "" -> DefaultValue;
                        _ -> Value
                    end,
                    forthic_interpreter:stack_push(Interp3, Result);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

default_star(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, DefaultForthic, Interp2} ->
            case forthic_interpreter:stack_pop(Interp2) of
                {ok, Value, Interp3} ->
                    case Value of
                        null ->
                            CodeStr = to_string(DefaultForthic),
                            case forthic_interpreter:run(Interp3, CodeStr) of
                                {ok, Interp4} -> {ok, Interp4};
                                {error, _} = Error -> Error
                            end;
                        "" ->
                            CodeStr = to_string(DefaultForthic),
                            case forthic_interpreter:run(Interp3, CodeStr) of
                                {ok, Interp4} -> {ok, Interp4};
                                {error, _} = Error -> Error
                            end;
                        _ ->
                            forthic_interpreter:stack_push(Interp3, Value)
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% ========================================
%% Options
%% ========================================

to_options(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, Array, Interp2} ->
            case forthic_word_options:from_array(Array) of
                {ok, Opts} ->
                    forthic_interpreter:stack_push(Interp2, Opts);
                {error, Reason} ->
                    {error, forthic_errors:new_forthic_error(io_lib:format("Invalid options: ~p", [Reason]))}
            end;
        {error, _} = Error ->
            Error
    end.

%% ========================================
%% Profiling (Placeholder implementations)
%% ========================================

profile_start(Interp) ->
    % TODO: Implement profiling
    {ok, Interp}.

profile_end(Interp) ->
    % TODO: Implement profiling
    {ok, Interp}.

profile_timestamp(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, _Label, Interp2} ->
            % TODO: Implement profiling
            {ok, Interp2};
        {error, _} = Error ->
            Error
    end.

profile_data(Interp) ->
    % TODO: Implement profiling
    Result = #{
        <<"word_counts">> => [],
        <<"timestamps">> => []
    },
    forthic_interpreter:stack_push(Interp, Result).

%% ========================================
%% Logging (Placeholder implementations)
%% ========================================

start_log(Interp) ->
    % TODO: Implement logging
    {ok, Interp}.

end_log(Interp) ->
    % TODO: Implement logging
    {ok, Interp}.

%% ========================================
%% String Operations
%% ========================================

interpolate(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, TopVal, Interp2} ->
            % Check if we have options
            case is_word_options(TopVal) of
                true ->
                    Opts = TopVal,
                    case forthic_interpreter:stack_pop(Interp2) of
                        {ok, Str, Interp3} ->
                            StrVal = to_string(Str),
                            Separator = get_option(Opts, "separator", ", "),
                            NullText = get_option(Opts, "null_text", "null"),
                            UseJSON = get_option(Opts, "json", false),
                            Result = interpolate_string(Interp3, StrVal, Separator, NullText, UseJSON),
                            forthic_interpreter:stack_push(Interp3, Result);
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    Str = to_string(TopVal),
                    Result = interpolate_string(Interp2, Str, ", ", "null", false),
                    forthic_interpreter:stack_push(Interp2, Result)
            end;
        {error, _} = Error ->
            Error
    end.

print(Interp) ->
    case forthic_interpreter:stack_pop(Interp) of
        {ok, TopVal, Interp2} ->
            % Check if we have options
            case is_word_options(TopVal) of
                true ->
                    Opts = TopVal,
                    case forthic_interpreter:stack_pop(Interp2) of
                        {ok, Value, Interp3} ->
                            Separator = get_option(Opts, "separator", ", "),
                            NullText = get_option(Opts, "null_text", "null"),
                            UseJSON = get_option(Opts, "json", false),
                            Result = case is_list(Value) orelse is_binary(Value) of
                                true when not is_map(hd(Value)) -> % String check
                                    Str = to_string(Value),
                                    interpolate_string(Interp3, Str, Separator, NullText, UseJSON);
                                _ ->
                                    value_to_string(Value, Separator, NullText, UseJSON)
                            end,
                            io:format("~s~n", [Result]),
                            {ok, Interp3};
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    Value = TopVal,
                    Result = case is_list(Value) orelse is_binary(Value) of
                        true -> % Might be a string
                            Str = to_string(Value),
                            case is_string(Value) of
                                true -> interpolate_string(Interp2, Str, ", ", "null", false);
                                false -> value_to_string(Value, ", ", "null", false)
                            end;
                        _ ->
                            value_to_string(Value, ", ", "null", false)
                    end,
                    io:format("~s~n", [Result]),
                    {ok, Interp2}
            end;
        {error, _} = Error ->
            Error
    end.

interpolate_string(Interp, Str, Separator, NullText, UseJSON) ->
    % Handle escape sequences by replacing \. with a temporary placeholder
    % Use a unique string that's unlikely to appear in normal text
    Placeholder = "<<FORTHIC_ESCAPED_DOT>>",
    Escaped = re:replace(Str, "\\\\\\.", Placeholder, [global, {return, list}]),

    % Manually find and replace .variable patterns
    CurModule = forthic_interpreter:cur_module(Interp),
    ModuleName = forthic_module:get_name(CurModule),

    % Process the string character by character (start with AtStart = true)
    Interpolated = interpolate_scan(Escaped, Interp, ModuleName, Separator, NullText, UseJSON, [], true),

    % Restore escaped dots
    re:replace(Interpolated, Placeholder, ".", [global, {return, list}]).

% Scan through string and interpolate variables
interpolate_scan([], _Interp, _ModuleName, _Separator, _NullText, _UseJSON, Acc, _AtStart) ->
    lists:reverse(Acc);
interpolate_scan([$. | Rest], Interp, ModuleName, Separator, NullText, UseJSON, Acc, true) ->
    % Dot at start of string or after whitespace - check if it's a variable
    case scan_var_name(Rest, []) of
        {VarName, Remaining} when VarName /= [] ->
            % Get variable value
            _ = get_or_create_variable(Interp, VarName),
            ValueStr = case forthic_interpreter:get_variable(Interp, ModuleName, VarName) of
                {ok, Value} ->
                    value_to_string(Value, Separator, NullText, UseJSON);
                not_found ->
                    NullText
            end,
            % Add value to accumulator (reversed)
            NewAcc = lists:reverse(ValueStr) ++ Acc,
            interpolate_scan(Remaining, Interp, ModuleName, Separator, NullText, UseJSON, NewAcc, false);
        no_var_name ->
            % Not a variable, just add the dot
            interpolate_scan(Rest, Interp, ModuleName, Separator, NullText, UseJSON, [$. | Acc], false)
    end;
interpolate_scan([$. | Rest], Interp, ModuleName, Separator, NullText, UseJSON, Acc, false) ->
    % Dot not at start or after whitespace, just add it
    interpolate_scan(Rest, Interp, ModuleName, Separator, NullText, UseJSON, [$. | Acc], false);
interpolate_scan([C | Rest], Interp, ModuleName, Separator, NullText, UseJSON, Acc, _AtStart) ->
    % Check if next position is at start (after whitespace)
    AtStartNext = (C =:= $\s orelse C =:= $\t orelse C =:= $\n orelse C =:= $\r),
    interpolate_scan(Rest, Interp, ModuleName, Separator, NullText, UseJSON, [C | Acc], AtStartNext).

% Scan variable name (letters, digits, underscore, hyphen)
scan_var_name([], Acc) when Acc /= [] ->
    {lists:reverse(Acc), []};
scan_var_name([], []) ->
    no_var_name;
scan_var_name([C | Rest], Acc) when (C >= $a andalso C =< $z) orelse
                                      (C >= $A andalso C =< $Z) orelse
                                      (C >= $0 andalso C =< $9 andalso Acc /= []) orelse
                                      C =:= $_ orelse
                                      (C =:= $- andalso Acc /= []) ->
    scan_var_name(Rest, [C | Acc]);
scan_var_name(Rest, Acc) when Acc /= [] ->
    {lists:reverse(Acc), Rest};
scan_var_name(_Rest, []) ->
    no_var_name.

value_to_string(null, _, NullText, _) ->
    NullText;
value_to_string(Value, _, _, true) ->
    % JSON encoding
    case jsx:encode(Value) of
        Encoded when is_binary(Encoded) -> binary_to_list(Encoded);
        Encoded -> Encoded
    end;
value_to_string(Value, Separator, NullText, _UseJSON) when is_list(Value) ->
    Strs = lists:map(fun(V) -> value_to_string(V, Separator, NullText, false) end, Value),
    string:join(Strs, Separator);
value_to_string(Value, _, _, _) when is_map(Value) ->
    % JSON encoding for maps
    case jsx:encode(Value) of
        Encoded when is_binary(Encoded) -> binary_to_list(Encoded);
        Encoded -> Encoded
    end;
value_to_string(Value, _, _, _) when is_binary(Value) ->
    binary_to_list(Value);
value_to_string(Value, _, _, _) when is_integer(Value) ->
    integer_to_list(Value);
value_to_string(Value, _, _, _) when is_float(Value) ->
    float_to_list(Value);
value_to_string(Value, _, _, _) when is_atom(Value) ->
    atom_to_list(Value);
value_to_string(Value, _, _, _) ->
    lists:flatten(io_lib:format("~p", [Value])).

%% ========================================
%% Debug Operations
%% ========================================

peek(Interp) ->
    Stack = forthic_interpreter:get_stack(Interp),
    case forthic_stack:peek(Stack) of
        {ok, Value} ->
            io:format("~p~n", [Value]);
        {error, stack_underflow} ->
            io:format("<STACK EMPTY>~n")
    end,
    {error, {intentional_stop, "PEEK!"}}.

stack_debug(Interp) ->
    Stack = forthic_interpreter:get_stack(Interp),
    Items = forthic_stack:to_list(Stack),
    Reversed = lists:reverse(Items),
    case jsx:encode(Reversed, [{space, 1}, {indent, 2}]) of
        Encoded when is_binary(Encoded) ->
            io:format("~s~n", [binary_to_list(Encoded)]);
        Encoded ->
            io:format("~s~n", [Encoded])
    end,
    {error, {intentional_stop, "STACK!"}}.

%% ========================================
%% Helper Functions
%% ========================================

to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_list(V) -> V;
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_float(V) -> float_to_list(V);
to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(_) -> "".

is_string([]) -> true;
is_string([H|_]) when is_integer(H), H >= 0, H =< 1114111 -> true;
is_string(_) -> false.

is_word_options(Value) ->
    % Check if value is a word options record/map
    is_map(Value) andalso maps:is_key(<<"__word_options__">>, Value).

get_option(Opts, Key, Default) ->
    case forthic_word_options:get(Opts, Key) of
        undefined -> Default;
        Value -> Value
    end.
