-module(forthic_errors).

-export([
    new_code_location/3,
    code_location_to_string/1,
    new_forthic_error/1,
    with_location/2,
    with_forthic/2,
    with_cause/2,
    error_to_string/1,
    new_unknown_word_error/1,
    new_unknown_module_error/1,
    new_stack_underflow_error/0,
    new_word_execution_error/2,
    new_missing_semicolon_error/0,
    new_extra_semicolon_error/0,
    new_module_error/2,
    new_intentional_stop_error/1,
    new_invalid_variable_name_error/1
]).

%% ============================================================================
%% Records
%% ============================================================================

-record(code_location, {
    file = "" :: string(),
    line = 0 :: integer(),
    column = 0 :: integer()
}).

-record(forthic_error, {
    message :: string(),
    forthic = "" :: string(),
    location = undefined :: undefined | #code_location{},
    cause = undefined :: undefined | term()
}).

-record(unknown_word_error, {
    error :: #forthic_error{},
    word :: string()
}).

-record(unknown_module_error, {
    error :: #forthic_error{},
    module :: string()
}).

-record(stack_underflow_error, {
    error :: #forthic_error{}
}).

-record(word_execution_error, {
    error :: #forthic_error{},
    word :: string()
}).

-record(missing_semicolon_error, {
    error :: #forthic_error{}
}).

-record(extra_semicolon_error, {
    error :: #forthic_error{}
}).

-record(module_error, {
    error :: #forthic_error{},
    module :: string()
}).

-record(intentional_stop_error, {
    error :: #forthic_error{}
}).

-record(invalid_variable_name_error, {
    error :: #forthic_error{},
    var_name :: string()
}).

%% ============================================================================
%% CodeLocation Functions
%% ============================================================================

new_code_location(File, Line, Column) ->
    #code_location{file = File, line = Line, column = Column}.

code_location_to_string(#code_location{file = "", line = Line, column = Column}) ->
    io_lib:format("line ~p, col ~p", [Line, Column]);
code_location_to_string(#code_location{file = File, line = Line, column = Column}) ->
    io_lib:format("~s:~p:~p", [File, Line, Column]).

%% ============================================================================
%% ForthicError Functions
%% ============================================================================

new_forthic_error(Message) ->
    #forthic_error{message = Message}.

with_location(Error = #forthic_error{}, Location) ->
    Error#forthic_error{location = Location};
with_location(#unknown_word_error{error = E} = Error, Location) ->
    Error#unknown_word_error{error = E#forthic_error{location = Location}};
with_location(#unknown_module_error{error = E} = Error, Location) ->
    Error#unknown_module_error{error = E#forthic_error{location = Location}};
with_location(#stack_underflow_error{error = E} = Error, Location) ->
    Error#stack_underflow_error{error = E#forthic_error{location = Location}};
with_location(#word_execution_error{error = E} = Error, Location) ->
    Error#word_execution_error{error = E#forthic_error{location = Location}};
with_location(#missing_semicolon_error{error = E} = Error, Location) ->
    Error#missing_semicolon_error{error = E#forthic_error{location = Location}};
with_location(#extra_semicolon_error{error = E} = Error, Location) ->
    Error#extra_semicolon_error{error = E#forthic_error{location = Location}};
with_location(#module_error{error = E} = Error, Location) ->
    Error#module_error{error = E#forthic_error{location = Location}};
with_location(#intentional_stop_error{error = E} = Error, Location) ->
    Error#intentional_stop_error{error = E#forthic_error{location = Location}};
with_location(#invalid_variable_name_error{error = E} = Error, Location) ->
    Error#invalid_variable_name_error{error = E#forthic_error{location = Location}}.

with_forthic(Error = #forthic_error{}, Forthic) ->
    Error#forthic_error{forthic = Forthic};
with_forthic(#unknown_word_error{error = E} = Error, Forthic) ->
    Error#unknown_word_error{error = E#forthic_error{forthic = Forthic}};
with_forthic(#unknown_module_error{error = E} = Error, Forthic) ->
    Error#unknown_module_error{error = E#forthic_error{forthic = Forthic}};
with_forthic(#stack_underflow_error{error = E} = Error, Forthic) ->
    Error#stack_underflow_error{error = E#forthic_error{forthic = Forthic}};
with_forthic(#word_execution_error{error = E} = Error, Forthic) ->
    Error#word_execution_error{error = E#forthic_error{forthic = Forthic}};
with_forthic(#missing_semicolon_error{error = E} = Error, Forthic) ->
    Error#missing_semicolon_error{error = E#forthic_error{forthic = Forthic}};
with_forthic(#extra_semicolon_error{error = E} = Error, Forthic) ->
    Error#extra_semicolon_error{error = E#forthic_error{forthic = Forthic}};
with_forthic(#module_error{error = E} = Error, Forthic) ->
    Error#module_error{error = E#forthic_error{forthic = Forthic}};
with_forthic(#intentional_stop_error{error = E} = Error, Forthic) ->
    Error#intentional_stop_error{error = E#forthic_error{forthic = Forthic}};
with_forthic(#invalid_variable_name_error{error = E} = Error, Forthic) ->
    Error#invalid_variable_name_error{error = E#forthic_error{forthic = Forthic}}.

with_cause(Error = #forthic_error{}, Cause) ->
    Error#forthic_error{cause = Cause};
with_cause(#unknown_word_error{error = E} = Error, Cause) ->
    Error#unknown_word_error{error = E#forthic_error{cause = Cause}};
with_cause(#unknown_module_error{error = E} = Error, Cause) ->
    Error#unknown_module_error{error = E#forthic_error{cause = Cause}};
with_cause(#stack_underflow_error{error = E} = Error, Cause) ->
    Error#stack_underflow_error{error = E#forthic_error{cause = Cause}};
with_cause(#word_execution_error{error = E} = Error, Cause) ->
    Error#word_execution_error{error = E#forthic_error{cause = Cause}};
with_cause(#missing_semicolon_error{error = E} = Error, Cause) ->
    Error#missing_semicolon_error{error = E#forthic_error{cause = Cause}};
with_cause(#extra_semicolon_error{error = E} = Error, Cause) ->
    Error#extra_semicolon_error{error = E#forthic_error{cause = Cause}};
with_cause(#module_error{error = E} = Error, Cause) ->
    Error#module_error{error = E#forthic_error{cause = Cause}};
with_cause(#intentional_stop_error{error = E} = Error, Cause) ->
    Error#intentional_stop_error{error = E#forthic_error{cause = Cause}};
with_cause(#invalid_variable_name_error{error = E} = Error, Cause) ->
    Error#invalid_variable_name_error{error = E#forthic_error{cause = Cause}}.

error_to_string(#forthic_error{} = Error) ->
    format_error(Error);
error_to_string(#unknown_word_error{error = Error}) ->
    format_error(Error);
error_to_string(#unknown_module_error{error = Error}) ->
    format_error(Error);
error_to_string(#stack_underflow_error{error = Error}) ->
    format_error(Error);
error_to_string(#word_execution_error{error = Error}) ->
    format_error(Error);
error_to_string(#missing_semicolon_error{error = Error}) ->
    format_error(Error);
error_to_string(#extra_semicolon_error{error = Error}) ->
    format_error(Error);
error_to_string(#module_error{error = Error}) ->
    format_error(Error);
error_to_string(#intentional_stop_error{error = Error}) ->
    format_error(Error);
error_to_string(#invalid_variable_name_error{error = Error}) ->
    format_error(Error).

%% ============================================================================
%% Specific Error Constructors
%% ============================================================================

new_unknown_word_error(Word) ->
    #unknown_word_error{
        error = new_forthic_error(io_lib:format("Unknown word: ~s", [Word])),
        word = Word
    }.

new_unknown_module_error(Module) ->
    #unknown_module_error{
        error = new_forthic_error(io_lib:format("Unknown module: ~s", [Module])),
        module = Module
    }.

new_stack_underflow_error() ->
    #stack_underflow_error{
        error = new_forthic_error("Stack underflow")
    }.

new_word_execution_error(Word, Cause) ->
    #word_execution_error{
        error = (new_forthic_error(io_lib:format("Error executing word: ~s", [Word])))#forthic_error{cause = Cause},
        word = Word
    }.

new_missing_semicolon_error() ->
    #missing_semicolon_error{
        error = new_forthic_error("Missing semicolon (;) to end definition")
    }.

new_extra_semicolon_error() ->
    #extra_semicolon_error{
        error = new_forthic_error("Extra semicolon (;) outside of definition")
    }.

new_module_error(Module, Message) ->
    #module_error{
        error = new_forthic_error(io_lib:format("Module error in ~s: ~s", [Module, Message])),
        module = Module
    }.

new_intentional_stop_error(Message) ->
    #intentional_stop_error{
        error = new_forthic_error(Message)
    }.

new_invalid_variable_name_error(VarName) ->
    #invalid_variable_name_error{
        error = new_forthic_error(io_lib:format("Invalid variable name: ~s", [VarName])),
        var_name = VarName
    }.

%% ============================================================================
%% Private Helper Functions
%% ============================================================================

format_error(#forthic_error{message = Message, location = Location, forthic = Forthic, cause = Cause}) ->
    Parts = [Message],
    Parts2 = case Location of
        undefined -> Parts;
        Loc -> Parts ++ [io_lib:format("at ~s", [code_location_to_string(Loc)])]
    end,
    Parts3 = case Forthic of
        "" -> Parts2;
        F -> Parts2 ++ [io_lib:format("in: ~s", [F])]
    end,
    Parts4 = case Cause of
        undefined -> Parts3;
        C -> Parts3 ++ [io_lib:format("caused by: ~p", [C])]
    end,
    string:join(Parts4, "\n  ").
