-module(forthic_word).

-export([
    new_push_value_word/2,
    new_module_word/3,
    new_definition_word/2,
    execute/2,
    get_name/1,
    get_location/1,
    set_location/2,
    add_error_handler/2,
    add_word_to_definition/2
]).

%% Forward declaration - interpreter will be defined in forthic_interpreter module
%% For now, we'll use term() as the interpreter type
-type interpreter() :: term().

%% Error handler function type
-type error_handler() :: fun((term(), word(), interpreter()) -> {ok, interpreter()} | {error, term()}).

%% ============================================================================
%% Word Types
%% ============================================================================

-record(push_value_word, {
    name :: string(),
    value :: term(),
    location :: term() | undefined
}).

-record(module_word, {
    name :: string(),
    handler :: fun((interpreter()) -> {ok, interpreter()} | {error, term()}),
    location :: term() | undefined,
    error_handlers :: list(error_handler())
}).

-record(definition_word, {
    name :: string(),
    words :: list(word()),
    location :: term() | undefined,
    error_handlers :: list(error_handler())
}).

-type word() :: #push_value_word{} | #module_word{} | #definition_word{}.

-export_type([word/0, error_handler/0, interpreter/0]).

%% ============================================================================
%% Construction
%% ============================================================================

%% Create word that pushes a value onto the stack
-spec new_push_value_word(string(), term()) -> word().
new_push_value_word(Name, Value) ->
    #push_value_word{name = Name, value = Value, location = undefined}.

%% Create module word with handler function
-spec new_module_word(string(), fun((interpreter()) -> {ok, interpreter()} | {error, term()}), term() | undefined) -> word().
new_module_word(Name, Handler, Location) ->
    #module_word{
        name = Name,
        handler = Handler,
        location = Location,
        error_handlers = []
    }.

%% Create definition word (sequence of words)
-spec new_definition_word(string(), list(word())) -> word().
new_definition_word(Name, Words) ->
    #definition_word{
        name = Name,
        words = Words,
        location = undefined,
        error_handlers = []
    }.

%% ============================================================================
%% Execution
%% ============================================================================

%% Execute a word with the interpreter
-spec execute(word(), interpreter()) -> {ok, interpreter()} | {error, term()}.
execute(#push_value_word{value = Value}, Interp) ->
    % Push value onto interpreter stack
    % We'll call forthic_interpreter:stack_push/2
    forthic_interpreter:stack_push(Interp, Value);

execute(#module_word{handler = Handler, error_handlers = ErrorHandlers} = Word, Interp) ->
    case Handler(Interp) of
        {ok, NewInterp} -> {ok, NewInterp};
        {error, Error} ->
            % Try error handlers
            try_error_handlers(ErrorHandlers, Error, Word, Interp)
    end;

execute(#definition_word{words = Words, error_handlers = ErrorHandlers} = Word, Interp) ->
    execute_words(Words, Interp, ErrorHandlers, Word).

%% ============================================================================
%% Access Methods
%% ============================================================================

%% Get word name
-spec get_name(word()) -> string().
get_name(#push_value_word{name = Name}) -> Name;
get_name(#module_word{name = Name}) -> Name;
get_name(#definition_word{name = Name}) -> Name.

%% Get word location
-spec get_location(word()) -> term() | undefined.
get_location(#push_value_word{location = Loc}) -> Loc;
get_location(#module_word{location = Loc}) -> Loc;
get_location(#definition_word{location = Loc}) -> Loc.

%% Set word location
-spec set_location(word(), term() | undefined) -> word().
set_location(Word = #push_value_word{}, Location) ->
    Word#push_value_word{location = Location};
set_location(Word = #module_word{}, Location) ->
    Word#module_word{location = Location};
set_location(Word = #definition_word{}, Location) ->
    Word#definition_word{location = Location}.

%% Add error handler to word
-spec add_error_handler(word(), error_handler()) -> word().
add_error_handler(Word = #push_value_word{}, _Handler) ->
    % Push value words don't support error handlers
    Word;
add_error_handler(Word = #module_word{error_handlers = Handlers}, Handler) ->
    Word#module_word{error_handlers = Handlers ++ [Handler]};
add_error_handler(Word = #definition_word{error_handlers = Handlers}, Handler) ->
    Word#definition_word{error_handlers = Handlers ++ [Handler]}.

%% Add word to definition
-spec add_word_to_definition(word(), word()) -> word().
add_word_to_definition(DefWord = #definition_word{words = Words}, NewWord) ->
    DefWord#definition_word{words = Words ++ [NewWord]};
add_word_to_definition(Word, _NewWord) ->
    % Only definition words can have words added
    Word.

%% ============================================================================
%% Private Helper Functions
%% ============================================================================

%% Execute a list of words sequentially
execute_words([], Interp, _ErrorHandlers, _Word) ->
    {ok, Interp};
execute_words([Word | Rest], Interp, ErrorHandlers, DefWord) ->
    case execute(Word, Interp) of
        {ok, NewInterp} ->
            execute_words(Rest, NewInterp, ErrorHandlers, DefWord);
        {error, Error} ->
            case try_error_handlers(ErrorHandlers, Error, DefWord, Interp) of
                {ok, NewInterp} ->
                    execute_words(Rest, NewInterp, ErrorHandlers, DefWord);
                {error, _} = Err ->
                    Err
            end
    end.

%% Try error handlers in sequence
try_error_handlers([], Error, _Word, _Interp) ->
    {error, Error};
try_error_handlers([Handler | Rest], Error, Word, Interp) ->
    case Handler(Error, Word, Interp) of
        {ok, NewInterp} ->
            {ok, NewInterp};
        {error, _} ->
            try_error_handlers(Rest, Error, Word, Interp)
    end.
