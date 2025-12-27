-module(forthic_interpreter).

-include("forthic_tokenizer.hrl").

-export([
    new/0,
    stack_push/2,
    stack_pop/1,
    stack_peek/1,
    get_stack/1,
    cur_module/1,
    module_stack_push/2,
    module_stack_pop/1,
    register_module/3,
    find_module/2,
    find_word/2,
    run/2
]).

%% ============================================================================
%% Interpreter - Core Forthic interpreter
%% ============================================================================

-record(interpreter, {
    stack :: forthic_stack:stack(),
    app_module :: forthic_module:forthic_module(),
    module_stack :: list(forthic_module:forthic_module()),
    registered_modules :: #{string() => forthic_module:forthic_module()},
    literal_handlers :: list(fun((string()) -> {ok, term()} | {error, term()})),
    is_compiling :: boolean(),
    is_memo_definition :: boolean(),
    cur_definition :: forthic_word:word() | undefined
}).

-type interpreter() :: #interpreter{}.

-export_type([interpreter/0]).

%% ============================================================================
%% Construction
%% ============================================================================

%% Create new interpreter
-spec new() -> interpreter().
new() ->
    AppModule = forthic_module:new("", ""),
    #interpreter{
        stack = forthic_stack:new(),
        app_module = AppModule,
        module_stack = [AppModule],
        registered_modules = #{},
        literal_handlers = register_standard_literals(),
        is_compiling = false,
        is_memo_definition = false,
        cur_definition = undefined
    }.

%% ============================================================================
%% Stack Operations
%% ============================================================================

%% Push value onto stack
-spec stack_push(interpreter(), term()) -> {ok, interpreter()}.
stack_push(Interp = #interpreter{stack = Stack}, Value) ->
    NewStack = forthic_stack:push(Stack, Value),
    {ok, Interp#interpreter{stack = NewStack}}.

%% Pop value from stack
-spec stack_pop(interpreter()) -> {ok, term(), interpreter()} | {error, term()}.
stack_pop(Interp = #interpreter{stack = Stack}) ->
    case forthic_stack:pop(Stack) of
        {ok, Value, NewStack} ->
            {ok, Value, Interp#interpreter{stack = NewStack}};
        {error, stack_underflow} ->
            {error, forthic_errors:new_stack_underflow_error()}
    end.

%% Peek at top of stack
-spec stack_peek(interpreter()) -> {ok, term()} | {error, term()}.
stack_peek(#interpreter{stack = Stack}) ->
    case forthic_stack:peek(Stack) of
        {ok, Value} -> {ok, Value};
        {error, stack_underflow} ->
            {error, forthic_errors:new_stack_underflow_error()}
    end.

%% Get stack
-spec get_stack(interpreter()) -> forthic_stack:stack().
get_stack(#interpreter{stack = Stack}) ->
    Stack.

%% ============================================================================
%% Module Operations
%% ============================================================================

%% Get current module (top of module stack)
-spec cur_module(interpreter()) -> forthic_module:forthic_module().
cur_module(#interpreter{module_stack = [Module | _]}) ->
    Module;
cur_module(#interpreter{app_module = AppModule}) ->
    AppModule.

%% Push module onto module stack
-spec module_stack_push(interpreter(), forthic_module:forthic_module()) -> interpreter().
module_stack_push(Interp = #interpreter{module_stack = Stack}, Module) ->
    Interp#interpreter{module_stack = [Module | Stack]}.

%% Pop module from module stack
-spec module_stack_pop(interpreter()) -> {ok, interpreter()} | {error, term()}.
module_stack_pop(Interp = #interpreter{module_stack = [_ | Rest]}) when length(Rest) > 0 ->
    {ok, Interp#interpreter{module_stack = Rest}};
module_stack_pop(_Interp) ->
    {error, forthic_errors:new_forthic_error("Cannot pop last module from stack")}.

%% Register module
-spec register_module(interpreter(), string(), forthic_module:forthic_module()) -> interpreter().
register_module(Interp = #interpreter{registered_modules = Modules}, Name, Module) ->
    Interp#interpreter{registered_modules = maps:put(Name, Module, Modules)}.

%% Find registered module
-spec find_module(interpreter(), string()) -> {ok, forthic_module:forthic_module()} | not_found.
find_module(#interpreter{registered_modules = Modules}, Name) ->
    case maps:get(Name, Modules, undefined) of
        undefined -> not_found;
        Module -> {ok, Module}
    end.

%% Find word (search current module, then registered modules)
-spec find_word(interpreter(), string()) -> {ok, forthic_word:word()} | not_found.
find_word(Interp, Name) ->
    Module = cur_module(Interp),
    forthic_module:find_word(Module, Name).

%% ============================================================================
%% Execution
%% ============================================================================

%% Run forthic code
-spec run(interpreter(), string()) -> {ok, interpreter()} | {error, term()}.
run(Interp, Code) ->
    Tokenizer = forthic_tokenizer:new(Code, undefined, false),
    run_with_tokenizer(Interp, Tokenizer).

%% ============================================================================
%% Private Helper Functions
%% ============================================================================

%% Run with tokenizer
run_with_tokenizer(Interp, Tokenizer) ->
    case forthic_tokenizer:next_token(Tokenizer) of
        {ok, Token, NewTokenizer} ->
            case handle_token(Interp, Token) of
                {ok, NewInterp} ->
                    run_with_tokenizer(NewInterp, NewTokenizer);
                {done, FinalInterp} ->
                    % End of stream reached
                    {ok, FinalInterp};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% Handle a single token
handle_token(Interp, Token) ->
    TokenType = Token#token.type,
    case TokenType of
        eos ->
            {done, Interp};  % End of stream
        string ->
            handle_string_token(Interp, Token);
        word ->
            handle_word_token(Interp, Token);
        comment ->
            {ok, Interp};  % Ignore comments
        start_array ->
            {ok, Interp};  % Arrays handled in tokenizer for now
        end_array ->
            {ok, Interp};
        start_module ->
            handle_start_module_token(Interp, Token);
        end_module ->
            handle_end_module_token(Interp);
        start_def ->
            handle_start_def_token(Interp, Token);
        end_def ->
            handle_end_def_token(Interp);
        start_memo ->
            handle_start_memo_token(Interp, Token);
        dot_symbol ->
            % For now, push dot symbols as strings (they'll be used as option keys)
            stack_push(Interp, Token#token.string);
        _ ->
            {error, forthic_errors:new_forthic_error(io_lib:format("Unknown token type: ~p", [TokenType]))}
    end.

%% Handle string token
handle_string_token(Interp, Token) ->
    stack_push(Interp, Token#token.string).

%% Handle word token
handle_word_token(Interp, Token) ->
    WordName = Token#token.string,
    Location = Token#token.location,
    % Try to find and execute word
    case find_word(Interp, WordName) of
        {ok, Word} ->
            forthic_word:execute(Word, Interp);
        not_found ->
            % Try literal handlers
            try_literal_handlers(Interp, WordName, Location)
    end.

%% Handle start module token
handle_start_module_token(Interp, Token) ->
    ModuleName = Token#token.string,
    % Find or create module and push onto module stack
    case find_module(Interp, ModuleName) of
        {ok, Module} ->
            {ok, module_stack_push(Interp, Module)};
        not_found ->
            NewModule = forthic_module:new(ModuleName, ""),
            Interp2 = register_module(Interp, ModuleName, NewModule),
            {ok, module_stack_push(Interp2, NewModule)}
    end.

%% Handle end module token
handle_end_module_token(Interp) ->
    module_stack_pop(Interp).

%% Handle start definition token
handle_start_def_token(Interp, Token) ->
    WordName = Token#token.string,
    Location = Token#token.location,
    DefWord = forthic_word:new_definition_word(WordName, []),
    DefWord2 = forthic_word:set_location(DefWord, Location),
    {ok, Interp#interpreter{
        is_compiling = true,
        is_memo_definition = false,
        cur_definition = DefWord2
    }}.

%% Handle start memo token
handle_start_memo_token(Interp, Token) ->
    WordName = Token#token.string,
    Location = Token#token.location,
    DefWord = forthic_word:new_definition_word(WordName, []),
    DefWord2 = forthic_word:set_location(DefWord, Location),
    {ok, Interp#interpreter{
        is_compiling = true,
        is_memo_definition = true,
        cur_definition = DefWord2
    }}.

%% Handle end definition token
handle_end_def_token(#interpreter{is_compiling = false}) ->
    {error, forthic_errors:new_extra_semicolon_error()};
handle_end_def_token(Interp = #interpreter{cur_definition = DefWord}) ->
    % Add definition to current module
    Module = cur_module(Interp),
    NewModule = forthic_module:add_word(Module, DefWord),

    % Update module stack
    #interpreter{module_stack = [_ | RestStack]} = Interp,
    NewInterp = Interp#interpreter{
        module_stack = [NewModule | RestStack],
        is_compiling = false,
        cur_definition = undefined
    },
    {ok, NewInterp}.

%% Try literal handlers
try_literal_handlers(_Interp, _String, _Location, []) ->
    {error, forthic_errors:new_unknown_word_error(_String)};
try_literal_handlers(Interp, String, Location, [Handler | Rest]) ->
    case Handler(String) of
        {ok, Value} ->
            stack_push(Interp, Value);
        nil ->
            try_literal_handlers(Interp, String, Location, Rest);
        {error, _} ->
            try_literal_handlers(Interp, String, Location, Rest)
    end.

try_literal_handlers(Interp = #interpreter{literal_handlers = Handlers}, String, Location) ->
    try_literal_handlers(Interp, String, Location, Handlers).

%% Register standard literal handlers
register_standard_literals() ->
    [
        fun forthic_literals:to_bool/1,
        fun forthic_literals:to_int/1,
        fun forthic_literals:to_float/1
    ].
