-module(forthic_module).

-export([
    new/2,
    get_name/1,
    get_forthic_code/1,
    add_word/2,
    add_exportable/2,
    add_exportable_word/2,
    exportable_words/1,
    find_word/2,
    find_dictionary_word/2,
    find_variable/2,
    add_variable/3,
    get_variable/2,
    register_module/4,
    find_module/2
]).

%% ============================================================================
%% Module - Container for words, variables, and imported modules
%% ============================================================================

-record(forthic_module, {
    name :: string(),
    forthic_code :: string(),
    words :: list(forthic_word:word()),
    exportable :: list(string()),
    variables :: #{string() => forthic_variable:variable()},
    modules :: #{string() => forthic_module()},
    module_prefixes :: #{string() => list(string())}
}).

-type forthic_module() :: #forthic_module{}.

-export_type([forthic_module/0]).

%% ============================================================================
%% Construction
%% ============================================================================

%% Create new module with name and forthic code
-spec new(string(), string()) -> forthic_module().
new(Name, ForthicCode) ->
    #forthic_module{
        name = Name,
        forthic_code = ForthicCode,
        words = [],
        exportable = [],
        variables = #{},
        modules = #{},
        module_prefixes = #{}
    }.

%% ============================================================================
%% Access Methods
%% ============================================================================

%% Get module name
-spec get_name(forthic_module()) -> string().
get_name(#forthic_module{name = Name}) ->
    Name.

%% Get forthic code
-spec get_forthic_code(forthic_module()) -> string().
get_forthic_code(#forthic_module{forthic_code = Code}) ->
    Code.

%% ============================================================================
%% Word Management
%% ============================================================================

%% Add word to module (returns new module)
-spec add_word(forthic_module(), forthic_word:word()) -> forthic_module().
add_word(Module = #forthic_module{words = Words}, Word) ->
    Module#forthic_module{words = Words ++ [Word]}.

%% Add exportable word names
-spec add_exportable(forthic_module(), list(string())) -> forthic_module().
add_exportable(Module = #forthic_module{exportable = Exportable}, Names) ->
    Module#forthic_module{exportable = Exportable ++ Names}.

%% Add word and mark it as exportable
-spec add_exportable_word(forthic_module(), forthic_word:word()) -> forthic_module().
add_exportable_word(Module, Word) ->
    Module2 = add_word(Module, Word),
    WordName = forthic_word:get_name(Word),
    add_exportable(Module2, [WordName]).

%% Get list of exportable words
-spec exportable_words(forthic_module()) -> list(forthic_word:word()).
exportable_words(#forthic_module{words = Words, exportable = Exportable}) ->
    ExportableSet = sets:from_list(Exportable),
    lists:filter(
        fun(Word) ->
            sets:is_element(forthic_word:get_name(Word), ExportableSet)
        end,
        Words
    ).

%% Find word by name (checks dictionary words first, then variables)
-spec find_word(forthic_module(), string()) -> {ok, forthic_word:word()} | not_found.
find_word(Module, Name) ->
    case find_dictionary_word(Module, Name) of
        {ok, Word} -> {ok, Word};
        not_found -> find_variable(Module, Name)
    end.

%% Find dictionary word (search from end to beginning - last added wins)
-spec find_dictionary_word(forthic_module(), string()) -> {ok, forthic_word:word()} | not_found.
find_dictionary_word(#forthic_module{words = Words}, WordName) ->
    find_word_in_list(lists:reverse(Words), WordName).

%% Find variable and wrap as PushValueWord
-spec find_variable(forthic_module(), string()) -> {ok, forthic_word:word()} | not_found.
find_variable(#forthic_module{variables = Variables}, VarName) ->
    case maps:get(VarName, Variables, undefined) of
        undefined -> not_found;
        Variable ->
            Value = forthic_variable:get_value(Variable),
            PushWord = forthic_word:new_push_value_word(VarName, Value),
            {ok, PushWord}
    end.

%% ============================================================================
%% Variable Management
%% ============================================================================

%% Add variable to module
-spec add_variable(forthic_module(), string(), term()) -> forthic_module().
add_variable(Module = #forthic_module{variables = Variables}, Name, Value) ->
    case maps:is_key(Name, Variables) of
        true -> Module;  % Already exists, don't overwrite
        false ->
            Variable = forthic_variable:new(Name, Value),
            Module#forthic_module{variables = maps:put(Name, Variable, Variables)}
    end.

%% Get variable
-spec get_variable(forthic_module(), string()) -> {ok, forthic_variable:variable()} | not_found.
get_variable(#forthic_module{variables = Variables}, Name) ->
    case maps:get(Name, Variables, undefined) of
        undefined -> not_found;
        Variable -> {ok, Variable}
    end.

%% ============================================================================
%% Module Management
%% ============================================================================

%% Register module with name and prefix
-spec register_module(forthic_module(), string(), string(), forthic_module()) -> forthic_module().
register_module(Module = #forthic_module{modules = Modules, module_prefixes = Prefixes}, ModuleName, Prefix, NewModule) ->
    % Add module to modules map
    UpdatedModules = maps:put(ModuleName, NewModule, Modules),

    % Add prefix to module_prefixes
    CurrentPrefixes = maps:get(ModuleName, Prefixes, []),
    UpdatedPrefixes = maps:put(ModuleName, CurrentPrefixes ++ [Prefix], Prefixes),

    Module#forthic_module{modules = UpdatedModules, module_prefixes = UpdatedPrefixes}.

%% Find registered module by name
-spec find_module(forthic_module(), string()) -> {ok, forthic_module()} | not_found.
find_module(#forthic_module{modules = Modules}, Name) ->
    case maps:get(Name, Modules, undefined) of
        undefined -> not_found;
        Mod -> {ok, Mod}
    end.

%% ============================================================================
%% Private Helper Functions
%% ============================================================================

%% Find word in list by name
find_word_in_list([], _Name) ->
    not_found;
find_word_in_list([Word | Rest], Name) ->
    case forthic_word:get_name(Word) of
        Name -> {ok, Word};
        _ -> find_word_in_list(Rest, Name)
    end.
