-module(forthic_variable).

-export([
    new/2,
    new/3,
    get_name/1,
    get_module_name/1,
    get_value/2,
    set_value/3,
    get_metadata/1,
    set_metadata/2
]).

%% ============================================================================
%% Variable - Metadata container with ETS-backed value storage
%%
%% Variables store metadata (name, module, constraints) immutably,
%% while values are stored mutably in the interpreter's ETS table.
%% This allows Erlang-idiomatic immutable records with mutable state.
%% ============================================================================

-record(variable, {
    name :: string(),
    module_name :: string(),
    metadata = #{} :: map()  % Extensible metadata: type, constraints, etc.
    % NO value field - values stored in interpreter's ETS table
}).

-type variable() :: #variable{}.

-export_type([variable/0]).

%% ============================================================================
%% Construction
%% ============================================================================

%% Create new variable with name and module name
-spec new(string(), string()) -> variable().
new(Name, ModuleName) ->
    #variable{name = Name, module_name = ModuleName, metadata = #{}}.

%% Create new variable with name, module name, and metadata
-spec new(string(), string(), map()) -> variable().
new(Name, ModuleName, Metadata) ->
    #variable{name = Name, module_name = ModuleName, metadata = Metadata}.

%% ============================================================================
%% Access Methods
%% ============================================================================

%% Get variable name
-spec get_name(variable()) -> string().
get_name(#variable{name = Name}) ->
    Name.

%% Get variable's module name
-spec get_module_name(variable()) -> string().
get_module_name(#variable{module_name = ModuleName}) ->
    ModuleName.

%% Get variable value from interpreter's ETS table
-spec get_value(variable(), term()) -> term().
get_value(#variable{name = Name, module_name = ModuleName}, Interp) ->
    case forthic_interpreter:get_variable(Interp, ModuleName, Name) of
        {ok, Value} -> Value;
        not_found -> null  % Variable exists but no value set yet
    end.

%% Set variable value in interpreter's ETS table
-spec set_value(variable(), term(), term()) -> ok.
set_value(#variable{name = Name, module_name = ModuleName}, Value, Interp) ->
    forthic_interpreter:set_variable(Interp, ModuleName, Name, Value).

%% Get variable metadata
-spec get_metadata(variable()) -> map().
get_metadata(#variable{metadata = Metadata}) ->
    Metadata.

%% Set variable metadata (returns new variable with updated metadata)
-spec set_metadata(variable(), map()) -> variable().
set_metadata(Variable, NewMetadata) ->
    Variable#variable{metadata = NewMetadata}.
