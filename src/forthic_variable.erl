-module(forthic_variable).

-export([
    new/2,
    get_name/1,
    get_value/1,
    set_value/2
]).

%% ============================================================================
%% Variable - Mutable value container
%% ============================================================================

-record(variable, {
    name :: string(),
    value :: term()
}).

-type variable() :: #variable{}.

-export_type([variable/0]).

%% ============================================================================
%% Construction
%% ============================================================================

%% Create new variable with name and initial value
-spec new(string(), term()) -> variable().
new(Name, Value) ->
    #variable{name = Name, value = Value}.

%% ============================================================================
%% Access Methods
%% ============================================================================

%% Get variable name
-spec get_name(variable()) -> string().
get_name(#variable{name = Name}) ->
    Name.

%% Get variable value
-spec get_value(variable()) -> term().
get_value(#variable{value = Value}) ->
    Value.

%% Set variable value (returns new variable)
-spec set_value(variable(), term()) -> variable().
set_value(Variable, NewValue) ->
    Variable#variable{value = NewValue}.
