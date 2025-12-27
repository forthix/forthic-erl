-module(forthic_stack).

-export([
    new/0,
    push/2,
    pop/1,
    peek/1,
    length/1,
    clear/1,
    at/2,
    set/3,
    to_list/1
]).

%% ============================================================================
%% Stack - LIFO data stack for interpreter
%% ============================================================================

%% Stack is represented as a list in Erlang (head = top of stack)
-type stack() :: list(term()).

-export_type([stack/0]).

%% ============================================================================
%% Construction
%% ============================================================================

%% Create new empty stack
-spec new() -> stack().
new() ->
    [].

%% ============================================================================
%% Stack Operations
%% ============================================================================

%% Push value onto stack
-spec push(stack(), term()) -> stack().
push(Stack, Value) ->
    [Value | Stack].

%% Pop value from stack
-spec pop(stack()) -> {ok, term(), stack()} | {error, stack_underflow}.
pop([]) ->
    {error, stack_underflow};
pop([Head | Tail]) ->
    {ok, Head, Tail}.

%% Peek at top value without removing
-spec peek(stack()) -> {ok, term()} | {error, stack_underflow}.
peek([]) ->
    {error, stack_underflow};
peek([Head | _]) ->
    {ok, Head}.

%% Get stack length
-spec length(stack()) -> non_neg_integer().
length(Stack) ->
    erlang:length(Stack).

%% Clear all items
-spec clear(stack()) -> stack().
clear(_Stack) ->
    [].

%% Get item at index (0 = bottom, length-1 = top)
%% Note: This converts index to list position (0 = last element)
-spec at(stack(), non_neg_integer()) -> {ok, term()} | {error, out_of_bounds}.
at(Stack, Index) ->
    Len = erlang:length(Stack),
    if
        Index >= Len ->
            {error, out_of_bounds};
        true ->
            % Convert index (0=bottom) to list position (0=top)
            % Index 0 should be the last element (bottom of stack)
            % Index Len-1 should be the first element (top of stack)
            Position = Len - Index,
            {ok, lists:nth(Position, Stack)}
    end.

%% Set item at index
-spec set(stack(), non_neg_integer(), term()) -> {ok, stack()} | {error, out_of_bounds}.
set(Stack, Index, Value) ->
    Len = erlang:length(Stack),
    if
        Index >= Len ->
            {error, out_of_bounds};
        true ->
            % Convert index to list position
            Position = Len - Index,
            NewStack = set_nth(Stack, Position, Value),
            {ok, NewStack}
    end.

%% Convert to list (for debugging/testing)
-spec to_list(stack()) -> list(term()).
to_list(Stack) ->
    lists:reverse(Stack).  % Reverse to show bottom-to-top order

%% ============================================================================
%% Private Helper Functions
%% ============================================================================

%% Set nth element of a list (1-indexed)
set_nth([_ | Rest], 1, Value) ->
    [Value | Rest];
set_nth([Head | Rest], N, Value) when N > 1 ->
    [Head | set_nth(Rest, N - 1, Value)];
set_nth([], _, _) ->
    [].
