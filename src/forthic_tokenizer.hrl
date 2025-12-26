%% Header file for forthic_tokenizer
%% Defines records used by tokenizer

-record(code_location, {
    source = "" :: string(),
    line = 1 :: integer(),
    column = 1 :: integer(),
    start_pos = 0 :: integer(),
    end_pos = 0 :: integer()
}).

-record(token, {
    type :: atom(),
    string :: string(),
    location :: #code_location{}
}).

-record(string_delta, {
    start = 0 :: integer(),
    end_pos = 0 :: integer()
}).

-record(tokenizer, {
    reference_location :: #code_location{},
    line :: integer(),
    column :: integer(),
    input_string :: string(),
    input_pos :: integer(),
    whitespace :: [integer()],
    quote_chars :: [integer()],
    token_start_pos :: integer(),
    token_end_pos :: integer(),
    token_line :: integer(),
    token_column :: integer(),
    token_string :: string(),
    string_delta :: undefined | #string_delta{},
    streaming :: boolean()
}).
