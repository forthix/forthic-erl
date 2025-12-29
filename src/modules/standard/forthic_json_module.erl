-module(forthic_json_module).
-export([new/0, register_words/1]).

new() -> forthic_module:new("json", "").

register_words(Module) ->
    forthic_module:add_word(Module, ">JSON", fun to_json/1),
    forthic_module:add_word(Module, "JSON>", fun from_json/1),
    forthic_module:add_word(Module, "JSON-PRETTIFY", fun json_prettify/1),
    Module.

to_json(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    JSON = jsx:encode(Val),
    forthic_interpreter:stack_push(Interp, JSON).

from_json(Interp) ->
    Str = forthic_interpreter:stack_pop(Interp),
    try jsx:decode(Str) of
        Decoded -> forthic_interpreter:stack_push(Interp, Decoded)
    catch
        _:_ -> forthic_interpreter:stack_push(Interp, null)
    end.

json_prettify(Interp) ->
    Val = forthic_interpreter:stack_pop(Interp),
    JSON = jsx:prettify(jsx:encode(Val)),
    forthic_interpreter:stack_push(Interp, JSON).
