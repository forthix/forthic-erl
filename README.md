# Forthic Erlang Runtime

An Erlang/OTP implementation of the Forthic stack-based concatenative programming language.

## Overview

Forthic is a stack-based, concatenative language designed for composable transformations. This is the official Erlang runtime implementation, providing full compatibility with other Forthic runtimes and leveraging the power of OTP for distributed computing.

## Features

- ✅ Complete Forthic language implementation
- ✅ All 8 standard library modules
- ✅ OTP behaviors (gen_server, supervisor)
- ✅ gRPC support for multi-runtime execution
- ✅ Hot code reloading
- ✅ Distributed erlang integration
- ✅ Escript CLI tool
- ✅ Comprehensive EUnit test suite

## Installation

```bash
rebar3 compile
```

## Usage

### As a Library

```erlang
%% Start the application
application:start(forthic).

%% Create an interpreter
{ok, Interp} = forthic_interpreter:new().

%% Run Forthic code
{ok, Result} = forthic_interpreter:run(Interp, "[1 2 3] \"2 *\" MAP").

%% Get result from stack
Value = forthic_interpreter:stack_pop(Interp).
%% Value = [2, 4, 6]
```

### CLI (via escript)

```bash
# REPL mode
./forthic repl

# Execute a script
./forthic run script.forthic

# Eval mode (one-liner)
./forthic eval "[1 2 3] LENGTH"
```

## Development

```bash
# Compile
rebar3 compile

# Run tests
rebar3 eunit

# Run specific test
rebar3 eunit --module=forthic_interpreter_test

# Run with property-based testing
rebar3 proper

# Start shell with application loaded
rebar3 shell
```

## Project Structure

```
forthic-erl/
├── src/
│   ├── forthic.app.src           # OTP application
│   ├── forthic_app.erl            # Application behavior
│   ├── forthic_sup.erl            # Supervisor
│   ├── forthic_interpreter.erl    # Core interpreter
│   ├── forthic_tokenizer.erl      # Lexical analysis
│   ├── forthic_module.erl         # Module system
│   └── modules/standard/          # Standard library (8 modules)
├── test/                          # EUnit tests
└── priv/                          # Resources
```

## Standard Library Modules

- **core**: Stack operations, variables, control flow
- **array**: Data transformation (MAP, SELECT, SORT, etc.)
- **record**: Dictionary operations
- **string**: Text processing
- **math**: Arithmetic operations
- **boolean**: Logical operations
- **datetime**: Date/time manipulation
- **json**: JSON serialization (using jsx)

## OTP Integration

The Forthic runtime is a proper OTP application with supervision:

```erlang
%% Supervised interpreter process
{ok, Pid} = forthic_interpreter:start_link().

%% Execute in process
gen_server:call(Pid, {run, "[1 2 3] REVERSE"}).
```

## Distributed Execution

Leverage distributed erlang for multi-node Forthic execution:

```erlang
%% Execute on remote node
rpc:call('node@host', forthic_interpreter, run, [Interp, Code]).
```

## Multi-Runtime Execution

This runtime supports calling words from other Forthic runtimes via gRPC:

```erlang
%% Call a Java word from Erlang
{ok, Result} = forthic_grpc_client:execute_word(
    "java-runtime", "MY-WORD", Args
).
```

## License

Apache 2.0

## Links

- [Forthic Language Specification](https://github.com/forthix/forthic)
- [TypeScript Runtime](https://github.com/forthix/forthic-ts) (reference implementation)
- [Documentation](https://forthix.github.io/forthic-erl)
