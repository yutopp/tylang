# Tylang

**WORK IN PROGRESS**

Tylang, TYped erLANG, extends Erlang with type annotations and etc.  
This repository provides only a transpiler from Tylang to Erlang.

A type checker for Tylang is provided as [Eir](https://github.com/yutopp/eir).

## How To Use

Install tylang transpiler as a rebar3 plugin. Add the following to your rebar.config file:
```
{plugins, [
    {tylang, ".*", {git, "https://github.com/yutopp/tylang.git", {brunch, "master"}}}
]}.
{provider_hooks, [{pre, [{compile, {tylang, compile}}]}]}.
```

## Build

```
$ rebar3 compile
```

## License

This software is licensed under the [Boost Software License](http://www.boost.org/LICENSE_1_0.txt).  
NOTE: [src/tylang_parse.yrl](src/tylang_parse.yrl) is derived from [otp original](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_parse.yrl) and licenced under the [Apache License](http://www.apache.org/licenses/LICENSE-2.0).
