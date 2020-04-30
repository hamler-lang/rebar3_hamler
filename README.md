rebar3_hamler
=====

A rebar plugin that enables Erlang projects working along with [Hamler](https://github.com/hamler-lang/hamler) code.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
            {rebar3_hamler, {git, "https://github.com/hamler-lang/rebar3_hamler", {branch, "master"}}}
        ]}.

    {provider_hooks, [
        {pre, [{compile, {hamler, compile}}]}
    ]}.

Then just call your plugin directly in an existing application:

    $ rebar3 hamler compile
