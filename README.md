rebar3_hamler
=============

A rebar plugin that enables Erlang projects working along with [Hamler](https://github.com/hamler-lang/hamler) code.

This plugin support both Hamler dependencies and Hamler apps in a Erlang umbrella project.

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

Optionally you could suppress all the `hamler provider_hooks` by provide
an `overrides` section, in case they dependents on this plugin too:

    {overrides, [
        {del, [
            {provider_hooks, [
                {pre, [{compile, {hamler, compile}}]}
            ]}
        ]}
    ]}.

And add the dependency written in Hamler that to be added to your Erlang project:

    {deps, [
        {json, {hamler_git, "https://github.com/hamler-lang/json", {branch, "master"}}}
    ]}.

Where `https://github.com/hamler-lang/json` is a JSON parser written in Hamler, we use it for demo here.

**Note that the source type of the dependency must be `hamler_git`**.

Then just `rebar3 compile` or `release` as usual, all Hamler apps/deps will be compiled before compiling the Erlang project:

    $ rebar3 compile

    $ rebar3 release

Or you could run `hamler compile` manually:

    $ rebar3 hamler compile

The `rebar shell` is also supported:

    $ rebar3 shell

You could even start the Hamler's REPL by running:

    $ rebar3 hamler repl

Example Project
---------------

You could checkout the example project [here](https://github.com/terry-xiaoyu/tjson).
