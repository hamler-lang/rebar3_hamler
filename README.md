rebar3_hamler
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_hamler, {git, "https://host/user/rebar3_hamler.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_hamler
    ===> Fetching rebar3_hamler
    ===> Compiling rebar3_hamler
    <Plugin Output>
