h1
=====

Library for simplifying interactions with the HackerOne API.

Build
-----

    $ rebar3 compile


Install
-------

Add to your dependencies in ```rebar.config```:

```erlang
{ deps, [
    { h1, { git, "git://github.com/wrren/h1.erl.git"} }
] }
```

Test
----

Copy ```config/test.config.example``` to ```config/test.config``` and fill in the values there, the 
included common test suite requires these configuration values in order to run.

```bash
rebar3 do compile, eunit, ct
```

Usage
-----

```erlang
Handle = h1:init( "token_id", "token_key" ),    %% Initialize a handle for use in requests
{ ok, Report } = h1:report( 12345, Handle )     %% Retrieve the report with the specified ID
```

