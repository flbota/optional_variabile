optional_variable
=====
```
%% Optional shared variable.
%%
%% A shared variable is identified by a key:
-type key() :: term().

%% One Erlang process can set and un-set the variable:
-spec set(key(), _Value) -> ok.
-spec unset(key()) -> ok.

%% Other processes can read the variable:
-spec read(key()) -> _Value.

%% When the variable is set, `read' function returns its
%% value. Otherwise it blocks the reading process until the variable
%% is set, and then returns the value.
%%
%% Tips:
%% 0) Please feel free to use http://erlang.org/doc/search/
%% 1) You can use _any_ function from the Erlang/Elixir standard library.
%%    Bonus points are added for not reinventing the wheel.
```

An OTP application

Build
-----

    $ rebar3 compile

Run Unit tests
--------------
    $ rebar3 eunit


Start and test locally
--------------------
    $ rebar3 shell

Use the scripts from the root folder to make http request towards the app:
```
    $./http_read_variable 5
```
-open a new terminal and run a set/unset request
```
    $./http_set_variable 5 five
    $./http_unset_variable 5
```