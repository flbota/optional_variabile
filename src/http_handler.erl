-module(http_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, [set]=State) ->
    #{key := Key, value := Value } = cowboy_req:bindings(Req0),
    ok = optional_variable:set(Key, Value),
    {ok, reply(Req0, <<"ok">>), State};
init(Req0, [unset]=State) ->
    #{ key := Key } = cowboy_req:bindings(Req0),
    ok = optional_variable:unset(Key),
    {ok, reply(Req0, <<"ok">>), State};
init(Req0, [get_value]=State) ->
    #{key := Key } = cowboy_req:bindings(Req0),
    Value = optional_variable:read(Key),
    {ok, reply(Req0, Value), State}.

reply(Req, Res) ->
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Res, Req).