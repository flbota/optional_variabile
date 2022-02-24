-module(optional_variable_test).

-include_lib("eunit/include/eunit.hrl").

-define(T(Function), {??Function, Function}).

-define(KEY, my_key).
-define(VALUE, my_value).
-define(KEY2, my_key2).

setup() ->
    {ok, Pid} = optional_variable:start_link(),
    Pid.

teardown(Pid) ->
    flush(),
    gen_server:stop(Pid).

optional_variable_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [ ?T(fun test_server_registration/0)
     , ?T(fun test_set_read_variable/0)
     , ?T(fun test_unset_variable/0)
     , ?T(fun test_client_waits_unset_variable/0)
     ]
    }.

test_server_registration() ->
    Pid = whereis(optional_variable),
    ?assert(erlang:is_process_alive(Pid)).

test_set_read_variable() ->
    ok = optional_variable:set(?KEY, ?VALUE),
    ?assertEqual(?VALUE, optional_variable:read(?KEY)).

test_unset_variable() ->
    ok = optional_variable:set(?KEY, ?VALUE),
    ok = optional_variable:unset(?KEY),
    {_Ref, Client} = spawn_read_process(?KEY),
    timer:sleep(50),
    ?assert(erlang:is_process_alive(Client)).

test_client_waits_unset_variable() ->
    {Ref, Client0} = spawn_read_process(?KEY),
    ?assert(erlang:is_process_alive(Client0)),
    Client = spawn(fun () -> optional_variable:read(?KEY2) end),
    ?assert(erlang:is_process_alive(Client)),
    optional_variable:set(?KEY, ?VALUE),
    timer:sleep(50),
    ?assertNot(erlang:is_process_alive(Client0)),
    ?assertEqual({Ref, ?VALUE}, receive V -> V end),
    ?assert(erlang:is_process_alive(Client)).

%internal
spawn_read_process(ReadKey) ->
    TestPid = self(),
    Ref = erlang:make_ref(),
    Pid = spawn(fun () -> TestPid  ! {Ref, optional_variable:read(ReadKey)} end),
    {Ref, Pid}.

flush() ->
    receive
        _ -> flush()
    after 0 ->
        ok
    end.