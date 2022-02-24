-module(optional_variable).
-behaviour(gen_server).

% API
-export([ set/2
        , unset/1
        , read/1
        ]).

-export([ start_link/0 ]).

% callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , terminate/2]).

-type key() :: term().


% API
-spec set(key(), term()) -> ok.
set(Key, Value) ->
    gen_server:cast(?MODULE, {set, Key, Value}),
    ok.

-spec unset(key()) -> ok.
unset(Key) ->
    gen_server:cast(?MODULE, {unset, Key}),
    ok.

-spec read(key()) -> term().
read(Key) ->
    gen_server:call(?MODULE, {read, Key}, infinity).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% callbacks
init(_Args) ->
    StoredValues = maps:new(),
    AwaitingClients = maps:new(),
    {ok, {StoredValues, AwaitingClients}}.

handle_cast({set, Key, Value}, {StoredValues0, AwaitingClients0}) ->
    AwaitingClients = maybe_reply(Key, Value, AwaitingClients0),
    StoredValues = maps:put(Key, Value, StoredValues0),
    {noreply, {StoredValues, AwaitingClients}};
handle_cast({unset, Key}, {StoredValues0, AwaitingClients}) ->
    StoredValues = maps:remove(Key, StoredValues0),
    {noreply, {StoredValues, AwaitingClients}}.

handle_call({read, Key}, From, {StoredValues, AwaitingClients0}=State) ->
    case maps:get(Key, StoredValues, undefined) of
        undefined ->
            AwaitingClients = add_waiting_client(Key, From, AwaitingClients0),
            {noreply, {StoredValues, AwaitingClients}, infinity};
        Value ->
            {reply, Value, State}
    end.

terminate(_Reason, {_, AwaitingClients}) ->
    Clients = lists:flatten(maps:values(AwaitingClients)),
    lists:foreach(fun(Client) -> gen_server:reply(Client, server_down) end, Clients),
    ok.

% internal
-spec maybe_reply(term(), term(), map()) -> map().
maybe_reply(Key, Value, AwaitingClients) ->
    case maps:get(Key, AwaitingClients, undefined) of
        undefined ->
            AwaitingClients;
        Clients ->
            lists:foreach(fun (Client) -> gen_server:reply(Client, Value) end, Clients),
            maps:remove(Key, AwaitingClients)
    end.

-spec add_waiting_client(term(), {pid(), reference()}, map()) -> map().
add_waiting_client(Key, Client, AwaitingClients) ->
    case maps:get(Key, AwaitingClients, undefined) of
        undefined ->
            maps:put(Key, [Client], AwaitingClients);
        AwaitingKey ->
            maps:put(Key, [Client | AwaitingKey], AwaitingClients)
    end.

