%%%-------------------------------------------------------------------
%% @doc optional_variable public API
%% @end
%%%-------------------------------------------------------------------

-module(optional_variable_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([ {'_', routes() } ]),
    {ok, _} = cowboy:start_clear( my_http_listener
							 	, [{port, 8080}]
								, #{env => #{dispatch => Dispatch}}
								),
	optional_variable_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
routes() ->
	[ {"/:key", http_handler, [get_value]}
	, {"/set/:key/:value", http_handler, [set]}
	, {"/unset/:key", http_handler, [unset]}
	].

