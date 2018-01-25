-module(ymixer_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/api/mix/:mix_id", ymixer_rest_mix, []},
               {"/api/mixes", ymixer_rest_mixes, []}]}

    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    ymixer_sup:start_link().

stop(_State) ->
	ok.
