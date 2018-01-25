-module(rest_handler).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([mixes/2]).
-export([mix_channels/2]).
-export([resource_exists/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
%		{<<"application/json">>, mixes},
          	{<<"application/json">>, mix_channels}
         ], Req, State}.

resource_exists(Req, _State) ->
    io:format("~p", [cowboy_req:bindings(Req)]),
    {true, Req, _State}.

mixes(Req, State) ->
    {ok, Mixes} = ymixer_api:mixes(),
    Body = jiffy:encode(Mixes),
    {Body, Req, State}.

mix_channels(Req, State) ->
    {ok, Mixes} = ymixer_api:mixes(),
    Body = jiffy:encode(Mixes),
    {Body, Req, State}.
