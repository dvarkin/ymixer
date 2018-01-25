-module(ymixer_rest_mixes).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([mixes/2]).

init(Req, _Opts) ->
    {ok, Mixes} = application:get_env(ymixer, mixes),
    {cowboy_rest, Req, #{mixes => Mixes}}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, mixes}
     ], Req, State}.

mixes(Req, #{mixes := Mixes} = State) ->
    Body = jiffy:encode(Mixes),
    {Body, Req, State}.
