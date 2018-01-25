-module(ymixer_rest_mix).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([mix_channels/2]).
-export([resource_exists/2]).

init(Req, Opts) ->
    io:format("init opts ~p", [Opts]),
    {ok, Ip} = application:get_env(ymixer, mixer_ip),
    {ok, Channels} = application:get_env(ymixer, channels),
    {cowboy_rest, Req, #{mixer_ip => Ip, channels => Channels}}.


allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.


content_types_provided(Req, State) ->
    {[
      {<<"application/json">>, mix_channels}
     ], Req, State}.


resource_exists(Req, State) ->
    MixIdBinary = cowboy_req:binding(mix_id, Req),
    case list_to_integer(binary_to_list(MixIdBinary)) of
        MixID when MixID >= 0 andalso MixID =< 5 ->
            {true, Req, State#{mix_id => MixID}};
        _MixID ->
            {false, Req, State}
    end.


mix_channels(Req, #{mix_id := MixID, mixer_ip := Ip, channels := Channels } = State) ->
    
    MixChannels = ymixer_api:mix_channels_state(Ip, MixID, Channels),
    Body = jiffy:encode(MixChannels),
    {Body, Req, State}.
