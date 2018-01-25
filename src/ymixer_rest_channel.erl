-module(ymixer_rest_channel).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([channel_handler/2]).

init(Req, _Opts) ->
    {ok, Ip} = application:get_env(ymixer, mixer_ip),
    {cowboy_rest, Req, #{mixer_ip => Ip}}.


allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.


content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, channel_handler}],
         Req, State}.


resource_exists(Req, State) ->
    ChannelIdBinary = cowboy_req:binding(channel_id, Req),
    MixIdBinary = cowboy_req:binding(mix_id, Req),
    OnOffBinary = cowboy_req:binding(on_off, Req),
    MixId = list_to_integer(binary_to_list(MixIdBinary)),
    ChannelId = list_to_integer(binary_to_list(ChannelIdBinary)),
    mixes_channels(MixId, ChannelId, OnOffBinary, Req, State).

mixes_channels(MixId, ChannelID, OnOff, Req, State) when MixId >= 0  andalso ChannelID >= 0 ->
    {true, Req, State#{channel_id => ChannelID, mix_id => MixId, on_off => OnOff}};
mixes_channels(_MixId, _ChannelId, _OnfOff, Req, State) ->
    {false, Req, State}.


channel_handler(Req, #{mixer_ip := Ip, channel_id := ChannelId, mix_id := MixId, on_off := <<"on">>} = State) ->
    ymixer_api:channel_switch_on(Ip, MixId, ChannelId),
    Req1 = cowboy_req:set_resp_body(<<"ok">>, Req),
    {true, Req1, State};

channel_handler(Req, #{mixer_ip := Ip, channel_id := ChannelId, mix_id := MixId, on_off := <<"off">>} = State) ->
    ymixer_api:channel_switch_off(Ip, MixId, ChannelId),
    Req1 = cowboy_req:set_resp_body(<<"ok">>, Req),
    {true, Req1, State};

channel_handler(Req, State) ->
    {false, Req, State}.

