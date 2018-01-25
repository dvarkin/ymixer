-module(ymixer_rest_mix).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([allowed_methods/2]).
-export([mix_channels/2]).
-export([resource_exists/2]).

init(Req, _Opts) ->
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


mix_channels(Req, State) ->
    Method = cowboy_req:method(Req),
    Result = mix_handler(Method, State),
    Body = jiffy:encode(Result),
    {Body, Req, State}.
    


mix_handler(<<"GET">>, #{mix_id := MixID, mixer_ip := Ip, channels := Channels }) ->
    MixChannels = ymixer_api:mix_channels_state(Ip, MixID, Channels),
    [#{<<"id">> => Channel, 
       <<"on">> => true_false(Off), 
       <<"image">> => <<"https://api.adorable.io/avatars/285/channel-1.png">>}  
     || #{<<"channel">> := Channel, <<"volume">> := Off} <- MixChannels];

mix_handler(<<"POST">>, #{mix_id := MixID, mixer_ip := Ip, channels := Channels} ) ->
    ymixer_api:mix_turn_off(Ip, MixID, Channels),
    [].


true_false(-32768) -> false;
true_false(_) -> true.



