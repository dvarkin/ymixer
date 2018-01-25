-module(ymixer_rest_mix).
-behavior(cowboy_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
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

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, []}, mix_channels}],
         Req, State}.


resource_exists(Req, State) ->
    MixIdBinary = cowboy_req:binding(mix_id, Req),
    case list_to_integer(binary_to_list(MixIdBinary)) of
        MixID when MixID >= 0  ->
            {true, Req, State#{mix_id => MixID}};
        _MixID ->
            {false, Req, State}
    end.


mix_channels(Req, State) ->
    Method = cowboy_req:method(Req),
    mix_handler(Method, Req, State).


mix_handler(<<"GET">>, Req,  #{mix_id := MixID, mixer_ip := Ip, channels := Channels } = State) ->
    MixChannels = ymixer_api:mix_channels_state(Ip, MixID, Channels),
    Result = [#{<<"id">> => Channel, 
       <<"on">> => true_false(Off), 
       <<"image">> => <<"https://api.adorable.io/avatars/285/channel-1.png">>}  
              || #{<<"channel">> := Channel, <<"volume">> := Off} <- MixChannels],
    {jiffy:encode(Result), Req, State};


mix_handler(<<"POST">>, Req, #{mix_id := MixID, mixer_ip := Ip, channels := Channels} = State) ->
    ymixer_api:mix_turn_off(Ip, MixID, Channels),
    Req1 = cowboy_req:set_resp_body(<<"ok">>, Req),
    {true, Req1, State};

mix_handler(_, _Req,  _State) ->
    false.


true_false(-32768) -> false;
true_false(_) -> true.



