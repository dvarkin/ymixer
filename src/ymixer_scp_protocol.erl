-module(ymixer_scp_protocol).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

-export([get_product_name/0, get_model_name/0, get_input_ports_count/0, get_mix_bus_count/0, 
         set_channel_on/1, set_channel_off/1, 
         set_channel_to_mix_on/2, set_channel_to_mix_off/2,
         set_channel_level/2,
         get_mix_name/1,
         set_channel_mix_level/3, get_channel_mix_level/2
        ]).

%% API funcs
-spec get_product_name() -> {ok, binary()}.
get_product_name() ->
    mixer_api("devinfo productname\n", []).

-spec get_model_name() -> {ok, binary()}.
get_model_name() ->
    mixer_api("devinfo modelname\n", []).

-spec get_input_ports_count() -> {ok, binary()}.
get_input_ports_count() ->
     mixer_api("devinfo inputport\n", []).

-spec get_mix_bus_count() -> {ok, binary()}.
get_mix_bus_count() ->
    mixer_api("devinfo mixbus\n", []).

%%% Channels API
    
-spec set_channel_on(ChannelNumber :: pos_integer()) -> {ok, binary()}.
set_channel_on(ChannelNumber) ->
    mixer_api("set MIXER:Current/InCh/Fader/On ~p 0 1\n", [ChannelNumber]).

-spec set_channel_off(ChannelNumber :: pos_integer()) -> {ok, binary()}.
set_channel_off(ChannelNumber) ->
    mixer_api("set MIXER:Current/InCh/Fader/On ~p 0 0\n", [ChannelNumber]).

-spec set_channel_level(ChannelNumber :: pos_integer(), Level :: pos_integer()) -> {ok, binary()}.
set_channel_level(ChannelNumber, Level) when Level >= -32768 andalso Level =< 1000 ->
    mixer_api("set MIXER:Current/InCh/Fader/On ~p 0 ~p\n", [ChannelNumber, Level]).

%%% MIX API

get_mix_name(Get_MixNumber) ->
    mixer_api("get ", [Get_MixNumber]).

-spec set_channel_to_mix_on(ChannelNumber :: pos_integer(), Mix :: pos_integer()) -> {ok, binary()}.
set_channel_to_mix_on(ChannelNumber, Mix) ->
    mixer_api("set MIXER:Current/InCh/ToMix/On ~p ~p 1\n", [ChannelNumber, Mix]).

-spec set_channel_to_mix_off(ChannelNumber :: pos_integer(), Mix :: pos_integer()) -> {ok, binary()}.
set_channel_to_mix_off(ChannelNumber, Mix) ->
    mixer_api("set MIXER:Current/InCh/ToMix/On ~p ~p 0\n", [ChannelNumber, Mix]).

-spec set_channel_mix_level(ChannelNumber :: pos_integer(), Mix :: pos_integer(), Level :: pos_integer()) -> {ok, binary()}.
set_channel_mix_level(ChannelNumber, Mix, Level) ->
    mixer_api("set MIXER:Current/InCh/ToMix/Level ~p ~p ~p\n", [ChannelNumber, Mix, Level]).

-spec get_channel_mix_level(ChannelNumber :: pos_integer(), Mix :: pos_integer()) -> {ok, binary()}.
get_channel_mix_level(ChannelNumber, Mix) ->
    mixer_api("get MIXER:Current/InCh/ToMix/Level ~p ~p 0\n", [ChannelNumber, Mix]).


%%% non api functions

string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

-spec mixer_api(Command :: string(), Args::list()) -> {ok, binary()}.
mixer_api(Command = [_|_], Args) when is_list(Args) ->
    StrCommand = string_format(Command, Args),
    io:format(StrCommand),
    {ok, erlang:list_to_binary(StrCommand)}.
