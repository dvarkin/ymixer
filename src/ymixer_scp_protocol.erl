-module(ymixer_scp_protocol).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

-export([info/0, 
         channel_on/1, channel_off/1, 
         channel_to_mix_on/2, channel_to_mix_off/2,
         channel_level/2,
         mixer_api/2
        ]).

%% API funcs
-spec info_product_name() -> {ok, binary()}.
info_product_name() ->
    mixer_api("devinfo productname\n", []).

-spec info_model_name() -> {ok, binary()}.
info_model_name() ->
    mixer_api("devinfo modelname\n", []).

-spec info_input_port_count() -> {ok, binary()}.
info_input_ports_count() ->-spec info_product_name() -> {ok, binary()}.
     mixer_api("devinfo inputport\n", []).

-spec info_mix_bus_count() -> {ok, binary()}.
info_mix_bus_count() ->
    mixer_api("devinfo mixbus\n", []).

    
-spec channel_on(ChannelNumber :: pos_integer()) -> {ok, binary()};
channel_on(ChannelNumber) ->
    mixer_api("set MIXER:Current/InCh/Fader/On ~p 0 1\n", [ChaannelNumber]).

-spec channel_off(ChannelNumber :: pos_integer()) -> {ok, binary()};
channel_off(ChannelNumber) ->
    mixer_api("set MIXER:Current/InCh/Fader/On ~p 0 0\n", [ChannelNumber]).

-spec channel_to_mix_on(ChannelNumber :: pos_integer(), Mix :: pos_integer()) -> {ok, binary()};
channel_to_mix_on(ChannelNumber, Mix) ->
    mixer_api("set MIXER:Current/InCh/ToMix/On ~p ~p 1\n", [ChannelNumber, Mix]).

-spec channel_to_mix_off(ChannelNumber :: pos_integer(), Mix :: pos_integer()) -> {ok, binary()};
channel_to_mix_off(ChannelNumber, Mix) ->
    mixer_api("set MIXER:Current/InCh/ToMix/On ~p ~p 0\n", [ChannelNumber, Mix]).

-spec channel_level(ChannelNumber :: pos_integer(), Level :: pos_integer()) -> {ok, binary()};
channel_level(ChannelNumber, Level) when Level >= -32768 andalso Level =< 1000 ->
    mixer_api("set MIXER:Current/InCh/Fader/On ~p 0 ~p\n", [ChannelNumber, Level]).

%%% non api functions

string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

mixer_api(Command = [_|_], Args) when is_list(Args) ->
    StrCommand = string_format(Command, Args),
    io:format(StrCommand),
    {ok, erlang:list_to_binary(StrCommand)}.
