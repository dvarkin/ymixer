-module(ymixer_scp_protocol).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

-export([set_channel_mix_state/3,
         get_channel_mix_state/2,
         response_channel_mix_state/1,
         set_channel_mix_level/3, 
         get_channel_mix_level/2,
         set_channel_mix_level_on/2,
         set_channel_mix_level_off/2,
         response_channel_mix_level/1
        ]).

%%% MIX API

set_channel_mix_level_off(ChannelNumber, Mix) ->
    set_channel_mix_level(ChannelNumber, Mix, -32768).


set_channel_mix_level_on(ChannelNumber, Mix) ->
    set_channel_mix_level(ChannelNumber, Mix, 0).


set_channel_mix_level(ChannelNumber, Mix, Level) ->
    mixer_api("set MIXER:Current/InCh/ToMix/Level ~p ~p ~p\n", [ChannelNumber, Mix, Level]).


get_channel_mix_level(ChannelNumber, Mix) ->
    mixer_api("get MIXER:Current/InCh/ToMix/Level ~p ~p 0\n", [ChannelNumber, Mix]).


response_channel_mix_level(Response) -> 
    <<"OK get MIXER:Current/InCh/ToMix/Level ", BinResponse/bitstring>>  = Response, 
    Cutted = mixer_parse_response(BinResponse),
    [Channel, Mix, Volume] = [list_to_integer(binary_to_list(B)) || B <- Cutted],
    #{<<"channel">> => Channel, <<"mix">> => Mix, <<"volume">> => Volume}.


get_channel_mix_state(ChannelNumber, Mix) ->
    mixer_api("get MIXER:Current/InCh/ToMix/On ~p ~p 0\n",[ChannelNumber, Mix]).


set_channel_mix_state(ChannelNumber, Mix, OnOff) when OnOff == 0 orelse OnOff == 1 ->
    mixer_api("set MIXER:Current/InCh/ToMix/On ~p ~p ~p\n",[ChannelNumber, Mix, OnOff]).


response_channel_mix_state(Response) ->
    <<"OK get MIXER:Current/InCh/ToMix/On ", BinResponse/bitstring>>  = Response, 
    Cutted = mixer_parse_response(BinResponse),
    [Channel, Mix, Volume] = [list_to_integer(binary_to_list(B)) || B <- Cutted],
    #{<<"channel">> => Channel, <<"mix">> => Mix, <<"state">> => Volume}.

%%% non api functions

string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).


mixer_api(Command = [_|_], Args) when is_list(Args) ->
    StrCommand = string_format(Command, Args),
%    io:format(StrCommand),
    erlang:list_to_binary(StrCommand).


mixer_parse_response(Value) ->
    [B, <<>>] = binary:split(Value, [<<"\n">>], [global]),
    binary:split(B, [<<" ">>], [global]).

    
