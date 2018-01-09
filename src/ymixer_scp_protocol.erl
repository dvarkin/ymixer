-module(ymixer_scp_protocol).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

-export([channel_on/1, channel_off/1, channel_to_mix_off/2, channel_to_mix_on/2, status/0, mixer_api/2]).



string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

mixer_api(Command = [_|_], Args) when is_list(Args) ->
    StrCommand = string_format(Command, Args),
    io:format(StrCommand),
    erlang:list_to_binary(StrCommand).

status() ->
    mixer_api("devstatus productname", []).

channel_on(ChannelNumber) ->
    mixer_api("set MIXER:Current/InCh/Fader/On ~p 0 1", [ChannelNumber]).

channel_off(ChannelNumber) ->
    mixer_api("set MIXER:Current/InCh/Fader/On ~p 0 0", [ChannelNumber]).

channel_to_mix_on(ChannelNumber, Mix) ->
    mixer_api("MIXER:Current/InCh/ToMix/On ~p ~p 1", [ChannelNumber, Mix]).

channel_to_mix_off(ChannelNumber, Mix) ->
    mixer_api("MIXER:Current/InCh/ToMix/On ~p ~p 0", [ChannelNumber, Mix]).

