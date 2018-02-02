%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2018, Dmitry Omelechko
%%% @doc
%%% API for interact with Ymaha CL series mixers
%%% @end
%%% Created : 24 Jan 2018 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(ymixer_api).

-export([mixes/0, mix_channels_state/3, mix_turn_off/3]).

-export([channel_switch_on/3, channel_switch_off/3]).

mixes() ->
    application:get_env(ymixer, mixes).

mix_channels_state(_MixerIP, Mix, Channels) ->
    ChannelVolumeCommands = [Command || Command <- [ymixer_scp_protocol:get_channel_mix_level(Channel, Mix) || Channel <- Channels]],
    RawResponseVolume = [R || {ok, R} <- [ymixer_command_queue:get(Command) || Command <- ChannelVolumeCommands]],
    Volumes = [ymixer_scp_protocol:response_channel_mix_level(R) || R <- RawResponseVolume],
    
    ChannelStateCommands = [Command || Command <- [ymixer_scp_protocol:get_channel_mix_state(Channel, Mix) || Channel <- Channels]],
    RawResponseState = [R || {ok, R} <- [ymixer_command_queue:get(Command) || Command <- ChannelStateCommands]],
    States = [ymixer_scp_protocol:response_channel_mix_state(R) || R <- RawResponseState],
  
    lists:zipwith(fun maps:merge/2, Volumes, States).

mix_turn_off(Ip, MixID, Channels) ->
    [channel_mix_turn_off(Ip, MixID, Channel) || Channel <- Channels].

channel_mix_turn_off(_Ip, MixID, Channel) ->
    OnCmd = ymixer_scp_protocol:set_channel_mix_state(Channel, MixID, 1),
    VolCmd = ymixer_scp_protocol:set_channel_mix_level_off(Channel, MixID),
    ymixer_command_queue:set(OnCmd),
    ymixer_command_queue:set(VolCmd).


channel_switch_on(_Ip, MixID, Channel) ->
    VolCmd = ymixer_scp_protocol:set_channel_mix_level_on(Channel, MixID),
    ymixer_command_queue:set(VolCmd).
    

channel_switch_off(_Ip, MixID, Channel) ->
    VolCmd = ymixer_scp_protocol:set_channel_mix_level_off(Channel, MixID),
    ymixer_command_queue:set(VolCmd).
    
    

