%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2018, Dmitry Omelechko
%%% @doc
%%% API for interact with Ymaha CL series mixers
%%% @end
%%% Created : 24 Jan 2018 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(ymixer_api).

-export([mixes/0, mix_channels_state/3]).

mixes() ->
    application:get_env(ymixer, mixes).

mix_channels_state(MixerIP, Mix, Channels) ->
    ChannelVolumeCommands = [Command || Command <- [ymixer_scp_protocol:get_channel_mix_level(Mix, Channel) || Channel <- Channels]],
    RawResponseVolume = [R || {ok, R} <- [ymixer_tcp:send(MixerIP, Command) || Command <- ChannelVolumeCommands]],
    Volumes = [ymixer_scp_protocol:response_channel_mix_level(R) || R <- RawResponseVolume],
    
    ChannelStateCommands = [Command || Command <- [ymixer_scp_protocol:get_channel_mix_state(Mix, Channel) || Channel <- Channels]],
    RawResponseState = [R || {ok, R} <- [ymixer_tcp:send(MixerIP, Command) || Command <- ChannelStateCommands]],
    States = [ymixer_scp_protocol:response_channel_mix_state(R) || R <- RawResponseState],
  
    lists:zipwith(fun maps:merge/2, Volumes, States).
    
    

