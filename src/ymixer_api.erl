%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2018, Dmitry Omelechko
%%% @doc
%%% API for interact with Ymaha CL series mixers
%%% @end
%%% Created : 24 Jan 2018 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(ymixer_api).

-export([mixes/3]).

mixes(MixerIP, Mixes, Channels) ->
    StatusCommands = [Command || {ok, Command} <- [ymixer_scp_protocol:get_channel_mix_level(Mix, Channel) ||  Mix <- Mixes, Channel <- Channels]],
    [ymixer_tcp:send(MixerIP, Command) || Command <- StatusCommands].
    
    

