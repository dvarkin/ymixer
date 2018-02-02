%%%-------------------------------------------------------------------
%%% @author Dmitry Omelechko <dvarkin@gmail.com>
%%% @copyright (C) 2018, Dmitry Omelechko
%%% @doc
%%%
%%% @end
%%% Created : 29 Jan 2018 by Dmitry Omelechko <dvarkin@gmail.com>
%%%-------------------------------------------------------------------
-module(ymixer_command_queue).

-behaviour(gen_server).

%% API
-export([start_link/1, set/1, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {commands, mixer_ip, timeout}).

%%%===================================================================
%%% API
%%%===================================================================

set(Cmd) when is_binary(Cmd) ->
    gen_server:call(?SERVER, {run, Cmd}).

get(Cmd) when is_binary(Cmd) -> 
    gen_server:call(?SERVER, {get, Cmd}).



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

init(#{mixer_ip := MixerIP, timeout := Timeout}) ->
    process_flag(trap_exit, true),
    {ok, #state{commands = [], mixer_ip = MixerIP, timeout = Timeout}}.

handle_call({run, Cmd}, _From, #state{commands = Commands} =  State) ->
    self() ! run,
    {reply, ok, State#state{commands = Commands ++ [Cmd]}};

handle_call({get, Cmd}, _From, #state{mixer_ip = MixerIP} =  State) ->
    Result = ymixer_tcp:send(MixerIP, Cmd),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(run, #state{mixer_ip = MixerIP, commands = Commands, timeout = Timeout} = State) ->
    Commands1 =run_commands(MixerIP, Timeout, Commands),
    {noreply, State#state{commands = Commands1}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


run_commands(_MixerIP, _Timeout, []) ->
    [];
run_commands(MixerIP, Timeout, [Command |  Tail]) ->
    error_logger:info_msg("run command ~p with timeout: ~p", [Command, Timeout]),
    ymixer_tcp:send(MixerIP, Command),
    timer:sleep(Timeout),
    run_commands(MixerIP, Timeout, Tail).

        
