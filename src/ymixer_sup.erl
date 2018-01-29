-module(ymixer_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

    {ok, MixerIP} = application:get_env(ymixer, mixer_ip),
    {ok, CommandTimeout} = application:get_env(ymixer, command_send_timeout),

    Queue = #{
        id => ymixer_command_queue,
        start => {ymixer_command_queue, start_link, [#{mixer_ip => MixerIP, timeout => CommandTimeout}]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [ymixer_command_queue]
    },
    Procs = [Queue],
    {ok, {{one_for_one, 1, 5}, Procs}}.
