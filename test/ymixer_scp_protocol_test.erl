-module(ymixer_scp_protocol_test).

-include_lib("eunit/include/eunit.hrl").

make_command_test() ->
    ?assertEqual(<<"Command 1 2 3">>, ymixer_scp_protocol:mixer_api("Command ~p ~p ~p", [1,2,3])).
