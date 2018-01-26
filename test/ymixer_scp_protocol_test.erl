-module(ymixer_scp_protocol_test).


-include_lib("eunit/include/eunit.hrl").


set_channel_mix_level_off_test() ->
    ?assertEqual(<<"set MIXER:Current/InCh/ToMix/Level 0 1 -32768\n">>, ymixer_scp_protocol:set_channel_mix_level_off(0,1)).


set_channel_mix_level_on_test() ->
    ?assertEqual(<<"set MIXER:Current/InCh/ToMix/Level 0 1 0\n">>, ymixer_scp_protocol:set_channel_mix_level_on(0,1)).


response_channel_mix_level_test(Response) -> 
    ?assertEqual(#{<<"channel">> => 0, <<"mix">> => 1, <<"volume">> => -32768}, 
                 ymixer_scp_protocol:response_channel_mix_level(<<"OK get MIXER:Current/InCh/ToMix/Level 0 1 -32768\n">>)).
                 
