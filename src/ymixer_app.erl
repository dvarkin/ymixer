-module(ymixer_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    {ok, Image_dir} = application:get_env(ymixer, image_dir),
    Dispatch = cowboy_router:compile([
        {'_', [{"/api/mix/:mix_id", ymixer_rest_mix, []},
               {"/api/mixes", ymixer_rest_mixes, []},
               {"/api/channel/switch/:mix_id/:channel_id/:on_off", ymixer_rest_channel, []},
               {"/api/channel/upload/:channel_id", ymixer_channel_upload,#{image_dir =>  Image_dir}}
              ]}
              
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    ymixer_sup:start_link().

stop(_State) ->
	ok.
