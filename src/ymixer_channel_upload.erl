-module(ymixer_channel_upload).

-export([init/2]).

init(Req, #{image_dir := Dir} = Opts) ->

    ChannelId = binary_to_list(cowboy_req:binding(channel_id, Req)),
    {ok, Headers, Req2} = cowboy_req:read_part(Req),
    {ok, Data, Req3} = cowboy_req:read_part_body(Req2),
    {file, _FileField, _Filename, ContentType}
        = cow_multipart:form_data(Headers),
    Req4 = case upload_handler(ContentType, Dir, ChannelId, Data) of
               ok -> cowboy_req:reply(200, #{
                                        <<"content-type">> => <<"text/plain; charset=utf-8">>
                                       }, <<"ok">>, Req3);
               {error, Some} ->
                   error_logger:error_msg("File updload error ~p~n", [Some]),
                   cowboy_req:reply(200, #{
                                      <<"content-type">> => <<"text/plain; charset=utf-8">>
                                     }, <<"Upload error">>, Req3)
           end,
    {ok, Req4, Opts}.


upload_handler(<<"image/jpeg">>, Dir, ChannelId, Data) when is_binary(Data) ->
    FileName = Dir ++ "/channel-" ++ ChannelId ++ ".jpg",
    file:write_file(FileName, Data);
upload_handler(<<"image/png">>, Dir, ChannelId, Data) when is_binary(Data) ->
    FileName = Dir ++ "/channel-" ++ ChannelId ++ ".png",
    file:write_file(FileName, Data);
upload_handler(ContentType, _Dir,  _ChannelId, _Data) ->
    {error, ContentType}.

    
