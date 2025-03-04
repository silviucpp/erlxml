-module(erlxml_nif).
-author("silviu.caragea").

-define(NOT_LOADED, not_loaded(?LINE)).
%% Maximum bytes passed to the NIF handler at once (20Kb)
-define(MAX_BYTES_TO_NIF, 20000).

-on_load(load_nif/0).

-export([
    new_stream/1,
    chunk_feed_stream/2,
    reset_stream/1,
    dom_parse/1,
    to_binary/1
]).

%% nif functions

load_nif() ->
    ok = erlang:load_nif(get_nif_library_path(), 0).

get_nif_library_path() ->
    case code:priv_dir(erlxml) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                false ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

new_stream(_Opts) ->
    ?NOT_LOADED.

feed_stream(_Parser, _Data) ->
    ?NOT_LOADED.

reset_stream(_Parser) ->
    ?NOT_LOADED.

dom_parse(_Data) ->
    ?NOT_LOADED.

to_binary(_Data) ->
    ?NOT_LOADED.

chunk_feed_stream(Parser, Data) when is_binary(Data) ->
    chunk_feed_stream(Parser, Data, byte_size(Data), null);
chunk_feed_stream(Parser, Data) ->
    chunk_feed_stream(Parser, iolist_to_binary(Data)).

chunk_feed_stream(Parser, Data, Size, Acc) ->
    case Size > ?MAX_BYTES_TO_NIF of
        true ->
            <<Chunk:?MAX_BYTES_TO_NIF/binary, Rest/binary>> = Data,
            case feed_stream(Parser, Chunk) of
                {ok, Elements} ->
                    chunk_feed_stream(Parser, Rest, Size - ?MAX_BYTES_TO_NIF, aggregate_els(Acc, Elements));
                Error ->
                    Error
            end;
        _ ->
            case feed_stream(Parser, Data) of
                {ok, Elements} ->
                    {ok, aggregate_els(Acc, Elements)};
                Error ->
                    Error
            end
    end.

aggregate_els(null, Els) ->
    Els;
aggregate_els(Acc, Els) ->
    Els ++ Acc.
