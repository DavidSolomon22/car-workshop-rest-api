-module(helpers).

-export([response/3,
         serialize_element/1,
         serialize_list/1]).

serialize_element({_, Content}) ->
    Mapper = fun (Field, Value, Acc) ->
                     Json = io_lib:format("\"~s\": ~p,", [Field, Value]),
                     [Json] ++ Acc
             end,
    OfferFields = maps:fold(Mapper, [], Content),
    lists:flatten(io_lib:format("{~s},", [OfferFields])).

serialize_list(List) ->
    lists:foldl(fun (Element, Acc) ->
                        [serialize_element(Element) | Acc]
                end,
                [],
                List).

response(Status, Body, Req) ->
    cowboy_req:reply(Status,
                     #{<<"content-type">> => <<"application/json">>},
                     Body,
                     Req).
