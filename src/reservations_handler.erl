-module(reservations_handler).

-import(reservations_service,
        [create_reservation/2,
         delete_reservation/1,
         generate_reservation_id/0,
         get_all_reservations/0,
         get_reservation/1,
         update_reservation/2]).

-import(helpers,
        [response/3, serialize_element/1, serialize_list/1]).

-behavior(cowboy_handler).

-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    Params = cowboy_req:parse_qs(Req),
    {Status, ResBody} = case Params of
                            [{<<"id">>, ReservationIdRaw}] ->
                                ReservationId =
                                    binary_to_list(ReservationIdRaw),
                                case get_reservation(ReservationId) of
                                    [{Id, Data}] ->
                                        {200, serialize_element({Id, Data})};
                                    [] ->
                                        {404,
                                         io_lib:format("{\"statusCode\":\"404\",\"message\":\"Reserva"
                                                       "tion with id '~s' not found.\",\"error\":\"No"
                                                       "t Found\"}",
                                                       [ReservationId])}
                                end;
                            [] ->
                                Reservations = get_all_reservations(),
                                SerializedReservations =
                                    serialize_list(Reservations),
                                {200,
                                 io_lib:format("[~s]",
                                               [SerializedReservations])}
                        end,
    Res = response(Status, ResBody, Req),
    {ok, Res, State};
init(Req = #{method := <<"POST">>}, State) ->
    Body = cowboy_req:read_urlencoded_body(Req),
    ReservationId = generate_reservation_id(),
    {Status, ResBody} = case create_reservation(Body,
                                                ReservationId)
                            of
                            wrongEndDate ->
                                {400,
                                 io_lib:format("{\"statusCode\":\"400\",\"message\":\"Paramet"
                                               "er 'endDate' must be greater than parameter "
                                               "'startDate'.\",\"error\":\"Bad Request\"}",
                                               [])};
                            dateAlreadyReserved ->
                                {400,
                                 io_lib:format("{\"statusCode\":\"400\",\"message\":\"Reserva"
                                               "tion for this date already exists.\",\"error\""
                                               ":\"Bad Request\"}",
                                               [])};
                            [{Id, Data}] -> {201, serialize_element({Id, Data})}
                        end,
    Res = response(Status, ResBody, Req),
    {ok, Res, State};
init(Req = #{method := <<"PUT">>}, State) ->
    [{<<"id">>, ReservationIdRaw}] =
        cowboy_req:parse_qs(Req),
    ReservationId = binary_to_list(ReservationIdRaw),
    Body = cowboy_req:read_urlencoded_body(Req),
    [{Id, Data}] = update_reservation(ReservationId, Body),
    Res = response(200, serialize_element({Id, Data}), Req),
    {ok, Res, State};
init(Req = #{method := <<"DELETE">>}, State) ->
    [{<<"id">>, ReservationId}] = cowboy_req:parse_qs(Req),
    delete_reservation(binary_to_list(ReservationId)),
    {ok, response(204, <<"">>, Req), State};
init(Req, State) ->
    Res = cowboy_req:reply(405, #{}, Req),
    {ok, Res, State}.
