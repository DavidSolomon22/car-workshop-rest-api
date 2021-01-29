-module(inspections_handler).

-import(inspections_service,
        [create_inspection/2,
         delete_inspection/1,
         generate_inspection_id/0,
         get_all_inspections/0,
         get_inspection/1,
         update_inspection/2]).

-import(helpers,
        [response/3, serialize_element/1, serialize_list/1]).

-behavior(cowboy_handler).

-export([init/2]).

init(Req = #{method := <<"GET">>}, State) ->
    Params = cowboy_req:parse_qs(Req),
    {Status, ResBody} = case Params of
                            [{<<"id">>, InspectionIdRaw}] ->
                                InspectionId = binary_to_list(InspectionIdRaw),
                                case get_inspection(InspectionId) of
                                    [{Id, Data}] ->
                                        {200, serialize_element({Id, Data})};
                                    [] ->
                                        {404,
                                         io_lib:format("{\"statusCode\":\"404\",\"message\":\"Inspect"
                                                       "ion with id '~s' not found.\",\"error\":\"Not "
                                                       "Found\"}",
                                                       [InspectionId])}
                                end;
                            [] ->
                                Inspections = get_all_inspections(),
                                SerializedInspections =
                                    serialize_list(Inspections),
                                {200,
                                 io_lib:format("[~s]", [SerializedInspections])}
                        end,
    Res = response(Status, ResBody, Req),
    {ok, Res, State};
init(Req = #{method := <<"POST">>}, State) ->
    Body = cowboy_req:read_urlencoded_body(Req),
    InspectionId = generate_inspection_id(),
    [Inspection] = create_inspection(Body, InspectionId),
    Res = response(201, serialize_element(Inspection), Req),
    {ok, Res, State};
init(Req = #{method := <<"PUT">>}, State) ->
    [{<<"id">>, InspectionIdRaw}] =
        cowboy_req:parse_qs(Req),
    InspectionId = binary_to_list(InspectionIdRaw),
    Body = cowboy_req:read_urlencoded_body(Req),
    [{Id, Data}] = update_inspection(InspectionId, Body),
    Res = response(200, serialize_element({Id, Data}), Req),
    {ok, Res, State};
init(Req = #{method := <<"DELETE">>}, State) ->
    [{<<"id">>, InspectionId}] = cowboy_req:parse_qs(Req),
    delete_inspection(binary_to_list(InspectionId)),
    {ok, response(204, <<"">>, Req), State};
init(Req, State) ->
    Res = cowboy_req:reply(405, #{}, Req),
    {ok, Res, State}.
