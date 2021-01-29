-module(inspections_repository).

-export([create_one/2,
         delete_one/1,
         generate_id/0,
         get_all/0,
         get_one/1]).

delete_one(InspectionId) ->
    {ok, Recordfilename} =
        application:get_env(car_workshop_rest_api,
                            inspections_records_file_name),
    {ok, _} = dets:open_file(records_db,
                             [{file, Recordfilename}, {type, set}]),
    Query = fun () -> dets:delete(records_db, InspectionId)
            end,
    Response = Query(),
    ok = dets:close(records_db),
    Response.

generate_id() ->
    {ok, Statefilename} =
        application:get_env(car_workshop_rest_api,
                            inspections_state_file_name),
    dets:open_file(state_db,
                   [{file, Statefilename}, {type, set}]),
    Records = dets:lookup(state_db, current_id),
    Response = case Records of
                   [{current_id, CurrentId}] ->
                       NextId = CurrentId + 1,
                       dets:insert(state_db, {current_id, NextId}),
                       lists:flatten(io_lib:format("id_~4..0B", [CurrentId]));
                   [] -> error
               end,
    dets:close(state_db),
    Response.

create_one(InspectionId, Inspection) ->
    {ok, Recordfilename} =
        application:get_env(car_workshop_rest_api,
                            inspections_records_file_name),
    {ok, _} = dets:open_file(records_db,
                             [{file, Recordfilename}, {type, set}]),
    Query = fun () ->
                    ok = dets:insert(records_db,
                                     {InspectionId, Inspection}),
                    dets:sync(records_db)
            end,
    Response = Query(),
    ok = dets:close(records_db),
    Response.

get_one(InspectionId) ->
    {ok, Recordfilename} =
        application:get_env(car_workshop_rest_api,
                            inspections_records_file_name),
    {ok, _} = dets:open_file(records_db,
                             [{file, Recordfilename}, {type, set}]),
    Query = fun () -> dets:lookup(records_db, InspectionId)
            end,
    Inspection = Query(),
    ok = dets:close(records_db),
    Inspection.

get_all() ->
    {ok, Recordfilename} =
        application:get_env(car_workshop_rest_api,
                            inspections_records_file_name),
    {ok, _} = dets:open_file(records_db,
                             [{file, Recordfilename}, {type, set}]),
    Query = fun () ->
                    dets:foldl(fun (Reservation, Acc) -> [Reservation | Acc]
                               end,
                               [],
                               records_db)
            end,
    Inspections = Query(),
    ok = dets:close(records_db),
    Inspections.
