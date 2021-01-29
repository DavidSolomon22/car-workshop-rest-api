-module(inspections_service).

-import(inspections_repository,
        [create_one/2,
         delete_one/1,
         generate_id/0,
         get_all/0,
         get_one/1]).

-export([create_inspection/2,
         delete_inspection/1,
         generate_inspection_id/0,
         get_all_inspections/0,
         get_inspection/1,
         update_inspection/2]).

delete_inspection(InspectionId) ->
    delete_one(InspectionId).

create_inspection(Body, InspectionId) ->
    Inspection = create_inspection_from_req_body(Body,
                                                 InspectionId),
    ok = create_one(InspectionId, Inspection),
    get_one(InspectionId).

get_inspection(InspectionId) -> get_one(InspectionId).

create_inspection_from_req_body(Body, InspectionId) ->
    {ok,
     [{<<"reservationId">>, ReservationId},
      {<<"servicePerformed">>, ServicePerformed},
      {<<"serviceCost">>, ServiceCost}],
     _} =
        Body,
    #{id => InspectionId,
      reservationId => binary_to_list(ReservationId),
      servicePerformed => binary_to_list(ServicePerformed),
      serviceCost => binary_to_list(ServiceCost)}.

update_inspection(InspectionId, Body) ->
    delete_one(InspectionId),
    create_inspection(Body, InspectionId).

generate_inspection_id() -> generate_id().

get_all_inspections() -> get_all().
