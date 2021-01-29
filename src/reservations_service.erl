-module(reservations_service).

-import(reservations_repository,
        [create_one/2,
         delete_one/1,
         generate_id/0,
         get_all/0,
         get_one/1]).

-export([create_reservation/2,
         delete_reservation/1,
         generate_reservation_id/0,
         get_all_reservations/0,
         get_reservation/1,
         get_reservation_from_req_body/2,
         update_reservation/2]).

delete_reservation(ReservationId) ->
    delete_one(ReservationId).

create_reservation(Body, ReservationId) ->
    Reservation = get_reservation_from_req_body(Body,
                                                ReservationId),
    StartDate = maps:get(startDate, Reservation),
    EndDate = maps:get(endDate, Reservation),
    case check_reservation_possibility(StartDate, EndDate)
        of
        wrongEndDate -> wrongEndDate;
        dateAlreadyReserved -> dateAlreadyReserved;
        ok ->
            ok = create_one(ReservationId, Reservation),
            get_one(ReservationId)
    end.

get_reservation(ReservationId) ->
    get_one(ReservationId).

get_all_reservations() -> get_all().

update_reservation(ReservationId, Body) ->
    delete_one(ReservationId),
    create_reservation(Body, ReservationId).

get_reservation_from_req_body(Body, ReservationId) ->
    {ok,
     [{<<"startDate">>, StartDate},
      {<<"endDate">>, EndDate},
      {<<"carName">>, CarName},
      {<<"carRegistrationNumber">>, CarRegistrationNumber}],
     _} =
        Body,
    #{id => ReservationId,
      startDate => binary_to_list(StartDate),
      endDate => binary_to_list(EndDate),
      carName => binary_to_list(CarName),
      carRegistrationNumber =>
          binary_to_list(CarRegistrationNumber)}.

check_reservation_possibility(StartDate, EndDate) ->
    StartDateInt =
        calendar:rfc3339_to_system_time(StartDate),
    EndDateInt = calendar:rfc3339_to_system_time(EndDate),
    if EndDateInt =< StartDateInt -> wrongEndDate;
       true ->
           Reservations = get_all(),
           CheckIfDateAlreadyReserved =
               check_if_date_already_reserved(StartDateInt,
                                              EndDateInt),
           IsDateAlreadyReserved =
               lists:any(CheckIfDateAlreadyReserved, Reservations),
           if IsDateAlreadyReserved -> dateAlreadyReserved;
              true -> ok
           end
    end.

check_if_date_already_reserved(NewStartDateInt,
                               NewEndDateInt) ->
    fun (Reservation) ->
            {_, ReservationMap} = Reservation,
            StartDateInt =
                calendar:rfc3339_to_system_time(maps:get(startDate,
                                                         ReservationMap)),
            EndDateInt =
                calendar:rfc3339_to_system_time(maps:get(endDate,
                                                         ReservationMap)),
            (NewStartDateInt >= StartDateInt) and
                (NewStartDateInt < EndDateInt)
                or
                (NewEndDateInt > StartDateInt) and
                    (NewEndDateInt =< EndDateInt)
                or
                (StartDateInt >= NewStartDateInt) and
                    (StartDateInt < NewEndDateInt)
    end.

generate_reservation_id() -> generate_id().
