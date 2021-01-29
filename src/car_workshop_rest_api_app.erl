-module(car_workshop_rest_api_app).

-behaviour(application).

-export([start/2]).

-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/inspections",
                                         inspections_handler,
                                         []},
                                        {"/reservations",
                                         reservations_handler,
                                         []}]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    car_workshop_rest_api_sup:start_link().

stop(_State) -> ok.
