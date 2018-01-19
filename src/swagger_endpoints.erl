-module(swagger_endpoints).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    application:ensure_started(yamerl),
    {ok, State1} = swagger_endpoints_prv:init(State),
    {ok, State1}.
