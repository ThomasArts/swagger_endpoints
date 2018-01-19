-module(swagger_endpoints).

-export([init/1, from_yaml/2]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  application:ensure_started(yamerl),
  {ok, State1} = swagger_endpoints_prv:init(State),
  {ok, State1}.

from_yaml(Doc, Options) ->
  BaseUri =  proplists:get_value("basePath", Doc, "/"),
  Endpoints = proplists:get_value("paths", Doc, []),
  Definitions = proplists:get_value("definitions", Doc, []),
  endpoint_map(Endpoints,  #{}, [{defs, Definitions}, {baseuri, BaseUri} | Options]).

endpoint_map([], Map, _Options) ->
  Map;
endpoint_map([{Path, EP}|EPs], Map, Options) ->
  {Key, Value} = method_part(Path, EP, Options),
  endpoint_map(EPs, maps:put(Key, Value, Map), Options).


method_part(Path, EP, Options) ->
  case {lists:keyfind("post", 1, EP),
        lists:keyfind("get", 1, EP)} of
    {{"post", Attr}, false} ->
      mk_operation(Path, Attr, post, Options);
    {false, {"get", Attr}} ->
      mk_operation(Path, Attr, get, Options);
    {true, true} ->
      throw({error, "use either method POST or GET in", Path}); 
    {false, false} ->
      throw({error, "need method POST or GET in", Path})
  end.

mk_operation(Path, Attr, Method, Options) ->
  BaseUri = 
      iolist_to_binary(string:trim(proplists:get_value(baseuri, Options, "/"), trailing, "/")), 
  BPath = iolist_to_binary(Path),
  case proplists:get_value("operationId", Attr) of
    undefined ->
      throw({error, "no operationId provided", Path});
    Id ->
      IdName = 
        case proplists:get_value(id_type, Options, atom) of
          atom ->
            list_to_atom(Id);
          binary ->
            iolist_to_binary(Id);
          string ->
            Id
        end,
      {IdName, #{method => Method, 
                 path => <<BaseUri/binary, BPath/binary>> }}
  end.
