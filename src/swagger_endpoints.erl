-module(swagger_endpoints).

-export([init/1, from_yaml/2]).

%% Yaml parser outputs strings (list of characters).
%% Both jsx and jesse expect eitehr atoms or binaries
%% This causes additional code to transform these strings to binaries.

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  application:ensure_started(yamerl),
  {ok, State1} = swagger_endpoints_prv:init(State),
  {ok, State1}.

from_yaml(Doc, Options) ->
  BaseUri =  proplists:get_value("basePath", Doc, "/"),
  Endpoints = proplists:get_value("paths", Doc, []),
  Definitions = 
    build_definitions(proplists:get_value("definitions", Doc, []), [], Options),
  {Definitions, endpoint_map(Endpoints,  #{}, [{defs, Definitions}, {baseuri, BaseUri} | Options])}.

build_definitions([], Definitions, _Options) ->
  Definitions;
build_definitions([{Def, Schema} | Defs], Definitions, Options) ->
  NewDef = {"/definitions/" ++ Def, to_json_schema(Schema, [{defs, Definitions}|Options])},
  build_definitions(Defs, [NewDef | Definitions], Options).


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
  IdName = get_value("operationId", proplists:get_value(id_type, Options, atom), Attr, Path),
  Tags = get_value("tags", {list, binary}, Attr, Path),
  Params = get_value("parameters", {list, string}, Attr, Path, null),
  ReadResponse =
    fun({StatusCode, Resp}) ->
      response(StatusCode, Resp, [{path, Path} | Options])
    end,
  Responses =
    maps:from_list(get_value("responses", {list, ReadResponse}, Attr, Path, [])),
  {IdName, #{Method => #{ 
                path => <<BaseUri/binary, BPath/binary>>,
                tags => Tags,
                parameters => params_to_json_schema(Params, [{endpoint, IdName} | Options]),
                responses  => Responses
              }}}.

get_value(Name, Type, Attr, Path) ->
  get_value(Name, Type, Attr, Path, undefined).

get_value(Name, Type, Attr, Path, Default) ->
  case proplists:get_value(Name, Attr, Default) of
    undefined ->
      throw({error, "no " ++ Name ++ " provided", Path});
    %% if a key is present but has no elements in it - the value is null
    null when element(1, Type) =:= list ->
      Default;
    Val ->
      Parse =
        fun(T, V) ->
          case T of
            atom -> list_to_atom(V);
            binary -> iolist_to_binary(V);
            string -> V;
            F when is_function(F, 1) -> F(V)
          end
        end,
      case Type of
        {list, ElType} ->
          lists:map(fun(Element) -> Parse(ElType, Element) end, Val);
          _ -> Parse(Type, Val)
      end
   end.

response(StatusCode, Resp, Options) ->
  StrictCompilation = proplists:get_value(strict, Options, false),
  Code = 
    try {SC, []} = string:to_integer(StatusCode), SC    
    catch
      _:_ ->
        rebar_api:error("Response status code for path ~p cannot be parsed (~p, ~p)", 
                        [ proplists:get_value(path, Options), StatusCode, Resp])
    end,
  case proplists:get_value("schema", Resp) of
    undefined when StrictCompilation ->
      rebar_api:warn("Empty response body for path ~p (~p, ~p)", 
                     [ proplists:get_value(path, Options), StatusCode, Resp]),
      {Code, undefined};
    undefined -> {Code, undefined};
    Yaml    ->
      Schema = (catch to_json_schema(Yaml, Options)),
      {Code, Schema}
  end .

params_to_json_schema([], Options) ->
  rebar_api:error("~p: use YAML array for parameters not JSON array!", 
                  [proplists:get_value(endpoint, Options)]),
  [];
params_to_json_schema(null, _Options) ->
  [];
params_to_json_schema(Params, Options) ->
  %% According to swagger.io even a parameter may have a "schema:" 
  [ begin 
      Schema = proplists:get_value("schema", Param, mk_param_to_schema(Param, Options)),
      lists:keydelete("schema", 1, Param) ++ [{"schema", to_json_schema(Schema, Options)}]
    end || Param <- Params ].

mk_param_to_schema(Param, _Options) ->
  [ {K, V} || {K, V} <- Param, 
              not lists:member(K, ["required", "example", "description", "name", "in"])].

to_json_schema(Schema, Options) ->
  %% Turn Yaml into a map with binaries as keys
  Result = jesse_json_schema(Schema, Options),
  rebar_api:debug("translating ~p into ~p", [Schema, Result]),
  Result.

jesse_json_schema(Schema, Options) when is_list(Schema) ->
  lists:foldl(fun(Item, Acc) -> 
                  maps:merge(to_json_schema(Item, Options), Acc)
              end, #{}, Schema);
jesse_json_schema({"$ref", Reference}, _Options) ->
  DefRef = string:trim(Reference, leading, "#"),
  #{<<"$ref">> => list_to_binary(DefRef)};
jesse_json_schema({Key, Items}, _Options) when Key == "required"; Key == "enum" ->
  BinKey = list_to_binary(Key),
  #{BinKey => [ list_to_binary(Item) || Item <- Items ]};
jesse_json_schema({Key, Value}, _Options) when 
    is_number(Value); is_boolean(Value); is_binary(Value) ->
  BinKey = list_to_binary(Key),
  #{BinKey => Value};
jesse_json_schema({Key, Value}, Options) when is_list(Value) ->
  BinKey = list_to_binary(Key),
  case lists:all(fun(C) -> is_integer(C) andalso C > 0 end, Value) of
    true ->
      #{BinKey => list_to_binary(Value)};
    false ->
      #{BinKey => jesse_json_schema(Value, Options)}
  end;
jesse_json_schema(Schema, _Options) ->
  #{error => Schema}.
