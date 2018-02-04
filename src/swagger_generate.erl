-module(swagger_generate).

-export([erlang/4, json_schema/4]).
-include_lib("syntax_tools/include/merl.hrl").
-define(QUOTE(X), ?Q(??X)).

json_schema(FileName, _Map, Definitions, Options) ->
  Schema = 
    maps:merge(#{<<"$schema">> => 
                   <<"http://json-schema.org/draft-04/schema#">>}, 
               #{<<"properties">> => 
                   maps:from_list([{list_to_binary(K), V} 
                                   || {K,V} <- Definitions])}),
  Binary = jsx:prettify(jsx:encode(Schema)),
  ok = file:write_file(FileName, Binary).

erlang(FileName, Map, Definitions, Options) ->
  Mod = filename:basename(FileName, ".erl"),
  Body = io_lib:format("~p", [Map]),
  Code =
    ["%% This code is generated from ", proplists:get_value(src, Options), "\n",
     "%% Using rebar3 swagger_endpoints\n"
     "%% Do not manually change this code!\n"
     "%%\n"
     "%% json_schema/0 implements a JSON Schema for the definitions\n"
     "%% Reference should be fixed!\n"
     "%% Use jsx:prettify(jsx:encode(json_schema())) to get a JSON string.\n\n",
     "-module("++Mod++").\n\n"
     "-export([operation/1, operations/0, definitions/0, json_schema/0,\n"
     "         validate_request/3, validate_response/4, path/3, validate/2]).\n\n"
     "operations() ->\n    ", Body, ".\n\n"
     "definitions() ->\n    ", io_lib:format("~p",[Definitions]), ".\n\n"] ++
     [erl_prettypr:format(erl_syntax:form_list(template_code()))],
  ok = file:write_file(FileName, Code).

template_code() ->
  [?QUOTE(json_schema() -> 
             maps:merge(#{<<"$schema">> => 
                            <<"http://json-schema.org/draft-04/schema#">>}, 
                        #{<<"properties">> => 
                            maps:from_list([{list_to_binary(K), V} 
                                            || {K,V} <- definitions()])}).),
   ?QUOTE(operation(Id) -> 
             maps:get(Id, operations()).),
   ?QUOTE(path(Method, OperationId, Args) when is_map(Args) ->
             path(Method, OperationId, maps:to_list(Args));
          path(Method, OperationId, Args) ->
             begin
               #{path := Endpoint, parameters := Parameters} = maps:get(Method, operation(OperationId)),
               InPath = [ Param || Param <- Parameters, lists:member({"in", "path"}, Param) ],
               lists:foldl(fun(Param, Path) -> 
                               Name = proplists:get_value("name", Param),
                               case {proplists:get_value("required", Param, false),
                                     proplists:get_value(Name, Args)} of
                                 {false, undefined} -> Path;
                                 {true, undefined}  ->
                                   throw({error, {required, Name, Param, OperationId}});
                                 {_, Value} ->
                                   string:replace(Path, "{"++Name++"}", lists:concat([Value]))
                               end
                           end, Endpoint, InPath)
             end.),
   ?QUOTE(prepare_validation() ->
             case ets:info(jesse_ets) of
               undefined ->
                 [ case jesse:add_schema(Def, Schema) of
                     ok -> ok;
                     Other ->
                       Other
                   end || {Def, Schema} <- definitions() ];
               _ -> []
             end.),
   ?QUOTE(validate(Schema, Term) ->
             try jesse_schema_validator:validate(Schema, Term, [])
             catch
               throw:Error ->
                 {error, Error}
             end.),
   ?QUOTE(validate_request(OperationId, Method, Args) when is_map(Args) ->
             validate_request(OperationId, Method, maps:to_list(Args));
            validate_request(OperationId, Method, Args) when is_list(Args) ->
             begin
               prepare_validation(),
               #{parameters := Parameters} = maps:get(Method, endpoints:operation(OperationId)),
               ToCheck = [ Param || Param <- Parameters, not lists:member({"in", "path"}, Param) ],
               Errors = lists:foldl(fun(Param, Errs) -> 
                                        Name = proplists:get_value("name", Param),
                                        case {proplists:get_value("required", Param, false),
                                              proplists:get_value(Name, Args)} of
                                          {false, undefined} -> Errs;
                                          {true, undefined}  ->
                                            [{required, Name, Param, OperationId}|Errs];
                                          {_, Value} ->
                                            case validate(proplists:get_value("schema", Param, #{}), Value) of
                                              {error, E} -> [E|Errs];
                                              _ -> Errs
                                            end
                                        end
                                    end, [], ToCheck),
               case Errors of
                 [] -> ok;
                 _ -> {errors, {OperationId, Args, Errors}}
               end
             end.),
   ?QUOTE(validate_response(OperationId, Method, StatusCode, Response) ->
             begin
               #{responses := Resps} = maps:get(Method, endpoints:operation(OperationId)),
               prepare_validation(),
               case maps:get(StatusCode, Resps, error) of
                 error -> {error, {StatusCode, unspecified}};
                 undefined ->
                   {ok, StatusCode, Response};
                 Schema ->
                   case validate(Schema, Response) of
                     {ok, _} ->
                       {ok, StatusCode, Response};
                     {error, E} ->
                       {error, {validation, E}}
                   end
               end
             end.)
  ].


