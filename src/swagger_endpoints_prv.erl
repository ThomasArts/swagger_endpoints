-module(swagger_endpoints_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, swagger_endpoints).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 swagger_endpoints swagger.yaml"}, 
                                          % How to use the plugin
            {opts, [{swaggerfile, $f, "file", string, "filename of swagger.yaml file"},
                    {outfile, $o, "out", string, "filename for generated code (e.g. endpoints.erl)"},
                    {id_type, $t, "idtype", atom, "type of generated id (atom | binary | string)"},
                    {strict, $w, "strict", atom, "Produce extra warnings"}
                   ]},                    % list of options understood by the plugin
            {short_desc, "Generate endpoints code from swagger"},
            {desc, "A rebar plugin to generate code to access swagger defined endpoints"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {KVs, _} = rebar_state:command_parsed_args(State),
    Source = proplists:get_value(swaggerfile, KVs, get_from_state(src, State, undefined)),
    DefaultDest = 
       case Source of
           undefined -> undefined;
           _ -> filename:basename(Source, [".yaml"]) ++ "_endpoints"
      end,
    Dest = proplists:get_value(outfile, KVs, get_from_state(dst, State, DefaultDest)),
    case Source of
        undefined ->
            {error, "Provide swagger file using option --file Filename"};
        Path ->
            {Defs, Endpoints} = 
                try [YamlDoc] = yamerl_constr:file(Path),
                    rebar_api:info("Generating code from ~p writing to ~p", [Path, Dest]),
                    swagger_endpoints:from_yaml(YamlDoc, KVs)
                catch
                    _:Reason ->
                        {error, io_lib:format("Failed to parse ~p (~p)", [Path, Reason])}
                end,
            try generate(Dest, Endpoints, Defs, [{src, Source}]),
                {ok, State}
            catch  
                _:Error ->
                    {error, io_lib:format("Failed to generate code ~p (~p)", [Dest, Error])}
            end
    end.

generate(Dest, Endpoints, Definitions, Options) ->
    case filename:extension(Dest) of
      ".erl" ->
        swagger_generate:erlang(Dest, Endpoints, Definitions, Options);
      ".json" ->
        swagger_generate:json_schema(Dest, Endpoints, Definitions, Options);
      [] ->
        swagger_generate:erlang(Dest ++ ".erl", Endpoints, Definitions, Options);
      Ext ->
        throw({extension, Ext, not_allowed})
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

get_from_state(Key, State, Default) ->
    KVs = rebar_state:get(State, swagger_endpoints, []),
    proplists:get_value(Key, KVs, Default).

