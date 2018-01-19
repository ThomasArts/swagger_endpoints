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
            {opts, [{swaggerfile, $f, "file", string, "filename of swagger.yaml file"}]},
                                                % list of options understood by the plugin
            {short_desc, "Generate endpoints code from swagger"},
            {desc, "A rebar plugin to generate code to access swagger defined endpoints"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {KVs, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(swaggerfile, KVs, 
                             rebar_state:get(State, swagger_endpoints, undefined)) of
        undefined ->
            {error, "Provide swagger file"};
        Path ->
            try Yaml = yamerl_constr:file(Path),
                rebar_api:info("Generating code from ~p\n", [Path]),
                 io:format("Got yaml: ~p\n", [Yaml]),
                 {ok, State}
            catch
                _:_ ->
                    {error, io_lib:format("Failed to parse ~p", [Path])}
            end
    end.
   


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
