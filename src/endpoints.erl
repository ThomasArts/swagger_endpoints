-module(endpoints).

-export([generate/3]).

generate(FileName, Map, Options) ->
  Mod = filename:basename(FileName, ".erl"),
  Body = io_lib:format("~p", [Map]),
  Code =
    ["%% This code is generated from ", proplists:get_value(src, Options), "\n",
     "%% Using rebar3 swagger_endpoints\n", 
     "%% Do not manually change this code!\n\n",
     "-module("++Mod++").\n\n",
     "-export([operation/1, operations/0]).\n\n",
     "operations(Id) ->\n  maps:get(Id, operations()).\n\n",
     "operations() ->\n  ", Body, "."
    ],
  ok = file:write_file(FileName, Code).
