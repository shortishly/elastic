-module(elastic_application).
-behaviour(application).

-export([
	 start/2,
	 stop/1
	]).

start(_Type, _Args) ->
	elastic_supervisor:start_link().

stop(_State) ->
	ok.
