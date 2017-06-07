-module(elastic_config).
-export([debug/1]).
-export([tcp_host/0]).
-export([tcp_port/0]).



debug(applications) ->
    envy(to_list, debug_applications, "betash").

envy(To, Name, Default) ->
    envy:To(betash, Name, default(Default)).

default(Default) ->
    [os_env, app_env, {default, Default}].

tcp_host() ->
    envy(to_binary, elastic_host, <<"10.16.13.10">>).

tcp_port() ->
    envy(to_binary, elastic_port, <<"9200">>).


