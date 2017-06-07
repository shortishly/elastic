-module(elastic_config).
-export([debug/1]).
-export([tcp_host/0]).
-export([tcp_port/0]).
-export([elastic_user/0]).
-export([elastic_pass/0]).



debug(applications) ->
    envy(to_list, debug_applications, "betash").

envy(To, Name, Default) ->
    envy:To(betash, Name, default(Default)).

default(Default) ->
    [os_env, app_env, {default, Default}].

tcp_host() ->
    envy(to_list, elastic_host, "10.16.13.10").

tcp_port() ->
    envy(to_integer, elastic_port, 9200).

elastic_user() ->
    envy(to_list, elastic_user, "elastic").

elastic_pass() ->
    envy(to_list, elastic_pass, "changeme").

