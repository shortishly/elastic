%% Copyright (c) 2012-2015 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(elastic).
-export([
	 start/0,
	 make/0,
	 get_env/1,
	 index_document/4,
	 index_document/3
	]).

start() ->
    application:ensure_all_started(?MODULE).

make() ->
    make:all([load]).

get_env(Key) ->
    gproc:get_env(l, ?MODULE, Key, [os_env, app_env]).

-type index() :: hourly | daily | monthly | yearly.


-spec index_document(index(), iolist(), iolist(), iolist()) -> {ok, map()} | {error, binary()}.
index_document(Index, Type, Id, Document) ->
    elastic_http:index(connection(), index(Index), Type, Id, Document).

-spec index_document(index(), iolist(), iolist()) -> {ok, map()} | {error, binary()}.
index_document(Index, Type, Document) ->
    elastic_http:index(connection(), index(Index), Type, Document).

connection() ->
    {ok, Connection} = elastic_http_supervisor:start_child(tcp_addr(), tcp_port()),
    Connection.

tcp_addr() ->
    get_env(elasticsearch_port_9200_tcp_addr).

tcp_port() ->
    get_env(elasticsearch_port_9200_tcp_port).

index(Type) ->
    [prefix(), "-", postfix(Type)].

postfix(hourly) ->
    {{Year, Month, Date}, {Hour, _, _}} = erlang:universaltime(),
    io_lib:format("~4..0b.~2..0b.~2..0b.~2..0b", [Year, Month, Date, Hour]);
postfix(daily) ->
    {{Year, Month, Date}, _} = erlang:localtime(),
    io_lib:format("~4..0b.~2..0b.~2..0b", [Year, Month, Date]);
postfix(monthly) ->
    {{Year, Month, _}, _} = erlang:localtime(),
    io_lib:format("~4..0b.~2..0b", [Year, Month]);
postfix(yearly) ->
    {{Year, _, _}, _} = erlang:localtime(),
    io_lib:format("~4..0b", [Year]).

prefix() ->
    elastic:get_env(index_prefix).
