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
         bulk_index_documents/3,
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

-spec index_document(index(), iolist(), map() | iolist()) -> {ok, map()} | {error, binary()}.
index_document(Index, Type, #{timestamp := {{_, _, _}, {_, _, _}} = DateTime} = Document) ->
    elastic_http:index(connection(), #{index => index(Index, DateTime),
                                       type => Type,
                                       document => jsx:encode(Document)});

index_document(Index, Type, #{} = Document) ->
    elastic_http:index(connection(), #{index => index(Index),
                                       type => Type,
                                       document => jsx:encode(Document)});

index_document(Index, Type, Document) ->
    elastic_http:index(connection(), #{index => index(Index),
                                       type => Type,
                                       document => Document}).

-spec index_document(index(), iolist(), iolist(), map()) -> {ok, map()} | {error, binary()}.
index_document(Index, Type, Id,  #{timestamp := {{_, _, _}, {_, _, _}} = DateTime} = Document) ->
    elastic_http:index(connection(), #{index => index(Index, DateTime), type => Type, id => Id, document => jsx:encode(Document)});
index_document(Index, Type, Id,  #{} = Document) ->
    elastic_http:index(connection(), #{index => index(Index), type => Type, id => Id, document => jsx:encode(Document)});
index_document(Index, Type, Id,  Document) ->
    elastic_http:index(connection(), #{index => index(Index), type => Type, id => Id, document => Document}).

-spec bulk_index_documents(index(), iolist(), list()) -> {ok, map()} | {error, binary()}.
bulk_index_documents(Index, Type, Documents) ->
    elastic_http:bulk_index(connection(), #{index => index(Index),
                                            type => Type,
                                            documents => Documents}).

connection() ->
    {ok, Connection} = elastic_http_supervisor:start_child(elastic_config:tcp_host(),
                                                           elastic_config:tcp_port()),
    Connection.


-spec index(index()) -> iolist().
index(Type) ->
    index(Type, erlang:universaltime()).

-spec index(index(), calendar:datetime()) -> iolist().
index(Type, DateTime) ->
    [elastic_config:elastic_index(), "-", postfix(Type, DateTime)].


-spec postfix(index(), calendar:datetime()) -> iolist().
postfix(hourly, {{Year, Month, Date}, {Hour, _, _}}) ->
    io_lib:format("~4..0b.~2..0b.~2..0b.~2..0b", [Year, Month, Date, Hour]);
postfix(daily, {{Year, Month, Date}, _}) ->
    io_lib:format("~4..0b.~2..0b.~2..0b", [Year, Month, Date]);
postfix(monthly, {{Year, Month, _}, _}) ->
    io_lib:format("~4..0b.~2..0b", [Year, Month]);
postfix(yearly, {{Year, _, _}, _}) ->
    io_lib:format("~4..0b", [Year]).
