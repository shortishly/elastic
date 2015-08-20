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

-module(elastic_http).
-behaviour(gen_server).
-export([
	 start_link/2,
         index/2
	]).
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

start_link(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

-spec index(pid(), map()) -> iodata().
index(Elastic, Parameters) ->
    gen_server:call(Elastic, {index, Parameters}, infinity).

init([Host, Port]) ->
    case gun:open(Host, list_to_integer(Port)) of
	{ok, Gun} ->
	    {ok, #{gun => Gun}};

	{error, _} = Error ->
	    {stop, Error, stateless}
    end.

handle_call({index, #{index := Index, type := Type, id := Id, document := Document}}, Reply, #{gun := Gun} = S) ->
    {noreply, maps:put(gun:put(Gun, [Index, "/", Type, "/", Id], [], Document), Reply, S)};

handle_call({index, #{index := Index, type := Type, document := Document}}, Reply, #{gun := Gun} = S) ->
    {noreply, maps:put(gun:post(Gun, [Index, "/", Type, "/"], [], Document), Reply, S)}.

handle_cast(stop, S) ->
    {stop, normal, S}.

handle_info({gun_up, Gun, http}, #{gun := Gun} = S) ->
    {noreply, S};

handle_info({gun_response, Gun, _, nofin, _Status, _Headers}, #{gun := Gun} = S) ->
    {noreply, S};

handle_info({gun_data, Gun, Stream, fin, Body}, #{gun := Gun} = S) ->
    case jsx:decode(Body, [return_maps]) of
	#{<<"error">> := Reason} ->
	    gen_server:reply(maps:get(Stream, S), {error, Reason}),
	    {stop, normal, S};

	Response ->
	    gen_server:reply(maps:get(Stream, S), {ok, Response}),
	    {stop, normal, S}
    end.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, #{gun := Gun}) ->
    gun:close(Gun).
