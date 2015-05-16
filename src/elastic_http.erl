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
	 index/5,
	 index/4
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

-spec index(pid(), iodata(), iodata(), iodata(), iodata()) -> iodata().
index(Elastic, Index, Type, Id, Document) ->
    gen_server:call(Elastic, {index, Index, Type, Id, Document}).

-spec index(pid(), iodata(), iodata(), iodata()) -> iodata().
index(Elastic, Index, Type, Document) ->
    gen_server:call(Elastic, {index, Index, Type, Document}).

init([Host, Port]) ->
    self() ! open,
    {ok, #{host => Host, port => list_to_integer(Port)}}.

handle_call({index, Index, Type, Id, Document}, Reply, #{gun := Gun} = S) ->
    Stream = gun:put(Gun, [Index, "/", Type, "/", Id], [], Document),
    {noreply, maps:put(Stream, Reply, S)};

handle_call({index, Index, Type, Document}, Reply, #{gun := Gun} = S) ->
    Stream = gun:post(Gun, [Index, "/", Type, "/"], [], Document),
    {noreply, maps:put(Stream, Reply, S)}.

handle_cast(stop, S) ->
    {stop, normal, S}.


handle_info({gun_up, Gun, http}, #{gun := Gun} = S) ->
    {noreply, S};

handle_info({gun_response, Gun, _Stream, nofin, _Status, _Headers}, #{gun := Gun} = S) ->
    {noreply, S};

handle_info({gun_data, Gun, Stream, fin, Body}, #{gun := Gun} = S) ->
    From = maps:get(Stream, S),
    case jsx:decode(Body, [return_maps]) of
	#{<<"error">> := Reason} ->
	    gen_server:reply(From, {error, Reason});

	Response ->
	    gen_server:reply(From, {ok, Response})
    end;

handle_info(open, #{host := Host, port := Port} = S) ->
    case gun:open(Host, Port) of
	{ok, Gun} ->
	    {noreply, S#{gun => Gun}};
	{error, Reason} ->
	    {stop, Reason, S}
    end.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, _) ->
    ok.

