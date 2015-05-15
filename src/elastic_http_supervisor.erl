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

-module(elastic_http_supervisor).
-behaviour(supervisor).
-export([
	 start_link/0,
	 start_child/2
	]).
-export([
	 init/1
	]).


start_link() ->
    supervisor:start_link(ref(), ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 1, 5}, [elastic_supervisor:worker(elastic_http, temporary)]}}.

start_child(Host, Port) ->
    supervisor:start_child(ref(), [Host, Port]).

ref() ->
    {via, gproc, {n, l, ?MODULE}}.
