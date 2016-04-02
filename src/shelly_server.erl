%% Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
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


-module(shelly_server).
-behaviour(gen_server).

-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([start_link/0]).
-export([stop/0]).
-export([terminate/2]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


stop() ->
    gen_server:call(?MODULE, stop).


init([]) ->
    case shelly_config:enabled(sshd) of
        true ->
            case ssh:daemon(shelly_config:port(sshd), options()) of
                {ok, Daemon} ->
                    {ok, #{daemon => Daemon}};

                {error, _} = Error ->
                    {stop, Error, undefined}
            end;

        false ->
            ignore
    end.


options() ->
    case shelly_config:authorized_keys() of
        undefined ->
            [{inet, inet},
             {subsystems, []},
             {system_dir, shelly_config:system_dir()},
             {user_dir, shelly_config:user_dir()},
             {auth_methods, "publickey"}];
        Keys ->
            UserDir = shelly_config:tmp_dir(),
            ok = filelib:ensure_dir(UserDir),
            ok = file:make_dir(UserDir),
            AuthorizedKeys = filename:join(UserDir, "authorized_keys"),
            ok = file:write_file(AuthorizedKeys, Keys),
            [{inet, inet},
             {subsystems, []},
             {system_dir, shelly_config:system_dir()},
             {user_dir, UserDir},
             {auth_methods, "publickey"}]
    end.

handle_call(stop, _, S) ->
    {stop, normal, ok, S}.

handle_cast(_, S) ->
    {stop, error, S}.

handle_info(_, S) ->
    {error, error, S}.

terminate(_, #{daemon := Daemon}) ->
    ssh:stop_daemon(Daemon).

code_change(_, State, _) ->
    {ok, State}.
