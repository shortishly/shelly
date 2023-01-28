%% Copyright (c) 2012-2022 Peter Morgan <peter.james.morgan@gmail.com>
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


-export([callback_mode/0]).
-export([init/1]).
-export([start_link/0]).


start_link() ->
    gen_statem:start_link(?MODULE, [], []).


init([]) ->
    case shelly_config:enabled(sshd) of
        true ->
            case ssh:daemon(shelly_config:port(sshd), options()) of
                {ok, Daemon} ->
                    {ok, ready, #{daemon => Daemon}};

                {error, Reason} ->
                    {stop, Reason}
            end;

        false ->
            ignore
    end.


callback_mode() ->
    handle_event_function.


options() ->
    Base = case shelly_config:password() of
               undefined ->
                   [{inet, inet},
                    {subsystems, []},
                    {system_dir, shelly_config:system_dir()}];

               Password ->
                   [{inet, inet},
                    {subsystems, []},
                    {system_dir, shelly_config:system_dir()},
                    {password, Password}]
           end,

    case shelly_config:authorized_keys() of
        undefined ->
            [{user_dir, shelly_config:user_dir()},
             {auth_methods, "publickey,password"},
             {password, shelly_config:password()} | Base];

        Keys ->
            UserDir = shelly_config:tmp_dir(),
            ok = filelib:ensure_dir(UserDir),
            ok = file:make_dir(UserDir),
            AuthorizedKeys = filename:join(UserDir, "authorized_keys"),
            ok = file:write_file(AuthorizedKeys, Keys),
            [{user_dir, UserDir},
             {auth_methods, "publickey,password"} | Base]
    end.
