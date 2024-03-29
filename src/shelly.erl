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


-module(shelly).


-export([priv_file/1]).
-export([start/0]).


start() ->
    application:ensure_all_started(?MODULE).


priv_dir() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            error(badarg, [?MODULE]);

        Filename ->
            Filename
    end.


priv_file(Filename) ->
    filename:join(priv_dir(), Filename).
