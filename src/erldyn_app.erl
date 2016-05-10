%%% ----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2016 by Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% ----------------------------------------------------------------------------
-module(erldyn_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).




-spec start(atom(), application:restart_tpe()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    case erldyn_sup:start_link() of
        {ok, Pid} ->
            inets:start(),
            ssl:start(),
            {ok, Pid};
        Error ->
            Error
    end.


-spec stop(#{}) -> ok.
stop(_State) ->
    ok.


