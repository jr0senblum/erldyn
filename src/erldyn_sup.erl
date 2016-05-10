%%%-------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2016 by Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%%-------------------------------------------------------------------
-module(erldyn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor.
%%

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


-spec init([]) -> supervisor:result().

init([]) ->

    SupFlags = #{strategy => one_for_one,
                 intensity => 60,
                 period => 300},

    AChild = #{id => 'erldyn_ets',
               start => {'erldyn_ets', start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => ['erldyn_ets']},

    {ok, {SupFlags, [AChild]}}.


