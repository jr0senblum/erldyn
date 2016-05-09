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
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
