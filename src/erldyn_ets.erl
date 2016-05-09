%%%-------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc
%%%
%%% @end
%%% Created :  9 May 2016 by Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%%-------------------------------------------------------------------
-module(erldyn_ets).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(erldyn_ets_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
    _ = ets:new(aim_cred, [set, 
                        named_table, 
                        public, 
                        {read_concurrency, true}, 
                        {write_concurrency, false}]),
    {ok, #erldyn_ets_state{}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
