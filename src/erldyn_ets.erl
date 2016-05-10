%%%-----------------------------------------------------------------------------
%%% @author Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%% @copyright (C) 2016, Jim Rosenblum
%%% @doc Lives to create and own the ets which persists the credentials.
%%%
%%% @end
%%% Created :  9 May 2016 by Jim Rosenblum <jrosenblum@Jims-MBP.attlocal.net>
%%%-----------------------------------------------------------------------------
-module(erldyn_ets).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-define(SERVER, ?MODULE).

-record(erldyn_ets_state, {}).



%%% ============================================================================
%%% API
%%% ============================================================================


-spec start_link() -> gen_server:result().

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================


-spec init([]) -> {ok, #erldyn_ets_state{}}.
init([]) ->
    _ = ets:new(aim_cred, [set, 
                        named_table, 
                        public, 
                        {read_concurrency, true}, 
                        {write_concurrency, false}]),
    {ok, #erldyn_ets_state{}}.


-spec handle_call(gen_server:request(),gen_server:from(),#{}) -> {reply, ok, #{}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


-spec handle_cast(gen_server:requenst(), #{}) -> {noreply, #{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.


-spec handle_info(gen_server:info(), #{}) -> {noreply, #{}}.
handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(gen_server:reason(), #{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(gen_server:oldvsn(), gen_server:state(), gen_server:extra()) -> {ok, #{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

