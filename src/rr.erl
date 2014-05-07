%%%-------------------------------------------------------------------
%%% @author Brian H. Ward <glyphrider@gmail.com>
%%% @copyright (C) 2014, Brian H. Ward
%%% @doc
%%%
%%% @end
%%% Created :  6 May 2014 by Brian H. Ward <brian@galago>
%%%-------------------------------------------------------------------
-module(rr).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add/2,next/1,rm/2,set/2]).
-export([state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(SEED_FILE, "roundrobin.seed").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Class,Item) ->
    gen_server:call(?SERVER,{add,Class,Item}).

next(Class) ->
    gen_server:call(?SERVER,{next,Class}).

rm(Class,Item) ->
    gen_server:call(?SERVER,{rm,Class,Item}).

set(Class,Items) ->
    gen_server:call(?SERVER,{set,Class,Items}).

state() ->
    gen_server:call(?SERVER,state).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    lager:info("roundrobin_server starting"),
    case file:consult(?SEED_FILE) of
	{ok, State} ->
	    process_flag(trap_exit,true),
	    {ok, State};
	{error,Reason} ->
	    lager:warning("could not load seed file ~p (reason: ~p)",[?SEED_FILE,Reason]),
	    {ok,[]}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add,Class,Item},_From,State) ->
    lager:debug("adding ~p for class ~p",[Item,Class]),
    case lists:keyfind(Class,1,State) of
	false ->
	    {reply, ok, lists:append(State,[{Class,[Item]}])};
	{Class,Items} ->
	    {reply, ok, lists:keyreplace(Class,1,State,{Class,lists:append(Items,[Item])})}
    end;
handle_call({next,Class},_From,State) ->
    lager:debug("fetching next value for class ~p",[Class]),
    case lists:keyfind(Class,1,State) of
	false ->
	    {reply,undefined,State};
	{Class,[]} ->
	    {reply,undefined,State};
	{Class,[Item|Items]} ->
	    {reply,Item,lists:keyreplace(Class,1,State,{Class,lists:append(Items,[Item])})}
    end;
handle_call({set,Class,Items},_From,State) ->
    lager:debug("setting ~p for class ~p",[Items,Class]),
    case lists:keyfind(Class,1,State) of
	false ->
	    {reply, ok, lists:append(State,[{Class,Items}])};
	{Class,_OldItems} ->
	    {reply, ok, lists:keyreplace(Class,1,State,{Class,Items})}
    end;
handle_call({rm,Class,Item},_From,State) ->
    lager:debug("removing ~p for class ~p",[Item,Class]),
    case lists:keyfind(Class,1,State) of
	false ->
	    {reply, undefined, State};
	{Class,Items} ->
	    {reply, ok, lists:keyreplace(Class,1,State,{Class,lists:dropwhile(fun(I) -> I == Item end,Items)})}
    end;
handle_call(state,_From,State) ->
    {reply,State,State};
handle_call(Request, From, State) ->
    lager:warning("unexpected call ~p from ~p",[Request,From]),
    {reply, undefined, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    lager:warning("unexpected info -> ~p",[Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason,State) ->
    lager:info("roundrobin_server:terminate(~p)",[Reason]),
    case file:open(?SEED_FILE,write) of
	{error,Reason} ->
	    lager:error("could not persist state data");
	{ok,File} ->
	    lists:foreach(
	      fun(Class) ->
		      lager:info("saving ~p",[Class]),
		      io:format(File,"~p.~n",[Class])
	      end,State),
	    file:close(File)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
