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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/0]).
-export([add/2,next/1,rm/2,set/2]).
-ifdef(EXPOSE_STATE).
-export([state/0]).
-endif.

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(SEED_FILE, "roundrobin.seed").

%%%===================================================================
%%% API
%%%===================================================================
-type class() :: term().
-type item() :: term().
-spec(add(class(),item()) -> ok).
-spec(next(class()) -> undefined | item()).
-spec(rm(class(),item()) -> undefined | ok).
-spec(set(class(),[item()]) -> ok).
-ifdef(EXPOSE_STATE).
-spec(state() -> [{class(),[item()]}]).
-endif.

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

-ifdef(EXPOSE_STATE).
state() ->
    gen_server:call(?SERVER,state).
-endif.

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
	    case lists:subtract(Items,[Item]) of
		Items ->
		    {reply,undefined,State};
		[] ->
		    {reply, ok, lists:keydelete(Class,1,State)};
		NewList ->
		    {reply, ok, lists:keyreplace(Class,1,State,{Class,NewList})}
	    end
    end;
handle_call(state,_From,State) ->
    {reply,State,State};
handle_call(Request, From, State) ->
    lager:error("rr:handle_call() -> unexpected call: ~p (from ~p)",[Request,From]),
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
handle_cast(Msg, State) ->
    lager:error("rr:handle_cast() -> unexpected cast: ~p",[Msg]),
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
    lager:error("rr:handle_info() -> unexpected info: ~p",[Info]),
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
    lager:info("rr:terminate(): reason -> ~p",[Reason]),
    case file:open(?SEED_FILE,write) of
	{error,Reason} ->
	    lager:error("rr:terminate(): could not persist state data to ~p",[?SEED_FILE]);
	{ok,File} ->
	    lists:foreach(
	      fun(Class) ->
		      lager:info("rr:terminate(): persisting class -> ~p",[Class]),
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
code_change(OldVsn, State, Extra) ->
    lager:info("rr:code_change(): old version -> ~p, extra -> ~p",[OldVsn,Extra]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(EUNIT).

next_returns_undefined_for_empty_state_test() ->
    {reply,undefined,[]} = rr:handle_call({next,any},test,[]).

next_returns_first_in_list_and_rotates_list_test() ->
    {reply,first,[{class,[second,first]}]} =
	rr:handle_call({next,class},
		test,
		[{class,[first,second]}]).

next_handles_single_item_in_list_test() ->
    {reply,only,[{class,[only]}]} = rr:handle_call({next,class},test,[{class,[only]}]).

next_returns_undefined_for_missing_class_test() ->
    {reply,
     undefined,
     [{present,[value]}]
    } = rr:handle_call(
	    {next,missing},test,[{present,[value]}]).

next_preserves_unaskedfor_values_test() ->
    {reply,return_first,
     [{askedfor,[return_second,return_first]},
      {unaskedfor,[not_returned_first,not_returned_second]}]
    } = rr:handle_call(
	  {next,askedfor},test,
	  [{askedfor,[return_first,return_second]},
	   {unaskedfor,[not_returned_first,not_returned_second]}]).

add_creates_new_tuple_in_state_test() ->
    {reply,ok,[{new_class,[new_value]}]}
	= rr:handle_call({add,new_class,new_value},test,[]).

add_creates_new_list_item_in_state_test() ->
    {reply,ok,[{existing_class,[existing_value,new_value]}]}
	= rr:handle_call(
	    {add,existing_class,new_value},
	    test,
	    [{existing_class,[existing_value]}]).

rm_returns_undefined_for_unknown_class_test() ->
    {reply,undefined,[{known_class,[known_value]}]}
	= rr:handle_call({rm,unknown_class,known_value},
			test,
			[{known_class,[known_value]}]).

rm_returns_undefined_for_unknown_item_test() ->
    {reply,undefined,[{known_class,[known_value]}]}
	= rr:handle_call({rm,known_class,unknown_value},
			test,
			[{known_class,[known_value]}]).

rm_removes_requested_item_from_state_test() ->
    {reply,ok,
     [{class_one,[value_one]},
      {class_two,[value_one,value_two]}]}
	= rr:handle_call(
	    {rm,class_one,value_two},test,
	    [{class_one,[value_one,value_two]},
	     {class_two,[value_one,value_two]}]).

rm_removes_class_if_removing_only_item_test() ->	       
    {reply,ok,
     [{class_two,[value_one,value_two]}]}
	= rr:handle_call(
	    {rm,class_one,only_value},test,
	    [{class_one,[only_value]},
	     {class_two,[value_one,value_two]}]).
    

-endif.
