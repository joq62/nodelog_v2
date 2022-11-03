%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(nodelog). 

-behaviour(gen_server). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
-define(SERVER,?MODULE).
-define(MAX_LOG_LENGTH,100).

%% External exports
-export([
	 read/1,
	 log/4,
	 create/1,
	 appl_start/1,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
		notice=[],
		warning=[],
		alert=[]
	
	       }).


%% ====================================================================
%% External functions
%% ====================================================================
appl_start([])->
    application:start(?MODULE).

%% ====================================================================
%% Server functions
%% ====================================================================
%% Gen server functions

start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).

%% ====================================================================
%% Application handling
%% ====================================================================

%% ====================================================================
%% Support functions
%% ====================================================================
%%---------------------------------------------------------------
%% Function:all_specs()
%% @doc: all service specs infromation       
%% @param: non 
%% @returns:State#state.service_specs_info
%%
%%---------------------------------------------------------------
read(LogLevel)->
    gen_server:call(?SERVER, {read,LogLevel},infinity).


create(LogFile)->
    gen_server:call(?SERVER, {create,LogFile},infinity).


log(Level,ModuleString,Line,Msg)-> 
    gen_server:cast(?SERVER, {log,Level,ModuleString,Line,Msg}).

%% 
%% @doc:check if service is running
%% @param: non
%% @returns:{pong,node,module}|{badrpc,Reason}
%%
-spec ping()-> {atom(),node(),module()}|{atom(),term()}.
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

%% ====================================================================
%% Gen Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    
    {ok, #state{}
    }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({read,notice},_From, State) ->
    Reply=State#state.notice,
    {reply, Reply, State};

handle_call({read,warning},_From, State) ->
    Reply=State#state.warning,
    {reply, Reply, State};

handle_call({read,alert},_From, State) ->
    Reply=State#state.alert,
    {reply, Reply, State};

handle_call({create,LogFile},_From, State) ->
    Reply=rpc:call(node(),lib_logger,create_logger,[LogFile],5000),
    {reply, Reply, State};

handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({stopped},_From, State) ->
    Reply=ok,
    {reply, Reply, State};


handle_call({not_implemented},_From, State) ->
    Reply=not_implemented,
    {reply, Reply, State};

handle_call({stop}, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call(Request, From, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched call",[Request, From])]),
    Reply = {ticket,"unmatched call",Request, From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({log,notice,ModuleString,Line,Msg}, State) ->
    R= io_lib:format("~p",[Msg]),
    MsgAsString=lists:flatten(R),
    logger:notice(MsgAsString,#{file=>ModuleString,line=>Line}),
    Len=list_length:start(State#state.notice),
 %   io:format("notice Len= ~p~n",[{Len,?MODULE,?LINE}]),
    if
	Len<?MAX_LOG_LENGTH->
	    NewState=State#state{notice=[{erlang:system_time(microsecond),ModuleString,Line,MsgAsString}|State#state.notice]};
	true->
	    Templist=lists:delete(lists:last(State#state.notice),State#state.notice),
	    NewState=State#state{notice=[{erlang:system_time(microsecond),ModuleString,Line,MsgAsString}|Templist]}
    end,
    {noreply,NewState};

handle_cast({log,warning,ModuleString,Line,Msg}, State) ->
    R= io_lib:format("~p",[Msg]),
    MsgAsString=lists:flatten(R),
    logger:warning(MsgAsString,#{file=>ModuleString,line=>Line}),
    Len=list_length:start(State#state.warning),
  %  io:format("waning Len= ~p~n",[{Len,?MODULE,?LINE}]),
    if
	Len<?MAX_LOG_LENGTH->
	    NewState=State#state{warning=[{erlang:system_time(microsecond),ModuleString,Line,MsgAsString}|State#state.warning]};
	true->
	    Templist=lists:delete(lists:last(State#state.warning),State#state.warning),
	    NewState=State#state{warning=[{erlang:system_time(microsecond),ModuleString,Line,MsgAsString}|Templist]}
    end,
    {noreply,NewState};

handle_cast({log,alert,ModuleString,Line,Msg}, State) ->
    R= io_lib:format("~p",[Msg]),
    MsgAsString=lists:flatten(R),
    logger:alert(MsgAsString,#{file=>ModuleString,line=>Line}),
    Len=list_length:start(State#state.alert),
  %  io:format("alert Len= ~p~n",[{Len,?MODULE,?LINE}]),
    if
	Len<?MAX_LOG_LENGTH->
	    NewState=State#state{alert=[{erlang:system_time(microsecond),ModuleString,Line,MsgAsString}|State#state.alert]};
	true->
	    Templist=lists:delete(lists:last(State#state.alert),State#state.alert),
	    NewState=State#state{alert=[{erlang:system_time(microsecond),ModuleString,Line,MsgAsString}|Templist]}
    end,
    {noreply,NewState};

handle_cast(_Msg, State) ->
  %  rpc:cast(node(),log,log,[?Log_ticket("unmatched cast",[Msg])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    %rpc:cast(node(),log,log,[?Log_ticket("unmatched info",[Info])]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

		  
