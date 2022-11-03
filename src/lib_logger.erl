%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(lib_logger).   
 
-export([
	 log/4,
	 create_logger/1
	]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("kernel/include/logger.hrl").
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
log(notice,ModuleString,Line,Msg)->
    logger:notice(Msg,#{file=>ModuleString,line=>Line});
log(warning,ModuleString,Line,Msg)->
    logger:warning(Msg,#{file=>ModuleString,line=>Line});
log(alert,ModuleString,Line,Msg)->
    logger:alert(Msg,#{file=>ModuleString,line=>Line}).
    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
%create_logger(LogDir)->
create_logger(LogFile)->
 %   {VmId,_HostId}=misc_node:vmid_hostid(node()),    
  %  LogFile=filename:join(LogDir,VmId++".log"),
    ok=logger:add_handler(my_standar_disk_h, logger_std_h,
			  #{formatter => {logger_formatter,
					  #{ template => [time," | ", file," | ",line," | ",level," | ",msg,"\n"]}}}),
    ok=logger:add_handler(my_disk_log_h, logger_disk_log_h,
			  #{
			    config => #{file => LogFile,
					type => wrap,
					max_no_files => 4,
					max_no_bytes =>1000*100,
					filesync_repeat_interval => 1000},
			    formatter => {logger_formatter,
					  #{ template => [time," | ", file," | ",line," | ",level," | ",msg,"\n"]}}}),
    ok.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% -------------------------------------------------------------------
