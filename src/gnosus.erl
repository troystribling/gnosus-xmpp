%%--------------------------------------------------------------------------------
-module(gnosus).

%% API
-export([
	start/0, 
	stop/0
]).

%% include

%%================================================================================
start() ->
    create_tables(),
    wait_for_tables(),
	crypto:start(),
    application:start(gnosus).

%%--------------------------------------------------------------------------------
stop() ->
    application:stop(gnosus).

%%================================================================================
create_tables() ->
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    % do_create_ejabberd_tables(),
    do_create_tables(),
    wait_for_tables().
 
%%================================================================================
do_create_tables() ->
    user_model:create_table(),
    host_model:create_table().
    
%%--------------------------------------------------------------------------------
% do_create_ejabberd_tables() ->
%     mnesia:add_table_copy(acl, node(), ram_copies),
%     mnesia:add_table_copy(passwd, node(), ram_copies),
%     ok.
    
%%--------------------------------------------------------------------------------
local_tables() ->
    mnesia:system_info(local_tables).

%%--------------------------------------------------------------------------------
wait_for_tables() ->
    case mnesia:wait_for_tables(local_tables(), infinity) of
        {timeout, BadTables} -> 
	        gnosus_logger:warning({mnesia_start_timeout, BadTables});
        {error, Reason} -> 
	        gnosus_logger:warning({mnesia_start_error, Reason});
        _ -> 
	        gnosus_logger:message({started, mnesia})
    end.
