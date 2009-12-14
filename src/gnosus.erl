%%--------------------------------------------------------------------------------
-module(gnosus).

%% API
-export([
	start/0, 
	stop/0,
	shell/0,
    create_tables/0
]).

%% include

%%================================================================================
start() ->
    % wait_for_tables(),
    application:start(gnosus).

%%--------------------------------------------------------------------------------
stop() ->
    application:stop(xmppaas).

%%================================================================================
shell() ->
    wait_for_tables(),
    ok.

%%--------------------------------------------------------------------------------
create_tables() ->
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    do_create_ejabberd_tables(),
    do_create_tables(),
    wait_for_tables(),
    init:stop(),
    ok.
 
%%================================================================================
do_create_tables() ->
    user_model:create_table(),
    domain_model:create_table(),
    ok.
    
%%--------------------------------------------------------------------------------
do_create_ejabberd_tables() ->
    mnesia:add_table_copy(acl, node(), ram_copies),
    mnesia:add_table_copy(passwd, node(), ram_copies),
    ok.
    
%%--------------------------------------------------------------------------------
wait_for_tables() ->
    case mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity) of
        {timeout, BadTables} -> 
	    xmppaas_logger:warning({mnesia_start_timeout, BadTables});
        {error, Reason} -> 
	    xmppaas_logger:warning({mnesia_start_error, Reason});
        _ -> 
	    xmppaas_logger:message({started, mnesia})
    end.
