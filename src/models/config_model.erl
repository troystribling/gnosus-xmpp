%% ejabberd passwd database model
%%--------------------------------------------------------------------------------
-module(config_model).
 
%% API
-export([
    fields/0,
    create_table/0,
    delete_table/0,
    clear_table/0,
    find/1,
    modules/0,
    modules/1,
    ejabberd/0,
    write_config/1
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%================================================================================
fields() ->
    record_info(fields, config).

%%================================================================================
create_table() ->
    gnosus_dbi:create_table(config, [{attributes, fields()}, {disc_only_copies, [node()]}]).

%%--------------------------------------------------------------------------------
delete_table() ->
    gnosus_dbi:delete_table(config).
    
%%--------------------------------------------------------------------------------
clear_table() ->
    gnosus_dbi:clear_table(config).
  
%%================================================================================
find(all) ->
	gnosus_dbi:q(qlc:q([X || X <- mnesia:table(config)]));

%%--------------------------------------------------------------------------------
find(Key) ->
    case gnosus_dbi:read_row({config, Key}) of
        [] ->
            notfound;
    	aborted ->
    	    error;
        Result ->
            hd(Result)
     end.

%%--------------------------------------------------------------------------------
modules() ->
    DefMods = find(default_modules),
    OptMods = find(optional_modules),
    DefMods#config.value++OptMods#config.value.

%%--------------------------------------------------------------------------------
modules(_Host) ->
    modules().

%%--------------------------------------------------------------------------------
ejabberd() ->   
    Ejabberd = find(ejabberd),
    Ejabberd#config.value.

%%--------------------------------------------------------------------------------
write_config(Config) ->
    lists:foreach(fun({Key, Value}) ->
                      write(#config{key=Key, value=Value})
                  end, Config).
     
%%================================================================================
write(D) when is_record(D, config) ->
    gnosus_dbi:write_row(D);
write(_) ->
    error.
