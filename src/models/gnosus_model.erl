%% ejabberd passwd database model
%%--------------------------------------------------------------------------------
-module(gnosus_model).
 
%% API
-export([
    fields/0,
    create_table/0,
    delete_table/0,
    clear_table/0,
    find/1,
    start_modules/1,
    stop_modules/1,
    ejabberd/0,
    tld/0,
    excluded_hosts/0,
    load_config_file/0
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%================================================================================
fields() ->
    record_info(fields, gnosus).

%%================================================================================
create_table() ->
    gnosus_dbi:create_table(gnosus, [{attributes, fields()}, {disc_only_copies, [node()]}]).

%%--------------------------------------------------------------------------------
delete_table() ->
    gnosus_dbi:delete_table(gnosus).
    
%%--------------------------------------------------------------------------------
clear_table() ->
    gnosus_dbi:clear_table(gnosus).
  
%%--------------------------------------------------------------------------------
load_config_file() ->
    case file:consult("gnosus.cfg") of
    	{ok, Config} ->
    	    gnosus_logger:message({config_loaded, Config}),
    	    write_config(Config);
    	{error, {_LineNumber, erl_parse, _ParseMessage} = Reason} ->
    	    ExitText = lists:flatten("gnosus.cfg approximately in the line "++file:format_error(Reason)),
    	    gnosus_logger:alarm({config_load_failed, ExitText}),
    	    exit(ExitText);
    	{error, Reason} ->
    	    ExitText = lists:flatten("gnosus.cfg : " ++ file:format_error(Reason)),
    	    gnosus_logger:alarm({config_load_failed, ExitText}),
    	    exit(ExitText)
    end.

%%--------------------------------------------------------------------------------
find(all) ->
	gnosus_dbi:q(qlc:q([X || X <- mnesia:table(gnosus)]));

%%--------------------------------------------------------------------------------
find(Key) ->
    case gnosus_dbi:read_row({gnosus, Key}) of
        [] ->
            notfound;
    	aborted ->
    	    error;
        Result ->
            hd(Result)
     end.

%%================================================================================
start_modules(_Host) ->
    DefMods = find(default_modules),
    OptMods = find(optional_modules),
    SpecMods = find(special_modules),
    DefMods#gnosus.value++OptMods#gnosus.value++SpecMods#gnosus.value.

%%--------------------------------------------------------------------------------
stop_modules(_Host) ->
    DefMods = find(default_modules),
    OptMods = find(optional_modules),
    DefMods#gnosus.value++OptMods#gnosus.value.

%%--------------------------------------------------------------------------------
ejabberd() ->   
    Ejabberd = find(ejabberd),
    Ejabberd#gnosus.value.

%%--------------------------------------------------------------------------------
tld() ->   
    Tld = find(tld),
    Tld#gnosus.value.

%%--------------------------------------------------------------------------------
excluded_hosts() ->   
    Hosts = find(excluded_hots),
    Hosts#gnosus.value.

%%================================================================================
write(D) when is_record(D, gnosus) ->
    gnosus_dbi:write_row(D);
write(_) ->
    error.

%%--------------------------------------------------------------------------------
write_config(Config) ->
    lists:foreach(
        fun({Key, Value}) ->
              case Key of
                  default_modules ->                              
                       write(#gnosus{key=Key, value=merge_config_list(Key, Value)});
                  optional_modules ->                              
                       write(#gnosus{key=Key, value=merge_config_list(Key, Value)});
                  special_modules ->                              
                        write(#gnosus{key=Key, value=merge_config_list(Key, Value)});
                  _ ->
                       write(#gnosus{key=Key, value=Value}) 
              end
        end, Config).

%%--------------------------------------------------------------------------------
merge_config_list(Key, ConfigList) ->
    InstList = case find(Key) of
                   notfound -> [];
                   aborted -> [];
                   L -> L#gnosus.value
                end,
    (InstList--ConfigList)++ConfigList.
