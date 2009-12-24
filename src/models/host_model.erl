%% host database model
%%--------------------------------------------------------------------------------
-module(host_model).
 
%% API
-export([
	 create_table/0,
	 delete_table/0,
	 clear_table/0,
	 delete/1,
	 delete_by_uid/1,
	 find/1,
	 find_all_by_uid/1,
	 hosts_list_by_uid/1,
	 hosts_list/0,
	 fields/0, 
	 count/0,
	 write/1,
	 new/2
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib("stdlib/include/qlc.hrl").
 
%%================================================================================
fields() ->
    record_info(fields, hosts).

%%================================================================================
create_table() ->
    gnosus_dbi:create_table(hosts, [{attributes, fields()}, {disc_only_copies, [node()]}, {index, [uid]}]).

%%--------------------------------------------------------------------------------
delete_table() ->
    gnosus_dbi:delete_table(hosts).
    
%%--------------------------------------------------------------------------------
clear_table() ->
    gnosus_dbi:clear_table(hosts).
 
%%================================================================================
find(all) ->
    gnosus_dbi:q(qlc:q([X || X <- mnesia:table(hosts)]));
 
%%--------------------------------------------------------------------------------
find(Host) ->
    case gnosus_dbi:read_row({hosts, Host}) of
        [] ->
            notfound;
	aborted ->
	    error;
        Result ->
            hd(Result)
     end.

%%--------------------------------------------------------------------------------
find_all_by_uid(Uid) ->
    gnosus_dbi:q(qlc:q([X || X <- mnesia:table(hosts), X#hosts.uid =:= Uid])).

%%--------------------------------------------------------------------------------
count() ->
    gnosus_dbi:count(hosts).
 
%%--------------------------------------------------------------------------------
delete(Host) ->
    gnosus_dbi:delete_row({hosts, Host}).

%%--------------------------------------------------------------------------------
delete_by_uid(Uid) ->
    lists:foldl(fun(Host, ok) ->
			delete(Host);
		   (_, error) ->
			error
		end, ok, hosts_list_by_uid(Uid)).		  

%%--------------------------------------------------------------------------------
hosts_list_by_uid(Uid) ->
    lists:foldl(fun(D,L) -> [D#hosts.host|L] end, [], find_all_by_uid(Uid)).

%%--------------------------------------------------------------------------------
hosts_list() ->
    lists:foldl(fun(D,L) -> [D#hosts.host|L] end, [], find(all)).

%%--------------------------------------------------------------------------------
new(Host, Uid) ->
    write(#hosts{uid=Uid, host=Host}).

%%================================================================================
write(D) when is_record(D, hosts) ->
    gnosus_dbi:write_row(D);
write(_) ->
    error.

