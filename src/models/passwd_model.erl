%% ejabberd passwd database model
%%--------------------------------------------------------------------------------
-module(passwd_model).
 
%% API
-export([
    find/1,
    find/2,
    find_all_by_host/1,
    authenticate/2
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib("stdlib/include/qlc.hrl").
 
%%--------------------------------------------------------------------------------
find_all_by_host(Host) ->
	gnosus_dbi:dirty_select(passwd, [{#passwd{us = '$1', _ = '_'}, [{'==', {element, 2, '$1'}, Host}], ['$1']}]).

%%--------------------------------------------------------------------------------
find(all) ->
	gnosus_dbi:q(qlc:q([X || X <- mnesia:table(passwd)]));

%%--------------------------------------------------------------------------------
find(Jid) ->
    case client_user_model:bare_jid_tuple(Jid) of
        {Host, Uid} -> find(Host, Uid);
        _ -> error
    end.

%%--------------------------------------------------------------------------------
find(Host, Uid) ->
    case gnosus_dbi:read_row({passwd, {Uid, Host}}) of
        [] -> notfound;
        aborted -> error;
        Result -> hd(Result)
    end.

 
%%--------------------------------------------------------------------------------
authenticate(Jid, EnteredPassword) ->
    case find(Jid) of
        notfound ->
            false;
        #passwd{password=Password} ->
	    	Password =:= EnteredPassword
    end.
