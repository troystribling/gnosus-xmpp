%% client_user database model
%%--------------------------------------------------------------------------------
-module(client_user_model).
 
%% API
-export([
    fields/0, 
    status_values/0,
    create_table/0,
    delete_table/0,
    clear_table/0,
    delete/2,
    find/1,
    find/2,
    find_by_email/1,
    find_all_by_host/1,
    count/0,
    password/3,
    email/3,
    status/3,
    register/3,
    new_user/4,
    new_client_user_from_user/3,
    write/1,
    new/5,
    init_record/5,
    update/5
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib("stdlib/include/qlc.hrl").
 
%%================================================================================
fields() ->
	record_info(fields, client_users).

%%--------------------------------------------------------------------------------
status_values() ->
    [active, inactive, registered].

%%================================================================================
create_table() ->
	gnosus_dbi:create_table(client_users, [{attributes, fields()}, {disc_only_copies, [node()]}, {index, [email]}]).
    
%%--------------------------------------------------------------------------------
delete_table() ->
    gnosus_dbi:delete_table(client_users).
    
%%--------------------------------------------------------------------------------
clear_table() ->
    gnosus_dbi:clear_table(client_users).
 
%%================================================================================
find(all) ->
    gnosus_dbi:q(qlc:q([X || X <- mnesia:table(client_users)])).
 
%%--------------------------------------------------------------------------------
find(Uid, Host) ->
	case gnosus_dbi:read_row(client_users, {Uid, Host}, #client_users.jid) of
		[] ->
			notfound;
		aborted ->
			error;
        Result ->
            hd(Result)
     end.

%%--------------------------------------------------------------------------------
find_by_email(EMail) ->
    case gnosus_dbi:read_row({client_users, EMail}) of
        [] ->
            notfound;
	aborted ->
	    error;
        Result ->
            hd(Result)
     end.

%%--------------------------------------------------------------------------------
find_all_by_host(Host) ->
	gnosus_dbi:dirty_select(client_users, [{#client_users{jid = '$1', _ = '_'}, [{'==', {element, 2, '$1'}, Host}], ['$1']}]).
 
%%--------------------------------------------------------------------------------
count() ->
    gnosus_dbi:count(users).
 
%%--------------------------------------------------------------------------------
delete(Uid, Host) ->
    case find(Uid, Host) of	
		notfound -> 
	    	notfound;
		error -> 
	    	error;
		_ ->
	   		gnosus_dbi:delete_row({client_users, {Uid, Host}})
    end.
 

%%================================================================================
password(Uid, Host, Password) ->
	case find(Uid, Host) of
    	notfound ->
            notfound;
		error -> 
	    	error;
        User ->
            write(User#client_users{password=Password, updated_at = now()})
    end.

%%================================================================================
email(Uid, Host, EMail) ->
    case find(Uid, Host) of
        notfound ->
            notfound;
    	error -> 
        	error;
        User ->
            write(User#client_users{email=EMail, updated_at = now()})
    end.

%%--------------------------------------------------------------------------------
status(Uid, Host, Status) ->
    case find(Uid, Host) of
        notfound ->
            notfound;
		error -> 
	    	error;
        User ->
            write(User#client_users{status=Status, updated_at = now()})
    end.

%%================================================================================
write(U) when is_record(U, client_users) ->
    gnosus_dbi:write_row(U);
write(_) ->
    error.
 
%%--------------------------------------------------------------------------------
new(Uid, Host, EMail, Password, Status) ->
    write(init_record(Uid, Host, EMail, Password, Status)).

%%--------------------------------------------------------------------------------
init_record(Uid, Host, EMail, Password, Status) ->
    {S1,S2,S3} = now(),
    random:seed(S1,S2,S3),
    #client_users{
		      jid={Uid, Host},		 
              email=EMail,
		      password=Password, 
		      status=Status,
		      registration_code=random:uniform(100000000),
		      created_at=now(),
		      updated_at=now(),
		      last_login=never,
		      login_count=0,
		      failed_login_count=0
		  }.

%%--------------------------------------------------------------------------------
register(Uid, Host, EMail) ->
   new(Uid, Host, EMail, undefined, registered).

%%--------------------------------------------------------------------------------
new_user(Uid, Host, EMail, Password) ->
   new(Uid, Host, EMail, Password, active).

%%--------------------------------------------------------------------------------
new_client_user_from_user(Host, User, Status) when is_record(User, users) ->
    new(User#users.uid, Host, User#users.email, User#users.password, Status).
    
%%--------------------------------------------------------------------------------
update(Uid, Host, EMail, Password, Status) ->
   case find(Uid, Host) of
      notfound ->
          	new(Uid, Host, EMail, Password, Status);
      User ->
	    	write(User#client_users{
		              jid={Uid, Host},			     
	    	          email=EMail,
			          password=Password,
			          status=Status, 
			          updated_at=now()
			      })
    end.
