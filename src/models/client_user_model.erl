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
    register/2,
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
find(Host, Uid) ->
	case gnosus_dbi:read_row({client_users, {Host, Uid}}) of
		[] ->
			notfound;
		aborted ->
			error;
        Result ->
            hd(Result)
     end.

%%--------------------------------------------------------------------------------
find_by_email(EMail) ->
    case gnosus_dbi:read_row(client_users, EMail, #client_users.email) of
        [] ->
            notfound;
	aborted ->
	    error;
        Result ->
            hd(Result)
     end.

%%--------------------------------------------------------------------------------
find_all_by_host(Host) ->
	gnosus_dbi:dirty_match_object({client_users, {Host, '_'}, '_','_','_','_','_','_','_','_','_'}).
 
%%--------------------------------------------------------------------------------
count() ->
    gnosus_dbi:count(users).
 
%%--------------------------------------------------------------------------------
delete(Host, Uid) ->
    case find(Host, Uid) of	
		notfound -> 
	    	notfound;
		error -> 
	    	error;
		_ ->
	   		gnosus_dbi:delete_row({client_users, {Host, Uid}})
    end.
 

%%================================================================================
password(Host, Uid, Password) ->
	case find(Host, Uid) of
    	notfound ->
            notfound;
		error -> 
	    	error;
        User ->
            write(User#client_users{password=Password, updated_at = now()})
    end.

%%================================================================================
email(Host, Uid, EMail) ->
    case find(Host, Uid) of
        notfound ->
            notfound;
    	error -> 
        	error;
        User ->
            write(User#client_users{email=EMail, updated_at = now()})
    end.

%%--------------------------------------------------------------------------------
status(Host, Uid,Status) ->
    case find(Host, Uid) of
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
new(Host, Uid, EMail, Password, Status) ->
    write(init_record(Host, Uid, EMail, Password, Status)).

%%--------------------------------------------------------------------------------
init_record(Host, Uid, EMail, Password, Status) ->
    {S1,S2,S3} = now(),
    random:seed(S1,S2,S3),
    #client_users{
		      jid={Host, Uid},		 
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
register(Host, EMail) ->
   new(Host, EMail, EMail, undefined, registered).

%%--------------------------------------------------------------------------------
new_user(Host, Uid, EMail, Password) ->
   new(Host, Uid, EMail, Password, active).

%%--------------------------------------------------------------------------------
new_client_user_from_user(Host, User, Status) when is_record(User, users) ->
    new(Host, User#users.uid, User#users.email, User#users.password, Status).
    
%%--------------------------------------------------------------------------------
update(Host, Uid, EMail, Password, Status) ->
   case find(Host, Uid) of
      notfound ->
          	new(Host, Uid, EMail, Password, Status);
      User ->
	    	write(User#client_users{
		              jid={Host, Uid},			     
	    	          email=EMail,
			          password=Password,
			          status=Status, 
			          updated_at=now()
			      })
    end.
