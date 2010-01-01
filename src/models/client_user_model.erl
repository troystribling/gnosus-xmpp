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
    find_by_jid/1,
    find_all_by_host/1,
    count/0,
    status/3,
    register/2,
    new_user/3,
    new_client_user_from_user/3,
    write/1,
    new/4,
    init_record/4,
    update/4,
    authenticate/2
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
find_by_jid(Jid) ->
    case gnosus_utils:jid_to_tuple(Jid) of
        {Host, Uid} -> find(Host, Uid);
        _ -> error
    end.

%%--------------------------------------------------------------------------------
find_all_by_host(Host) ->
	gnosus_dbi:dirty_match_object({client_users, {Host, '_'}, '_','_','_','_','_','_','_','_'}).
 
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
 

%%--------------------------------------------------------------------------------
status(Host, Uid, Status) ->
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
new(Host, Uid, EMail, Status) ->
    write(init_record(Host, Uid, EMail, Status)).

%%--------------------------------------------------------------------------------
init_record(Host, Uid, EMail, Status) ->
    {S1,S2,S3} = now(),
    random:seed(S1,S2,S3),
    #client_users{
		      jid={Host, Uid},		 
              email=EMail,
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
   new(Host, EMail, EMail, registered).

%%--------------------------------------------------------------------------------
new_user(Host, Uid, EMail) ->
   new(Host, Uid, EMail, active).

%%--------------------------------------------------------------------------------
new_client_user_from_user(Host, User, Status) when is_record(User, users) ->
    new(Host, User#users.uid, User#users.email, Status).
    
%%--------------------------------------------------------------------------------
update(Host, Uid, EMail, Status) ->
   case find(Host, Uid) of
      notfound ->
          	new(Host, Uid, EMail, Status);
      User ->
	    	write(User#client_users{
		              jid={Host, Uid},			     
	    	          email=EMail,
			          status=Status, 
			          updated_at=now()
			      })
    end.

%%--------------------------------------------------------------------------------
authenticate(Jid, EnteredPassword) ->
    case find_by_jid(Jid) of
        notfound -> 
            gnosus_logger:warning({authentication_failed, Jid}),                     
            false;
        error -> 
            gnosus_logger:warning({authentication_failed, Jid}),                     
            false;
        User -> case passwd_model:find(Jid) of
                    notfound -> 
                        gnosus_logger:warning({authentication_failed, Jid}),                     
                        false;
                    error -> 
                        gnosus_logger:warning({authentication_failed, Jid}),                     
                        false;
                    #passwd{password=Password} ->
            	    	[Count, Failed, Auth] = case User#client_users.status of
            	    	                            active ->
                    	    	                        case EnteredPassword =:= Password of
                    										true ->
                                                                gnosus_logger:message({authenticated, Jid}),                     
                    											[User#client_users.login_count+1, User#client_users.failed_login_count, true];
                    										false ->
                                                                gnosus_logger:warning({authentication_failed, Jid}),                     
                    											[User#client_users.login_count, User#client_users.failed_login_count+1, false]
                    									end;
                    								_ ->
                                                        gnosus_logger:warning({authentication_failed, Jid}),                     
                    								    [User#client_users.login_count, User#client_users.failed_login_count+1, false]
                    							end,
                        write(User#client_users{login_count=Count, failed_login_count=Failed, last_login=now(), updated_at = now()}),
                        Auth            	    	
                end
    end.
