%% user database model
%%--------------------------------------------------------------------------------
-module(user_model).
 
%% API
-export([
    fields/0, 
    registration_status_values/0,
    role_values/0,
    product_values/0,
    create_table/0,
    delete_table/0,
    clear_table/0,
    delete/1,
    find/1,
    find_by_email/1,
    count/0,
    password/2,
    email/2,
    registration_status/2,
    role/2,
    product/2,
    register/1,
    new_user/3,
    new_admin/3,
    write/1,
    new/6,
    new/1,
    update/6,
    update/1,
    authenticate/2
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib("stdlib/include/qlc.hrl").
 
%%================================================================================
fields() ->
	record_info(fields, users).

%%--------------------------------------------------------------------------------
registration_status_values() ->
    [active, inactive, registered].

%%--------------------------------------------------------------------------------
role_values() ->
    [admin, user].

%%--------------------------------------------------------------------------------
product_values() ->
    [free, unlimited].

%%================================================================================
create_table() ->
	gnosus_dbi:create_table(users, [{attributes, fields()}, {disc_only_copies, [node()]}, {index, [uid]}]).
    
%%--------------------------------------------------------------------------------
delete_table() ->
    gnosus_dbi:delete_table(users).
    
%%--------------------------------------------------------------------------------
clear_table() ->
    gnosus_dbi:clear_table(users).
 
%%================================================================================
find(all) ->
    gnosus_dbi:q(qlc:q([X || X <- mnesia:table(users)]));
 
%%--------------------------------------------------------------------------------
find(Uid) ->
	case gnosus_dbi:read_row(users, Uid, #users.uid) of
		[] ->
			notfound;
		aborted ->
			error;
        Result ->
            hd(Result)
     end.

%%--------------------------------------------------------------------------------
find_by_email(EMail) ->
    case gnosus_dbi:read_row({users, EMail}) of
        [] ->
            notfound;
	aborted ->
	    error;
        Result ->
            hd(Result)
     end.
 
%%--------------------------------------------------------------------------------
count() ->
    gnosus_dbi:count(users).
 
%%--------------------------------------------------------------------------------
delete(Uid) ->
    case find(Uid) of	
		notfound -> 
	    	notfound;
		error -> 
	    	error;
		User ->
	   		gnosus_dbi:delete_row({users, User#users.email})
    end.
 

%%================================================================================
password(Uid, Password) ->
	case find(Uid) of
    	notfound ->
            notfound;
        User ->
            write(User#users{password=Password, updated_at = now()})
    end.

%%================================================================================
email(Uid, EMail) ->
    case find(Uid) of
        notfound ->
            notfound;
        User ->
            write(User#users{email=EMail, updated_at = now()})
    end.

%%--------------------------------------------------------------------------------
registration_status(Uid, RegistrationStatus) ->
    case find(Uid) of
        notfound ->
            notfound;
        User ->
            write(User#users{registration_status=RegistrationStatus, updated_at = now()})
    end.

%%--------------------------------------------------------------------------------
role(Uid, Role) ->
    case find(Uid) of
        notfound ->
            notfound;
        User ->
            write(User#users{role=Role, updated_at = now()})
    end.

%%--------------------------------------------------------------------------------
product(Uid, Product) ->
    case find(Uid) of
        notfound ->
            notfound;
        User ->
            write(User#users{product=Product, updated_at = now()})
    end.

%%================================================================================
write(U) when is_record(U, users) ->
    gnosus_dbi:write_row(U);
write(_) ->
    error.
 
%%--------------------------------------------------------------------------------
new(EMail, Uid, Password, RegistrationStatus, Role, Product) ->
    {S1,S2,S3} = now(),
    random:seed(S1,S2,S3),
    write(#users{email=EMail,
		 uid=Uid,		 
		 password=Password, 
		 registration_status=RegistrationStatus,
		 role=Role,
		 product = Product,
		 registration_code=random:uniform(100000000),
		 created_at=now(),
		 updated_at=now(),
		 last_login=never,
		 login_count=0,
		 failed_login_count=0}).

%%--------------------------------------------------------------------------------
new(U) when is_record(U, users) ->
    new(
      U#users.email,
      U#users.uid, 
      U#users.password, 
      U#users.registration_status, 
      U#users.role, 
      U#users.product);
new(_) ->
    error.

%%--------------------------------------------------------------------------------
register(EMail) ->
   new(EMail, "undefined", "undefined", registered, user, free).

%%--------------------------------------------------------------------------------
new_user(EMail, Uid, Password) ->
   new(EMail, Uid, Password, active, user, free).

%%--------------------------------------------------------------------------------
new_admin(EMail, Uid, Password) ->
   new(EMail, Uid, Password, active, admin, unlimited).

%%--------------------------------------------------------------------------------
update(EMail, Uid, Password, RegistrationStatus, Role, Product) ->
   case find(Uid) of
      notfound ->
          	new(EMail, Uid, Password, RegistrationStatus, Role, Product);
      User ->
	    	write(User#users{email=EMail,
				uid=Uid,			     
			    password=Password,
			    registration_status=RegistrationStatus, 
			    role=Role,
			    product=Product,
			    updated_at=now()})
    end.

%%--------------------------------------------------------------------------------
update(U) when is_record(U, users) ->
    update(
      	U#users.email,
      	U#users.uid, 
      	U#users.password, 
      	U#users.registration_status, 
      	U#users.role, 
      	U#users.product);
update(_) ->
    	error.


%%--------------------------------------------------------------------------------
authenticate(Uid, Password) ->
    case find(Uid) of
        notfound -> 
            gnosus_logger:warning({authentication_failed, Uid}),                     
            false;
        error -> 
            gnosus_logger:warning({authentication_failed, Uid}),                     
            false;
        User ->
	    	[Count, Failed, Auth] = case User#users.password =:= Password of
										true ->
											[User#users.login_count+1, User#users.failed_login_count, true];
										false ->
											[User#users.login_count, User#users.failed_login_count+1, false]
									end,
	    	write(User#users{login_count=Count, failed_login_count=Failed, last_login=now(), updated_at = now()}),   
            gnosus_logger:message({authenticated, Uid}),                     
	    	Auth
    end.

