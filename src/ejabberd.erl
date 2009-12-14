%%--------------------------------------------------------------------------------
-module(ejabberd).

%% API
-export([
	 register_user/3,
	 unregister_user/2,
	 unregister_all_users/1,
	 domains/0,
	 modules/0,
	 add_access_control/3,
	 add_domain_admin_access_control/2,
	 remove_access_control/2,
	 remove_domain_admin_access_control/1,
	 add_domain/1,
	 add_domain_and_user/3,
	 remove_domain/1,
	 remove_domains_and_users/1,
	 remove_domain_and_users/1
]).

%% include
-include_lib("gnosus.hrl").

%%================================================================================
register_user(Uid, Domain, Password) ->
    case rpc:call(?EJABBERD, ejabberd_admin, register, [Uid, Domain, Password]) of
	{ok, _} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
unregister_user(Uid, Domain) ->
    case rpc:call(?EJABBERD, ejabberd_admin, unregister, [Uid, Domain]) of
	{ok, _} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
unregister_all_users(Domain) ->
    lists:foldl(fun({Uid, _UserDomain}, ok) ->
			unregister_user(Uid, Domain);
		   (_, error) ->
			error
		end, ok, passwd_model:find_all_by_domain(Domain)).		  

%%--------------------------------------------------------------------------------
add_access_control(Domain, Acl, AclSpec) ->
    case rpc:call(?EJABBERD, acl, add, [Domain, Acl, AclSpec]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
add_domain_admin_access_control(Uid, Domain) ->
    add_access_control(Domain, admin, {user, Uid, Domain}).

%%--------------------------------------------------------------------------------
remove_access_control(Domain, Acl) ->
    case rpc:call(?EJABBERD, acl, remove, [Domain, Acl]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
remove_domain_admin_access_control(Domain) ->
    remove_access_control(Domain, admin).

%%--------------------------------------------------------------------------------
add_domain(Domain) ->
    add_domain(Domain, {add_domain_config, []}).

add_domain(Domain, {add_domain_config, State}) ->
    case add_domain_config(Domain) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Domain, add_domain_config]}),
	    add_domain(Domain, {add_authentication, [update_domains|State]});
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Domain, add_domain_config]}),
	    {error, State}
    end;

add_domain(Domain, {add_authentication, State}) ->
    case add_authentication(Domain, internal) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Domain, add_authentication]}),
	    add_domain(Domain, {add_modules, [add_authentication|State], modules()});
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Domain, add_authentication]}),
	    {error, State}
    end;

add_domain(Domain, {add_modules, State, Modules}) ->
    case add_modules(Domain, Modules) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Domain, add_modules]}),
	    add_domain(Domain, {start_modules, [add_modules|State], Modules});
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Domain, add_modules]}),
	    {error, State}
    end;

add_domain(Domain, {start_modules, State, Modules}) ->
    case start_modules(Domain, Modules) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Domain, start_modules]}),
	    add_domain(Domain, {register_route, [start_modules|State]});
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Domain, start_modules]}),
	    {error, State}
    end;

add_domain(Domain, {register_route, State}) ->
    case register_route(Domain) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Domain, register_route]}),
	    {ok, [register_route|State]};
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Domain, register_route]}),
	    {error, State}
    end;

add_domain(Domain, _) ->
    add_domain(Domain).

%%--------------------------------------------------------------------------------
add_domain_and_user(Domain, Uid, Password) ->
    case add_domain(Domain) of
	{ok, State} ->
	    gnosus_logger:message({add_domain_and_user_succeeded, [Domain, Uid, add_domain]}),
	    add_domain_and_user(Domain, Uid, Password, {register_user, State}) ;
	{error, State} ->
	    gnosus_logger:alarm({add_domain_and_user_failed, [Domain, Uid, add_domain]}),
	    {error, State}
    end.

add_domain_and_user(Domain, Uid, Password, {register_user, State}) ->
    case register_user(Uid, Domain, Password) of
	ok ->
	    gnosus_logger:message({add_domain_and_user_succeeded, [Domain, Uid, register_user]}),
	    add_domain_and_user(Domain, Uid, Password, {add_domain_admin_access_control, [register_user|State]}) ;
	error ->
	    gnosus_logger:alarm({add_domain_and_user_failed, [Domain, Uid, register_user]}),
	    {error, State}
    end;

add_domain_and_user(Domain, Uid, _Password, {add_domain_admin_access_control, State}) ->
    case add_domain_admin_access_control(Uid, Domain) of
	ok ->
	    gnosus_logger:message({add_domain_and_user_succeeded, [Domain, Uid, add_domain_admin_access_control]}),
	    {ok, [add_domain_admin_access_control|State]};
	error ->
	    gnosus_logger:alarm({add_domain_and_user_failed, [Domain, Uid, add_domain_admin_access_control]}),
	    {error, State}
    end;

add_domain_and_user(Domain, Uid, Password, _) ->
    add_domain_and_user(Domain, Uid, Password).


%%--------------------------------------------------------------------------------
remove_domain(Domain) ->
    remove_domain(Domain, {unregister_route, []}).

remove_domain(Domain, {unregister_route, State}) ->
    case unregister_route(Domain) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Domain, unregister_route]}),
	    remove_domain(Domain, {stop_modules, [unregister_route|State], modules(Domain)});
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Domain, unregister_route]}),
	    {error, State}
    end;

remove_domain(Domain, {stop_modules, State, Modules}) ->
    case stop_modules(Domain, Modules) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Domain, stop_modules]}),
	    remove_domain(Domain, {remove_domain_config, [stop_modules|State]});
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Domain, stop_modules]}),
	    {error, State}
    end;

remove_domain(Domain, {remove_domain_config, State}) ->
    case remove_domain_config(Domain) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Domain, remove_domain_config]}),
	    remove_domain(Domain, {remove_authentication, [remove_domains|State]});
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Domain, remove_domain_config]}),
	    {error, State}
    end;

remove_domain(Domain, {remove_authentication, State}) ->
    case remove_authentication(Domain) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Domain, remove_authentication]}),
	    remove_domain(Domain, {remove_modules, [remove_authentication|State]});
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Domain, remove_authentication]}),
	    {error, State}
    end;

remove_domain(Domain, {remove_modules, State}) ->
    case remove_modules(Domain) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Domain, remove_modules]}),
	    {ok, [remove_modules|State]};
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Domain, remove_modules]}),
	    {error, State}
    end;

remove_domain(Domain, _) ->
    remove_domain(Domain).

%%--------------------------------------------------------------------------------
remove_domains_and_users(Domains) ->
    lists:foldl(fun(Domain, {ok, _State}) ->
			remove_domain_and_users(Domain);
		   (_, {error, State}) ->
			{error, State}
		end, {ok, []}, Domains).		  

%%--------------------------------------------------------------------------------
remove_domain_and_users(Domain) ->
    case remove_domain_admin_access_control(Domain) of
	ok ->
	    gnosus_logger:message({remove_domain_and_users_succeeded, [Domain, remove_domain_admin_access_control]}),
	    remove_domain_and_users(Domain, {unregister_all_users, [remove_domain_admin_access_control]}) ;
	error ->
	    gnosus_logger:alarm({remove_domain_and_users_failed, [Domain, remove_domain_admin_access_control]}),
	    {error, []}
    end.

remove_domain_and_users(Domain, {unregister_all_users, State}) ->
    case unregister_all_users(Domain) of
	ok ->
	    gnosus_logger:message({remove_domain_and_users_succeeded, [Domain, unregister_all_users]}),
	    remove_domain_and_users(Domain, {remove_domain, [unregister_all_users|State]}) ;
	error ->
	    gnosus_logger:alarm({remove_domain_and_users_failed, [Domain, unregister_all_users]}),
	    {error, State}
    end;

remove_domain_and_users(Domain, {remove_domain, State}) ->
    case remove_domain(Domain) of
	{ok, DomainState} ->
	    gnosus_logger:message({remove_domain_and_users_succeeded, [Domain, remove_domain]}),
	    {ok, DomainState++State};
	{error, DomainState} ->
	    gnosus_logger:alarm({remove_domain_and_users_failed, [Domain, remove_domain]}),
	    {error, DomainState++State}
    end;

remove_domain_and_users(Domain, _) ->
    remove_domain_and_users(Domain).


%%================================================================================
domains() ->
    rpc:call(?EJABBERD, ejabberd_config, get_global_option, [hosts]).
 
%%--------------------------------------------------------------------------------
modules() ->   
    modules(?EJABBERD_ROOT_DOMAIN).

%%--------------------------------------------------------------------------------
modules(Domain) ->   
    rpc:call(?EJABBERD, ejabberd_config, get_local_option, [{modules, Domain}]).

%%================================================================================
add_domain_config(Domain) ->   
    case rpc:call(?EJABBERD, ejabberd_config, add_global_option, [hosts, domains()++[Domain]]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
remove_domain_config(Domain) ->  
    case rpc:call(?EJABBERD, ejabberd_config, add_global_option, [hosts, lists:delete(Domain, domains())]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.
    
%%--------------------------------------------------------------------------------
add_modules(Domain, Modules) ->   
    case rpc:call(?EJABBERD, ejabberd_config, add_local_option, [{modules, Domain}, Modules]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
remove_modules(Domain) ->   
    case rpc:call(?EJABBERD, ejabberd_config, remove_local_option, [{modules, Domain}]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
add_authentication(Domain, AuthMethod) ->
    case rpc:call(?EJABBERD, ejabberd_config, add_local_option, [{auth_method, Domain}, AuthMethod]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.
    
%%--------------------------------------------------------------------------------
remove_authentication(Domain) ->
    case rpc:call(?EJABBERD, ejabberd_config, remove_local_option, [{auth_method, Domain}]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.
    
%%--------------------------------------------------------------------------------
start_modules(Domain, Modules) ->
    lists:foldl(fun({Module, Args}, ok) ->
			start_module(Domain, Module, Args);
		   (_, error) ->
			error
		end, ok, Modules).		  

%%--------------------------------------------------------------------------------
start_module(Domain, Module, Args) ->
    case rpc:call(?EJABBERD, gen_mod, start_module, [Domain, Module, Args]) of
	ok ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
stop_modules(Domain, Modules) ->
    lists:foldl(fun({Module, _Args}, ok) ->
			stop_module(Domain, Module);
		   (_, error) ->
			error
		end, ok, Modules).		  


%%--------------------------------------------------------------------------------
stop_module(Domain, Module) ->
    case rpc:call(?EJABBERD, gen_mod, stop_module, [Domain, Module]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
register_route(Domain) ->
    case rpc:call(?EJABBERD, ejabberd_local, register_host, [Domain]) of
	{register_host, _} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
unregister_route(Domain) ->
    case rpc:call(?EJABBERD, ejabberd_local, unregister_host, [Domain]) of
	{unregister_host, _} ->
	    ok;
	_ ->
	    error
    end.
