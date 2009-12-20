%%--------------------------------------------------------------------------------
-module(ejabberd).

%% API
-export([
	 register_user/3,
	 unregister_user/2,
	 unregister_all_users/1,
	 hosts/0,
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
-include_lib("models.hrl").

%%================================================================================
register_user(Uid, Host, Password) ->
    case rpc:call(ejabberd(), ejabberd_admin, register, [Uid, Host, Password]) of
	{ok, _} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
unregister_user(Uid, Host) ->
    case rpc:call(ejabberd(), ejabberd_admin, unregister, [Uid, Host]) of
	{ok, _} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
unregister_all_users(Host) ->
    lists:foldl(fun({Uid, _UserHost}, ok) ->
			unregister_user(Uid, Host);
		   (_, error) ->
			error
		end, ok, passwd_model:find_all_by_domain(Host)).		  

%%--------------------------------------------------------------------------------
add_access_control(Host, Acl, AclSpec) ->
    case rpc:call(ejabberd(), acl, add, [Host, Acl, AclSpec]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
add_domain_admin_access_control(Uid, Host) ->
    add_access_control(Host, admin, {user, Uid, Host}).

%%--------------------------------------------------------------------------------
remove_access_control(Host, Acl) ->
    case rpc:call(ejabberd(), acl, remove, [Host, Acl]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
remove_domain_admin_access_control(Host) ->
    remove_access_control(Host, admin).

%%--------------------------------------------------------------------------------
add_domain(Host) ->
    add_domain(Host, {add_host_config, []}).

add_domain(Host, {add_host_config, State}) ->
    case add_host_config(Host) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Host, add_host_config]}),
	    add_domain(Host, {add_authentication, [update_domains|State]});
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Host, add_host_config]}),
	    {error, State}
    end;

add_domain(Host, {add_authentication, State}) ->
    case add_authentication(Host, internal) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Host, add_authentication]}),
	    add_domain(Host, {add_modules, [add_authentication|State], modules()});
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Host, add_authentication]}),
	    {error, State}
    end;

add_domain(Host, {add_modules, State, Modules}) ->
    case add_modules(Host, Modules) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Host, add_modules]}),
	    add_domain(Host, {start_modules, [add_modules|State], Modules});
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Host, add_modules]}),
	    {error, State}
    end;

add_domain(Host, {start_modules, State, Modules}) ->
    case start_modules(Host, Modules) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Host, start_modules]}),
	    add_domain(Host, {register_route, [start_modules|State]});
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Host, start_modules]}),
	    {error, State}
    end;

add_domain(Host, {register_route, State}) ->
    case register_route(Host) of
	ok ->
	    gnosus_logger:message({add_domain_succeeded, [Host, register_route]}),
	    {ok, [register_route|State]};
	error ->
	    gnosus_logger:alarm({add_domain_failed, [Host, register_route]}),
	    {error, State}
    end;

add_domain(Host, _) ->
    add_domain(Host).

%%--------------------------------------------------------------------------------
add_domain_and_user(Host, Uid, Password) ->
    case add_domain(Host) of
	{ok, State} ->
	    gnosus_logger:message({add_domain_and_user_succeeded, [Host, Uid, add_domain]}),
	    add_domain_and_user(Host, Uid, Password, {register_user, State}) ;
	{error, State} ->
	    gnosus_logger:alarm({add_domain_and_user_failed, [Host, Uid, add_domain]}),
	    {error, State}
    end.

add_domain_and_user(Host, Uid, Password, {register_user, State}) ->
    case register_user(Uid, Host, Password) of
	ok ->
	    gnosus_logger:message({add_domain_and_user_succeeded, [Host, Uid, register_user]}),
	    add_domain_and_user(Host, Uid, Password, {add_domain_admin_access_control, [register_user|State]}) ;
	error ->
	    gnosus_logger:alarm({add_domain_and_user_failed, [Host, Uid, register_user]}),
	    {error, State}
    end;

add_domain_and_user(Host, Uid, _Password, {add_domain_admin_access_control, State}) ->
    case add_domain_admin_access_control(Uid, Host) of
	ok ->
	    gnosus_logger:message({add_domain_and_user_succeeded, [Host, Uid, add_domain_admin_access_control]}),
	    {ok, [add_domain_admin_access_control|State]};
	error ->
	    gnosus_logger:alarm({add_domain_and_user_failed, [Host, Uid, add_domain_admin_access_control]}),
	    {error, State}
    end;

add_domain_and_user(Host, Uid, Password, _) ->
    add_domain_and_user(Host, Uid, Password).


%%--------------------------------------------------------------------------------
remove_domain(Host) ->
    remove_domain(Host, {unregister_route, []}).

remove_domain(Host, {unregister_route, State}) ->
    case unregister_route(Host) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Host, unregister_route]}),
	    remove_domain(Host, {stop_modules, [unregister_route|State], modules(Host)});
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Host, unregister_route]}),
	    {error, State}
    end;

remove_domain(Host, {stop_modules, State, Modules}) ->
    case stop_modules(Host, Modules) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Host, stop_modules]}),
	    remove_domain(Host, {remove_host_config, [stop_modules|State]});
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Host, stop_modules]}),
	    {error, State}
    end;

remove_domain(Host, {remove_host_config, State}) ->
    case remove_host_config(Host) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Host, remove_host_config]}),
	    remove_domain(Host, {remove_authentication, [remove_domains|State]});
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Host, remove_host_config]}),
	    {error, State}
    end;

remove_domain(Host, {remove_authentication, State}) ->
    case remove_authentication(Host) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Host, remove_authentication]}),
	    remove_domain(Host, {remove_modules, [remove_authentication|State]});
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Host, remove_authentication]}),
	    {error, State}
    end;

remove_domain(Host, {remove_modules, State}) ->
    case remove_modules(Host) of
	ok ->
	    gnosus_logger:message({remove_domain_succeeded, [Host, remove_modules]}),
	    {ok, [remove_modules|State]};
	error ->
	    gnosus_logger:alarm({remove_domain_failed, [Host, remove_modules]}),
	    {error, State}
    end;

remove_domain(Host, _) ->
    remove_domain(Host).

%%--------------------------------------------------------------------------------
remove_domains_and_users(Hosts) ->
    lists:foldl(fun(Host, {ok, _State}) ->
			            remove_domain_and_users(Host);
		            (_, {error, State}) ->
			            {error, State}
		end, {ok, []}, Hosts).		  

%%--------------------------------------------------------------------------------
remove_domain_and_users(Host) ->
    case remove_domain_admin_access_control(Host) of
	ok ->
	    gnosus_logger:message({remove_domain_and_users_succeeded, [Host, remove_domain_admin_access_control]}),
	    remove_domain_and_users(Host, {unregister_all_users, [remove_domain_admin_access_control]}) ;
	error ->
	    gnosus_logger:alarm({remove_domain_and_users_failed, [Host, remove_domain_admin_access_control]}),
	    {error, []}
    end.

remove_domain_and_users(Host, {unregister_all_users, State}) ->
    case unregister_all_users(Host) of
	ok ->
	    gnosus_logger:message({remove_domain_and_users_succeeded, [Host, unregister_all_users]}),
	    remove_domain_and_users(Host, {remove_domain, [unregister_all_users|State]}) ;
	error ->
	    gnosus_logger:alarm({remove_domain_and_users_failed, [Host, unregister_all_users]}),
	    {error, State}
    end;

remove_domain_and_users(Host, {remove_domain, State}) ->
    case remove_domain(Host) of
	{ok, HostState} ->
	    gnosus_logger:message({remove_domain_and_users_succeeded, [Host, remove_domain]}),
	    {ok, HostState++State};
	{error, HostState} ->
	    gnosus_logger:alarm({remove_domain_and_users_failed, [Host, remove_domain]}),
	    {error, HostState++State}
    end;

remove_domain_and_users(Host, _) ->
    remove_domain_and_users(Host).


%%================================================================================
hosts() ->
    rpc:call(ejabberd(), ejabberd_config, get_global_option, [hosts]).
 
%%--------------------------------------------------------------------------------
modules() ->   
    config_model:modules().

%%--------------------------------------------------------------------------------
ejabberd() ->   
    config_model:ejabberd().

%%--------------------------------------------------------------------------------
modules(Host) ->   
    rpc:call(ejabberd(), ejabberd_config, get_local_option, [{modules, Host}]).

%%================================================================================
add_host_config(Host) ->   
    case rpc:call(ejabberd(), ejabberd_config, add_global_option, [hosts, hosts()++[Host]]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
remove_host_config(Host) ->  
    case rpc:call(ejabberd(), ejabberd_config, add_global_option, [hosts, lists:delete(Host, hosts())]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.
    
%%--------------------------------------------------------------------------------
add_modules(Host, Modules) ->   
    case rpc:call(ejabberd(), ejabberd_config, add_local_option, [{modules, Host}, Modules]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
remove_modules(Host) ->   
    case rpc:call(ejabberd(), ejabberd_config, remove_local_option, [{modules, Host}]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
add_authentication(Host, AuthMethod) ->
    case rpc:call(ejabberd(), ejabberd_config, add_local_option, [{auth_method, Host}, AuthMethod]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.
    
%%--------------------------------------------------------------------------------
remove_authentication(Host) ->
    case rpc:call(ejabberd(), ejabberd_config, remove_local_option, [{auth_method, Host}]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.
    
%%--------------------------------------------------------------------------------
start_modules(Host, Modules) ->
    lists:foldl(fun({Module, Args}, ok) ->
			start_module(Host, Module, Args);
		   (_, error) ->
			error
		end, ok, Modules).		  

%%--------------------------------------------------------------------------------
start_module(Host, Module, Args) ->
    case rpc:call(ejabberd(), gen_mod, start_module, [Host, Module, Args]) of
	    ok ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
stop_modules(Host, Modules) ->
    lists:foldl(fun({Module, _Args}, ok) ->
			stop_module(Host, Module);
		   (_, error) ->
			error
		end, ok, Modules).		  


%%--------------------------------------------------------------------------------
stop_module(Host, Module) ->
    case rpc:call(ejabberd(), gen_mod, stop_module, [Host, Module]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
register_route(Host) ->
    case rpc:call(ejabberd(), ejabberd_local, register_host, [Host]) of
	    {register_host, _} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
unregister_route(Host) ->
    case rpc:call(ejabberd(), ejabberd_local, unregister_host, [Host]) of
	    {unregister_host, _} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
apply_method_list(Host, [Method|MethodList], State) ->
    case apply(ejabberd, Method, Host) of
	    ok ->
	        gnosus_logger:message({remove_domain_succeeded, [Host, Method]}),
	        case MethodList of
	            [] -> {ok, [Method|State]};
	            _ -> apply_method_list(Host, MethodList, [Method|State])
            end;
	    error ->
	        gnosus_logger:alarm({remove_domain_failed, [Host, Method]}),
	        {error, State}
    end.
