%%--------------------------------------------------------------------------------
-module(ejabberd).

%% API
-export([
    add_user/1,
	add_user/3,
	remove_user/1,
	remove_user/2,
	remove_all_users/1,
	add_access_control/3,
	add_host_admin_access_control/1,
	add_host_admin_access_control/2,
	remove_access_control/2,
	remove_host_admin_access_control/1,
	add_host/1,
	add_host_and_user/3,
	remove_host/1,
	remove_host_and_users/1,
	remove_hosts_and_users/1,
	ejabberd_hosts/0,
	modules/1,
	password/1,
	add_host_config/1,
	remove_host_config/1,
	add_module_configs/1,
	remove_module_configs/1,
	add_authentication_method/1,
	remove_authentication_method/1,
	add_modules/1,
	add_module/3,
	remove_modules/1,
	remove_module/2,
	add_route/1,
	remove_route/1
]).

%% include
-include_lib("gnosus.hrl").
-include_lib("models.hrl").

%%================================================================================
add_user(Args) ->
    add_user(uid(Args), host(Args), password(Args)).

%%--------------------------------------------------------------------------------
add_user(Uid, Host, Password) ->
    case rpc:call(ejabberd(), ejabberd_admin, register, [Uid, Host, Password]) of
	    {ok, _} ->
	        gnosus_logger:message({add_host_user_succeeded, [Uid, Host]}),
	        ok;
	    _ ->
	        gnosus_logger:message({add_host_user_failed, [Uid, Host]}),
	        error
    end.

%%--------------------------------------------------------------------------------
remove_user(Args) ->
    remove_user(uid(Args), host(Args)).

%%--------------------------------------------------------------------------------
remove_user(Uid, Host) ->
    case rpc:call(ejabberd(), ejabberd_admin, unregister, [Uid, Host]) of
	    {ok, _} ->
	        gnosus_logger:message({remove_host_user_succeeded, [Uid, Host]}),
	        ok;
	    _ ->
	        gnosus_logger:message({remove_host_user_failed, [Uid, Host]}),
	        error
    end.

%%--------------------------------------------------------------------------------
remove_all_users(Args) when is_list(Args) ->
    remove_all_users(host(Args));
    
remove_all_users(Host) ->
    lists:foldl(fun({Uid, _UserHost}, ok) ->
			           remove_user(Uid, Host);
		           (_, error) ->
			           error
		        end, ok, passwd_model:find_all_by_host(Host)).		  

%%--------------------------------------------------------------------------------
add_access_control(Host, Acl, AclSpec) ->
    case rpc:call(ejabberd(), acl, add, [Host, Acl, AclSpec]) of
	{atomic, ok} ->
	    ok;
	_ ->
	    error
    end.

%%--------------------------------------------------------------------------------
add_host_admin_access_control(Args) ->
    add_host_admin_access_control(uid(Args), host(Args)).

%%--------------------------------------------------------------------------------
add_host_admin_access_control(Uid, Host) ->
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
remove_host_admin_access_control(Args)  when is_list(Args) ->
    remove_host_admin_access_control(host(Args));

remove_host_admin_access_control(Host) ->
    remove_access_control(Host, admin).

%%================================================================================
add_host(Host) ->
    Args = [{host, Host}],
    case apply_method_list([add_module_configs, add_authentication_method, add_host_config, add_modules, add_route], Args) of
	    {ok, State} ->
	        gnosus_logger:message({add_host_succeeded, Host}),
	        {ok, State};
	    {error, State} ->
	        gnosus_logger:alarm({add_host_failed, Host}),
	        rollback(Args, add_to_remove(State))
    end.

%%--------------------------------------------------------------------------------
add_host_and_user(Host, Uid, Password) ->
    Args = [{host, Host}, {uid, Uid}, {password, Password}],
    case add_host(Host) of
	    {ok, _} ->
	        case apply_method_list([add_user, add_host_admin_access_control], Args) of
        	    {ok, State} ->
        	        gnosus_logger:message({add_host_and_user_succeeded, [Host, Uid]}),
        	        {ok, State};
        	    {error, State} ->
        	        gnosus_logger:alarm({add_host_and_user_failed, [Host, Uid]}),
        	        rollback(Args, add_to_remove(State))
    	    end;
	    {error, State} ->
	        gnosus_logger:alarm({add_host_and_user_failed, [Host, Uid]}),
	        {error, State}
    end.
                             
%%================================================================================
remove_host(Host) ->
    Args = [{host, Host}],
    case apply_method_list([remove_route, remove_modules, remove_host_config, remove_authentication_method, remove_module_configs], Args) of
   	    {ok, State} ->
   	        gnosus_logger:message({remove_host_succeeded, Host}),
   	        {ok, State};
   	    {error, State} ->
   	        gnosus_logger:alarm({remove_host_failed, Host}),
	        rollback(Args, remove_to_add(State))
    end.

%%--------------------------------------------------------------------------------
remove_host_and_users(Host) ->
    Args = [{host, Host}],
    case apply_method_list([remove_host_admin_access_control, remove_all_users], Args) of
	    {ok, _State} ->
	        gnosus_logger:alarm({remove_host_and_users_succeeded, Host}),
	        remove_host(Host);
	    {error, State} ->
	        gnosus_logger:alarm({remove_host_and_users_failed, Host}),
	        rollback(Args, remove_to_add(State))
    end.

%%--------------------------------------------------------------------------------
remove_hosts_and_users(Hosts) ->
    lists:foldl(fun(Host, {ok, _State}) ->
			            remove_host_and_users(Host);
		            (_, {error, State}) ->
			            {error, State}
		end, {ok, []}, Hosts).		  

%%================================================================================
ejabberd_hosts() ->
    rpc:call(ejabberd(), ejabberd_config, get_global_option, [hosts]).

%%--------------------------------------------------------------------------------
host(Args) ->
    {host, Val} = lists:keyfind(host, 1, Args),
    Val.

%%--------------------------------------------------------------------------------
password(Args) ->
    {host, Val} = lists:keyfind(password, 1, Args),
    Val.

%%--------------------------------------------------------------------------------
uid(Args) ->
    {host, Val} = lists:keyfind(uid, 1, Args),
    Val.

%%--------------------------------------------------------------------------------
modules(Host) ->   
    gnosus_model:modules(Host).

%%--------------------------------------------------------------------------------
ejabberd() ->   
    gnosus_model:ejabberd().

%%--------------------------------------------------------------------------------
add_to_remove(MethodList) ->
    lists:map(fun(M)->list_to_atom("remove"++(atom_to_list(M)--"add"))end, MethodList).

%%--------------------------------------------------------------------------------
remove_to_add(MethodList) ->
    lists:map(fun(M)->list_to_atom("add"++(atom_to_list(M)--"remove"))end, MethodList).

%%================================================================================
apply_method_list(MethodList, Args) ->
    apply_method_list(MethodList, Args, []).

%%--------------------------------------------------------------------------------
apply_method_list([Method|MethodList], Args, State) ->
    case apply(ejabberd, Method, [Args]) of
	    ok ->
	        gnosus_logger:message({apply_method_succeeded, [Method, Args]}),
	        case MethodList of
	            [] -> {ok, [Method|State]};
	            _ -> apply_method_list(MethodList, Args, [Method|State])
            end;
	    error ->
	        gnosus_logger:alarm({apply_method_failed, [Method, Args]}),
	        {error, State}
    end.

%%--------------------------------------------------------------------------------
rollback(Args, MethodList) ->
    case apply_method_list(Args, MethodList) of
	    {ok, State} ->
            gnosus_logger:message({rollback_succeeded}),
            {error, State};
	    {error, State} ->
            gnosus_logger:alarm({rollback_failed, [MethodList--State, Args]}),
            {error, State}
    end.
    

%%================================================================================
add_host_config(Args) ->   
    case rpc:call(ejabberd(), ejabberd_config, add_global_option, [hosts, ejabberd_hosts()++[host(Args)]]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
remove_host_config(Args) ->  
    case rpc:call(ejabberd(), ejabberd_config, add_global_option, [hosts, lists:delete(host(Args), ejabberd_hosts())]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.
    
%%--------------------------------------------------------------------------------
add_module_configs(Args) ->   
    Host = host(Args),
    case rpc:call(ejabberd(), ejabberd_config, add_local_option, [{modules, Host}, modules(Host)]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
remove_module_configs(Args) ->   
    case rpc:call(ejabberd(), ejabberd_config, remove_local_option, [{modules, host(Args)}]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
add_authentication_method(Args) ->
    case rpc:call(ejabberd(), ejabberd_config, add_local_option, [{auth_method, host(Args)}, internal]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.
    
%%--------------------------------------------------------------------------------
remove_authentication_method(Args) ->
    case rpc:call(ejabberd(), ejabberd_config, remove_local_option, [{auth_method, host(Args)}]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.
    
%%--------------------------------------------------------------------------------
add_modules(Args) ->
    Host = host(Args),
    lists:foldl(fun({Module, Opts}, ok) ->
			           add_module(Host, Module, Opts);
		           (_, error) ->
			           error
		        end, ok, modules(Host)).		  

%%--------------------------------------------------------------------------------
add_module(Host, Module, Opts) ->
    case rpc:call(ejabberd(), gen_mod, start_module, [Host, Module, Opts]) of
	    ok ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
remove_modules(Args) ->
    Host = host(Args),
    lists:foldl(fun({Module, _Opts}, ok) ->
			           remove_module(Host, Module);
		           (_, error) ->
			           error
		        end, ok, modules(Host)).		  

%%--------------------------------------------------------------------------------
remove_module(Host, Module) ->
    case rpc:call(ejabberd(), gen_mod, stop_module, [Host, Module]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
add_route(Args) ->
    case rpc:call(ejabberd(), ejabberd_local, register_host, [host(Args)]) of
	    {register_host, _} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
remove_route(Args) ->
    case rpc:call(ejabberd(), ejabberd_local, unregister_host, [host(Args)]) of
	    {unregister_host, _} ->
	        ok;
	    _ ->
	        error
    end.
