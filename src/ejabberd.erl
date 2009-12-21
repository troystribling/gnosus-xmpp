%%--------------------------------------------------------------------------------
-module(ejabberd).

%% API
-export([
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
	 remove_hosts_and_users/1
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
    case apply_method_list(Args, [add_module_configs, add_authentication, add_host_config, add_modules, add_route]) of
	    {ok, State} ->
	        gnosus_logger:message({add_host_succeeded, Host}),
	        {ok, State};
	    {error, State} ->
	        gnosus_logger:alarm({add_host_failed, Host}),
	        {error, State}
    end.

%%--------------------------------------------------------------------------------
add_host_and_user(Host, Uid, Password) ->
    Args = [{host, Host}, {uid, Uid}, {password, Password}],
    case add_host(Host) of
	    {ok, State} ->
	        apply_method_list(Args, [add_user, add_host_admin_access_control], State);
	    {error, State} ->
	        gnosus_logger:alarm({add_host_and_user_failed, [Host, Uid]}),
	        {error, State}
    end.
                             
%%--------------------------------------------------------------------------------
% add_domain(Host) ->
%     add_domain(Host, {add_host_config, []}).
% 
% add_domain(Host, {add_host_config, State}) ->
%     case add_host_config(Host) of
%   ok ->
%       gnosus_logger:message({add_domain_succeeded, [Host, add_host_config]}),
%       add_domain(Host, {add_authentication, [update_domains|State]});
%   error ->
%       gnosus_logger:alarm({add_domain_failed, [Host, add_host_config]}),
%       {error, State}
%     end;
% 
% add_domain(Host, {add_authentication, State}) ->
%     case add_authentication(Host, internal) of
%   ok ->
%       gnosus_logger:message({add_domain_succeeded, [Host, add_authentication]}),
%       add_domain(Host, {add_modules, [add_authentication|State], modules()});
%   error ->
%       gnosus_logger:alarm({add_domain_failed, [Host, add_authentication]}),
%       {error, State}
%     end;
% 
% add_domain(Host, {add_modules, State, Modules}) ->
%     case add_modules(Host, Modules) of
%   ok ->
%       gnosus_logger:message({add_domain_succeeded, [Host, add_modules]}),
%       add_domain(Host, {start_modules, [add_modules|State], Modules});
%   error ->
%       gnosus_logger:alarm({add_domain_failed, [Host, add_modules]}),
%       {error, State}
%     end;
% 
% add_domain(Host, {start_modules, State, Modules}) ->
%     case start_modules(Host, Modules) of
%   ok ->
%       gnosus_logger:message({add_domain_succeeded, [Host, start_modules]}),
%       add_domain(Host, {register_route, [start_modules|State]});
%   error ->
%       gnosus_logger:alarm({add_domain_failed, [Host, start_modules]}),
%       {error, State}
%     end;
% 
% add_domain(Host, {register_route, State}) ->
%     case register_route(Host) of
%   ok ->
%       gnosus_logger:message({add_domain_succeeded, [Host, register_route]}),
%       {ok, [register_route|State]};
%   error ->
%       gnosus_logger:alarm({add_domain_failed, [Host, register_route]}),
%       {error, State}
%     end;
% 
% add_domain(Host, _) ->
%     add_domain(Host).
%
%%--------------------------------------------------------------------------------
% add_domain_and_user(Host, Uid, Password) ->
%     case add_domain(Host) of
%   {ok, State} ->
%       gnosus_logger:message({add_domain_and_user_succeeded, [Host, Uid, add_domain]}),
%       add_domain_and_user(Host, Uid, Password, {register_user, State}) ;
%   {error, State} ->
%       gnosus_logger:alarm({add_domain_and_user_failed, [Host, Uid, add_domain]}),
%       {error, State}
%     end.
% 
% add_domain_and_user(Host, Uid, Password, {register_user, State}) ->
%     case register_user(Uid, Host, Password) of
%   ok ->
%       gnosus_logger:message({add_domain_and_user_succeeded, [Host, Uid, register_user]}),
%       add_domain_and_user(Host, Uid, Password, {add_domain_admin_access_control, [register_user|State]}) ;
%   error ->
%       gnosus_logger:alarm({add_domain_and_user_failed, [Host, Uid, register_user]}),
%       {error, State}
%     end;
% 
% add_domain_and_user(Host, Uid, _Password, {add_domain_admin_access_control, State}) ->
%     case add_domain_admin_access_control(Uid, Host) of
%   ok ->
%       gnosus_logger:message({add_domain_and_user_succeeded, [Host, Uid, add_domain_admin_access_control]}),
%       {ok, [add_domain_admin_access_control|State]};
%   error ->
%       gnosus_logger:alarm({add_domain_and_user_failed, [Host, Uid, add_domain_admin_access_control]}),
%       {error, State}
%     end;
% 
% add_domain_and_user(Host, Uid, Password, _) ->
%     add_domain_and_user(Host, Uid, Password).
% 
% 
%%================================================================================
remove_host(Host) ->
    remove_host(Host, []).

%%--------------------------------------------------------------------------------
remove_host(Host, State) ->
    case apply_method_list([{host, Host}], [remove_route, remove_modules, remove_host_config, remove_authentication, remove_module_configs], State) of
   	    {ok, State} ->
   	        gnosus_logger:message({remove_host_succeeded, Host}),
   	        {ok, State};
   	    {error, State} ->
   	        gnosus_logger:alarm({remove_host_failed, Host}),
   	        {error, State}
    end.

%%--------------------------------------------------------------------------------
remove_host_and_users(Host) ->
    Args = [{host, Host}],
    case apply_method_list(Args, [remove_host_admin_access_control, remove_all_users]) of
	    {ok, State} ->
	        remove_host(Host, State);
	    {error, State} ->
	        gnosus_logger:alarm({remove_host_and_users_failed, Host}),
	        {error, State}
    end.

%%--------------------------------------------------------------------------------
remove_hosts_and_users(Hosts) ->
    lists:foldl(fun(Host, {ok, _State}) ->
			            remove_host_and_users(Host);
		            (_, {error, State}) ->
			            {error, State}
		end, {ok, []}, Hosts).		  

%%--------------------------------------------------------------------------------
% remove_domain(Host) ->
%     remove_domain(Host, {unregister_route, []}).
% 
% remove_domain(Host, {unregister_route, State}) ->
%     case unregister_route(Host) of
%   ok ->
%       gnosus_logger:message({remove_domain_succeeded, [Host, unregister_route]}),
%       remove_domain(Host, {stop_modules, [unregister_route|State], modules(Host)});
%   error ->
%       gnosus_logger:alarm({remove_domain_failed, [Host, unregister_route]}),
%       {error, State}
%     end;
% 
% remove_domain(Host, {stop_modules, State, Modules}) ->
%     case stop_modules(Host, Modules) of
%   ok ->
%       gnosus_logger:message({remove_domain_succeeded, [Host, stop_modules]}),
%       remove_domain(Host, {remove_host_config, [stop_modules|State]});
%   error ->
%       gnosus_logger:alarm({remove_domain_failed, [Host, stop_modules]}),
%       {error, State}
%     end;
% 
% remove_domain(Host, {remove_host_config, State}) ->
%     case remove_host_config(Host) of
%   ok ->
%       gnosus_logger:message({remove_domain_succeeded, [Host, remove_host_config]}),
%       remove_domain(Host, {remove_authentication, [remove_domains|State]});
%   error ->
%       gnosus_logger:alarm({remove_domain_failed, [Host, remove_host_config]}),
%       {error, State}
%     end;
% 
% remove_domain(Host, {remove_authentication, State}) ->
%     case remove_authentication(Host) of
%   ok ->
%       gnosus_logger:message({remove_domain_succeeded, [Host, remove_authentication]}),
%       remove_domain(Host, {remove_modules, [remove_authentication|State]});
%   error ->
%       gnosus_logger:alarm({remove_domain_failed, [Host, remove_authentication]}),
%       {error, State}
%     end;
% 
% remove_domain(Host, {remove_modules, State}) ->
%     case remove_modules(Host) of
%   ok ->
%       gnosus_logger:message({remove_domain_succeeded, [Host, remove_modules]}),
%       {ok, [remove_modules|State]};
%   error ->
%       gnosus_logger:alarm({remove_domain_failed, [Host, remove_modules]}),
%       {error, State}
%     end;
% 
% remove_domain(Host, _) ->
%     remove_domain(Host).
% 
%%--------------------------------------------------------------------------------
% remove_domain_and_users(Host) ->
%     case remove_domain_admin_access_control(Host) of
%   ok ->
%       gnosus_logger:message({remove_domain_and_users_succeeded, [Host, remove_domain_admin_access_control]}),
%       remove_domain_and_users(Host, {unregister_all_users, [remove_domain_admin_access_control]}) ;
%   error ->
%       gnosus_logger:alarm({remove_domain_and_users_failed, [Host, remove_domain_admin_access_control]}),
%       {error, []}
%     end.
% 
% remove_domain_and_users(Host, {unregister_all_users, State}) ->
%     case unregister_all_users(Host) of
%   ok ->
%       gnosus_logger:message({remove_domain_and_users_succeeded, [Host, unregister_all_users]}),
%       remove_domain_and_users(Host, {remove_domain, [unregister_all_users|State]}) ;
%   error ->
%       gnosus_logger:alarm({remove_domain_and_users_failed, [Host, unregister_all_users]}),
%       {error, State}
%     end;
% 
% remove_domain_and_users(Host, {remove_domain, State}) ->
%     case remove_domain(Host) of
%   {ok, HostState} ->
%       gnosus_logger:message({remove_domain_and_users_succeeded, [Host, remove_domain]}),
%       {ok, HostState++State};
%   {error, HostState} ->
%       gnosus_logger:alarm({remove_domain_and_users_failed, [Host, remove_domain]}),
%       {error, HostState++State}
%     end;
% 
% remove_domain_and_users(Host, _) ->
%     remove_domain_and_users(Host).
% 
% 
%%================================================================================
hosts() ->
    rpc:call(ejabberd(), ejabberd_config, get_global_option, [hosts]).

%%--------------------------------------------------------------------------------
modules(Host) ->   
    config_model:modules(Host).

%%--------------------------------------------------------------------------------
ejabberd() ->   
    config_model:ejabberd().

%%--------------------------------------------------------------------------------
host(Args) ->
    lists:keyfind(host, 0, Args).

%%--------------------------------------------------------------------------------
password(Args) ->
    lists:keyfind(password, 0, Args).

%%--------------------------------------------------------------------------------
uid(Args) ->
    lists:keyfind(uid, 0, Args).

%%================================================================================
add_host_config(Args) ->   
    case rpc:call(ejabberd(), ejabberd_config, add_global_option, [hosts, hosts()++[host(Args)]]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.

%%--------------------------------------------------------------------------------
remove_host_config(Args) ->  
    case rpc:call(ejabberd(), ejabberd_config, add_global_option, [hosts, lists:delete(host(Args), hosts())]) of
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
add_authentication(Args) ->
    case rpc:call(ejabberd(), ejabberd_config, add_local_option, [{auth_method, host(Args)}, internal]) of
	    {atomic, ok} ->
	        ok;
	    _ ->
	        error
    end.
    
%%--------------------------------------------------------------------------------
remove_authentication(Args) ->
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

%%================================================================================
apply_method_list(Args, MethodList) ->
    apply_method_list(Args, MethodList, []).

%%--------------------------------------------------------------------------------
apply_method_list(Args, [Method|MethodList], State) ->
    case apply(ejabberd, Method, Args) of
	    ok ->
	        gnosus_logger:message({apply_method_succeeded, [Method, Args]}),
	        case MethodList of
	            [] -> {ok, [Method|State]};
	            _ -> apply_method_list(Args, MethodList, [Method|State])
            end;
	    error ->
	        gnosus_logger:alarm({apply_method_failed, [Method, Args]}),
	        {error, State}
    end.
