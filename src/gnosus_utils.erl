%% gnosus utilities
%%--------------------------------------------------------------------------------
-module(gnosus_utils).

%% API
-export([
    logout/0,
    host_page_redirect/0,
    start_page_redirect/0,
    add_host/0,
    remove_host/1,
    navigation/1,
    table_data/2,
    new_host_and_client_user/2,
    delete_host_and_client_users/1,
    to_options_list/1
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib ("nitrogen/include/wf.inc").

%%================================================================================
logout() ->
    #users{uid=Uid} = wf:user(),
    gnosus_logger:message({terminate_session, Uid}),
    wf:logout(),
    wf:flash("logged out"),
    wf:redirect("/").

%%--------------------------------------------------------------------------------
host_page_redirect() ->
    User = wf:user(),
    case User#users.product of
        unlimited ->
            wf:redirect("/web/hosts");
        _ ->
            Host = hd(wf:session(hosts)),
            wf:redirect("/web/host/"++Host)
    end.

%%--------------------------------------------------------------------------------
start_page_redirect() ->
    case wf:session(hosts) of
        [] ->
            wf:redirect("/web/host/add");
        _ ->
            host_page_redirect()
    end.
        
%%================================================================================
add_host() ->
    User = wf:user(),
    [Host] = wf:q(hostTextBox),
    case ejabberd:add_host_and_user(Host, User#users.uid, User#users.password) of
        {ok, _} ->
            Hosts = wf:session(hosts),
            wf:session(hosts, Hosts++[Host]),
            case new_host_and_client_user(Host, User) of
                ok ->
                    gnosus_logger:message({add_host_ui_succeeded, [Host, User#users.uid]}),
                    gnosus_utils:host_page_redirect();
                _ ->
                    ejebberd:remove_host_and_users(Host, User#users.uid),
                    gnosus_logger:alarm({host_and_client_user_database_update_failed, [Host, User#users.uid]}),
                    wf:flash("database update failed")            
            end;
        {error, _} ->
            wf:flash("host creation failed")            
    end.

%%--------------------------------------------------------------------------------
remove_host(Host) ->
    User = wf:user(),
    case ejabberd:remove_host_and_users(Host, User#users.uid) of
        {ok, _} ->
            Hosts = wf:session(hosts),
            wf:session(hosts, Hosts--[Host]),
            case delete_host_and_client_users(Host) of
                ok ->
                    gnosus_logger:message({remove_host_ui_succeeded, [Host, User#users.uid]}),
                    case wf:session(hosts) of 
                        [] -> wf:redirect("/web/host/add");
                        _ -> ok
                    end;
                _ ->
                    ejebberd:add_host_and_users(Host, User#users.uid, User#users.password),
                    gnosus_logger:alarm({host_and_client_user_database_update_failed, [Host, User#users.uid]}),
                    wf:flash("host database update failed")            
            end;
        {error, _} ->
            wf:flash("failed to provision host on xmpp server")            
    end.
    
%%================================================================================
navigation(Current) ->
    User = wf:user(),
	AdminItem = case User#users.role of
	                admin -> case Current of
	                             admin -> [#listitem{body="<strong>admin</strong>"}];
	                             _ -> [#listitem{body=#link{text="admin", url="/web/admin"}}]
	                         end;
	                _ -> []
                end,
    HostItem =  case User#users.product of 
                    unlimited -> if
	                                 Current =:= host -> #listitem{body="<strong>hosts</strong>"};
	                                 true -> #listitem{body=#link{text="hosts", url="/web/hosts"}}
	                             end;
                    _ -> if
                             Current =:= host -> #listitem{body="<strong>host</strong>"};
                             true -> #listitem{body=#link{text="host", url="/web/host"}}
                         end
                end,
	UserItem = case Current of
	                profile -> #listitem{body=("<strong>"++User#users.uid++"</strong>")};	                         
	                _ -> #listitem{body=#link{text=User#users.uid, url="/web/profile"}}
                end,
	#list{body=(AdminItem++[UserItem,HostItem,#listitem{body=#link{text="logout", postback=logout}}])}.

%%--------------------------------------------------------------------------------
table_data(H, D) ->
    Header = [#tablerow{cells=lists:map(
                 fun(C) ->
                     #tableheader{text=C}
                 end, H)}],
    Data = lists:map(             
               fun(R) ->
                   #tablerow{cells=[
                       lists:map(
                           fun(C) ->
                               #tablecell{body=#panel{body=C, class="data-item"}}
                           end, R)], class="data-edit"}                           
               end, D),
    #table{rows=Header++Data, actions=#script{script="init_data_edit_row();"}}.

%%================================================================================
new_host_and_client_user(H, U) ->
    Host = #hosts{host=H, uid=U#users.uid},
    ClientUser = client_user_model:init_record(H, U#users.uid, U#users.email, U#users.password, active),
    gnosus_dbi:transaction(
         fun() ->
             mnesia:write(Host),
			 mnesia:write(ClientUser)
	     end).

%%--------------------------------------------------------------------------------
delete_host_and_client_users(Host) ->
    ClientUsers = client_user_model:find_all_by_host(Host),
    gnosus_dbi:transaction(
         fun() ->
             mnesia:delete({hosts, Host}),
             lists:foreach(
                fun(#client_users{jid=C}) ->
   	                mnesia:delete({client_users, C})
                end, ClientUsers)
	     end).

 %%================================================================================
to_options_list(AtomList) ->
    lists:map(fun(V) -> #option{text=atom_to_list(V), value=atom_to_list(V)} end, AtomList).