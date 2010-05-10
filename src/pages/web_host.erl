%% host info
%%--------------------------------------------------------------------------------
-module (web_host).

%% API
-compile(export_all).

%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib ("nitrogen/include/wf.inc").

%%================================================================================
main() -> 
	#template{file="./wwwroot/template.html"}.

%%--------------------------------------------------------------------------------
navigation() ->
    gnosus_utils:navigation(host).   

%%--------------------------------------------------------------------------------
toolbar() ->
    Host = wf:get_path_info(),
    User = wf:user(),
    HostsLink = case User#users.product of
                    unlimited ->
                        [#listitem{body=#link{text="hosts", url=?HOSTS}},
                         #listitem{body=#image{image="/images/toolbar-separator.png"}}];
                    _ -> []
                end,
	#list{body=HostsLink++[ 
        #listitem{body=#link{text="send user registration", url=?HOST_USER_REGISTER(Host)}},
        #listitem{body=#image{image="/images/toolbar-separator.png"}},
        #listitem{body=#link{text="add user", url=?HOST_USER_ADD(Host)}},
        #listitem{body=#image{image="/images/toolbar-separator.png"}},
	    #listitem{body=#link{text="delete this host", postback={remove_host, Host}}}
	]}.

%%--------------------------------------------------------------------------------
title() -> 
    #literal{text="<h1>host <em>"++wf:html_encode(wf:get_path_info())++"</em></h1>", html_encode=false}.

%%--------------------------------------------------------------------------------
body() -> 
    #panel{body=table_data(), id=tableData, class="data"}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:user_logout();

%%--------------------------------------------------------------------------------
event({remove_host, Host}) ->
    case gnosus_utils:remove_host(Host) of
        ok -> gnosus_utils:host_page_redirect();            
        Result -> Result
    end;

%%--------------------------------------------------------------------------------
event({remove_user, {Host, Uid}}) ->
    case client_user_model:find(Host, Uid) of
        error ->             
            gnosus_logger:alarm({client_user_not_found, [Host, Uid]}),
             wf:flash("database error");
        norfound ->
            gnosus_logger:alarm({client_user_not_found, [Host, Uid]}),
            wf:flash("user not found");
        User ->
            case User#client_users.status of
                active -> remove_active_user(Host, Uid);
                _ -> remove_client_user(Host, Uid)
            end
        end;
    
%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
table_data() ->
    Host = wf:get_path_info(),
    Header = ["uid", "email", "status", "last activity", "offline messages"],
    Data = lists:map(
                      fun(U) ->  
                          {_, Uid} = U#client_users.jid,
                          [
                              Uid, 
                              U#client_users.email, 
                              atom_to_list(U#client_users.status),
                              "never", 
                              [#link{body=#image{image="/images/data-delete.png"}, postback={remove_user, U#client_users.jid}, class="data-edit-controls"}, "0"]
                          ]
                      end, client_user_model:find_all_by_host(Host)),
    gnosus_utils:table_data(Header, Data).
    

%%--------------------------------------------------------------------------------
remove_active_user(Host, Uid) ->
    case ejabberd:remove_user(Host, Uid) of
        ok -> remove_client_user(Host, Uid);         
        error -> wf:flash("failed to deprovision user on xmpp server") 
    end.                       

%%--------------------------------------------------------------------------------
remove_client_user(Host, Uid) ->
    case client_user_model:delete(Host, Uid) of
        ok -> 
            gnosus_logger:message({host_user_remove_succeeded, [Host, Uid]}),
             wf:update(tableData, table_data());
        _ ->
            gnosus_logger:alarm({client_user_database_update_failed, [Host, Uid]}),
            wf:flash("user database update failed")                        
    end.
