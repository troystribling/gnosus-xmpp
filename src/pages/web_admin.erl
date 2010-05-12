%% host info
%%--------------------------------------------------------------------------------
-module (web_admin).

%% API
-compile(export_all).

%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib ("nitrogen/include/wf.inc").

%%================================================================================
main() -> 
	#template{file="./wwwroot/table_template.html"}.

%%--------------------------------------------------------------------------------
navigation() ->
    gnosus_utils:navigation(admin).   

%%--------------------------------------------------------------------------------
toolbar() ->
    #panel{body=users_toolbar(), id=toolBar}.

%%--------------------------------------------------------------------------------
title() -> 
    #panel{body=users_title(), id=title}.

%%--------------------------------------------------------------------------------
body() -> 
    #panel{body=users_table_data(), id=tableData, class="data"}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:user_logout();

%%--------------------------------------------------------------------------------
event(show_hosts) ->
    [wf:update(title, hosts_title()), wf:update(toolBar, hosts_toolbar()), wf:update(tableData, hosts_table_data())];  
 
%%--------------------------------------------------------------------------------
event(show_users) ->
    [wf:update(title, users_title()), wf:update(toolBar, users_toolbar()), wf:update(tableData, users_table_data())];  

%%--------------------------------------------------------------------------------
event({remove_user, EMail}) ->
    case user_model:delete_by_email(EMail) of
        ok -> 
            gnosus_logger:alarm({remove_user_succeeded, EMail}),
            wf:update(tableData, users_table_data());
        _ -> 
            gnosus_logger:alarm({remove_user_failed, EMail}),
            wf:flash("user database update failed")
    end;
    
%%--------------------------------------------------------------------------------
event({remove_host, Host}) ->
    case gnosus_utils:remove_host(Host) of
        ok -> wf:update(tableData, hosts_table_data());            
        Result -> Result
    end;
    
%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
users_table_data() ->
    Header = ["email", "uid", "status", "role", "last login", "login count"],
    Data = lists:map(
                      fun(U) ->  
                          [
                              #link{text=U#users.email, url=?USER(U#users.email)}, 
                              U#users.uid, 
                              atom_to_list(U#users.status),
                              atom_to_list(U#users.role),
                              "never", 
                              [#link{body=#image{image="/images/data-delete.png"}, postback={remove_user, U#users.email}, class="data-edit-controls"}, 
                               integer_to_list(U#users.login_count)]
                          ]
                      end, user_model:find(all)),
    gnosus_utils:table_data(Header, Data).

%%--------------------------------------------------------------------------------
users_toolbar() ->
    #list{body=[ 
        #listitem{body=#link{text="show hosts", postback=show_hosts}},
        #listitem{body=#image{image="/images/toolbar-separator.png"}},
        #listitem{body=#link{text="send user registration email", url=?REGISTER}},
        #listitem{body=#image{image="/images/toolbar-separator.png"}},
        #listitem{body=#link{text="add user", url=?USER_ADD}}
    ]}.

%%--------------------------------------------------------------------------------
hosts_table_data() ->
    Header = ["host", "uid", "users", "online users"],
    Data = lists:map(
                      fun(#hosts{host=H, uid=U}) ->  
                          [
                              #link{text=H, url=?HOST(H)}, 
                              #link{text=U, url=?USER(U)}, 
                              "0", 
                              [#link{body=#image{image="/images/data-delete.png"}, postback={remove_host, H}, class="data-edit-controls"}, "0"]
                          ]
                      end, host_model:find(all)),
    gnosus_utils:table_data(Header, Data).

%%--------------------------------------------------------------------------------
hosts_toolbar() ->
    #list{body=[ 
        #listitem{body=#link{text="show users", postback=show_users}}
    ]}.

%%--------------------------------------------------------------------------------
users_title() ->
    #h1{text="users"}.

%%--------------------------------------------------------------------------------
hosts_title() ->
    #h1{text="hosts"}.
