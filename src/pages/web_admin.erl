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
	#template{file="./wwwroot/template.html"}.

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
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(show_hosts) ->
    [wf:update(title, hosts_title()), wf:update(toolBar, hosts_toolbar()), wf:update(tableData, hosts_table_data())];  
 
%%--------------------------------------------------------------------------------
event(show_users) ->
    [wf:update(title, users_title()), wf:update(toolBar, users_toolbar()), wf:update(tableData, users_table_data())];  

%%--------------------------------------------------------------------------------
event({remove_user, Uid}) ->
    case user_model:delete(Uid) of
        ok -> 
            wf:update(tableData, users_table_data());
        _ -> 
            gnosus_logger:alarm({remove_user_failed, Uid}),
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
                              #link{text=U#users.email, url="/web/user/"++U#users.email}, 
                              U#users.uid, 
                              atom_to_list(U#users.status),
                              atom_to_list(U#users.role),
                              "never", 
                              [#link{body=#image{image="/images/data-delete.png"}, postback={remove_user, U#users.uid}, class="data-edit-controls"}, 
                               integer_to_list(U#users.login_count)]
                          ]
                      end, user_model:find(all)),
    gnosus_utils:table_data(Header, Data).

%%--------------------------------------------------------------------------------
users_toolbar() ->
    #list{body=[ 
        #listitem{body=#link{text="show hosts", postback=show_hosts}},
        #listitem{body=#image{image="/images/toolbar-separator.png"}},
        #listitem{body=#link{text="add user", url="/web/user/add"}}
    ]}.

%%--------------------------------------------------------------------------------
hosts_table_data() ->
    Header = ["host", "uid", "users", "online users"],
    Data = lists:map(
                      fun(#hosts{host=H, uid=U}) ->  
                          [
                              #link{text=H, url="/web/host/"++H}, 
                              #link{text=U, url="/web/user/"++U}, 
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
