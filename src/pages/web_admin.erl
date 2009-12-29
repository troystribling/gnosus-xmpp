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
    users_toolbar().

%%--------------------------------------------------------------------------------
title() -> 
    users_title().

%%--------------------------------------------------------------------------------
body() -> 
    #panel{body=user_table_data(), id=tableData, class="data"}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event({remove_user, _Uid}) ->
    ok;
    
%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
user_table_data() ->
    Header = ["uid", "email", "status", "role", "last login", "login count"],
    Data = lists:map(
                      fun(U) ->  
                          [
                              #link{text=U#users.uid, url="/web/user/"++U#users.uid}, 
                              U#users.email, 
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
users_title() ->
    #h1{text="users"}.
