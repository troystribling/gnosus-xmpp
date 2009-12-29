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
    Rows = [#tablerow{cells=[
                #tableheader{text="uid"},
                #tableheader{text="email"},
                #tableheader{text="status"},
                #tableheader{text="last login"},
                #tableheader{text="login count"}
               ]}] ++  lists:map(
                              fun(U) ->
                                  #tablerow{cells=[
                                      #tablecell{body=#link{text=U#users.uid, url="/web/user/"++U#users.uid}},
                                      #tablecell{body=U#users.email},
                                      #tablecell{body=atom_to_list(U#users.status)},
                                      #tablecell{body="never"},
                                      #tablecell{body=
                                          #panel{body=[
                                              #link{body=#image{image="/images/data-delete.png"}, postback={remove_user, U#users.uid}, class="data-edit-controls"},
                                              "0"
                                           ], class="data-item"}}
                                  ], class="data-edit"} 
                              end, user_model:find(all)),
    #table{rows=Rows, actions=#script{script="init_data_edit_row();"}}.

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
