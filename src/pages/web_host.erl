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
	#list{body=[ 
        #listitem{body=#link{text="send user registration email", url="/web/host/user/register/"++wf:html_encode(Host, true), class="up-button"}},
        #listitem{body=#link{text="add user", url="/web/host/user/add/"++wf:html_encode(Host, true), class="up-button"}},
	    #listitem{body=#link{text="delete this host", postback={remove_host, Host}, class="up-button"}}
	]}.

%%--------------------------------------------------------------------------------
title() -> 
    #literal{text="<h1><em>"++wf:get_path_info()++"</em> users</h1>", html_encode=false}.

%%--------------------------------------------------------------------------------
body() -> 
    #panel{body=table_data(), id=tableData, class="data"}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event({remove_host, Host}) ->
    case gnosus_utils:remove_host(Host) of
        ok -> gnosus_utils:host_page_redirect();            
        Result -> Result
    end;

%%--------------------------------------------------------------------------------
event({remove_user, _Jid}) ->
    ok;

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
table_data() ->
    Host = wf:get_path_info(),
    Rows = [#tablerow{cells=[
                #tableheader{text="jid"},
                #tableheader{text="email"},
                #tableheader{text="status"},
                #tableheader{text="last activity"},
                #tableheader{text="offline messages"}
               ]}] ++  lists:map(
                              fun(U) ->
                                  {Uid, _} = U#client_users.jid,
                                  #tablerow{cells=[
                                      #tablecell{body=Uid++"@"++Host},
                                      #tablecell{body=U#client_users.email},
                                      #tablecell{body=atom_to_list(U#client_users.status)},
                                      #tablecell{body="never"},
                                      #tablecell{body=
                                          #panel{body=[
                                              #link{body=#image{image="/images/data-delete.png"}, postback={remove_user, U#client_users.jid}, class="data-edit-controls"},
                                              "0"
                                           ], class="data-item"}}
                                  ], class="data-edit"} 
                              end, client_user_model:find_all_by_host(Host)),
    #table{rows=Rows, actions=#script{script="init_data_edit_row();"}}.
