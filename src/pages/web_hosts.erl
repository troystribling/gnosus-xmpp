%% hosts info
%%--------------------------------------------------------------------------------
-module (web_hosts).

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
    gnosus_utils:navigation(hosts).   

%%--------------------------------------------------------------------------------
toolbar() ->
	#list{body=[ 
	    #listitem{body=#link{text="new host", postback=add_host, class="up-button"}}
	]}.

%%--------------------------------------------------------------------------------
body() ->
    #panel{body=table_data(), id=tableData, class="data"}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(add_host) ->
    wf:redirect("/web/host/add");

%%--------------------------------------------------------------------------------
event({remove_host, Host}) ->
    case gnosus_utils:remove_host(Host) of
        ok -> wf:update(tableData, table_data());            
        Result -> Result
    end;

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
table_data() ->
    User = wf:user(),
    Rows = [#tablerow{cells=[
                #tableheader{text="host"},
                #tableheader{text="users"},
                #tableheader{text="online users"}
                ]}] ++ lists:map(
                           fun(H) ->
                               #tablerow{cells=[
                               #tablecell{body=#link{text=H#hosts.host, url="/web/host/"++wf:html_encode(H#hosts.host, true)}},
                               #tablecell{text="0"},
                               #tablecell{body=
                                   #p{body=[
                                        #link{body=#image{image="/images/data-delete.png"}, postback={remove_host, H#hosts.host}, class="data-edit-controls"},
                                        "0"], class="data-item"}}], class="data-edit"} 
                           end, host_model:find_all_by_uid(User#users.uid)),
    #table{rows=Rows, actions=#script{script="init_data_edit_row();"}}.
