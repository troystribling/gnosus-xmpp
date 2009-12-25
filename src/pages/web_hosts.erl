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
	#list{body=[ 
        #listitem{body="<strong>host</strong"},
        #listitem{body=#link{text="admin", url="/web/users"}},
	    #listitem{body=#link{text="logout", postback=logout}}
	]}.

%%--------------------------------------------------------------------------------
toolbar() ->
	#list{body=[ 
	    #listitem{body=#link{text="new host", postback=add_host, class="up-button"}}
	]}.

%%--------------------------------------------------------------------------------
body() ->
    User = wf:user(),
    Rows = [#tablerow{cells=[
                #tableheader{text="host"},
                #tableheader{text="users"},
                #tableheader{text="online users"}
                ]}] ++ lists:map(fun(H) ->
                                     #tablerow{cells=[
                                         #tablecell{body=[
                                             #link{body=#image{image="/images/data-delete.png"}, postback=remove_host, class="data-edit-controls"},
                                             #p{body=H#hosts.host, class="data-item"}
                                         ]},
                                         #tablecell{text="0"},
                                         #tablecell{text="0"}
                                     ], class="data-edit"} 
                                 end, host_model:find_all_by_uid(User#users.uid)),
    #table{rows=Rows, class="data"}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(add_host) ->
    wf:redirect("/web/host/add");

%%--------------------------------------------------------------------------------
event(remove_host) ->
    wf:redirect("/web/host/add");

%%--------------------------------------------------------------------------------
event(_) -> ok.

