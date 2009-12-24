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
        #listitem{body="host"},
        #listitem{body=#link{text="admin", url="/web_users"}},
	    #listitem{body=#link{text="logout", postback=logout}}
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
                                        #tablecell{text=H#hosts.host},
                                        #tablecell{text=integer_to_list(H#hosts.num_users)},
                                        #tablecell{text="0"}
                                    ]} 
                                end, host_model:find_all_by_uid(User#users.uid)),
    #table{rows=Rows, class="data"}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(_) -> ok.

