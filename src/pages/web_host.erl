%% host info
%%--------------------------------------------------------------------------------
-module (web_host).

%% API
-compile(export_all).

%% include
-include_lib ("nitrogen/include/wf.inc").

%%================================================================================
main() -> 
	#template { file="./wwwroot/template.html"}.

%%--------------------------------------------------------------------------------
navigation() ->
	#list{body=[ 
        #listitem{body="host"},
        #listitem{body=#link{text="admin", url="/web_users"}},
	    #listitem{body=#link{text="logout", url="/web_logout"}}
	]}.

%%--------------------------------------------------------------------------------
body() ->
    #label{text="host"}.
	
%%--------------------------------------------------------------------------------
event(_) -> ok.

