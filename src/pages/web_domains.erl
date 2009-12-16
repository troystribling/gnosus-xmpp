%% register
%%--------------------------------------------------------------------------------
-module (web_domains).

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
        #listitem{body="domain"},
        #listitem{body=#link{text="admin", url="/web_users"}},
	    #listitem{body=#link{text="logout", url="/web_logout"}}
	]}.

%%--------------------------------------------------------------------------------
body() ->
    #label{text="domains"}.
	
%%--------------------------------------------------------------------------------
event(_) -> ok.

