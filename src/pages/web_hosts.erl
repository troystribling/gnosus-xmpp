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
    #label{text="hosts"}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(_) -> ok.

