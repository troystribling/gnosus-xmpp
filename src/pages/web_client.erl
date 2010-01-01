%% host info
%%--------------------------------------------------------------------------------
-module (web_client).

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
    gnosus_utils:client_navigation(client).
    
%%--------------------------------------------------------------------------------
body() ->
    #panel{body="", id=client, actions=#script{script=""}}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:client_user_logout();

%%--------------------------------------------------------------------------------
event(_) -> ok.
