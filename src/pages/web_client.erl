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
	#template{file="./wwwroot/client_template.html"}.

%%--------------------------------------------------------------------------------
navigation() ->
    gnosus_utils:client_navigation(client).
    
%%--------------------------------------------------------------------------------
body() ->
    User = wf:user(),
    Jid = client_user_model:bare_jid(User),
    {Host, _} = User#client_users.jid,
    #passwd{password=Password} = passwd_model:find(Jid),
    Args = string:join([Host++"/http-bind", Jid, Password], ","),
?DUMP(Args),    
    #panel{body="", id=client, actions=#script{script="connect("++Args++");"}}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:client_user_logout();

%%--------------------------------------------------------------------------------
event(_) -> ok.
