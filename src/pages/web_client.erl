%% host info
%%--------------------------------------------------------------------------------
-module (web_client).

%% API
-compile(export_all).

%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib ("nitrogen/include/wf.inc").

%% define
-define(COLUMNS, 2).

%%================================================================================
main() -> 
	#template{file="./wwwroot/client_template.html"}.

%%--------------------------------------------------------------------------------
navigation() ->
    gnosus_utils:client_navigation(client).
    
%%--------------------------------------------------------------------------------
connect() ->
    User = wf:user(),
    Jid = client_user_model:bare_jid(User),
    #passwd{password=Password} = passwd_model:find(Jid),
    Args = string:join(["/http-bind/", Jid++"/gnos.us", Password], "','"),
    #panel{body="", actions=#script{script="connect('"++Args++"');"}}.

%%--------------------------------------------------------------------------------
client() ->
    wf:session(clients, []),
    add_client().
	
%%================================================================================
event(logout) ->
    gnosus_utils:client_user_logout();

%%--------------------------------------------------------------------------------
event(add_client) ->
    add_client();

%%--------------------------------------------------------------------------------
event({remove_client, Client}) ->
    remove_client(Client);

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
add_client() ->
    ClientId = client_id(),
    RowId = row_id(),
    Client = [#panel{body="", id="client-display-toolbar-"++integer_to_list(ClientId)},
              #panel{body="", id="client-display-"++integer_to_list(ClientId)},
              #panel{body="", id="client-toolbar-"++integer_to_list(ClientId)}],
    Body = case row_exists(RowId) of
               true -> Client;            
               false -> #panel{body=Client, id="client-row-"++integer_to_list(RowId)}}    
           end,
    wf_session(clients, [{client_id, ClientId}, {row_id, RowId}]),       
    #panel{body=Body, actions=#script{script="GnosusUiMgr.add_client('"++ClientId++"');"}}.
        
%%--------------------------------------------------------------------------------
remove_client(Client) -> ok.
    
%%--------------------------------------------------------------------------------
client_id() ->    
    {S1,S2,S3} = now(),
    random:seed(S1,S2,S3),
    client_id(random:uniform(1000)).   
    
client_id(Id) ->
    case lists:any(fun([{client_id, ClientId}, {row_id, _}]) ->
                       ClientId =:= Id
                   end, wf:session(clients)) of
        true -> client_id();
        false -> Id
    end.
    
%%--------------------------------------------------------------------------------
row_id() ->
    trunc(length(wf:session(clients))/?COLUMNS) + 1.
    
%%--------------------------------------------------------------------------------
row_exits(Id) ->
    lists:any(fun([{client_id, _}, {row_id, RowId}]) ->
                  RowId =:= Id
              end, wf:session(clients)).    