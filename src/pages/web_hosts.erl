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
    #template{file="./wwwroot/table_template.html"}.

%%--------------------------------------------------------------------------------
navigation() ->
    gnosus_utils:navigation(host).   

%%--------------------------------------------------------------------------------
toolbar() ->
	#list{body=[ 
	    #listitem{body=#link{text="new host", postback=add_host}}
	]}.

%%--------------------------------------------------------------------------------
title() -> 
    #literal{text="<h1>hosts</h1>", html_encode=false}.

%%--------------------------------------------------------------------------------
body() ->
    #panel{body=table_data(), id=tableData, class="data"}.
	
%%================================================================================
event(logout) ->
    gnosus_utils:user_logout();

%%--------------------------------------------------------------------------------
event(add_host) ->
    wf:redirect(?HOST_ADD);

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
    Header = ["host", "users", "online users"],
    Data = lists:map(
                      fun(H) ->  
                          [
                              #link{text=H#hosts.host, url=?HOST(H#hosts.host)}, 
                              "0", 
                              [#link{body=#image{image="/images/data-delete.png"}, postback={remove_host, H#hosts.host}, class="data-edit-controls"}, "0"]
                          ]
                      end, host_model:find_all_by_uid(User#users.uid)),
    gnosus_utils:table_data(Header, Data).
