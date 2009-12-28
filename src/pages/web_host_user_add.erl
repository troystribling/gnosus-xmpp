%% host info
%%--------------------------------------------------------------------------------
-module (web_host_user_add).

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
    gnosus_utils:navigation(host).   

%%--------------------------------------------------------------------------------
title() -> 
    #literal{text="<h1><em>"++wf:get_path_info()++"</em> add user</h1>", html_encode=false}.

%%--------------------------------------------------------------------------------
body() ->
    Body = [
        #p{body=[
            #label{text="host"},
            #textbox {id=hostTextBox, next=addHostButton}
        ], class="form host-user-add"},

        #p{body=#link{ id=addHostButton, text="add", postback=add_host, class="up-button form-button"}, class="form host-user-add-button"}
    ],

    wf:wire(addHostButton, hostTextBox, #validate {validators=[
        #is_required {text="host name required"},
        #custom {text="host name not available", function=fun host_available/2}
    ]}),

    wf:render(Body).
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(add_host) -> 
    gnosus_utils:add_host();

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
host_available(_Tag, _Value) ->
    true.	
