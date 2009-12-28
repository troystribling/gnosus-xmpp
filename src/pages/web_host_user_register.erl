%% host info
%%--------------------------------------------------------------------------------
-module (web_host_user_register).

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
    #literal{text="<h1><em>"++wf:get_path_info()++"</em> register user</h1>", html_encode=false}.

%%--------------------------------------------------------------------------------
body() ->
    Body = [
        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, next=uidTextBox}
        ], class="form host-user-register"},

        #p{body=[
            #label{text="uid"},
            #textbox {id=uidTextBox, next=cancelButton}
        ], class="form host-user-register"},

        #p{body=[#link{id=cancelButton, text="cancel", postback=cancel, class="up-button"},
                 #link{id=registerButton, text="send email", postback=register, class="up-button"}], class="form  form-button host-user-register-button"}
    ],

    wf:wire(registerButton, emailTextBox, #validate {validators=[
        #is_required {text="email required"}
    ]}),

    wf:wire(registerButton, uidTextBox, #validate {validators=[
        #is_required {text="uid required"}
    ]}),

    wf:render(Body).
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(register) -> 
    ok;

%%--------------------------------------------------------------------------------
event(cancel) -> 
    wf:redirect("/web/host/"++wf:get_path_info());

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
host_available(_Tag, _Value) ->
    true.	
