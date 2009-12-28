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
    #literal{text="<h1>register user: <em>"++wf:get_path_info()++"</em></h1>", html_encode=false}.

%%--------------------------------------------------------------------------------
body() ->
    Body = [
        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, next=uidTextBox}
        ], class="form host-user-register"},

        #p{body=[
            #label{text="user id"},
            #textbox {id=uidTextBox, next=cancelButton}
        ], class="form host-user-register"},

        #p{body=[
            #label{text="user id"},
            #textbox {id=uidTextBox, next=registerButton}
        ], class="form host-user-register"},

        #panel{body= #list{body=[ 
            #listitem{body=#link{id=cancelButton, text="cancel", postback=cancel, class="up-button"}},
            #listitem{body=#link{id=registerButton, text="send email", postback=register, class="up-button"}}
    	]}, class="form form-buttons host-user-register-buttons"}
    ],

    wf:wire(registerButton, emailTextBox, #validate {validators=[
        #is_email{text="invalid email address"},
        #is_required{text="email address required"},
        #custom{text="email address registered", tag=some_tag, function=fun validate_email/2}
    ]}),

    wf:wire(registerButton, uidTextBox, #validate {validators=[
        #is_required{text="uid required"},
        #custom{text="user id is not available", tag=some_tag, function=fun validate_uid/2}
    ]}),

    wf:render(Body).
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(register) -> 
    User = wf:user(),
    Host = wf:get_path_info(),
    [EMail] = wf:q(emailTextBox),
    [Uid] = wf:q(uidTextBox),
    case client_user_model:register(Uid, Host, EMail) of
        ok -> 
            gnosus_logger:message({host_user_registration_succeeded, [Uid, Host, User#users.uid]}),
            wf:redirect("/web/host/"++Host);
        _ ->
            gnosus_logger:alarm({client_user_database_update_failed, [Host, User#users.uid]}),
            wf:flash("user database update failed")                        
    end;
    

%%--------------------------------------------------------------------------------
event(cancel) -> 
    wf:redirect("/web/host/"++wf:get_path_info());

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
validate_email(_Tag, _Value) ->
    true.	

%%--------------------------------------------------------------------------------
validate_uid(_Tag, _Value) ->
    true.	
