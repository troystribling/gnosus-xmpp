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
    #literal{text="<h1>add user: <em>"++wf:get_path_info()++"</em></h1>", html_encode=false}.

%%--------------------------------------------------------------------------------
body() ->
    Body = [
        #p{body=[
            #label{text="user id"},
            #textbox {id=uidTextBox, next=emailTextBox}
        ], class="form host-user-add"},

        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, next=passwordTextBox}
        ], class="form host-user-add"},

        #p{body=[
            #label{text="password" },
            #password{id=passwordTextBox, next=confirmPasswordTextBox }
        ], class="form host-user-add"},

        #p{body=[
            #label{text="confirm password" },
            #password{id=confirmPasswordTextBox, next=cancelButton }
        ], class="form host-user-add"},

        #panel{body= #list{body=[ 
            #listitem{body=#link{id=cancelButton, text="cancel", postback=cancel, class="up-button"}},
            #listitem{body=#link{id=addButton, text="add", postback=add_user, class="up-button"}}
    	]}, class="form form-buttons host-user-add-buttons"}
    ],

    wf:wire(addButton, emailTextBox, #validate {validators=[
        #is_required{text="email address required"},
        #is_email{text="invalid email address"}
    ]}),

    wf:wire(addButton, uidTextBox, #validate {validators=[
        #is_required{text="uid required"},
        #custom{text="user id is not available", function=fun uid_available/2}        
    ]}),

    wf:wire(addButton, passwordTextBox, #validate {validators=[
      #is_required{text="password required"}
    ]}),

    wf:wire(addButton, confirmPasswordTextBox, #validate {validators=[
      #is_required{text="confirmation password required"},
      #confirm_password {text="passwords must match.", password=passwordTextBox}
    ]}),

    wf:render(Body).
	
%%================================================================================
event(logout) ->
    gnosus_utils:user_logout();

%%--------------------------------------------------------------------------------
event(add_user) -> 
    Host = wf:get_path_info(),
    User = wf:user(),    
    [EMail] = wf:q(emailTextBox),
    [Uid] = wf:q(uidTextBox),
    [Password] = wf:q(passwordTextBox),
    case ejabberd:add_user(Host, Uid, Password) of
        ok ->
            case client_user_model:new_user(Host, Uid, EMail) of
                ok -> 
                    gnosus_logger:message({host_user_add_succeeded, [Host, Uid]}),
                    wf:redirect(?HOST(Host));
                _ ->
                    User = wf:user(),
                    gnosus_logger:alarm({client_user_database_update_failed, [Host, Uid]}),
                    wf:flash("user database update failed")                        
            end;
        error ->
            wf:flash("failed to provision user on xmpp server") 
    end;                       
            
%%--------------------------------------------------------------------------------
event(cancel) -> 
    wf:redirect(?HOST(wf:get_path_info()));

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
uid_available(_Tag, _Value) ->
    [Uid] = wf:q(uidTextBox),
    Host = wf:get_path_info(),
    case client_user_model:find(Host, Uid) of
        notfound -> true;
        _ -> false
    end.
