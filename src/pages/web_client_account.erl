%% host info
%%--------------------------------------------------------------------------------
-module (web_client_account).

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
    gnosus_utils:client_navigation(account).

%%--------------------------------------------------------------------------------
title() ->
    #h1{text="account"}.

%%--------------------------------------------------------------------------------
body() ->
    user_form().
	
%%================================================================================
event(logout) ->
    gnosus_utils:client_user_logout();

%%--------------------------------------------------------------------------------
event(update_user) -> 
    User = wf:user(),
    {Host, Uid} = User#client_users.jid,
    [EMail] = wf:html_encode(wf:q(emailTextBox)),
    case client_user_model:update(Host, Uid, EMail, User#client_users.status) of
        ok ->
            wf:user(client_user_model:find(Host, Uid)), 
            gnosus_logger:message({update_client_user_succeeded, User#client_users.jid}),
            wf:flash("profile updated"); 
        _ ->
            gnosus_logger:alarm({update_client_user_failed, User#client_users.jid}),
            wf:flash("user database update failed")                        
    end;
            
%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
user_form() ->
    User = wf:user(),
    Body = [
        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, text=User#client_users.email, next=passwordTextBox}
        ], class="form client-profile"},

        #panel{body= #list{body=[ 
            #listitem{body=#link{id=updateButton, text="update", postback=update_user}, class="button"}
    	]}, class="form form-buttons"}
    ],

    wf:wire(updateButton, emailTextBox, #validate {validators=[
        #is_required{text="email address required"},
        #is_email{text="invalid email address"},
        #max_length{text="email cannot have more than "++integer_to_list(?MAX_EMAIL_LENGTH)++" characters"}
    ]}),

    wf:render(Body).
