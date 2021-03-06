%% host info
%%--------------------------------------------------------------------------------
-module (web_account).

%% API
-compile(export_all).

%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib ("nitrogen/include/wf.inc").

%%================================================================================
main() -> 
	#template{file="./wwwroot/form_template.html"}.

%%--------------------------------------------------------------------------------
navigation() ->
    gnosus_utils:navigation(account).   

%%--------------------------------------------------------------------------------
title() ->
    #h1{text="account"}.

%%--------------------------------------------------------------------------------
body() ->
    user_form().
	
%%================================================================================
event(logout) ->
    gnosus_utils:user_logout();

%%--------------------------------------------------------------------------------
event(update_user) -> 
    User = wf:user(),
    Uid = User#users.uid,
    [EMail] = wf:html_encode(wf:q(emailTextBox)),
    if 
        EMail =:= User#users.email -> ok;
        true -> user_model:delete(Uid)
    end,
    Password = case wf:q(passwordTextBox) of
                   [[]] -> 
                       User#users.password;
                   [P] -> P
               end,
    case user_model:update(EMail, Uid, Password, User#users.status, User#users.role, User#users.product) of
        ok ->
            wf:user(user_model:find(Uid)), 
            gnosus_logger:message({update_user_succeeded, Uid}),
            wf:flash("profile updated"); 
        _ ->
            gnosus_logger:alarm({update_user_failed, Uid}),
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
            #textbox {id=emailTextBox, text=User#users.email, next=passwordTextBox}
        ], class="form"},

        #p{body=[
            #label{text="password" },
            #password{id=passwordTextBox, next=confirmPasswordTextBox }
        ], class="form"},

        #p{body=[
            #label{text="confirm password" },
            #password{id=confirmPasswordTextBox, next=addButton }
        ], class="form"},

        #panel{body= #list{body=[ 
            #listitem{body=#link{id=updateButton, text="update", postback=update_user}, class="button"}
    	]}, class="form form-buttons"}
    ],

    wf:wire(updateButton, emailTextBox, #validate {validators=[
        #is_required{text="email address required"},
        #is_email{text="invalid email address"},
        #max_length{text="email cannot have more than "++integer_to_list(?MAX_EMAIL_LENGTH)++" characters"},
        #custom{text="email address registered", function=fun email_available/2}
    ]}),

    wf:wire(updateButton, passwordTextBox, #validate {validators=[
        #max_length{text="password cannot have more than "++integer_to_list(?MAX_INPUT_LENGTH)++" characters"}
    ]}),

    wf:wire(updateButton, confirmPasswordTextBox, #validate {validators=[
        #custom{text="confirmation password required", tag=atag, function=fun confirmation_password_present/2},       
        #confirm_password {text="passwords must match.", password=passwordTextBox}
    ]}),

    wf:render(Body).

%%--------------------------------------------------------------------------------
email_available(_Tag, _Value) ->
    true.	

%%--------------------------------------------------------------------------------
confirmation_password_present(_Tag, Value) ->
    case wf:q(passwordTextBox) of
        [[]] -> true;
        [_]  -> case Value of
                    [] -> false;
                    _  -> true
                end
    end.

