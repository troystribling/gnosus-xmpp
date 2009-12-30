%% host info
%%--------------------------------------------------------------------------------
-module (web_profile).

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
    gnosus_utils:navigation(profile).   

%%--------------------------------------------------------------------------------
title() ->
    #h1{text="user profile"}.

%%--------------------------------------------------------------------------------
body() ->
    Uid = wf:get_path_info(),
    case user_model:find(Uid) of
        notfound ->
            wf:flash("user not found");                      
        error ->
            wf:flash("database error");                        
        User -> 
            user_form(User)
    end.     
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(update_user) -> 
    Uid = wf:get_path_info(),
    User = user_model:find(Uid),
    [EMail] = wf:q(emailTextBox),
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
            gnosus_logger:message({update_user_succeeded, Uid}),
            wf:flash("profile updated"); 
        _ ->
            gnosus_logger:alarm({update_user_failed, Uid}),
            wf:flash("user database update failed")                        
    end;
            
%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
validate_email(_Tag, _Value) ->
    true.	

%%--------------------------------------------------------------------------------
validate_uid(_Tag, _Value) ->
    true.	

%%--------------------------------------------------------------------------------
user_form(User) ->
    Body = [
        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, text=User#users.email, next=rollDropdown}
        ], class="form user-add"},

        #p{body=[
            #label{text="password" },
            #password{id=passwordTextBox, next=confirmPasswordTextBox }
        ], class="form user-add"},

        #p{body=[
            #label{text="confirm password" },
            #password{id=confirmPasswordTextBox, next=addButton }
        ], class="form user-add"},

        #panel{body= #list{body=[ 
            #listitem{body=#link{id=addButton, text="update", postback=update_user, class="up-button"}}
    	]}, class="form form-buttons user-add-buttons"}
    ],

    wf:wire(addButton, emailTextBox, #validate {validators=[
        #is_required{text="email address required"},
        #is_email{text="invalid email address"},
        #custom{text="email address registered", tag=some_tag, function=fun validate_email/2}
    ]}),

    wf:wire(addButton, passwordTextBox, #validate {validators=[
    ]}),

    wf:wire(addButton, confirmPasswordTextBox, #validate {validators=[
      #confirm_password {text="passwords must match.", password=passwordTextBox}
    ]}),

    wf:render(Body).
