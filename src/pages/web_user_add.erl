%% host info
%%--------------------------------------------------------------------------------
-module (web_user_add).

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
    gnosus_utils:navigation(admin).   

%%--------------------------------------------------------------------------------
body() ->
    Body = [
        #p{body=[
            #label{text="user id"},
            #textbox {id=uidTextBox, next=passwordTextBox}
        ], class="form user-add"},

        #p{body=[
            #label{text="password" },
            #password{id=passwordTextBox, next=confirmPasswordTextBox }
        ], class="form user-add"},

        #p{body=[
            #label{text="confirm password" },
            #password{id=confirmPasswordTextBox, next=emailTextBox }
        ], class="form user-add"},

        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, next=cancelButton}
        ], class="form user-add"},

        #panel{body= #list{body=[ 
            #listitem{body=#link{id=cancelButton, text="cancel", postback=cancel, class="up-button"}},
            #listitem{body=#link{id=addButton, text="add", postback=add_user, class="up-button"}}
    	]}, class="form form-buttons user-add-buttons"}
    ],

    wf:wire(addButton, emailTextBox, #validate {validators=[
        #is_email{text="invalid email address"},
        #custom{text="email address registered", tag=some_tag, function=fun validate_email/2}
    ]}),

    wf:wire(addButton, uidTextBox, #validate {validators=[
        #is_required{text="uid required"},
        #custom{text="user id is not available", tag=some_tag, function=fun validate_uid/2}        
    ]}),

    wf:wire(addButton, passwordTextBox, #validate {validators=[
      #is_required{text="password required"}
    ]}),

    wf:wire(addButton, confirmPasswordTextBox, #validate {validators=[
      #is_required{text="confirmation password required"},
      #confirm_password { text="passwords must match.", password=passwordTextBox }
    ]}),

    wf:render(Body).
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(add_user) -> 
    [EMail] = wf:q(emailTextBox),
    [Uid] = wf:q(uidTextBox),
    [Password] = wf:q(passwordTextBox),
    case user_model:new(EMail, Uid, Password, active, user, free) of
        ok -> 
            gnosus_logger:message({add_user_succeeded, Uid}),
            wf:redirect("/web/admin");
        _ ->
            gnosus_logger:alarm({add_user_failed, Uid}),
            wf:flash("user database update failed")                        
    end;
            
%%--------------------------------------------------------------------------------
event(cancel) -> 
    wf:redirect("/web/admin");

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
validate_email(_Tag, _Value) ->
    true.	

%%--------------------------------------------------------------------------------
validate_uid(_Tag, _Value) ->
    true.	
