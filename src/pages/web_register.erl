%% register
%%--------------------------------------------------------------------------------
-module (web_register).

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
    case wf:user() of
        undefined -> login_navigation();
        #users{role=admin} -> gnosus_utils:navigation(admin);
        _ -> login_navigation()
    end.

%%--------------------------------------------------------------------------------
body() ->
    CancelButton = case wf:user() of
                       undefined -> [];
                       #users{role=admin} -> [#listitem{body=#link{id=cancelButton, text="cancel", postback=cancel, class="up-button"}}];
                       _ -> []
                   end,
    
    Body = [
        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, next=registerButton}
        ], class="form register"},

        #panel{body= #list{body=CancelButton++[ 
            #listitem{body=#link{ id=registerButton, text="register", postback=register, class="up-button"}}
    	]}, class="form form-buttons register-buttons"}
    ],

    wf:wire(registerButton, emailTextBox, #validate {validators=[
        #is_required{text="email address required"},
        #is_email{text="invalid email address"},
        #max_length{text="email cannot have more than "++integer_to_list(?MAX_EMAIL_LENGTH)++" characters"},
        #custom{text="email address registered", tag=some_tag, function=fun email_available/2}
    ]}),

    wf:render(Body).
	
%%================================================================================
event(register) ->
    [EMail] = wf:html_encode(wf:q(emailTextBox)),
    case user_model:register(EMail) of
        ok -> 
            gnosus_logger:message({user_registeration_succeeded, EMail}),
             case wf:user() of 
                undefined -> 
                    wf:flash(wf:f("an email has been be sent to: ~s", [EMail])),
                    wf:redirect(?LOGIN);
                #users{role=admin} -> 
                    wf:redirect(?ADMIN);
                _ -> 
                    wf:logout(),
                    wf:flash(wf:f("an email has been be sent to: ~s", [EMail])),
                    wf:redirect(?LOGIN)
            end;           
        _ ->
            gnosus_logger:alarm({user_registeration_failed, EMail}),
            wf:flash("user database update failed")                        
    end;

%%--------------------------------------------------------------------------------
event(logout) ->
    gnosus_utils:user_logout();

%%--------------------------------------------------------------------------------
event(cancel) -> 
    wf:redirect(?ADMIN);

event(_) -> ok.

%%================================================================================
email_available(_Tag, _Value) ->
    [EMail] = wf:q(emailTextBox),
    case user_model:find_by_email(EMail) of
        notfound -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------------------
login_navigation() ->
    #list{body=[ 
        #listitem{body=#link{text="login", url=?LOGIN}}
    ]}.
