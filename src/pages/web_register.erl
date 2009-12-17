%% register
%%--------------------------------------------------------------------------------
-module (web_register).

%% API
-compile(export_all).

%% include
-include_lib ("nitrogen/include/wf.inc").

%%================================================================================
main() -> 
	#template { file="./wwwroot/template.html"}.

%%--------------------------------------------------------------------------------
navigation() ->
	#list{body=[ 
	    #listitem{body=#link{text="login", url="/"}}
	]}.

%%--------------------------------------------------------------------------------
body() ->
    Body = [
        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, next=emailTextBox}
        ], class="form register"},

        #p{body=#button{ id=registerButton, text="register", postback=continue}, class="form-button register-button"}
    ],

    wf:wire(registerButton, emailTextBox, #validate {validators=[
        #is_required { text="email address required" },
        #is_email { text="invalid email address" },
        #custom { text="email address registered", tag=some_tag, function=fun validate_email/2}
    ]}),

    wf:render(Body).
	
%%--------------------------------------------------------------------------------
event(continue) ->
    [Name] = wf:q(emailTextBox),
    wf:flash(wf:f("an email will be sent to: ~s", [Name])),
    ok;

event(_) -> ok.

%%--------------------------------------------------------------------------------
validate_email(_Tag, _Value) ->
    true.	
