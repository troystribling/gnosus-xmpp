%% login
%%--------------------------------------------------------------------------------
-module (web_index).

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
	    #listitem{body=#link{text="register", url="web/register"}}
	]}.

%%--------------------------------------------------------------------------------
body() ->
    Body = [
        #p{body=[
            #label{text="username" },
            #textbox{id=userTextBox, next=emailTextBox }
        ], class="form login"},

        #p{body=[
            #label{text="password" },
            #password{id=passwordTextBox, next=confirmTextBox }
        ], class="form login"},

        #p{body=#button{id=loginButton, text="login", postback=continue}, class="form-button login-button"}
    ],

    wf:wire(loginButton, userTextBox, #validate { validators=[
      #is_required{text="username required"}
    ]}),

    wf:wire(loginButton, passwordTextBox, #validate {validators=[
      #is_required {text="password required"},
      #min_length {length=6, text="must be at least 6 characters long"}
    ]}),

    wf:render(Body).
	
%%--------------------------------------------------------------------------------
event(continue) ->
    [Uid] = wf:q(userTextBox),
    [Password] = wf:q(passwordTextBox),
    case user_model:authenticate(Uid, Password) of
        true ->
            wf:redirect("web/domain");
        false ->
            wf:flash("authentication failed")
    end;

event(_) -> ok.
