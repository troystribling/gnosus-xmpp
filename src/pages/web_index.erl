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
      #p{},
      #label { text="username" },
      #textbox { id=userTextBox, next=emailTextBox },

      #p{},  
      #label { text="password" },
      #password { id=passwordTextBox, next=confirmTextBox },

      #p{},  
      #button { id=loginButton, text="Continue", postback=continue }
    ],

    wf:wire(loginButton, userTextBox, #validate { validators=[
      #is_required { text="username required" }
    ]}),

    wf:wire(loginButton, passwordTextBox, #validate { validators=[
      #is_required { text="password required" },
      #min_length { length=6, text="Password must be at least 6 characters long." }
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
