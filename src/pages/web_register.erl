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
      #p{},
      #label { text="email" },
      #textbox { id=emailTextBox, next=emailTextBox },

      #p{},  
      #button { id=registerButton, text="register", postback=continue }
    ],

    wf:wire(registerButton, emailTextBox, #validate { validators=[
        #is_required { text="email address required" },
        #is_email { text="enter a valid email address" },
        #custom { text="email address is registered", tag=some_tag, function=fun validate_email/2 }
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
