%% login
%%--------------------------------------------------------------------------------
-module (web_index).

%% API
-compile(export_all).

%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").
-include_lib("nitrogen/include/wf.inc").

%%================================================================================
main() -> 
    #template{file="./wwwroot/form_no_title_template.html"}.

%%--------------------------------------------------------------------------------
navigation() ->
	#list{body=[ 
	    #listitem{body=#link{text="register", url=?REGISTER}}
	]}.

%%--------------------------------------------------------------------------------
body() ->
    Body = [
        #p{body=[
            #label{text="uid" },
            #textbox{id=userTextBox, next=passwordTextBox }
        ], class="form login"},

        #p{body=[
            #label{text="password" },
            #password{id=passwordTextBox, next=loginButton }
        ], class="form login"},

        #panel{body= #list{body=[ 
            #listitem{body=#link{id=loginButton, text="login", postback=login}, class="button"}
    	]}, class="form form-buttons"}
    ],

    wf:wire(loginButton, userTextBox, #validate { validators=[
      #is_required{text="uid required"}
    ]}),

    wf:wire(loginButton, passwordTextBox, #validate {validators=[
      #is_required{text="password required"}
    ]}),

    wf:render(Body).
	
%%================================================================================
event(login) ->
    [Uid] = wf:html_encode(wf:q(userTextBox)),
    [Password] = wf:q(passwordTextBox),
    case user_model:authenticate(Uid, Password) of
        true ->
            wf:user(user_model:find(Uid)),
            wf:session(hosts, host_model:hosts_list_by_uid(Uid)),
            gnosus_utils:start_page_redirect();
        false -> case client_user_model:authenticate(Uid, Password) of
                     true ->
                         wf:user(client_user_model:find_by_jid(Uid)),
                         wf:redirect(?CLIENT);
                     false ->   
                        gnosus_logger:warning({authentication_failed, Uid}),          
                        wf:flash("authentication failed")
                 end
    end;

%%--------------------------------------------------------------------------------
event(_) -> ok.
