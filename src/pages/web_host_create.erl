%% host info
%%--------------------------------------------------------------------------------
-module (web_host_create).

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
	#list{body=[ 
        #listitem{body="host"},
        #listitem{body=#link{text="admin", url="/web_users"}},
	    #listitem{body=#link{text="logout", postback=logout}}
	]}.

%%--------------------------------------------------------------------------------
body() ->
    Body = [
        #p{body=[
            #label{text="host"},
            #textbox {id=hostTextBox, next=createHostButton}
        ], class="form host-create"},

        #p{body=#link{ id=createHostButton, text="create", postback=create_host, class="form-button"}, class="form host-create-button"}
    ],

    wf:wire(createHostButton, hostTextBox, #validate {validators=[
        #is_required {text="host name required"},
        #custom {text="host name not available", function=fun validate_host/2}
    ]}),

    wf:render(Body).
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(create_host) -> 
    wf:redirect("/web/host");

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
validate_host(_Tag, _Value) ->
    true.	
