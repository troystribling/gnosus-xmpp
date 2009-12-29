%% host info
%%--------------------------------------------------------------------------------
-module (web_host_add).

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
    gnosus_utils:navigation(host).   

%%--------------------------------------------------------------------------------
body() ->
    CancelButton = case wf:session(hosts) of
                       [] -> [];
                       _ -> [#listitem{body=#link{id=cancelButton, text="cancel", postback=cancel, class="up-button"}}]
                   end,
    Body = [
        #p{body=[
            #label{text="host"},
            #textbox {id=hostTextBox, next=addHostButton}
        ], class="form host-add"},

        #panel{body= #list{body=
            CancelButton++[#listitem{body=#link{id=addHostButton, text="add", postback=add_host, class="up-button"}}]}, 
            class="form form-buttons host-add-buttons"}
    ],

    wf:wire(addHostButton, hostTextBox, #validate {validators=[
        #is_required{text="host name required"},
        #custom{text="host name not available", function=fun host_available/2}
    ]}),

    wf:render(Body).
	
%%================================================================================
event(logout) ->
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(add_host) -> 
    gnosus_utils:add_host();

%%--------------------------------------------------------------------------------
event(cancel) -> 
    wf:redirect("/web/hosts");

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
host_available(_Tag, _Value) ->
    true.	
