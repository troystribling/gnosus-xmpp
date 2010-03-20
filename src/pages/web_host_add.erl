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
            #textbox {id=hostTextBox, next=addHostButton},
            #span{text="."++gnosus_model:tld()}
        ], class="form host-add host-add-text"},

        #panel{body= #list{body=CancelButton++[
            #listitem{body=#link{id=addHostButton, text="add", postback=add_host, class="up-button"}}]}, 
            class="form form-buttons host-add-buttons"}
    ],

    wf:wire(addHostButton, hostTextBox, #validate {validators=[
        #is_required{text="host name required"},
        #max_length{text="host cannot have more than "++integer_to_list(?MAX_INPUT_LENGTH)++" characters"},
        #custom{text="host not available", function=fun host_available/2}
    ]}),

    wf:render(Body).
	
%%================================================================================
event(logout) ->
    gnosus_utils:user_logout();

%%--------------------------------------------------------------------------------
event(add_host) ->
    [Host] = wf:html_encode(wf:q(hostTextBox)), 
    gnosus_utils:add_host(Host++"."++gnosus_model:tld());

%%--------------------------------------------------------------------------------
event(cancel) -> 
    wf:redirect(?HOSTS);

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
host_available(_Tag, _Value) ->
    [Host] = wf:q(hostTextBox), 
    case host_model:find(Host++"."++gnosus_model:tld()) of
        notfound -> true;
        _ -> false
    end.
