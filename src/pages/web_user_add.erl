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
            #label{text="email"},
            #textbox {id=emailTextBox, next=passwordTextBox}
        ], class="form user-add"},

        #p{body=[
            #label{text="user id"},
            #textbox {id=uidTextBox, next=emailTextBox}
        ], class="form user-add"},

        #p{body=[
            #label{text="password" },
            #password{id=passwordTextBox, next=confirmPasswordTextBox}
        ], class="form user-add"},

        #p{body=[
            #label{text="confirm password" },
            #password{id=confirmPasswordTextBox, next=roleDropdown}
        ], class="form user-add"},

        #p{body=[
            #label{text="role"},
            #dropdown{id=roleDropdown, value=user_model:role_default(), options=gnosus_utils:to_options_list(user_model:role_values())}
        ], class="form user-add"},

        #p{body=[
            #label{text="status"},
            #dropdown{id=statusDropdown, value=user_model:status_default(), options=gnosus_utils:to_options_list(user_model:status_values())}
        ], class="form user-add"},

        #p{body=[
            #label{text="product"},
            #dropdown{id=productDropdown, value=user_model:product_default(), options=gnosus_utils:to_options_list(user_model:product_values())}
        ], class="form user-add"},

        #panel{body= #list{body=[ 
            #listitem{body=#link{id=cancelButton, text="cancel", postback=cancel, class="up-button"}},
            #listitem{body=#link{id=addButton, text="add", postback=add_user, class="up-button"}}
    	]}, class="form form-buttons user-add-buttons"}
    ],

    wf:wire(addButton, emailTextBox, #validate {validators=[
        #is_required{text="email address required"},
        #is_email{text="invalid email address"},
        #custom{text="email address registered", tag=some_tag, function=fun validate_email/2}
    ]}),

    wf:wire(addButton, uidTextBox, #validate {validators=[
        #custom{text="user id is required", tag=some_tag, function=fun validate_uid_present/2},       
        #custom{text="user id is not available", tag=some_tag, function=fun validate_uid/2}        
    ]}),

    wf:wire(addButton, passwordTextBox, #validate {validators=[
        #custom{text="password is required", tag=some_tag, function=fun validate_password/2}        
    ]}),

    wf:wire(addButton, confirmPasswordTextBox, #validate {validators=[
        #custom{text="confirmation password is required", tag=some_tag, function=fun validate_confirmation_password/2},       
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
    [Status] = wf:q(statusDropdown),
    [Role] = wf:q(roleDropdown),
    [Product] = wf:q(productDropdown),
    case Status of
        registered -> update_user_database(user_model:register(EMail), EMail);
        _ -> update_user_database(user_model:new(EMail, Uid, Password, list_to_atom(Status), list_to_atom(Role), list_to_atom(Product)), EMail)
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
validate_uid_present(_Tag, _Value) ->
    true.	

%%--------------------------------------------------------------------------------
validate_uid(_Tag, _Value) ->
    true.	

%%--------------------------------------------------------------------------------
validate_password(_Tag, _Value) ->
    true.	

%%--------------------------------------------------------------------------------
validate_confirmation_password(_Tag, _Value) ->
    true.	

%%--------------------------------------------------------------------------------
update_user_database(Update, EMail) ->
    case Update of
        ok -> 
            gnosus_logger:message({add_user_succeeded, EMail}),
            wf:redirect("/web/admin");
        _ ->
            gnosus_logger:alarm({add_user_failed, EMail}),
            wf:flash("user database update failed")                        
    end.
