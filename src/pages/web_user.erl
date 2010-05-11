%% host info
%%--------------------------------------------------------------------------------
-module (web_user).

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
title() ->
    #literal{text="<h1>update user <em>"++wf:html_encode(wf:get_path_info())++"</em></h1>", html_encode=false}.

%%--------------------------------------------------------------------------------
body() ->
    EMail = wf:get_path_info(),
    case user_model:find_by_email(EMail) of
        notfound ->
            wf:flash("user not found");                      
        error ->
            wf:flash("database error");                        
        User -> 
            user_form(User)
    end.     
	
%%================================================================================
event(logout) ->
    gnosus_utils:user_logout();

%%--------------------------------------------------------------------------------
event(update_user) -> 
    EMail = wf:html_encode(wf:get_path_info()),
    User = user_model:find_by_email(EMail),
    [FormEMail] = wf:q(emailTextBox),
    [Status] = wf:q(statusDropdown),
    [Role] = wf:q(roleDropdown),
    [Product] = wf:q(productDropdown),
    Uid = User#users.uid,
    if 
        FormEMail =:= EMail -> ok;
        true -> user_model:delete_by_email(EMail)
    end,
    Password = case wf:q(passwordTextBox) of
                   [[]] -> 
                       User = user_model:find_by_email(EMail),
                       User#users.password;
                   [P] -> P
               end,
    case user_model:update(EMail, Uid, Password, list_to_atom(Status), list_to_atom(Role), list_to_atom(Product)) of
        ok -> 
            gnosus_logger:message({update_user_succeeded, Uid}),
            wf:redirect(?ADMIN);
        _ ->
            gnosus_logger:alarm({update_user_failed, Uid}),
            wf:flash("user database update failed")                        
    end;
            
%%--------------------------------------------------------------------------------
event(cancel) -> 
    wf:redirect(?ADMIN);

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
user_form(User) ->
    Body = [
        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, text=User#users.email, next=passwordTextBox}
        ], class="form user-add"},

        #p{body=[
            #label{text="uid"},
            #textbox {id=uidTextBox, text=User#users.uid, next=emailTextBox}
        ], class="form user-add"},

        #p{body=[
            #label{text="password" },
            #password{id=passwordTextBox, next=confirmPasswordTextBox }
        ], class="form user-add"},

        #p{body=[
            #label{text="confirm password" },
            #password{id=confirmPasswordTextBox, next=roleDropdown }
        ], class="form user-add"},

        #p{body=[
            #label{text="role"},
            #dropdown{id=roleDropdown, value=atom_to_list(User#users.role), options=gnosus_utils:to_options_list(user_model:role_values())}
        ], class="form user-add"},

        #p{body=[
            #label{text="status"},
            #dropdown{id=statusDropdown, value=atom_to_list(User#users.status), options=gnosus_utils:to_options_list(user_model:status_values())}
        ], class="form user-add"},

        #p{body=[
            #label{text="product"},
            #dropdown{id=productDropdown, value=atom_to_list(User#users.product), options=gnosus_utils:to_options_list(user_model:product_values())}
        ], class="form user-add"},

        #panel{body= #list{body=[ 
            #listitem{body=#link{id=cancelButton, text="cancel", postback=cancel}, class="button"},
            #listitem{body=#link{id=addButton, text="update", postback=update_user}, class="button"}
    	]}, class="form form-buttons"}
    ],

    wf:wire(addButton, emailTextBox, #validate {validators=[
        #is_required{text="email address required"},
        #is_email{text="invalid email address"},
        #max_length{text="email cannot have more than "++integer_to_list(?MAX_EMAIL_LENGTH)++" characters"},
        #custom{text="email address registered", function=fun email_available/2}
    ]}),

    wf:wire(addButton, uidTextBox, #validate {validators=[
        #custom{text="uid required", function=fun uid_present/2},       
        #custom{text="uid is not available", function=fun uid_available/2},       
        #max_length{text="uid cannot have more than "++integer_to_list(?MAX_INPUT_LENGTH)++" characters"}
    ]}),

    wf:wire(addButton, passwordTextBox, #validate {validators=[
        #max_length{text="password cannot have more than "++integer_to_list(?MAX_INPUT_LENGTH)++" characters"}
    ]}),

    wf:wire(addButton, confirmPasswordTextBox, #validate {validators=[
        #custom{text="confirmation password required", function=fun confirmation_password_present/2},           
        #confirm_password {text="passwords must match.", password=passwordTextBox}
    ]}),

    wf:render(Body).

%%================================================================================
%% validatitors
email_available(_Tag, _Value) ->
    true.

%%--------------------------------------------------------------------------------
uid_available(_Tag, _Value) ->
    true.

%%--------------------------------------------------------------------------------
uid_present(_Tag, _Value) ->
    true.

%%--------------------------------------------------------------------------------
confirmation_password_present(_Tag, _Value) ->
    true.

