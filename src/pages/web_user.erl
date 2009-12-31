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
    #literal{text="<h1>update user: <em>"++wf:get_path_info()++"</em></h1>", html_encode=false}.

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
    gnosus_utils:logout();

%%--------------------------------------------------------------------------------
event(update_user) -> 
    EMail = wf:get_path_info(),
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
            wf:redirect("/web/admin");
        _ ->
            gnosus_logger:alarm({update_user_failed, Uid}),
            wf:flash("user database update failed")                        
    end;
            
%%--------------------------------------------------------------------------------
event(cancel) -> 
    wf:redirect("/web/admin");

%%--------------------------------------------------------------------------------
event(_) -> ok.

%%================================================================================
email_available(_Tag, _Value) ->
    true.

%%--------------------------------------------------------------------------------
uid_available(_Tag, _Value) ->
    true.

%%--------------------------------------------------------------------------------
uid_present(_Tag, _Value) ->
    true.
    
%%--------------------------------------------------------------------------------
confirmation_password(_Tag, _Value) ->
    true.
    
%%--------------------------------------------------------------------------------
user_form(User) ->
    Body = [
        #p{body=[
            #label{text="email"},
            #textbox {id=emailTextBox, text=User#users.email, next=passwordTextBox}
        ], class="form user-add"},

        #p{body=[
            #label{text="user id"},
            #textbox {id=uidTextBox, next=emailTextBox}
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
            #listitem{body=#link{id=cancelButton, text="cancel", postback=cancel, class="up-button"}},
            #listitem{body=#link{id=addButton, text="update", postback=update_user, class="up-button"}}
    	]}, class="form form-buttons user-add-buttons"}
    ],

    wf:wire(addButton, emailTextBox, #validate {validators=[
        #is_required{text="email address required"},
        #is_email{text="invalid email address"},
        #custom{text="email address registered", function=fun email_available/2}
    ]}),

    wf:wire(addButton, uidTextBox, #validate {validators=[
        #custom{text="user id is required", function=fun uid_present/2},       
        #custom{text="user id is not available", function=fun uid_available/2}        
    ]}),

    wf:wire(addButton, passwordTextBox, #validate {validators=[
    ]}),

    wf:wire(addButton, confirmPasswordTextBox, #validate {validators=[
        #custom{text="confirmation password is required", function=fun confirmation_password/2},           
        #confirm_password {text="passwords must match.", password=passwordTextBox}
    ]}),

    wf:render(Body).
