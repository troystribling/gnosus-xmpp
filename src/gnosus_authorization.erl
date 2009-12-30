%% interface authorization 
%%--------------------------------------------------------------------------------
-module(gnosus_authorization).
 
%% API
-export([
     authorize/1
]).
 
%% include
-include_lib("gnosus.hrl").
-include_lib("models.hrl").

%%================================================================================
-define(NOAUTHENTICATE_ROUTES, [web_index, web_register]).
-define(ADMIN_ROUTES, [web_admin, web_user_add, web_user]).

%%================================================================================
authorize(Module) ->
    case lists:member(Module, ?NOAUTHENTICATE_ROUTES) of
        false -> is_authenticated(Module);
        true -> is_admin(Module)
    end.
                        
%%================================================================================
is_admin(Module) ->
    case lists:member(Module, ?ADMIN_ROUTES) of
        false -> ok;
        true ->
            #users{uid=Uid, role=Role} = wf:user(),
            case Role of
                admin ->
                    gnosus_logger:message({admin_authorized, Uid}),                     
                    ok;
                _ -> 
                    wf:flash("access denied"),
                    gnosus_logger:alarm({admin_authorization_failed, Uid}),                     
                    wf:clear_user(),
                    wf:redirect("/")
            end
            
    end.

%%--------------------------------------------------------------------------------
is_authenticated(Module) ->
    case wf:user() of
        undefined -> 
            gnosus_logger:warning({authentication_authorization_failed, Module}),                     
            wf:redirect("/");
        _User -> ok
    end.   
    