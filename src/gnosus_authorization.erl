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
-define(ADMIN_ROUTES, []).

%%================================================================================
authorize(Module) ->
    case lists:member(Module, ?NOAUTHENTICATE_ROUTES) of
        false -> is_authenticated();
        true -> is_admin(Module)
    end.
                        
%%================================================================================
is_admin(Module) ->
    case lists:member(Module, ?ADMIN_ROUTES) of
        false -> ok;
        true ->
            #users{uid=Uid, role=Role} = user_model:find(wf:user()),
            case Role of
                admin ->
                    gnosus_logger:message({admin_authorized, Uid}),                     
                    ok;
                _ -> 
                    gnosus_logger:alarm({admin_authorization_failed, Uid}),                     
                    wf:clear_user(),
                    wf:redirect("web/index")
            end
            
    end.

%%--------------------------------------------------------------------------------
is_authenticated() ->
    case wf:user() of
        undefined -> 
            gnosus_logger:warning({authentication_authorization_failed, wf:get_path_info()}),                     
            wf:redirect("web/index");
        _Uid -> ok
    end.   
    