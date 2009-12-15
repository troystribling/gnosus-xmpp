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
        false -> wf:redirect("web/login");
        true -> is_admin(Module)
    end.
                        
%%================================================================================
is_admin(Module) ->
    case lists:member(Module, ?ADMIN_ROUTES) of
        false -> ok;
        true -> wf:redirect("web/login")
    end.
    