%% gnosus utilities
%%--------------------------------------------------------------------------------
-module(gnosus_utils).

%% API
-export([
    logout/0,
    load_config_file/0
]).
 
%% include
-include_lib("models.hrl").
-include_lib("gnosus.hrl").

%%================================================================================
logout() ->
    #users{uid=Uid} = wf:user(),
    gnosus_logger:message({terminate_session, Uid}),
    wf:clear_user(),
    wf:flash("logged out"),
    wf:redirect("/").

%%--------------------------------------------------------------------------------
load_config_file() ->
    case file:consult("gnosus.cfg") of
    	{ok, Config} ->
    	    gnosus_logger:message({config_loaded, Config}),
    	    config_model:write_config(Config);
    	{error, {_LineNumber, erl_parse, _ParseMessage} = Reason} ->
    	    ExitText = lists:flatten("gnosus.cfg approximately in the line "++file:format_error(Reason)),
    	    gnosus_logger:alarm({config_load_failed, ExitText}),
    	    exit(ExitText);
    	{error, Reason} ->
    	    ExitText = lists:flatten("gnosus.cfg : " ++ file:format_error(Reason)),
    	    gnosus_logger:alarm({config_load_failed, ExitText}),
    	    exit(ExitText)
    end.
