-module(session_middleware).
-export([
	execute/2
	]).

-include("wa.hrl").
-include("session.hrl").


-spec execute(Req, Env) -> {ok, Req, Env}
	when Req::cowboy_req:req(), Env::cowboy_middleware:env().
execute(Req0, Env) ->
	Req = handle_sessions(
		Req0, [session:session_opts(public), session:session_opts(backoffice)]),
	{ok, Req, Env}.


-spec handle_sessions(Req, Opts) -> Req
	when Req::cowboy_req:req(), Opts::[session_opts()].
handle_sessions(Req0, []) ->
	Req0;
handle_sessions(Req0, [SessionOpts|Rest]) ->
	case session:find_session(Req0, SessionOpts) of 
		undefined ->
			?DEBUG("Session not found. Creating new ~p", [SessionOpts]),
			Req = session:new_session(Req0, SessionOpts),
			handle_sessions(Req, Rest);

		Req ->
			handle_sessions(Req, Rest)
	end.


