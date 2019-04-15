-module(handler_api_auth).
-export([
	init/2
	]).

-include("wa.hrl").


init(Req0, State0) ->
	PathInfo = cowboy_req:path_info(Req0),
  try process(PathInfo, Req0, State0) of
    {ok, Req, State} -> 
      {ok, Req, State}
  catch 
    Ex:Exv:Stacktrace ->
      ?ERROR("Got exception ~p:~p\n~p", [Ex, Exv, Stacktrace]),
      Req = cowboy_req:reply(
      	500,
		    #{<<"content-type">> => <<"application/json">>},
		    <<"{}">>,
		    Req0),
      {ok, Req, State0}
  end.


-spec process(PathInfo, Req, State) -> {ok, Req, State}
	when
		PathInfo :: [binary()],
		Req      :: cowboy_req:req(),
		State    :: map().
process([], #{method := <<"POST">>} = Req0, State0) ->
	case http_body:body(Req0) of
		{ok, Data, Req} ->
			case auth_by_login:auth(Data) of
				{ok, #{id := Id} = User} ->
					Req1 = authenticate:auth(Req, public, Id),
					Req2 = http_handler:success_json_response(iomod:map_to_json(User), Req1),
					{ok, Req2, State0};

				{error, not_found} ->
					Req1 = http_handler:errormessage_json_response(<<"Пользователь не найден"/utf8>>, Req),
					{ok, Req1, State0};

				{error, Errors} ->
					Req1 = http_handler:notvalid_json_response(Errors, Req),
					{ok, Req1, State0};

				Any ->
					?ERROR("Authorisation failed ~p", [Any]),
					Req1 = http_handler:notauthenticated_json_response(Req),
					{ok, Req1, State0}
			end;

		_Any ->
			Req1 = http_handler:notauthenticated_json_response(Req0),
			{ok, Req1, State0}
	end;

process([<<"register">>], #{method := <<"POST">>} = Req0, State0) ->
%% TODO
	{ok, Req0, State0};

process([<<"myself">>], #{method := <<"GET">>} = Req0, State0) ->
	case authenticate:user(Req0) of
		undefined -> 
			Req1 = http_handler:notauthenticated_json_response(Req0),
			{ok, Req1, State0};

		#{ id := UserId } = User ->
			Data = #{
				user    => iomod:map_to_json(User),
				token   => session:key(Req0, public)
			},
			Req1 = http_handler:success_json_response(Data, Req0),
			{ok, Req1, State0}
	end;

process(PathInfo, Req0, State0) ->
	?INFO("Unknonwn path info ~p", [PathInfo]),
  {ok, http_handler:not_implemented(Req0), State0}.