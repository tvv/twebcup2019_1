-module(handler_api_oauth).
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
process([Type], #{method := <<"GET">>} = Req0, State0) ->
	case oauth_uri(Type) of
		{ok, Uri} ->
			Req = http_handler:success_json_response(#{uri => Uri}, Req0),
			{ok, Req, State0};

		_Any ->
			Req = http_handler:errormessage_json_response(<<"Неизвестная социальная сеть"/utf8>>, Req0),
			{ok, Req, State0}
	end;

process([<<"yandex">>, <<"callback">>], #{method := <<"GET">>} = Req0, State0) ->
	case cowboy_req:match_qs([{code, [], undefined}, {error, [], undefined}], Req0) of
		#{code := undefined} ->
			{ok, http_handler:redirect(wa_app:host(), Req0), State0};

		#{code := Code} ->
			case yaoauth:get_token_with_user(Code) of
				{ok, Token, UserInfo} ->
						case ensure_user(Token, UserInfo) of
							{ok, #{ id := UserId }} ->
								Req1 = authenticate:auth(Req0, public, UserId),					
								{ok, http_handler:redirect(wa_app:host(), Req1), State0};

							Any ->
								?ERROR("Cant ensure user ~p", [Any]),
								{ok, http_handler:redirect(wa_app:host(), Req0), State0}
						end;

				Any ->
					?ERROR("Failed to get yandex token and user ~p", [Any]),
					{ok, http_handler:redirect(wa_app:host(), Req0), State0}
			end
	end;

process(PathInfo, Req0, State0) ->
	?INFO("Unknonwn path info ~p", [PathInfo]),
  {ok, http_handler:not_implemented(Req0), State0}.


-spec oauth_uri(Type) -> {ok, Uri} | {error, term()}
	when
		Type :: user_social_accounts:accout_types(),
		Uri  :: binary().
oauth_uri(<<"yandex">>) ->
	case redirect_uri(<<"yandex">>) of
		{ok, RedirectUri} ->
			yaoauth:oauth_uri(RedirectUri);

		Any ->
			Any
	end;
oauth_uri(Type) ->
	?ERROR("Unknown oauth type ~p", [Type]),
	{error, unknown_type}.


-spec redirect_uri(Type) -> {ok, Uri} | {error, term()}
	when 
		Type :: user_social_accounts:accout_types(),
		Uri  :: binary().
redirect_uri(<<"yandex">>) ->
	{ok, <<(wa_app:host())/binary, "/api/v1/oauth/yandex/callback">>};
redirect_uri(Type) ->
	?ERROR("Unknown oauth redirect url type ~p", [Type]),
	{error, unknown_type}.


-spec ensure_user(Token, UserInfo) -> {ok, User} | {error, term()}
	when 
		Token    :: binary(),
		UserInfo :: map(),
		User     :: map().
ensure_user(Token, UserInfo) ->
	case users:get_by_token(Token) of
		{ok, User} -> 
			{ok, User};

		{error, not_found} ->
			case get_email(UserInfo) of
				{ok, Email} ->
					case users:get_by_email(Email) of
						{ok, User} ->
							{ok, User};

						{error, not_found} ->
							users:new_account(Token, get_name(UserInfo), UserInfo, Email);

						Any ->
							?ERROR("Cant get user by email ~p", [Any]),
							Any
					end;

				{error, not_found} ->
					users:new_account(Token, get_name(UserInfo), UserInfo, undefined)					
			end;

		Any ->
			?ERROR("Cant get user by yandex token ~p", [Any]),
			Any
	end.
	

-spec get_email(UserInfo) -> {ok, Email} | {error, not_found}
  when
    UserInfo :: map(),
    Email    :: binary().
get_email(#{<<"default_email">> := Email}) ->
  {ok, Email};
get_email(_) ->
  {error, not_found}.


-spec get_name(UserInfo) -> Name
  when
    UserInfo :: map(),
    Name     :: binary().
get_name(#{ <<"display_name">> := Name }) ->
  Name;
get_name(_) ->
  <<>>.
