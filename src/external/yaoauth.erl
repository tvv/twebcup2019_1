-module(yaoauth).

-export([
	auth_headers/1,
	oauth_uri/1,
	get_token_with_user/1,
	get_token/1,
	user_info/1
]).

-include("wa.hrl").


-spec auth_headers(Token) -> Headers
	when
		Token   :: binary() | undefined,
		Headers :: map().
auth_headers(undefined) ->
	#{};
auth_headers(Token) ->
	#{<<"Authorization">> => <<"OAuth ", Token/binary>>}.


-spec oauth_uri(RedirectUri) -> {ok, Url} | {error, no_network_id}
	when
		RedirectUri :: binary(),
		Url         :: binary().
oauth_uri(RedirectUri) ->
	case ?CONFIG(socials, #{}) of
		#{ yandex := #{ id := YandexId}} ->
			{ok, 
			<<"https://oauth.yandex.ru/authorize?response_type=code",
				"&client_id=", YandexId/binary,
				"&redirect_uri=", RedirectUri/binary>>};

		_ ->
			{error, no_network_id} 
	end.


-spec get_token_with_user(Code) -> {ok, Token, UserInfo} | {error, term()}
	when
		Code     :: binary(),
		Token    :: binary(),
		UserInfo :: map().
get_token_with_user(Code) ->
	case get_token(Code) of
		{ok, Token} ->
			case user_info(Token) of 
				{ok, UserInfo} ->
					{ok, Token, UserInfo};

				Any ->
					Any
			end;

		Any ->
			Any
	end.


-spec get_token(Code) -> {ok, Token, UserInfo} | {error, term()}
	when
		Code     :: binary(),
		Token    :: binary(),
		UserInfo :: map().
get_token(Code) ->
	case ?CONFIG(socials, #{}) of
		#{ yandex := #{ id := YandexId, secret := YandexSecret }} ->
			Request = #{
				<<"grant_type">>    => <<"authorization_code">>,
				<<"code">>          => Code,
				<<"client_id">>     => YandexId,
				<<"client_secret">> => YandexSecret
			},
			case rest:request(post, qs, <<"https://oauth.yandex.ru/token">>, [], #{}, Request) of
				{ok, 200, _, #{<<"access_token">> := Token}} ->
					{ok, Token};

				{_, _, _, Response} ->
					?ERROR("Failed to get yandex token ~p", [Response]),
					{error, no_token}
			end;

		Any ->
			?ERROR("Yandex oauth parameters not defined at config. ~p", [Any]),
			{error, wrong_config}
	end.


-spec user_info(Token) -> {ok, Result} | {error, term()}
	when
		Token  :: binary(),
		Result :: map().
user_info(Token) ->
	Headers = auth_headers(Token),
	case rest:request(get, qs, <<"https://login.yandex.ru/info?format=json">>, [], Headers, #{}) of
		{ok, 200, _, Response} ->
			{ok, Response};

		{_, _, [], Response} ->
			?ERROR("Failed to get yandex user info ~p", [Response]),
			{error, user_not_found}
	end.