-module(auth_by_login).
-compile({parse_transform, model_pt}).
-export([
	auth/1
]).

-include("models.hrl").
-include("wa.hrl").

-model([
  {login   , binary, [required, trim]   , #{ }},
  {password, binary, [required, trim, {min, 6}], #{ }}
]).


auth(Data) ->
	case validate(Data) of
		{ok, #{email := Email, password := Password}} ->
			users:get_by_cridentials(Email, iomod:hash_password(Password));

		{error, Errors} when is_map(Errors) ->
			{error, maps:map(
				fun
					(email, _)    -> <<"Укажите email"/utf8>>;
					(password, _) -> <<"Укажите пароль. Пароль должен содержать минимум 6 символов"/utf8>> 
				end,
				Errors)};

		Any ->
			Any
	end.