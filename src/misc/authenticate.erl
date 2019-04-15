-module(authenticate).
-export([
	auth/2,
	auth/3,

	deauth/2,

	user/1,
	user/2,

	is_authenticated/1,
	is_authenticated/2
	]).

-include("wa.hrl").
-include("session.hrl").
-include("authenticate.hrl").


-spec auth(cowboy_req:req(), session_type()) -> cowboy_req:req().
auth(Req0, Type) ->
	set_user_to_request(
		Req0, get_user(session:user_id(Req0, Type), Type), Type).


-spec auth(cowboy_req:req(), session_type(), integer()) -> cowboy_req:req().
auth(Req0, Type, UserId) ->
	case get_user(UserId, Type) of
		undefined ->
			Req0; 

		User ->			
			Req = session:set_user_id(Req0, Type, UserId),
			set_user_to_request(Req, User, Type)
	end.


-spec deauth(cowboy_req:req(), session_type()) -> cowboy_req:req().
deauth(Req0, Type) ->
	Req = session:set_user_id(Req0, Type, undefined),
	set_user_to_request(Req, undefined, Type).


-spec user(cowboy_req:req()) -> map() | undefined.
user(Req0) ->
	user(Req0, public).


-spec user(cowboy_req:req(), session_type()) -> map() | undefined.
user(#{?PUBLIC_USER_REQUEST_KEY := User}, public) ->
	User;
user(#{?BACKOFFICE_USER_REQUEST_KEY := User}, backoffice) ->
	User.


-spec is_authenticated(cowboy_req:req()) -> boolean().
is_authenticated(Req0) ->
	is_authenticated(Req0, public).


-spec is_authenticated(cowboy_req:req(), session_type()) -> boolean().
is_authenticated(Req0, Type) ->
	case user(Req0, Type) of
		undefined -> false;
		_         -> true
	end.


-spec get_user(integer() | undefined, session_type()) -> map() | undefined.
get_user(UserId, public) when is_integer(UserId), UserId > 0 ->
	get_public_user(UserId);
get_user(UserId, backoffice) when is_integer(UserId), UserId > 0 ->
	get_backoffice_user(UserId);
get_user(_, _) ->
	undefined.


-spec set_user_to_request(cowboy_req:req(), map() | undefined, session_type()) -> cowboy_req:req().
set_user_to_request(Req0, User, public) ->
	maps:put(?PUBLIC_USER_REQUEST_KEY, User, Req0);
set_user_to_request(Req0, User, backoffice) ->
	maps:put(?BACKOFFICE_USER_REQUEST_KEY, User, Req0).


-spec get_public_user(integer()) -> map().
get_public_user(UserId) ->
	case users:get(UserId) of
		{ok, User} -> User;
		_          -> undefined
	end.


-spec get_backoffice_user(integer()) -> map().
get_backoffice_user(UserId) ->
	#{id => UserId}.
