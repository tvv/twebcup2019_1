-module(session).
-export([
	key/1,
	key/2,
	user_id/1,
	user_id/2,
	set_user_id/2,
	set_user_id/3,

	type/1,

	params/1,
	params/2,

	get_param/2,
	get_param/3,
	get_param/4,

	set_param/3,
	set_param/4,

	del_param/2,
	del_param/3,

	session_opts/1,
	session/1,
	session/2,
	find_session/2,
	new_session/1,
	new_session/2,
	drop_session/1,
	drop_session/2
	]).

-include("wa.hrl").
-include("session.hrl").


-type fetch_session_key_fun() :: fun((cowboy_req:req(), session_opts()) -> session() | undefined).

-spec key(cowboy_req:req(), session_type()) -> undefined | binary().
key(Req0, Type) ->
	case session(Req0, Type) of
		undefined -> undefined;
		Session -> key(Session)
	end.


-spec key(session()) -> undefined | binary().
key(#{key := Key}) ->
	Key.


-spec user_id(cowboy_req:req(), session_type()) -> undefined | pos_integer().
user_id(Req0, Type) ->
	case session(Req0, Type) of
		undefined -> undefined;
		Session -> user_id(Session)
	end.


-spec user_id(session()) -> undefined | pos_integer().
user_id(#{user_id := UserId}) ->
	UserId.


-spec set_user_id(cowboy_req:req(), session_type(), integer() | undefined) -> cowboy_req:req().
set_user_id(Req0, Type, UserId) ->
	case session(Req0, Type) of
		undefined -> undefined;
		Session0 -> 
			Session = set_user_id(Session0, UserId),
			set_session_to_request(Req0, Session)
	end.


-spec set_user_id(session(), integer() | undefined) -> session().
set_user_id(Session0, UserId) when UserId == undefined; UserId < 1 ->
	Session = Session0#{ 
		user_id => undefined,
		expires_after => ?ANONYMOUS_SESSION_LIFETIME},
	save_session(Session),
	Session;
set_user_id(Session0, UserId) when is_integer(UserId) ->
	Session = Session0#{ 
		user_id => UserId,
		expires_after => ?AUTHENTICATED_SESSION_LIFETIME},
	save_session(Session),
	Session;
set_user_id(Session0, _) ->
	Session0.
	

-spec type(session()) -> session_type().
type(#{ type := SessionType}) ->
	SessionType.


-spec params(cowboy_req:req(), session_type()) -> session_params().
params(Req0, Type) ->
	case session(Req0, Type) of
		undefined -> undefined;
		Session0  -> params(Session0)
	end.


-spec params(session()) -> session_params().
params(#{params := Params}) ->
	Params.


-spec get_param(cowboy_req:req(), session_type(), atom(), any()) -> {ok, any()}.
get_param(Req0, Type, Name, Default) when is_atom(Name) ->
	case session(Req0, Type) of
		undefined -> undefined;
		Session0  -> get_param(Name, Session0, Default)
	end.


-spec get_param(atom(), session()) -> {ok, any()} | {error, not_found}.
get_param(Name, Session) when is_atom(Name) ->
	Params = params(Session),
	case maps:is_key(Name, Params) of
		true -> maps:get(Name, Params);
		_    -> {error, not_found}
	end.


-spec get_param(atom(), session(), any()) -> {ok, any()}.
get_param(Name, Session, Default) when is_atom(Name) ->
	maps:get(Name, params(Session), Default).


-spec set_param(cowboy_req:req(), session_type(), atom(), any()) -> cowboy_req:req().
set_param(Req0, Type, Name, Value) when is_atom(Name)  ->
	case session(Req0, Type) of
		undefined -> undefined;
		Session0  -> 
			Session = set_param(Name, Value, Session0),
			set_session_to_request(Req0, Session)
	end.


-spec set_param(atom(), any(), session()) -> session().
set_param(Name, Value, #{params := Params0} = Session0) when is_atom(Name)  ->
	Session = Session0#{params => maps:put(Name, Value, Params0)},
	save_session(Session),
	Session.


-spec del_param(cowboy_req:req(), session_type(), atom()) -> cowboy_req:req().
del_param(Req0, Type, Name) when is_atom(Name)  ->
	case session(Req0, Type) of
		undefined -> undefined;
		Session0  -> 
			Session = del_param(Name, Session0),
			set_session_to_request(Req0, Session)
	end.


-spec del_param(atom(), session()) -> session().
del_param(Name, #{params := Params} = Session) when is_atom(Name)  ->
	Params = maps:remove(Name, Params),
	Session2 = Session#{ params => Params},
	save_session(Session2),
	Session2.


-spec session_opts(session_type()) -> session_opts().
session_opts(public) ->
	#{
		stored        => cookie,
		expires_after => ?ANONYMOUS_SESSION_LIFETIME,
		type          => public,
		store_opts    => #{ 
			cookie => <<"_sid">>,
			header => <<"x-sid">>,
			args   => <<"_sid">> 
		},
		cookie_opts   => #{}
	};
session_opts(backoffice) ->
	#{
		stored        => cookie,
		expires_after => ?ANONYMOUS_SESSION_LIFETIME,
		type          => backoffice,
		store_opts    => #{ 
			cookie => <<"_b_sid">>,
			header => <<"x-b-sid">>,
			args   => <<"_b_sid">> 
		},
		cookie_opts   => #{}
	}.


-spec session(cowboy_req:req()) -> session() | undefined.
session(Req0) ->
	session(Req0, public).


-spec session(cowboy_req:req(), session_type()) -> session() | undefined.
session(#{ ?PUBLIC_SESSION_REQUEST_KEY := Session }, public) ->
	Session;
session(#{ ?BACKOFFICE_SESSION_REQUEST_KEY := Session }, backoffice) ->
	Session;
session(_, _) ->
	undefined.


-spec find_session(Req, Opts) -> Req | undefined
	when Req::cowboy_req:req(), Opts::session_opts().
find_session(Req0, SessionOpts) ->
	find_session(Req0, SessionOpts, session_store_types()).


-spec find_session(Req, Opts, [FetchSessionKeyFun]) -> Req | undefined
	when Req::cowboy_req:req(), Opts::session_opts(), FetchSessionKeyFun::fetch_session_key_fun().
find_session(_, _, []) ->
	undefined;
find_session(Req0, SessionOpts, [Fun|Rest]) ->
	case Fun(Req0, SessionOpts) of
		undefined -> find_session(Req0, SessionOpts, Rest);
		Session   -> set_request_store(Req0, Session, SessionOpts)
	end.


-spec new_session(cowboy_req:req()) -> cowboy_req:req().
new_session(Req0) ->
	new_session(Req0, session_opts(public)).


-spec new_session(Req, Opts) -> Req
	when Req::cowboy_req:req(), Opts::session_opts().
new_session(Req0, #{ stored := Stored, expires_after := ExpiresAfter, type := Type} = SessionOpts) ->
	Session0 = new_session_data(Stored, new_session_key(), ExpiresAfter, Type),
	case save_session(Session0) of
		{ok, Session}  -> set_request_store(Req0, Session, SessionOpts);
		_Any -> Req0
	end.


-spec drop_session(cowboy_req:req(), session_type()) -> cowboy_req:req().
drop_session(Req0, Type) ->
	case session(Req0, Type) of
		undefined -> Req0;
		Session0 -> 
			drop_session(Session0),
			new_session(Req0, session_opts(Type))
	end.


-spec drop_session(session()) -> ok | {error, any()}.
drop_session(#{key := SessionKey, type := Type}) ->
	case db_redis:q(session, ["DEL", cache_key(SessionKey, Type)]) of
		{ok, _} -> ok;
		Any ->
			?ERROR("Can't drop session ~p. ~p", [cache_key(SessionKey, Type), Any]),
			Any
	end.


-spec session_store_types() -> [FetchSessionKeyFun]
	when FetchSessionKeyFun::fetch_session_key_fun().
session_store_types() ->
	[ fun get_cookie_session/2,
		fun get_header_session/2,
		fun get_arguments_session/2
		].


-spec get_cookie_session(Req, Opts) -> Session | undefined
	when Req::cowboy_req:req(), Opts::session_opts(), Session::session().
get_cookie_session(Req0, #{store_opts := #{ cookie := Key}} = SessionOpts) ->
	Cookies = cowboy_req:parse_cookies(Req0),
	case lists:keyfind(Key, 1, Cookies) of
		{Key, SessionKey} -> get_session(cookie, SessionKey, SessionOpts);
		_ 				        -> undefined
	end.


-spec get_header_session(Req, Opts) -> Session | undefined
	when Req::cowboy_req:req(), Opts::session_opts(), Session::session().
get_header_session(Req0, #{store_opts := #{ header := Key}} = SessionOpts) ->
	case cowboy_req:header(Key, Req0) of
		undefined  -> undefined;
		SessionKey -> get_session(header, SessionKey, SessionOpts)
	end.


-spec get_arguments_session(Req, Opts) -> Session | undefined
	when Req::cowboy_req:req(), Opts::session_opts(), Session::session().
get_arguments_session(Req0, #{store_opts := #{ args := Key}} = SessionOpts) ->
	Qs = cowboy_req:parse_qs(Req0),
	case lists:keyfind(Key, 1, Qs) of
		{Key, SessionKey} -> get_session(args, SessionKey, SessionOpts);
		_ 				   -> undefined
	end.


-spec get_session(atom() | undefined, binary(), session_opts()) -> session() | undefined.
get_session(Stored, SessionKey, #{type := Type}) ->
	CacheKey = cache_key(SessionKey, Type),
	case db_redis:q(session, ["GET", CacheKey]) of
		{ok, undefined} ->
			undefined;
    {ok, Value} -> 
    	try ?FROM_JSON(Value) of
    		Cache ->    	
    			case validate_cache(Cache) of
    				{ok, {UserId, Params, LocationId}} ->
		    			ExpiresAfter = 
		    				case UserId of
		    					undefined -> ?ANONYMOUS_SESSION_LIFETIME;
		    					_         -> ?AUTHENTICATED_SESSION_LIFETIME
		    				end,
		    			set_cache_expire(CacheKey, ExpiresAfter),
		    			new_session_data(Stored, SessionKey, ExpiresAfter, Type, UserId, Params, LocationId);
		    		_ -> 
		    			undefined
		    	end
    	catch
          Type:Reason:Stacktrace -> 
            ?ERROR("Can't decode session ~p ~p:~p\n~p", [Value, Type, Reason, Stacktrace]),
            undefined
      end;
    Any -> 
    	?ERROR("Can't get session from redis ~p.", [Any]),
    	undefined
  end.


-spec new_session_key() -> binary().
new_session_key() ->
	base64:encode(crypto:strong_rand_bytes(?SESSION_KEY_SIZE)).


-spec new_session_data(Stored, SessionKey, ExpiresAfter, Type) -> session()
	when 
		Stored::session_stored(), 
		SessionKey::binary(), 
		ExpiresAfter::integer(), 
		Type::session_type().
new_session_data(Stored, SessionKey, ExpiresAfter, Type) ->
	new_session_data(
		Stored, SessionKey, ExpiresAfter, Type, undefined, #{}, undefined).


-spec new_session_data(Stored, SessionKey, ExpiresAfter, Type, UserId, Params, LocationId) -> session() 
	when 
		Stored::session_stored(), 
		SessionKey::binary(), 
		ExpiresAfter::integer(), 
		Type::session_type(),
		UserId::integer() | undefined,
		Params::session_params(),
		LocationId::session_location().
new_session_data(
		Stored, SessionKey, ExpiresAfter, Type, UserId, Params, LocationId) ->
	#{
		stored  			=> Stored,
		key     			=> SessionKey,
		expires_after => ExpiresAfter,
		type          => Type,

		user_id       => UserId,
		params  			=> Params,
		location_id   => LocationId
	}.


-spec save_session(session()) -> {ok, session()} | {error, any()}.
save_session(Session) ->
	#{key := SessionKey,
		type := Type,
		expires_after := ExpiresAfter, 
		user_id := UserId, 
		params := Params,
		location_id := LocationId} = Session,
	Data = #{
		user_id => UserId,
		params => Params,
		location_id => LocationId
	},
	case db_redis:q(session, ["SET", cache_key(SessionKey, Type), ?TO_JSON(Data), "EX", integer_to_binary(ExpiresAfter)]) of
    {ok, _} -> {ok, Session};
    Any -> Any
  end.


-spec set_request_store(cowboy_req:req(), session(), session_opts()) -> cowboy_req:req().
set_request_store(Req0, Session, SessionOpts) ->	
	#{stored        := Stored, 
		expires_after := ExpiresAfter, 
		key           := SessionKey} = Session,
	Req = 
		case Stored of
			cookie ->	
				#{cookie_opts := CookieOpts0, store_opts := #{cookie := Key}} = SessionOpts,	
				CookieOpts = maps:merge(#{
					max_age => ExpiresAfter,
					path => "/"
				}, CookieOpts0),
				cowboy_req:set_resp_cookie(Key, SessionKey, Req0, CookieOpts);

			header ->
				#{store_opts := #{header := Key}} = SessionOpts,	
				cowboy_req:set_resp_headers(
					maps:from_list([
						{Key, SessionKey},
						{<<Key/binary, "-lifetime">>, integer_to_binary(ExpiresAfter)}]),
					Req0);

			_ ->
				Req0
		end,
	set_session_to_request(Req, Session).


-spec set_session_to_request(cowboy_req:req(), session()) -> cowboy_req:req().
set_session_to_request(Req0, #{type := public} = Session) ->
	Req0#{?PUBLIC_SESSION_REQUEST_KEY => Session};
set_session_to_request(Req0, #{type := backoffice} = Session) ->
	Req0#{?BACKOFFICE_SESSION_REQUEST_KEY => Session}.


-spec cache_key(binary(), session_type()) -> binary().
cache_key(SessionKey, public) ->
	<<"session:public:", SessionKey/binary>>;
cache_key(SessionKey, backoffice) ->
	<<"session:backoffice:", SessionKey/binary>>.


-spec keys_to_atoms(any()) -> session_params().
keys_to_atoms(Params) when is_map(Params) ->
	maps:fold(
		fun
			(K, V, Acc) when is_atom(K) -> 
				maps:put(K, V, Acc);
			(K, V, Acc) when is_binary(K), byte_size(K) < 200 -> 
				maps:put(binary_to_atom(K, utf8), V, Acc);
			(K, V, Acc) when is_list(K), length(K) < 200 -> 
				maps:put(list_to_atom(K), V, Acc);
			(_, _, Acc) -> 
				Acc
		end, 
		#{}, Params);
keys_to_atoms(_) ->
	#{}.


-spec set_cache_expire(binary(), pos_integer()) -> any().
set_cache_expire(CacheKey, LifeTime) ->
	db_redis:q(session, ["EXPIRE", CacheKey, integer_to_binary(LifeTime)]).


-spec validate_cache(any()) -> {ok, {pos_integer() | undefined, session_params(), session_location()}} | {error, wrong_session}.
validate_cache(Cache) when is_map(Cache) ->
	{ok, { validate_cache_user_id(Cache), 
		validate_cache_params(Cache), 
		validate_cache_location_id(Cache)}};
validate_cache(_) ->
	{error, wrong_session}.


-spec validate_cache_user_id(map()) -> pos_integer() | undefined.
validate_cache_user_id(#{<<"user_id">> := UserId}) 
	when is_integer(UserId), UserId > 0 ->
	UserId;
validate_cache_user_id(_) ->
	undefined.


-spec validate_cache_params(map()) -> session_params() | undefined.
validate_cache_params(#{<<"params">> := Params}) ->
	keys_to_atoms(Params);
validate_cache_params(_) ->
	undefined.


-spec validate_cache_location_id(map()) -> session_location().
validate_cache_location_id(#{<<"location_id">> := <<"all">>}) ->
	all;
validate_cache_location_id(#{<<"location_id">> := LocationId}) 
	when LocationId == all; LocationId == undefined ->
	LocationId;
validate_cache_location_id(#{<<"location_id">> := LocationId}) 
	when is_integer(LocationId), LocationId > 0 ->
	LocationId;
validate_cache_location_id(_) ->
	undefined.
