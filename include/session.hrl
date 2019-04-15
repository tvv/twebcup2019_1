-type session() :: #{
	stored  					 => undefined | session_stored(),
	key     					 => undefined | binary(),
	expires_after 		 => undefined | pos_integer(),
	type               => session_type(),

	%% user id
	user_id            => undefined | pos_integer(), 
	%% session params
	params  				   => session_params()
}.

-type session_stored() :: cookie | header | args.
-type session_params() :: #{ atom() => any() }.

-type session_opts() :: #{
	stored 				=> cookie | header,
	expires_after => pos_integer(),
	type          => session_type(),

	store_opts    => #{
		cookie => binary(),
		header => binary(),
		args   => binary()
	},
	cookie_opts 	=> #{atom() => any()}
}.

-type session_type() :: public | backoffice.

-type session_location() :: undefined | all | pos_integer().

-define(ANONYMOUS_SESSION_LIFETIME, 10800). %% 3 hours
-define(AUTHENTICATED_SESSION_LIFETIME, 259200). %% 30 days

-define(SESSION_KEY_SIZE, 64).

-define(PUBLIC_SESSION_REQUEST_KEY, session).
-define(BACKOFFICE_SESSION_REQUEST_KEY, backoffice_session).