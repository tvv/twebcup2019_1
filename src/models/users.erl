-module(users).
-compile({parse_transform, model_pt}).
-export([
  new_account/4,
  create/1,
  get/1,
  update/2,
  delete/1,
  get_by_cridentials/2,
  get_by_token/1,
  get_by_email/1,
  reset_password/2
]).

-compile({no_auto_import,[get/1]}).

-include("models.hrl").
-include("wa.hrl").

-define(TABLE, "users").
-define(RETURNING, "id, name, email, token, meta, enabled, removed, ctime, atime, dtime").

-type record() :: #{
  name => binary(), 
  email => binary(), 
  token => binary(),
  meta  => map(),
  enabled => boolean()
}.

-model([
  {id     , integer, [optional, {min, 1}], #{ default => null, pk => true }},
  {name   , binary , [trim, {min, 1}]    , #{ default => <<>>}},
  {email  , binary,  [trim, {min, 1}]    , #{ default => <<>>}},
  {token  , binary,  [trim, {min, 1}]    , #{ default => <<>>}},
  {enabled, boolean, [optional]          , #{ default => true}}
]).


-spec new_account(Token, Name, UserInfo, Email) -> {ok, Result} | {error, term()}
  when
    Token    :: binary(),
    Name     :: binary(),
    UserInfo :: map(),
    Email    :: binary(),
    Result   :: map().
new_account(Token, Name, UserInfo, Email) ->
  UserRecord = (new())#{
    token => Token,
    name  => Name,
    meta  => UserInfo,
    email => Email
  },
  create(UserRecord).


-spec create(Record) -> {ok, Result} | {error, term()}
  when
    Record :: record(),
    Result :: map().
create(Record) ->
  db_table:new(?TABLE, Record, ?RETURNING).


-spec get(UserId) -> {ok, Result} | {error, term()}
  when 
    UserId :: integer(),
    Result :: map().
get(UserId) ->
  db_table:get(?TABLE, UserId, ?RETURNING).


-spec update(UserId, Record) -> {ok, Result} | {error, term()}
  when 
    UserId :: integer(),
    Record :: record(),
    Result :: map().
update(UserId, Record) ->
  db_table:update(?TABLE, UserId, Record, ?RETURNING).


-spec delete(UserId) -> {ok, Result} | {error, term()}
  when 
    UserId :: integer(),
    Result :: map().
delete(UserId) ->
  db_table:delete(?TABLE, UserId, ?RETURNING).


-spec get_by_cridentials(Email, Password) -> {ok, Result} | {error, term()}
  when 
    Email    :: binary(),
    Password :: binary(),
    Result   :: map().
get_by_cridentials(Email, Password) ->
  Query = "SELECT " ++ ?RETURNING ++ " FROM " ++ ?TABLE ++ " WHERE email=$1 AND password=$2 AND enabled=TRUE AND removed=FALSE",
  Params = [Email, Password],
  case db_table:find(users_get_by_cridentials, Query, Params) of
    {ok, []}     -> {error, not_found};
    {ok, [User]} -> {ok, User};
    Any          ->  Any
  end.


-spec get_by_token(Token) -> {ok, Result} | {error, term()}
  when 
    Token  :: binary(),
    Result :: map().
get_by_token(Token) ->
  Query = "SELECT " ++ ?RETURNING ++ " FROM " ++ ?TABLE ++ " WHERE token=$1 AND enabled=TRUE AND removed=FALSE",
  Params = [Token],
  case db_table:find(users_get_by_token, Query, Params) of
    {ok, []}     -> {error, not_found};
    {ok, [User]} -> {ok, User};
    Any          ->  Any
  end.


-spec get_by_email(Email) -> {ok, Result} | {error, term()}
  when 
    Email  :: binary(),
    Result :: map().
get_by_email(Email) ->
  Query = "SELECT " ++ ?RETURNING ++ " FROM " ++ ?TABLE ++ " WHERE email=$1 AND removed=FALSE",
  Params = [Email],
  case db_table:find(users_get_by_email, Query, Params) of
    {ok, []}     -> {error, not_found};
    {ok, [User]} -> {ok, User};
    Any          ->  Any
  end.


-spec reset_password(UserId, Length) -> {ok, PlainPassword} | {error, term()}
  when 
    UserId        :: integer(),
    Length        :: integer(),
    PlainPassword :: binary().
reset_password(UserId, Length) ->
  {Password, HashedPassword} = iomod:new_password(Length),
  Query = "UPDATE " ++ ?TABLE ++ " SET password=$2 WHERE id=$1 RETURNING " ++ ?RETURNING,
  Params = [UserId, HashedPassword],
  case db_table:find(users_reset_password, Query, Params) of
    {ok, _, []}    -> {error, not_found};
    {ok, 1, _}     -> {ok, Password};
    Any         ->  Any
  end.
