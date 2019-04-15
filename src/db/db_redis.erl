-module(db_redis).
-behaviour(gen_server).

-export([
          q/2, 
          qp/2, 
          kv/2,

          start_link/1, 
          init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2, 
          terminate/2, 
          code_change/3
        ]).

-include("wa.hrl").
-include("db.hrl").

-type pool_id() :: atom().
-type redis_key() :: list() | binary().
-type redis_query() :: [redis_key()].
-type redis_pipeline() :: [redis_query()].
-type redis_reply() :: {ok, binary | [binary]} | {error, any()}.

%
% external
%
 
-spec q(pool_id(), redis_query()) -> redis_reply(). 
q(Pool, Query)     -> 
  q_step(Pool, {q, Query}, ?REDIS_MAX_TRIES).

-spec qp(pool_id(), redis_pipeline()) -> redis_reply().
qp(Pool, Pipeline) -> 
  q_step(Pool, {qp, Pipeline}, ?REDIS_MAX_TRIES).

-spec kv(pool_id(), [redis_key()]) -> map().
kv(Pool, Keys) ->
  lists:map(fun({K, {ok, V}}) -> 
      {K, V} 
    end, lists:zip(Keys, qp(Pool, [["GET", Key] || Key <- Keys]))).

q_step(_Pool, _Query, 0) ->
  {error, can_connect};
q_step(Pool, Query, K) -> 
  case pooler:take_member(Pool, ?REDIS_WAIT_TIMEOUT) of
    Worker when is_pid(Worker) -> 
      R = gen_server:call(Worker, Query),
      pooler:return_member(Pool, Worker, ok),
      R;
    Any ->
      ?ERROR("Fail with redis pool member - ~p", [Any]),
      timer:sleep(?REDIS_TRY_SLEEP),
      q_step(Pool, Query, K - 1) 
  end.

%
% gen_server
%

start_link(Params) -> 
  gen_server:start_link(?MODULE, Params, []).

init(Params) ->
  try_redis(ok, Params).

handle_call({q, _Query}, _From, undefined) -> 
  {reply, {error, noconnection}, undefined};
handle_call({q, Query}, _From, C) -> 
  {reply, eredis:q(C, Query), C};
handle_call({qp, _Pipeline}, _From, undefined) -> 
  {reply, {error, noconnection}, undefined};
handle_call({qp, Pipeline}, _From, C) -> 
  {reply, eredis:qp(C, Pipeline), C};
handle_call(_Msg, _From, State) -> 
  {reply, ok, State}.

handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info({reinit, Params}, undefined) -> 
  try_redis(noreply, Params);
handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, undefined) -> 
    ok;
terminate(_Reason, Pid) -> 
  eredis:stop(Pid),
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% local
%

try_redis(RAtom, Params) ->
  case init_redis(Params) of
    {ok, RPid} ->
      ?INFO("Redis connection up ~p", [RPid]),
      {RAtom, RPid};
    {error, undefined} ->
      ?INFO("Redis misconfigured, not running"),
      {RAtom, undefined};
    Reason ->
      ?ERROR("Error in connection to Redis server - ~p", [Reason]),
      ?AFTER(?RECONNECT_TIMEOUT, {reinit, Params}),
      {RAtom, undefined}
  end.

init_redis(undefined) -> 
  {error, undefined};
init_redis(#{ host := Host, port := Port, db := Database, password := Password }) -> 
  eredis:start_link(Host, Port, Database, Password);
init_redis(#{ host := Host, port := Port, db := Database }) -> 
  eredis:start_link(Host, Port, Database);
init_redis(#{ host := Host, port := Port }) -> 
  eredis:start_link(Host, Port);
init_redis(#{ host := Host }) -> 
  eredis:start_link(Host, 6379);
init_redis(_) -> 
  eredis:start_link().
