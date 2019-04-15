-module(db_pg).
-behaviour(gen_server).

-export([
  q/2,
  q/3,
  q/4,
  tr/2,

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

-include_lib("epgsql/include/epgsql.hrl").

-type pool_id() :: atom().
-type query_id() :: atom().
-type reply() :: {ok, list(map())} | epgsql_cmd_prepared_query:response().

%
% external
%

-spec q(pool_id(), epgsql:sql_query()) -> reply().
q(Pool, Query) ->
  q(Pool, undefined, Query, []).

-spec q(pool_id(), query_id() | epgsql:sql_query(), epgsql:sql_query() | [epgsql:bind_param()]) -> reply().
q(Pool, QueryId, Query) when is_atom(QueryId) -> 
  q(Pool, QueryId, Query, []);
q(Pool, Query, Params) -> 
  q(Pool, undefined, Query, Params).

-spec q(pool_id() | pid(), query_id(), epgsql:sql_query(), [epgsql:bind_param()]) -> reply().
q(Pool, QueryId, Query, Params) when is_atom(Pool) -> 
  Worker = pooler:take_member(Pool, ?PG_WAIT_TIMEOUT),
  R = q(Worker, QueryId, Query, Params),
  pooler:return_member(Pool, Worker, ok),
  R;
q(Worker, QueryId, Query, Params) -> 
  gen_server:call(Worker, {q, QueryId, Query, Params}, ?PG_TIMEOUT).

-spec tr(pool_id(), fun((epgsql:connection()) -> any())) -> any() | {rollback, any()}.
tr(Pool, Function) -> 
  Worker = pooler:take_member(Pool, ?PG_WAIT_TIMEOUT),
  R = gen_server:call(Worker, {tr, Function}, ?PG_TIMEOUT),
  pooler:return_member(Pool, Worker, ok),
  R.
  

%
% gen_server
%

start_link(#{ variants := Variants, method := random }) ->
  All = lists:foldl(fun({Weight, Params}, A) -> 
      A ++ lists:duplicate(Weight, Params) 
    end, [], Variants),
  start_link(lists:nth(rand:uniform(length(All)), All));
start_link(Params) -> 
  gen_server:start_link(?MODULE, Params, []).

init(Params) ->
  ?AFTER(0, reconnect),
  {ok, #{
      c => undefined,
      queries => #{},
      params => Params
    }}.

handle_call(_, _From, #{ c := C } = State) when C =:= undefined -> 
  {reply, {error, noconnection}, State};
handle_call({q, QueryId, Query, Params}, _From, State) ->
  {Reply, NewState} = make_query(Query, QueryId, Params, State),
  {reply, Reply, NewState};  
handle_call({tr, Function}, _From, #{ c := C } = State) ->
  {reply, epgsql:with_transaction(C, Function, []), State};  
handle_call(_Msg, _From, State) -> 
  {reply, ok, State}.

handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(reconnect, #{ c := undefined } = State) -> 
  {noreply, try_connect(State)};
handle_info({'EXIT', C, Reason}, #{ c := C } = State) when C =/= undefined ->
  {noreply, reconnect(Reason, State)};
handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, #{ c := C }) when C =/= undefined -> 
  epgsql:close(C),
  ok;
terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% misc
%

make_query(Query, undefined, Params, #{ c := C } = State) ->
  {to_result(epgsql:equery(C, Query, Params)), State};
make_query(Query, QueryId, Params, #{ c := C, queries := PreparedQ } = State) ->
  case maps:get(QueryId, PreparedQ, undefined) of
    undefined ->
      {ok, Statement} = epgsql:parse(C, query_id(QueryId), Query, []),
      {to_result(epgsql:prepared_query(C, Statement#statement.name, Params)), State#{ queries := maps:put(QueryId, Statement, PreparedQ) }};
    Statement ->
      {to_result(epgsql:prepared_query(C, Statement#statement.name, Params)), State}
  end.

query_id(Id) when is_list(Id) ->
  binary_to_list(Id);
query_id(Id) when is_atom(Id) ->
  atom_to_binary(Id, latin1);
query_id(Id) ->
  Id.

to_result({ok, Count, Columns, Rows}) ->
  ColumnsA = [binary_to_atom(C#column.name, latin1) || C <- Columns],
  {ok, Count, [maps:from_list(lists:zip(ColumnsA, tuple_to_list(R))) || R <- Rows]};
to_result({ok, Columns, Rows}) ->
  ColumnsA = [binary_to_atom(C#column.name, latin1) || C <- Columns],
  {ok, [maps:from_list(lists:zip(ColumnsA, tuple_to_list(R))) || R <- Rows]};
to_result(Any) ->
  Any.

try_connect(#{ params := Params } = State) ->
  case epgsql:connect(Params) of
    {ok, C} -> 
      ?INFO("PgSQL connection up ~p", [C]), 
      State#{ c := C, queries := #{} };
    Reason -> 
      reconnect(Reason, State)
  end.

reconnect(Reason, State) ->
  ?ERROR("Error in connection to PgSQL server - ~p", [Reason]),
  ?AFTER(?PG_RECONNECT_TIMEOUT, reconnect),
  State.
