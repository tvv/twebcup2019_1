-module(mail).
-behaviour(gen_server).

-export([
  send/3, 
  
  start_link/1,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  
  smtp/5,
  mailgun/5
]).

-include("wa.hrl").


send(To, Subject, Message) ->
  gen_server:cast(?MODULE, {mail, To, Subject, Message}).

%
% gen_server
%

start_link(Params) -> 
  gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(Params) ->
  {ok, Params}.

handle_call(_Msg, _From, State) -> 
  {reply, ok, State}.

handle_cast({mail, To, Subject, Message}, #{ 
        from := From,
        method := Method, 
        options := Options 
      } = State) ->  
  ?MODULE:Method(From, To, Subject, Message, Options),
  {noreply, State};
handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

%
% methods
%

smtp(From, To, Subject, Body, Options) ->
  Msg = mimemail:encode({
    <<"text">>, <<"html">>,
    [ 
      {<<"Subject">>, Subject},
      {<<"From">>, From},
      {<<"To">>, To}
    ],
    [],
    Body
  }),
  gen_smtp_client:send({From, [To], Msg}, maps:to_list(Options)).  


mailgun(From, To, Subject, Body, #{ url := URL, priv_key := PrivKey }) ->  
  Base64 = base64:encode(<<"api:", PrivKey/binary>>),
  case rest:request(post, qs, URL, [200], 
      #{
        <<"Authorization">> => <<"Basic ", Base64/binary>>
      }, 
      #{
        <<"from">> => From,
        <<"to">> => To,
        <<"subject">> => Subject,
        <<"text">> => Body
      }) of
    {ok, _, _, Resp} ->
      ?INFO("Send mail to ~p with response ~p", [To, Resp]);
    Error ->
      ?ERROR("Can't send mail to ~p - ~p", [To, Error])
  end.
