-module(wa_app).
-behaviour(application).

-export([
  start/2,
  stop/1,
  priv_dir/0,
  host/0
]).

-include("wa.hrl").


start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile(
    routes()
  ),
  case get_port() of
    {ok, Port} ->
      ?INFO("Starting service at port ~p", [Port]),
      cowboy:start_clear(wa_http_listener,[ 
          {port, Port}
        ],
        #{
          env => #{
            dispatch => Dispatch
          },
          middlewares => [              
            session_middleware, 
            authenticate_middleware,
            cowboy_router, 
            cowboy_handler]
        }
      ),
      Ret = wa_sup:start_link(),
      set_sync(),
      Ret;
    {error, PortB} ->
      ?ERROR("Incorrect port ~p", [PortB]),
      exit({error, {wrong_port, PortB}})
  end.


stop(_State) ->
  ok.


-spec get_port() -> {ok, integer()} | {error, term()}.
get_port() ->
  case ?CONFIG(listen_port, 8080) of
    Port when is_integer(Port), Port > 0 ->
      {ok, Port};

    Any ->
      {error, Any}
  end.


-spec routes() -> cowboy_router:routes().
routes() ->
  Priv = priv_dir(),
  Routes = 
    [       
      {"/api/v1/auth/[...]" , handler_api_auth , #{}},
      {"/api/v1/oauth/[...]", handler_api_oauth, #{}},
      {"/api/v1/images/[...]", handler_api_images, #{}}      
    ] ++ static_routes(Priv) ++ [
      {"/[...]" , cowboy_static , {file, filename:join([Priv, "static", "index.html"])}}
    ],

  [
    {'_', Routes}
  ].


static_routes(Priv) ->
  lists:map(fun(I) ->
    {"/static/" ++ I ++ "/[...]", cowboy_static, {dir,  filename:join([Priv, "static", I])}}
  end, ["css", "js", "i", "audio", "fonts"]).


set_sync() ->
  case ?CONFIG(env, local) of  
    local ->   
      RootDir = root_dir(),
      application:set_env(sync, src_dirs, 
        {replace,
          [ { filename:join(RootDir, "src"),
            [ {i, filename:join(RootDir, "include")}, 
              {type, erl},
              {outdir, filename:join(RootDir, "ebin")}, 
              debug_info]
            }
          ]   
      }),      
      sync:go(),
      ok;  

    _ ->
      ok
  end.


-spec host() -> binary().
host() ->
  ?CONFIG(host, <<>>).


-spec priv_dir() -> binary().
priv_dir() ->
  filename:join(root_dir(), "priv").


-spec root_dir() -> binary().
root_dir() ->
  Ebin = filename:dirname(code:which(?MODULE)),
  filename:dirname(Ebin).
