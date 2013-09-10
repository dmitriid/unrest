-module(unrest).

-export([ start/0
        , start/1
        , stop/0
        , update_config/1
        ]).

%% API ------------------------------------------------------------------------

start() ->
  File = filename:join([code:priv_dir(unrest), "config.yml"]),
  start(File).

start(Configs) ->
  ok = application_start(crypto),
  ok = application_start(ranch),
  ok = application_start(cowboy),
  ok = application_start(lager),
  ok = application_start(yamerl),
  ok = application_start(unrest),
  update_config(Configs).

update_config(Configs) ->
  stop(),
  Dispatch = unrest_config:get_dispatch(Configs),
  CompiledDispatch = cowboy_router:compile(Dispatch),
  start_cowboy(CompiledDispatch).


start_cowboy(CompiledDispatch) ->
  Env = [],
  {ok, Acceptors}  = get_env(acceptors, Env),
  {ok, Port}       = get_env(port, Env),

  cowboy:start_http( unrest_http_listener
                   , Acceptors
                   , [{port, Port}]
                   , [ {env, [{dispatch, CompiledDispatch}]}
                     , {middlewares, [ cowboy_router
                                     , unrest_middleware
                                     , cowboy_handler
                                     ]
                       }
                     ]
                   ).

stop() ->
  cowboy:stop_listener(unrest_http_listener).


get_env(Key, Env) ->
    case lists:keyfind(Key, 1, Env) of
        {Key, Val} -> {ok, Val};
        false      -> application:get_env(unrest, Key)
    end.


application_start(App) ->
  case application:start(App) of
    ok                              -> ok;
    {error, {already_started, App}} -> ok;
    Other                           -> exit(Other)
  end.
