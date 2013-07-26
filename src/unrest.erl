-module(unrest).

-export([ start/0
        , start/1
        ]).

%% API ------------------------------------------------------------------------

start() ->
  File = filename:join([code:priv_dir(unrest), "webmachine.yml"]),
  start(File).

start(File) ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(lager),
  ok = application:start(yamerl),
  ok = application:start(unrest),
  start_cowboy(File).

start_cowboy(File) ->
  %% Dispatch = [ {'_'
  %%              , [ {"/", index_handler, []}
  %%                , {"/:test", index_handler, []}
  %%                , {"/internal/[...]", unrest_internal, []}
  %%                  %% Static handler
  %%                , { ["/static/[...]"]
  %%                    , cowboy_static
  %%                    , [ {directory, {priv_dir, unrest, [<<"static">>]}}
  %%                        , {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
  %%                      ]
  %%                  }
  %%                ]
  %%              }
  %%            ],
  Dispatch = unrest_config:get_dispatch(File),
  CompiledDispatch = cowboy_router:compile(Dispatch),

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

get_env(Key, Env) ->
    case lists:keyfind(Key, 1, Env) of
        {Key, Val} -> {ok, Val};
        false      -> application:get_env(unrest, Key)
    end.
