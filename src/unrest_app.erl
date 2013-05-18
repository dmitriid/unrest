-module(unrest_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

%% API ------------------------------------------------------------------------

start(_Type, _Args) ->
    Dispatch = [
        {'_', [
            {"/", index_handler, []},
            {"/internal/[...]", unrest_internal, []},
            %% Static handler
            {["/static/[...]"], cowboy_static, [
                        {directory, {priv_dir, unrest, [<<"static">>]}},
                        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}

        ]}
    ],
    giallo:start(Dispatch),
    unrest_sup:start_link().

stop(_State) ->
    ok.
