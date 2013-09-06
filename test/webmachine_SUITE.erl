%%%=============================================================================
%%% @doc Tests for the webmachine flow
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%=============================================================================
-module(webmachine_SUITE).

%%_* Exports ===================================================================
%%_* Common test callbacks -----------------------------------------------------
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%%_* Tests ---------------------------------------------------------------------
-export([ test/1 ]).

%%_* Includes ==================================================================
-include_lib("eunit/include/eunit.hrl").

%%_* Common test callbacks =====================================================

all() ->
  Exports = ?MODULE:module_info(exports),
  Tests = [Test || {Test, 1} <- Exports
                 , Test =/= all
                 , Test =/= init_per_suite
                 , Test =/= end_per_suite
                 , Test =/= init_per_testcase
                 , Test =/= end_per_testcase
                 , Test =/= module_info
          ],
  Tests.

init_per_suite(Config) ->
  ssl:start(),
  lhttpc:start(),
  application:stop(unrest),
  Path = filename:join([code:priv_dir(unrest), "webmachine.yml"]),

  Blueprint = filename:join([ proplists:get_value(priv_dir, Config)
                            , "webmachine.txt"
                            ]),
  ct:pal("~p~n", [Blueprint]),
  unrest:start(Path),
  [{blueprint, Blueprint} | Config].

end_per_suite(_Config) ->
  application:stop(unrest).

init_per_testcase(Testcase, Config0) ->
  TestConfig = ?MODULE:Testcase({init, Config0}),
  Config1 = lists:foldl( fun({Key, _} = New, Config) ->
                           lists:keystore(Key, 1, Config, New)
                         end
                       , Config0 ++ defaults()
                       , TestConfig
                       ),
  Blueprint = proplists:get_value(blueprint, Config1),
  {ok, File} = file:open(Blueprint, [write]),
  Config = [{file, File} | Config1],
  update_blueprint(Config).

end_per_testcase(_Testcase, _Config) ->
  ok.

%%_* Tests =====================================================================

test({init, _Config}) ->
  [ {request_headers, [{"Content-Length", "0"}]}
  , {response_code, "204"}
  ];
test(Config) ->
  run(Config).


%%_* Utility functions =========================================================

run(Config) ->
  Blueprint = proplists:get_value(blueprint, Config),
  Params = [{hostname, "localhost"}, {port, 8080}],
  Result = katt:run(Blueprint, Params, [{recall, recall(Config)}]),
  ?assertMatch({pass, _, _, _, _}, Result).


recall(Config) ->
  Hdrs = proplists:get_value(headers, Config, []),
  fun(headers, _, _, _) ->
    Hdrs;
     (What, Vals, Params, Callbacks) ->
    katt_callback:recall(What, Vals, Params, Callbacks)
  end.

update_blueprint(Config) ->
  File = proplists:get_value(file, Config),
  Data0 = [ "--- Test ---"
          , $\n
          , $\n
          ,  proplists:get_value(method, Config), " /", $\n
          , [  ["> ", H, ": ", V, $\n]
            || {H, V} <- proplists:get_value(request_headers, Config)
            ]
          ,"< ", proplists:get_value(response_code, Config), $\n
          , [  ["< ", H, ": ", V, $\n]
            || {H, V} <- proplists:get_value(response_headers, Config)
            ]
          ],
  Data = list_to_binary(lists:flatten(Data0)),
  ct:pal("~p~n", [Data]),
  file:write(File, Data),
  file:close(File),
  Config.


defaults() ->
  [ {method, "GET"}
  , {response_code, "200"}
  , {request_headers, []}
  , {response_headers, []}
  ].
