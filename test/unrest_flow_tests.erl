%%%=============================================================================
%%% @doc Unit tests for unrest_flow
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%=============================================================================
-module(unrest_flow_tests).
-compile(export_all).

%%_* Includes ==================================================================
-include_lib("eunit/include/eunit.hrl").

%%_* Test definitions ==========================================================
run_single_success_test_() ->
  [ { "Run flow with single module"
    , fun test_single_module_run_1/0
    }
  , { "Run flow with single function"
    , fun test_single_function_run_1/0
    }
  ].

run_single_error_test_() ->
  [ { "Run flow with single module, produce error"
    , fun test_err_single_module_run_1/0
    }
  , { "Run flow with single function, produce error"
    , fun test_err_single_function_run_1/0
    }
  ].

run_single_stop_flow_test_() ->
  [ { "Run flow with single module, stop flow"
    , fun test_stop_single_module_run_1/0
    }
  , { "Run flow with single function, stop flow"
    , fun test_stop_single_function_run_1/0
    }
  ].

run_single_stop_flow_error_test_() ->
  [ { "Run flow with single module, stop flow, produce error"
    , fun test_stop_err_single_module_run_1/0
    }
  , { "Run flow with single function, stop flow, produce error"
    , fun test_stop_err_single_function_run_1/0
    }
  ].

run_flow_test_() ->
  [ { "Run flow with success, error, success"
    , fun test_success_error_success_run_1/0
    }
  , { "Run flow with success, stop_flow, success"
    , fun test_success_stop_success_run_1/0
    }
  , { "Run flow with success, stop_flow error, success"
    , fun test_success_stop_error_success_run_1/0
    }
  ].

%%_* Test implementations ======================================================

test_single_module_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ {?MODULE, update_context_success}
                           ]
                           , Ctx0),
  assert_flow(Result, false, success).

test_single_function_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ fun update_context_success/1
                           ]
                           , Ctx0),
  assert_flow(Result, false, success).

test_err_single_module_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ {?MODULE, update_context_error}
                           ]
                           , Ctx0),
  assert_flow(Result, true, update_error).

test_err_single_function_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ fun update_context_error/1
                           ]
                           , Ctx0),
  assert_flow(Result, true, update_error).

test_stop_single_module_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ {?MODULE, update_context_stop_flow}
                           ]
                           , Ctx0),
  assert_flow(Result, false, stop_flow).

test_stop_single_function_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ fun update_context_stop_flow/1
                           ]
                           , Ctx0),
  assert_flow(Result, false, stop_flow).

test_stop_err_single_module_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ {?MODULE, update_context_stop_flow_error}
                           ]
                           , Ctx0),
  assert_flow(Result, true, stop_flow_error).

test_stop_err_single_function_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ fun update_context_stop_flow_error/1
                           ]
                           , Ctx0),
  assert_flow(Result, true, stop_flow_error).

test_success_error_success_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ fun update_context_success/1
                           , fun update_context_error/1
                           , fun update_context_success/1
                           ]
                           , Ctx0),
  assert_flow(Result, true, success).

test_success_stop_success_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ fun update_context_success/1
                           , fun update_context_stop_flow/1
                           , fun update_context_success/1
                           ]
                           , Ctx0),
  assert_flow(Result, false, stop_flow).

test_success_stop_error_success_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ fun update_context_success/1
                           , fun update_context_stop_flow_error/1
                           , fun update_context_success/1
                           ]
                           , Ctx0),
  assert_flow(Result, true, stop_flow_error).

%%_* Asserts ===================================================================

assert_flow(Result, IsErrorState, UpdatedValue)->
  ?assertMatch({ok, _}, Result),
  {ok, Ctx} = Result,
  ?assertEqual(IsErrorState, unrest_context:is_error_state(Ctx)),
  ?assertEqual({ok, UpdatedValue}, unrest_context:get(test, Ctx)),
  {ok, Callstack} = unrest_context:callstack(Ctx),
  ?assert(length(Callstack) > 0).

%%_* Exported funtions used by the module ======================================

update_context_success(Ctx) ->
  unrest_context:set(test, success, Ctx).

update_context_error(Ctx0) ->
  {ok, Ctx} = unrest_context:set(test, update_error, Ctx0),
  {error, Ctx, some_update_error}.

update_context_stop_flow(Ctx0) ->
  {ok, Ctx} = unrest_context:set(test, stop_flow, Ctx0),
  {stop_flow, Ctx}.

update_context_stop_flow_error(Ctx0) ->
  {ok, Ctx} = unrest_context:set(test, stop_flow_error, Ctx0),
  {stop_flow, Ctx, some_stop_flow_error}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
