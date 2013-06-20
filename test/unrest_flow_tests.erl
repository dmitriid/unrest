%%%=============================================================================
%%% @doc Unit tests for unrest_flow
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2013 Klarna AB, API team
%%%=============================================================================
-module(unrest_flow_tests).

%%_* Includes ==================================================================
-include_lib("eunit/include/eunit.hrl").

%%_* Test definitions ==========================================================
run_single_success_test_() ->
  [ { "Run flow with single module"
    , fun test_single_module_run_1/0
    }
  ].

%%_* Test implementations ======================================================

test_single_module_run_1() ->
  Ctx0 = unrest_context:new(),
  Result = unrest_flow:run([ {?MODULE, unrest_context_success}
                           ]
                           , Ctx0),
  ?assertMatch({ok, _}, Result),
  {ok, Ctx} = Result,
  ?assertEqual(false, unrest_context:is_error_state(Ctx)),
  ?assertEqual({ok, success}, unrest_context:get(test, Ctx)).

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
