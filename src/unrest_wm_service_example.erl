%%%=============================================================================
%%% @doc A dummy webmachine service implementation. Works with webmachine flow
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%=============================================================================
-module(unrest_wm_service_example).

%%_* Exports ===================================================================

-export([ init/1
        , ping/2
        , service_available/2
        , known_methods/2
        ]).

%%_* Types =====================================================================

-type context() :: unrest_context:context().
-type flow_result() :: unrest_flow:flow_result().
-type req() :: cowboy_req:req().

%%_* API =======================================================================

%%
%% @doc The entry point to your resource
%%
-spec init(context()) -> flow_result().
init(Ctx) ->
  {ok, Ctx}.

%%
%% @doc The `ping` function is optional, you don't need to implement it
%%
-spec ping(req(), context()) -> pong | pang.
ping(_, _) ->
  pong.

-spec service_available(req(), context()) -> pong | pang.
service_available(Req, Ctx) ->
  {true, Req, Ctx}.

-spec known_methods(req(), context()) -> pong | pang.
known_methods(Req, Ctx) ->
  {[<<"GET">>], Req, Ctx}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
