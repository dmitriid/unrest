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
        , uri_too_long/2
        , allowed_methods/2
        ]).

%%_* Types =====================================================================

-type context() :: term().
-type req() :: cowboy_req:req().

%%_* API =======================================================================

%%
%% @doc The entry point to your resource
%%
-spec init(proplists:proplist()) -> {ok, context()}.
init(_Params) ->
  {ok, []}.

%%
%% @doc The `ping` function is optional, you don't need to implement it
%%
-spec ping(req(), context()) -> pong | pang.
ping(Req, Ctx) ->
  {pong, Req, Ctx}.

-spec service_available(req(), context()) -> pong | pang.
service_available(Req, Ctx) ->
  {true, Req, Ctx}.

-spec known_methods(req(), context()) -> pong | pang.
known_methods(Req, Ctx) ->
  {[<<"GET">>], Req, Ctx}.

-spec uri_too_long(req(), context()) -> pong | pang.
uri_too_long(Req, Ctx) ->
  {false, Req, Ctx}.

-spec allowed_methods(req(), context()) -> pong | pang.
allowed_methods(Req, Ctx) ->
  {[<<"GET">>], Req, Ctx}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
