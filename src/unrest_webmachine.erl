%%%=============================================================================
%%% @doc Implements webmachine flow using unrest
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%=============================================================================
-module(unrest_webmachine).

%%_* Exports ===================================================================

-export([ init/1
        ]).

%%_* Decision flow -------------------------------------------------------------

%% Validation & Auth
-export([ v3b13_service_available/1
        ]).

%% Content negotiation
-export([ ping/1
        , v3c3_accept/1
        , v3b12_known_method/1
        , v3b11_uri_too_long/1
        , v3b10_method_allowed/1
        , v3b9_malformed/1
        , v3b8_authorized/1
        , v3b7_forbidden/1
        , v3b6_known_content_header/1
        , v3b5_unknown_content_type/1
        , v3b4_request_too_large/1
        , v3b3_options/1
        ]).

%%_* Types =====================================================================

-type context() :: unrest_context:context().
-type flow_result() :: unrest_flow:flow_result().

%%_* API =======================================================================

-spec init(context()) -> flow_result().
init(Ctx0) ->
  {ok, ModuleName} = unrest_context:get(<<"resource_module">>, Ctx0),
  Module  = unrest_util:binary_to_atom(ModuleName),
  Exports = Module:module_info(exports),
  {ok, Ctx} = unrest_context:multiset( [ {resource_module, Module}
                                       , {resource_exports, Exports}]
                                     , Ctx0),
  Module:init(Ctx).

%%_* Validation & Auth ---------------------------------------------------------
%%
%% @doc This function is not documanted in Webmachine docs, but it's part of
%%      webmachine flow. unrest implemetation makes this callback optional
%%
-spec ping(context()) -> flow_result().
ping(Ctx) ->
  case resource_call(ping, Ctx) of
    pang            -> error_response(503, req(Ctx));
    pong            -> {ok, Ctx};
    not_implemented -> {ok, Ctx}
  end.

-spec v3b13_service_available(context()) -> flow_result().
v3b13_service_available(Ctx0) ->
  case resource_call(service_available, Ctx0) of
    {false, Req, _}  -> error_response(503, Req);
    {true, Req, Ctx} -> unrest_context:set(req, Req, Ctx);
    not_implemented  -> {ok, Ctx0} %% assume it's available since the module
                                   %% is there
  end.

-spec v3b12_known_method(context()) -> flow_result().
v3b12_known_method(Ctx0) ->
  case resource_call(known_methods, Ctx0) of
    {List, Req, Ctx} -> v3b12_known_method(List, Req, Ctx);
    not_implemented  -> v3b12_known_method(known_methods(), req(Ctx0), Ctx0)
  end.

v3b12_known_method(List, Req, Ctx) ->
  case lists:member(method(Req), List) of
    true  -> unrest_context:set(req, Req, Ctx);
    false -> error_response(501, Req)
  end.

-spec v3b11_uri_too_long(context()) -> flow_result().
v3b11_uri_too_long(Ctx) ->
  {ok, Ctx}.
-spec v3b10_method_allowed(context()) -> flow_result().
v3b10_method_allowed(Ctx) ->
  {ok, Ctx}.
-spec v3b9_malformed(context()) -> flow_result().
v3b9_malformed(Ctx) ->
  {ok, Ctx}.
-spec v3b8_authorized(context()) -> flow_result().
v3b8_authorized(Ctx) ->
  {ok, Ctx}.
-spec v3b7_forbidden(context()) -> flow_result().
v3b7_forbidden(Ctx) ->
  {ok, Ctx}.
-spec v3b6_known_content_header(context()) -> flow_result().
v3b6_known_content_header(Ctx) ->
  {ok, Ctx}.
-spec v3b5_unknown_content_type(context()) -> flow_result().
v3b5_unknown_content_type(Ctx) ->
  {ok, Ctx}.
-spec v3b4_request_too_large(context()) -> flow_result().
v3b4_request_too_large(Ctx) ->
  {ok, Ctx}.
-spec v3b3_options(context()) -> flow_result().
v3b3_options(Ctx) ->
  {ok, Ctx}.

%%_* Content negotiation--------------------------------------------------------
-spec v3c3_accept(context()) -> flow_result().
v3c3_accept(Ctx) ->
  Trace = iolist_to_binary(io_lib:format("~p", [unrest_context:callstack(Ctx)])),
  {ok, Req0} = unrest_context:get(req, Ctx),
  {ok, Req} = cowboy_req:reply(200, [], Trace, Req0),
  {respond, Req}.

%%_* Internal ==================================================================

resource_call(Call, Ctx) ->
  {ok, Module}  = unrest_context:get(resource_module, Ctx),
  {ok, Exports} = unrest_context:get(resource_exports, Ctx),
  {ok, Req}     = unrest_context:get(req, Ctx),
  case lists:keyfind(Call, 1, Exports) of
    {Call, _} -> Module:Call(Req, Ctx);
    false     -> not_implemented
  end.

error_response(Code, Req0) when is_integer(Code) ->
  {ok, Req}  = cowboy_req:reply(Code, Req0),
  {respond, Req}.

%%_* Helpers ===================================================================

req(Ctx) ->
  {ok, Req} = unrest_context:get(req, Ctx),
  Req.

method(Req) ->
  {Method, _} = cowboy_req:method(Req),
  Method.

%%_* Defaults ==================================================================

known_methods() ->
  ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE', 'CONNECT', 'OPTIONS',
   'PATCH'].

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
