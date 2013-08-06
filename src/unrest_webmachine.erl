%%%=============================================================================
%%% @doc Implements webmachine flow using unrest
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%=============================================================================
-module(unrest_webmachine).
-compile([{parse_transform, lager_transform}]).

%%_* Exports ===================================================================

-export([ init/1
        ]).

%%_* Decision flow -------------------------------------------------------------

%% Validation & Auth
-export([ ping/1
        , v3b13_service_available/1
        , v3b12_known_method/1
        , v3b11_uri_too_long/1
        , v3b10_method_allowed/1
        , v3b9_content_checksum/1
        , v3b9_malformed_request/1
        , v3b8_authorized/1
        , v3b7_forbidden/1
        , v3b6_valid_content_headers/1
        , v3b5_known_content_type/1
        , v3b4_request_too_large/1
        , v3b3_options/1
        ]).

%% Content negotiation
-export([ v3c3_accept/1
        ]).

%%_* Types =====================================================================

-type context() :: unrest_context:context().
-type flow_result() :: unrest_flow:flow_result().
-type response(What) :: {What, cowboy_req:req(), term()}
                      | {halt, integer()}
                      | {error, term()}.

-export_type([response/1]).

%%_* API =======================================================================

-spec init(context()) -> flow_result().
init(Ctx0) ->
  {ok, Params0} = unrest_context:all(Ctx0),
  Params1 = lists:keydelete(flows, 1, Params0),
  Params2 = lists:keydelete(req, 1, Params1),
  Params  = lists:keydelete(<<"resource_module">>, 1, Params2),

  {ok, ModuleName} = unrest_context:get(<<"resource_module">>, Ctx0),
  Module  = unrest_util:binary_to_atom(ModuleName),
  Exports = Module:module_info(exports),
  {ok, ResourceCtx} = Module:init(Params),

  unrest_context:multiset( [ {resource_context, ResourceCtx}
                           , {resource_exports, Exports}
                           , {resource_module, Module}
                           ]
                         , Ctx0).

%%_* Validation & Auth ---------------------------------------------------------
%%
%% @doc This function is not documanted in Webmachine docs, but it's part of
%%      webmachine flow. unrest implemetation makes this callback optional
%%
-spec ping(context()) -> flow_result().
ping(Ctx) ->
  decision(resource_call(ping, Ctx), pong, 503, Ctx).

-spec v3b13_service_available(context()) -> flow_result().
v3b13_service_available(Ctx) ->
  decision(resource_call(service_available, Ctx), true, 503, Ctx).

-spec v3b12_known_method(context()) -> flow_result().
v3b12_known_method(Ctx0) ->
  case resource_call(known_methods, Ctx0) of
    {List, Req, ResCtx} ->
      {ok, Ctx1} = update_context(Req, ResCtx, Ctx0),
      {Method, Ctx} = method(Ctx1),
      decision(lists:member(Method, List), true, 501, Ctx);
    not_implemented ->
      {Method, Ctx} = method(Ctx0),
      decision( lists:member(Method, default(known_methods))
              , true
              , 501
              , Ctx
              );
    Other -> error_response(Other, Ctx0)
  end.

-spec v3b11_uri_too_long(context()) -> flow_result().
v3b11_uri_too_long(Ctx) ->
  decision(resource_call(uri_too_long, Ctx), false, 414, Ctx).

-spec v3b10_method_allowed(context()) -> flow_result().
v3b10_method_allowed(Ctx0) ->
  case resource_call(allowed_methods, Ctx0) of
    {List, Req, ResCtx} ->
      {ok, Ctx1} = update_context(Req, ResCtx, Ctx0),
      {Method, Ctx} = method(Ctx1),
      decision(lists:member(Method, List), true, 405, Ctx);
    not_implemented ->
      {Method, Ctx} = method(Ctx0),
      decision( lists:member(Method, default(allowed_methods))
              , true
              , 405
              , Ctx
              );
    Other -> error_response(Other, Ctx0)
  end.

-spec v3b9_content_checksum(context()) -> flow_result().
v3b9_content_checksum(Ctx0) ->
  case header(<<"content-md5">>, Ctx0) of
    {undefined, Ctx} -> {ok, Ctx};
    {MD5, Ctx}       -> validate_content_checksum(MD5, Ctx)
  end.

validate_content_checksum(MD5, Ctx0) ->
  case resource_call(validate_content_checksum, Ctx0) of
    {not_validated, Req, ResCtx} ->
      {ok, Ctx1} = update_context(Req, ResCtx, Ctx0),
      Checksum = base64:decode(MD5),
      {BodyHash, Ctx} = compute_body_md5(Ctx1),
      case BodyHash =:= Checksum of
        true  -> {ok, Ctx};
        false -> error_response(400, Ctx)
      end;
    {false, Req, ResCtx} ->
      {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
      error_response(400, Ctx);
    {true, Req, ResCtx} ->
      update_context(Req, ResCtx, Ctx0);
    Other ->
      error_response(Other, Ctx0)
  end.

-spec v3b9_malformed_request(context()) -> flow_result().
v3b9_malformed_request(Ctx) ->
  decision(resource_call(malformed_request, Ctx), false, 403, Ctx).

-spec v3b8_authorized(context()) -> flow_result().
v3b8_authorized(Ctx0) ->
  case resource_call(is_authorized, Ctx0) of
    {true, Req, ResCtx} ->
      update_context(Req, ResCtx, Ctx0);
    {_, _} = HaltOrError ->
      error_response(HaltOrError, Ctx0);
    {AuthHead, Req0, ResCtx} ->
      Req = cowboy_req:set_resp_header(<<"WWW-Authenticate">>, AuthHead, Req0),
      {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
      error_response(401, Ctx)
  end.

-spec v3b7_forbidden(context()) -> flow_result().
v3b7_forbidden(Ctx) ->
  decision(resource_call(forbidden, Ctx), false, 403, Ctx).

-spec v3b6_valid_content_headers(context()) -> flow_result().
v3b6_valid_content_headers(Ctx) ->
  decision(resource_call(valid_content_headers, Ctx), true, 501, Ctx).

-spec v3b5_known_content_type(context()) -> flow_result().
v3b5_known_content_type(Ctx) ->
  decision(resource_call(known_content_type, Ctx), true, 415, Ctx).

-spec v3b4_request_too_large(context()) -> flow_result().
v3b4_request_too_large(Ctx) ->
  decision(resource_call(valid_entity_length, Ctx), true, 413, Ctx).

-spec v3b3_options(context()) -> flow_result().
v3b3_options(Ctx0) ->
  case method(Ctx0) of
    {<<"OPTIONS">>, Ctx1} ->
      case resource_call(options, Ctx1) of
        {Headers, Req0, ResCtx} ->
          F = fun({H, V}, R) -> cowboy_req:set_resp_header(H, V, R) end,
          Req = lists:foldl(F, Req0, Headers),
          {ok, Ctx} = update_context(Req, ResCtx, Ctx1),
          error_response(200, Ctx);
        Other ->
          error_response(Other, Ctx0)
      end;
    {_, Ctx1} ->
      {ok, Ctx1}
  end.

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
  {ok, ResCtx}  = unrest_context:get(resource_context, Ctx),

  case lists:keyfind(Call, 1, Exports) of
    {Call, 2} ->
      Module:Call(Req, ResCtx);
    false     ->
      case default(Call) of
        not_implemented -> not_implemented;
        Data            -> {Data, Req, ResCtx}
      end
  end.

-spec decision( response(term())
              , ExpectedResponse::term()
              , HTTPCodeForFailResponse::integer()
              , context()
              ) -> flow_result().
decision({Response, Req, ResCtx}, Response, _, Ctx0) ->
  update_context(Req, ResCtx, Ctx0);
decision({_, Req, ResCtx}, _, Code, Ctx0) ->
  {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
  error_response(Code, Ctx);
decision({error, Reason}, _, _, Ctx) ->
  error_response(Reason, Ctx);
decision({halt, Code}, _, _, Ctx) ->
  error_response(Code, Ctx);
decision(not_implemented, _, _, Ctx) ->
  error_response(not_implemented, Ctx);
decision(Expected, Expected, _, Ctx) ->
  {ok, Ctx};
decision(_, _, Code, Ctx) ->
  error_response(Code, Ctx).


error_response({error, Reason}, Ctx) ->
  error_response(Reason, Ctx);
error_response({halt, Code}, Ctx) ->
  error_response(Code, Ctx);
error_response(Code, Ctx) when is_integer(Code) ->
  {ok, Resource} = unrest_context:get(resource_module, Ctx),
  {ok, Callstack} = unrest_context:callstack(Ctx),
  lager:info("Resource call finished with code ~p. "
              "Resource: ~p. Callstack: ~p"
              , [Code, Resource, Callstack]
  ),
  Req0 = req(Ctx),
  {ok, Req}  = cowboy_req:reply(Code, Req0),
  {respond, Req};
error_response(Reason, Ctx) ->
  {ok, Resource} = unrest_context:get(resource_module, Ctx),
  {ok, Callstack} = unrest_context:callstack(Ctx),
  lager:info( "Resource call finished with error ~p. "
               "Resource: ~p. Callstack: ~p"
             , [Reason, Resource, Callstack]
             ),
  Req0 = req(Ctx),
  {ok, Req}  = cowboy_req:reply(500, Req0),
  {respond, Req}.

%%_* Helpers ===================================================================

req(Ctx) ->
  {ok, Req} = unrest_context:get(req, Ctx),
  Req.

method(Ctx0) ->
  {Method, Req} = cowboy_req:method(req(Ctx0)),
  {ok, Ctx} = unrest_context:set(req, Req, Ctx0),
  {Method, Ctx}.

header(Header, Ctx0) ->
  {H, Req} = cowboy_req:header(Header, req(Ctx0)),
  {ok, Ctx} = unrest_context:set(req, Req, Ctx0),
  {H, Ctx}.

%% TODO: compute md5 on a streaming body
compute_body_md5(Ctx0) ->
  {Body, Req} = cowboy_req:body(req(Ctx0)),
  {ok, Ctx} = unrest_context:set(req, Req, Ctx0),
  {crypto:hash(md5, Body), Ctx}.

update_context(Req, ResCtx, Ctx) ->
  unrest_context:multiset( [ {resource_context, ResCtx}
                           , {req, Req}
                           ]
                         , Ctx
                         ).

%%_* Defaults ==================================================================

default(ping) ->
    pong;
default(service_available) ->
    true;
default(resource_exists) ->
    true;
default(auth_required) ->
    true;
default(is_authorized) ->
    true;
default(forbidden) ->
    false;
default(allow_missing_post) ->
    false;
default(malformed_request) ->
    false;
default(uri_too_long) ->
    false;
default(known_content_type) ->
    true;
default(valid_content_headers) ->
    true;
default(valid_entity_length) ->
    true;
default(options) ->
    [];
default(allowed_methods) ->
    [<<"GET">>, <<"HEAD">>];
default(known_methods) ->
  [ <<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"TRACE">>
  , <<"CONNECT">>, <<"OPTIONS">>, <<"PATCH">>];
default(content_types_provided) ->
    [{<<"text/html">>, to_html}];
default(content_types_accepted) ->
    [];
default(delete_resource) ->
    false;
default(delete_completed) ->
    true;
default(post_is_create) ->
    false;
default(create_path) ->
    undefined;
default(base_uri) ->
        undefined;
default(process_post) ->
    false;
default(language_available) ->
    true;
default(charsets_provided) ->
    no_charset; % this atom causes charset-negotation to short-circuit
    % the default setting is needed for non-charset responses such as image/png
    %    an example of how one might do actual negotiation
    %    [{"iso-8859-1", fun(X) -> X end}, {"utf-8", make_utf8}];
default(encodings_provided) ->
    [{"identity", fun(X) -> X end}];
    % this is handy for auto-gzip of GET-only resources:
    %    [{"identity", fun(X) -> X end}, {"gzip", fun(X) -> zlib:gzip(X) end}];
default(variances) ->
    [];
default(is_conflict) ->
    false;
default(multiple_choices) ->
    false;
default(previously_existed) ->
    false;
default(moved_permanently) ->
    false;
default(moved_temporarily) ->
    false;
default(last_modified) ->
    undefined;
default(expires) ->
    undefined;
default(generate_etag) ->
    undefined;
default(finish_request) ->
    true;
default(validate_content_checksum) ->
    not_validated;
default(_) ->
  not_implemented.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
