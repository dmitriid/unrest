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
-export([ v3c3_accept_init/1
        , v3c3_accept/1
        , v3d4_accept_language/1
        , v3e5_accept_charset/1
        , v3f6_accept_encoding/1
        , v3g7_variances/1
]).

%% Existence and redirection
-export([ v3g7_resource_exists/1 ]).

%% Existence and redirection
%% webmachine_non_existing_resource_flow
-export([ v3h7_if_match/1
        , v3i7_is_put/1
        , v3k7_previously_existed/1
        , v3l7_is_post/1
        , v3m7_allow_post_to_missing_resource/1
        ]).

%% Existence and redirection
%% webmachine_non_existing_put_flow
-export([ v3i4_v3k5_moved_permanently/1
        , v3p3_conflict/1
        ]).

%% Existence and redirection
%% webmachine_non_existing_previously_existed_flow
-export([ v3l5_moved_temporarily/1
        , v3m5_is_post/1
        , v3n5_allow_post_to_missing_resource/1
        ]).

%% Conditional request
-export([ v3g8_if_match/1
        , v3h10_if_unmodified_since/1
        , v3i12_if_none_match/1
        , v3l13_if_modified_since/1
        ]).

%% Dummy output
%% TODO: remove once all the flows are implemented
-export([ dummy_output/1
        ]).

%%_* Types =====================================================================

-type context() :: unrest_context:context().
-type flow_result() :: unrest_flow:flow_result().
-type response(What) :: {What, cowboy_req:req(), term()}
                      | {halt, integer()}
                      | {error, term()}.

-export_type([response/1]).

%%_* API =======================================================================

%% TODO: remove once all the flows are implemented
-spec dummy_output(context()) -> flow_result().
dummy_output(Ctx) ->
  Trace = io_lib:format("~p", [unrest_context:callstack(Ctx)]),
  {ok, Req0} = unrest_context:get(req, Ctx),
  {ok, Req} = cowboy_req:reply(200, [], Trace, Req0),
  {respond, Req}.

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

%%_* Content negotiation -------------------------------------------------------
-spec v3c3_accept_init(context()) -> flow_result().
v3c3_accept_init(Ctx0) ->
  case resource_call(content_types_provided, Ctx0) of
    {List, Req, ResCtx} ->
      {ok, Ctx1} = update_context(Req, ResCtx, Ctx0),

      ContentTypes = [normalize_content_types(Type) || Type <- List],

      unrest_context:set( content_types_provided
                        , ContentTypes
                        , Ctx1
                        );
    Other ->
      error_response(Other, Ctx0)
  end.

-spec v3c3_accept(context()) -> flow_result().
v3c3_accept(Ctx0) ->
  Req0 = req(Ctx0),
  case cowboy_req:parse_header(<<"accept">>, Req0) of
    {error, badarg} ->
      error_response(400, Ctx0);
    {ok, undefined, Req} ->
      unrest_context:multiset( [ {req, Req}
                               , {media_type, {<<"text">>, <<"html">>, []}}
                               , { content_type_a
                                 , {{<<"text">>, <<"html">>, []}, to_html}
                                 }
                               ]
                             , Ctx0
                             );
    {ok, Accept0, Req} ->
      {ok, Ctx} = unrest_context:set(req, Req, Ctx0),
      Accept = prioritize_accept(Accept0),
      choose_media_type(Accept, Ctx)
  end.

-spec v3d4_accept_language(context()) -> flow_result().
v3d4_accept_language(Ctx0) ->
  case resource_call(languages_provided, Ctx0) of
    not_implemented ->
      {ok, Ctx0};
    {_, _} = HaltOrError ->
      error_response(HaltOrError, Ctx0);
    {[], Req, ResCtx} ->
      {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
      error_response(406, Ctx);
    {Languages, Req0, ResCtx} ->
      {ok, Ctx1} = unrest_context:set(languages_provided, Languages, Ctx0),
      case cowboy_req:parse_header(<<"accept-language">>, Req0) of
        {error, badarg} ->
          error_response(406, Ctx1);
        {ok, undefined , Req1} ->
          Language = hd(Languages),
          {ok, Ctx} = update_context(Req1, ResCtx, Ctx1),
          set_language(Language, Ctx);
        {ok, AcceptLanguage0, Req1} ->
          {ok, Ctx} = update_context(Req1, ResCtx, Ctx1),
          AcceptLanguage = prioritize_languages(AcceptLanguage0),
          choose_language(AcceptLanguage, Ctx)
      end
  end.

-spec v3e5_accept_charset(context()) -> flow_result().
v3e5_accept_charset(Ctx0) ->
  case resource_call(charsets_provided, Ctx0) of
    not_implemented ->
      set_content_type(Ctx0);
    {_, _} = HaltOrError ->
      error_response(HaltOrError, Ctx0);
    {[], Req, ResCtx} ->
      {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
      error_response(406, Ctx);
    {Charsets, Req0, ResCtx} ->
      {ok, Ctx1} = unrest_context:set(charsets_provided, Charsets, Ctx0),
      case cowboy_req:parse_header(<<"accept-charset">>, Req0) of
        {error, badarg} ->
          error_response(406, Ctx1);
        {ok, undefined, Req1} ->
          {ok, Ctx2} = update_context(Req1, ResCtx, Ctx1),
          {ok, Ctx}  = unrest_context:set(charset_a, hd(Charsets), Ctx2),
          set_content_type(Ctx);
        {ok, AcceptCharset0, Req1} ->
          {ok, Ctx} = update_context(Req1, ResCtx, Ctx1),
          AcceptCharset = prioritize_charsets(AcceptCharset0),
          choose_charset(AcceptCharset, Ctx)
      end
  end.

-spec v3f6_accept_encoding(context()) -> flow_result().
v3f6_accept_encoding(Ctx0) ->
  case resource_call(encodings_provided, Ctx0) of
    not_implemented ->
      set_content_encoding(Ctx0);
    {_, _} = HaltOrError ->
      error_response(HaltOrError, Ctx0);
    {[], Req, ResCtx} ->
      {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
      error_response(406, Ctx);
    {EncodingMap, Req0, ResCtx} ->
      Encodings = [normalize_encoding(Enc) || {Enc, _Fun} <- EncodingMap],
      {ok, Ctx1} = unrest_context:multiset( [ {encodings_provided, Encodings}
                                            , {encodings_map, EncodingMap}
                                            ]
                                          , Ctx0
                                          ),
      case cowboy_req:parse_header(<<"accept-encoding">>, Req0) of
        {error, badarg} ->
          error_response(406, Ctx1);
        {ok, undefined, Req1} ->
          {ok, Ctx2} = update_context(Req1, ResCtx, Ctx1),
          {ok, Ctx} = unrest_context:set(encoding_a, hd(Encodings), Ctx2),
          set_content_encoding(Ctx);
        {ok, AcceptEncoding0, Req1} ->
          AcceptEncoding1 = [  normalize_encoding(Enc)
                            || {Enc, _Fun} <- AcceptEncoding0
                            ],
          {ok, Ctx} = update_context(Req1, ResCtx, Ctx1),
          AcceptEncoding = prioritize_encodings(AcceptEncoding1),
          choose_encoding(AcceptEncoding, Ctx)
      end
  end.

-spec v3g7_variances(context()) -> flow_result().
v3g7_variances(Ctx0) ->
  {ok, CTP} = unrest_context:get(content_types_provided, Ctx0),
  {ok, LP} = unrest_context:get(languages_provided, Ctx0),
  {ok, CP} = unrest_context:get(charsets_provided, Ctx0),
  {ok, EP} = unrest_context:get(encodings_provided, Ctx0),
  Variances = case CTP of
                [] -> [];
                [_] -> [];
                [_|_] -> [<<"accept">>]
              end,
  Variances2 = case LP of
                 [] -> Variances;
                 [_] -> Variances;
                 [_|_] -> [<<"accept-language">>|Variances]
               end,
  Variances3 = case CP of
                 [] -> Variances2;
                 [_] -> Variances2;
                 [_|_] -> [<<"accept-charset">>|Variances2]
               end,
  Variances4 = case EP of
                 [] -> Variances3;
                 [_] -> Variances3;
                 [_|_] -> [<<"accept-encoding">>|Variances3]
               end,

  case resource_call(variances, Ctx0) of
    not_implemented ->
      variances(Variances4, Ctx0);
    {_, _} = HaltOrError ->
      error_response(HaltOrError, Ctx0);
    {HandlerVariances, Req, ResCtx} ->
      {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
      variances(HandlerVariances ++ Variances4, Ctx)
  end.

%%_* Existence and redirection -------------------------------------------------

-spec v3g7_resource_exists(context()) -> flow_result().
v3g7_resource_exists(Ctx0) ->
  {Bool, Req, ResCtx} = resource_call(resource_exists, Ctx0),
  {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
  case Bool of
    true  ->
      {flow, <<"webmachine_conditional_request_flow">>, Ctx};
    false ->
      {flow, <<"webmachine_non_existing_resource_flow">>, Ctx}
  end.

-spec v3h7_if_match(context()) -> flow_result().
v3h7_if_match(Ctx0) ->
  {H, Ctx} = header(<<"if-match">>, Ctx0),
  decision(H, undefined, 412, Ctx).

-spec v3i7_is_put(context()) -> flow_result().
v3i7_is_put(Ctx0) ->
  {Method, Ctx} = method(Ctx0),
  case Method of
    <<"PUT">> ->
      {flow, <<"webmachine_non_existing_put_flow">>, Ctx};
    _         ->
      {ok, Ctx}
  end.

-spec v3k7_previously_existed(context()) -> flow_result().
v3k7_previously_existed(Ctx) ->
  case resource_call(previously_existed, Ctx) of
    not_implemented ->
      {ok, Ctx};
    {_, _} = HaltOrError ->
      error_response(HaltOrError, Ctx);
    {false, Req, ResCtx} ->
      update_context(Req, ResCtx, Ctx);
    {true, Req, ResCtx} ->
      {ok, Ctx} = update_context(Req, ResCtx, Ctx),
      {flow, <<"webmachine_non_existing_previously_existed_flow">>, Ctx}
  end.

-spec v3l7_is_post(context()) -> flow_result().
v3l7_is_post(Ctx0) ->
  {Method, Ctx} = method(Ctx0),
  case Method of
    <<"POST">> ->
      %% TODO: redirect to non_existing_post_flow
      {ok, Ctx};
    _ ->
      error_response(404, Ctx)
  end.

-spec v3i4_v3k5_moved_permanently(context()) -> flow_result().
v3i4_v3k5_moved_permanently(Ctx0) ->
  case resource_call(moved_permanently, Ctx0) of
    not_implemented ->
      {ok, Ctx0};
    {_, _} = HaltOrError ->
      error_response(HaltOrError, Ctx0);
    {false, Req, ResCtx} ->
      update_context(Req, ResCtx, Ctx0);
    {{true, MovedUri}, Req0, ResCtx} ->
      Req = cowboy_req:set_resp_header(<<"location">>, MovedUri, Req0),
      {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
      error_response(301, Ctx)
  end.

-spec v3l5_moved_temporarily(context()) -> flow_result().
v3l5_moved_temporarily(Ctx0) ->
  case resource_call(moved_temporarily, Ctx0) of
    not_implemented ->
      {ok, Ctx0};
    {_, _} = HaltOrError ->
      error_response(HaltOrError, Ctx0);
    {false, Req, ResCtx} ->
      update_context(Req, ResCtx, Ctx0);
    {{true, MovedUri}, Req0, ResCtx} ->
      Req = cowboy_req:set_resp_header(<<"location">>, MovedUri, Req0),
      {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
      error_response(302, Ctx)
  end.

-spec v3p3_conflict(context()) -> flow_result().
v3p3_conflict(Ctx) ->
  decision(resource_call(is_conflict, Ctx), false, 409, Ctx).

-spec v3m5_is_post(context()) -> flow_result().
v3m5_is_post(Ctx0) ->
  {Method, Ctx} = method(Ctx0),
  case Method of
    <<"POST">> ->
      {ok, Ctx};
    _ ->
      error_response(410, Ctx)
  end.

-spec v3m7_allow_post_to_missing_resource(context()) -> flow_result().
v3m7_allow_post_to_missing_resource(Ctx) ->
  decision(resource_call(allow_missing_post, Ctx), true, 404, Ctx).

-spec v3n5_allow_post_to_missing_resource(context()) -> flow_result().
v3n5_allow_post_to_missing_resource(Ctx) ->
  decision(resource_call(allow_missing_post, Ctx), true, 410, Ctx).

%%_* Conditional Request -------------------------------------------------------

-spec v3g8_if_match(context()) -> flow_result().
v3g8_if_match(Ctx0) ->
  Req0 = req(Ctx0),
  case cowboy_req:parse_header(<<"if-match">>, Req0) of
    {ok, undefined, Req} ->
      unrest_context:set(req, Req, Ctx0);
    {ok, '*', Req} ->
      unrest_context:set(req, Req, Ctx0);
    {ok, ETagsList, Req} ->
      {ok, Ctx} = unrest_context:set(req, Req, Ctx0),
      if_match(ETagsList, Ctx)
  end.

-spec v3h10_if_unmodified_since(context()) -> flow_result().
v3h10_if_unmodified_since(Ctx) ->
  {ok, Ctx}.

-spec v3i12_if_none_match(context()) -> flow_result().
v3i12_if_none_match(Ctx) ->
  {ok, Ctx}.

-spec v3l13_if_modified_since(context()) -> flow_result().
v3l13_if_modified_since(Ctx) ->
  {ok, Ctx}.

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

normalize_content_types({ContentType, Callback})
  when is_binary(ContentType) ->
  {cowboy_http:content_type(ContentType), Callback};
normalize_content_types(Normalized) ->
  Normalized.

prioritize_accept(Accept) ->
  lists:sort(
    fun({MediaTypeA, Quality, _AcceptParamsA},
        {MediaTypeB, Quality, _AcceptParamsB}) ->
      %% Same quality, check precedence in more details.
      prioritize_mediatype(MediaTypeA, MediaTypeB);
      ({_MediaTypeA, QualityA, _AcceptParamsA},
       {_MediaTypeB, QualityB, _AcceptParamsB}) ->
        %% Just compare the quality.
        QualityA > QualityB
    end, Accept).

%% Media ranges can be overridden by more specific media ranges or
%% specific media types. If more than one media range applies to a given
%% type, the most specific reference has precedence.
%%
%% We always choose B over A when we can't decide between the two.
prioritize_mediatype({TypeA, SubTypeA, ParamsA}, {TypeB, SubTypeB, ParamsB}) ->
  case TypeB of
    TypeA ->
      case SubTypeB of
        SubTypeA -> length(ParamsA) > length(ParamsB);
        <<"*">> -> true;
        _Any -> false
      end;
    <<"*">> -> true;
    _Any -> false
  end.

%% Ignoring the rare AcceptParams. Not sure what should be done about them.
choose_media_type([], Ctx) ->
  error_response(406, Ctx);
choose_media_type(Accept, Ctx) ->
  {ok, ContentTypes} = unrest_context:get(content_types_provided, Ctx),
  match_media_type(Accept, ContentTypes, Ctx).

match_media_type([H|T], ContentTypes, Ctx) ->
  match_media_type(H, ContentTypes, T, Ctx).

match_media_type(_MediaType, [], Accept, Ctx) ->
  choose_media_type(Accept, Ctx);
match_media_type( MediaType = {{<<"*">>, <<"*">>, _Params_A}, _QA, _APA}
                , ContentTypes
                , Accept
                , Ctx) ->
  match_media_type_params(MediaType, ContentTypes, Accept, Ctx);
match_media_type( MediaType = {{Type, SubType_A, _PA}, _QA, _APA}
                , ContentTypes = [{{Type, SubType_P, _PP}, _Fun}|_Tail]
                , Accept
                , Ctx
                ) when SubType_P =:= SubType_A; SubType_A =:= <<"*">> ->
  match_media_type_params(MediaType, ContentTypes, Accept, Ctx);
match_media_type(MediaType, [_|T], Accept, Ctx) ->
  match_media_type(MediaType, T, Accept, Ctx).

match_media_type_params({{_TA, _STA, Params_A}, _QA, _APA}
                       , [Provided = {{TP, STP, '*'}, _Fun}|_Tail]
                       , _Accept
                       , Ctx
                       ) ->
  PMT = {TP, STP, Params_A},
  unrest_context:multiset([ {media_type, PMT}
                          , {content_type_a, Provided}
                          ]
                          , Ctx
                         );
match_media_type_params( MediaType = {{_TA, _STA, Params_A}, _QA, _APA}
                       , [Provided = {PMT = {_TP, _STP, Params_P}, _Fun}|Tail]
                       , Accept
                       , Ctx
                       ) ->
  case lists:sort(Params_P) =:= lists:sort(Params_A) of
    true ->
      unrest_context:multiset( [ {media_type, PMT}
                               , {content_type_a, Provided}
                               ]
                              , Ctx
                             );
    false ->
      match_media_type(MediaType, Tail, Accept, Ctx)
  end.

%% A language-range matches a language-tag if it exactly equals the tag,
%% or if it exactly equals a prefix of the tag such that the first tag
%% character following the prefix is "-". The special range "*", if
%% present in the Accept-Language field, matches every tag not matched
%% by any other range present in the Accept-Language field.
%%
%% @todo The last sentence probably means we should always put '*'
%% at the end of the list.
prioritize_languages(AcceptLanguages) ->
  lists:sort(
    fun({_TagA, QualityA}, {_TagB, QualityB}) ->
      QualityA > QualityB
    end, AcceptLanguages).

choose_language([], Ctx) ->
  error_response(406, Ctx);
choose_language([Language | Tail], Ctx) ->
  {ok, LanguagesProvided} = unrest_context:get(languages_provided, Ctx),
  match_language(Language, LanguagesProvided, Tail, Ctx).

match_language(_Language, [], Accept, Ctx) ->
  choose_language(Accept, Ctx);
match_language({'*', _Quality}, [Provided | _], _Accept, Ctx) ->
  set_language(Provided, Ctx);
match_language({Provided, _Quality}, [Provided | _], _Accept, Ctx) ->
  set_language(Provided, Ctx);
match_language( Language = {Tag, _Quality}
              , [Provided | Tail]
              , Accept
              , Ctx
              ) ->
  Length = byte_size(Tag),
  case Provided of
    <<Tag:Length/binary, $-, _Any/bits>> ->
      set_language(Provided, Ctx);
    _Any ->
      match_language(Language, Tail, Accept, Ctx)
  end.

set_language(Language, Ctx) ->
  Req = cowboy_req:set_resp_header( <<"content-language">>
                                  , Language
                                  , req(Ctx)
                                  ),
  unrest_context:multiset( [ {language, Language}
                           , {req, Req}
                           ]
                          , Ctx
                         ).

%% The special value "*", if present in the Accept-Charset field,
%% matches every character set (including ISO-8859-1) which is not
%% mentioned elsewhere in the Accept-Charset field. If no "*" is present
%% in an Accept-Charset field, then all character sets not explicitly
%% mentioned get a quality value of 0, except for ISO-8859-1, which gets
%% a quality value of 1 if not explicitly mentioned.
prioritize_charsets(AcceptCharsets) ->
  AcceptCharsets2 = lists:sort(
    fun({_CharsetA, QualityA}, {_CharsetB, QualityB}) ->
      QualityA > QualityB
    end, AcceptCharsets),
  case lists:keymember(<<"*">>, 1, AcceptCharsets2) of
    true -> AcceptCharsets2;
    false ->
      case lists:keymember(<<"iso-8859-1">>, 1, AcceptCharsets2) of
        true -> AcceptCharsets2;
        false -> [{<<"iso-8859-1">>, 1000}|AcceptCharsets2]
      end
  end.

choose_charset([], Ctx) ->
  error_response(406, Ctx);
choose_charset([Charset | Tail], Ctx) ->
  {ok, CharsetsProvided} = unrest_context:get(charsets_provided, Ctx),
  match_charset(Charset, CharsetsProvided, Tail, Ctx).

match_charset(_, [], Accept, Ctx) ->
  choose_charset(Accept, Ctx);
match_charset({Provided, _}, [Provided | _], _Accept, Ctx0) ->
  {ok, Ctx} = unrest_context:set(charset_a, Provided, Ctx0),
  set_content_type(Ctx);
match_charset(Charset, [_ | Tail], Accept, Ctx) ->
  match_charset(Charset, Tail, Accept, Ctx).

normalize_encoding(Encoding0) when is_binary(Encoding0) ->
  [Encoding] = cowboy_http:list(Encoding0, fun cowboy_http:conneg/2),
  Encoding;
normalize_encoding(NormalizedEncoding) ->
  NormalizedEncoding.

prioritize_encodings(AcceptEncodings) ->
  AcceptEncodings2 = lists:sort(
    fun({_EncA, QualityA}, {_EncB, QualityB}) ->
      QualityA > QualityB
    end, AcceptEncodings),
  case lists:keymember(<<"*">>, 1, AcceptEncodings2) of
    true -> AcceptEncodings2;
    false ->
      case lists:keymember(<<"identity">>, 1, AcceptEncodings2) of
        true -> AcceptEncodings2;
        false -> [{<<"identity">>, 1000}|AcceptEncodings2]
      end
  end.

choose_encoding([], Ctx) ->
  error_response(406, Ctx);
choose_encoding([Encoding | Tail], Ctx) ->
  {ok, EncodingsProvided} = unrest_context:get(encodings_provided, Ctx),
  match_encoding(Encoding, EncodingsProvided, Tail, Ctx).

match_encoding(_, [], Accept, Ctx) ->
  choose_encoding(Accept, Ctx);
match_encoding({Provided, _}, [{Provided, _} | _], _Accept, Ctx0) ->
  {ok, Ctx} = unrest_context:set(encoding_a, Provided, Ctx0),
  set_content_encoding(Ctx);
match_encoding(Encoding, [_ | Tail], Accept, Ctx) ->
  match_encoding(Encoding, Tail, Accept, Ctx).

variances(Variances, Ctx0) ->
  {ok, Ctx} = case [[<<", ">>, V] || V <- Variances] of
                 [] ->
                   {ok, Ctx0};
                 [[<<", ">>, H]|Variances5] ->
                   Req0 = req(Ctx0),
                   Req = cowboy_req:set_resp_header( <<"vary">>
                                                   , [H|Variances5]
                                                   , Req0
                                                   ),
                   unrest_context:set(req, Req, Ctx0)
               end,
  set_content_type(Ctx).

set_content_type(Ctx0) ->
  {ok, {{Type, SubType, Params}, _Fun}} =
                                       unrest_context:get(content_type_a, Ctx0),
  {ok, Charset} = unrest_context:get(charset_a, Ctx0, undefined),
  ParamsBin = set_content_type_build_params(Params, []),
  ContentType = [Type, <<"/">>, SubType, ParamsBin],
  ContentType2 = case Charset of
                   undefined -> ContentType;
                   Charset -> [ContentType, <<"; charset=">>, Charset]
                 end,
  Req0 = req(Ctx0),
  Req1 = cowboy_req:set_resp_header(<<"content-type">>, ContentType2, Req0),
  unrest_context:multiset( [ {req,      Req1}
                           , {charset,  Charset}
                           ]
                         , Ctx0
                         ).

set_content_type_build_params('*', []) ->
  <<>>;
set_content_type_build_params([], []) ->
  <<>>;
set_content_type_build_params([], Acc) ->
  lists:reverse(Acc);
set_content_type_build_params([{Attr, Value}|Tail], Acc) ->
  set_content_type_build_params(Tail, [[Attr, <<"=">>, Value], <<";">>|Acc]).

set_content_encoding(Ctx0) ->
  {ok, Encoding0} = unrest_context:get( encoding_a
                                      , Ctx0
                                      , <<"identity;q=1.0,*;q=0.5">>
                                      ),
  Encoding = case Encoding0 of {Enc, _Q} -> Enc; _ -> Encoding0 end,
  Req0 = req(Ctx0),
  Req1 = cowboy_req:set_resp_header(<<"content-encoding">>, Encoding, Req0),
  unrest_context:multiset( [ {req, Req1}
                           , {encoding, Encoding}
                           ]
                          , Ctx0
                         ).

if_match(EtagList, Ctx0) ->
  case generate_etag(Ctx0) of
    {undefined, Ctx} ->
      error_response(412, Ctx);
    {Etag, Ctx} ->
      io:format("ETAGS ~p~n~p~n", [Etag, EtagList]),
      case lists:member(Etag, EtagList) of
        true  -> {ok, Ctx};
        false -> error_response(412, Ctx)
      end
  end.

generate_etag(Ctx0) ->
  case unrest_context:get(etag, Ctx0, undefined) of
    {ok, no_call}   -> {undefined, Ctx0};
    {ok, undefined} ->
      case resource_call(generate_etag, Ctx0) of
        not_implemented ->
          {ok, Ctx0};
        {_, _} = HaltOrError ->
          error_response(HaltOrError, Ctx0);
        {Etag0, Req, ResCtx} when is_binary(Etag0)->
          [Etag] = cowboy_http:entity_tag_match(Etag0),
          {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
          {Etag, Ctx};
        {Etag, Req, ResCtx} ->
          {ok, Ctx} = update_context(Req, ResCtx, Ctx0),
          {Etag, Ctx}
      end;
    {ok, Etag} ->
      {Etag, Ctx0}
  end.

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
  [{{<<"text">>, <<"html">>, '*'}, to_html}];
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
    not_implemented; % this atom causes charset-negotation to short-circuit
    % the default setting is needed for non-charset responses such as image/png
    %    an example of how one might do actual negotiation
    %    [{"iso-8859-1", fun(X) -> X end}, {"utf-8", make_utf8}];
default(encodings_provided) ->
    [{<<"identity">>, fun(X) -> X end}];
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
