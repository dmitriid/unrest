%%%=============================================================================
%%% @doc A dummy webmachine service implementation. Works with webmachine flow
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%=============================================================================
-module(unrest_wm_service_example).

%%_* Exports ===================================================================

%% init
-export([ init/1
        , ping/2
        ]
       ).

%% validation and auth
-export([ service_available/2
        , known_methods/2
        , uri_too_long/2
        , allowed_methods/2
        , validate_content_checksum/2
        , malformed_request/2
        , is_authorized/2
        , forbidden/2
        , valid_content_headers/2
        , known_content_type/2
        , valid_entity_length/2
        , options/2
       ]).

%%_* Types =====================================================================

-type context() :: term().
-type req() :: cowboy_req:req().
-type response(What) :: unrest_webmachine:response(What).

%%_* API =======================================================================

%%
%% @doc The entry point to your resource
%%
-spec init(proplists:proplist()) -> {ok, context()}.
init(_Params) ->
  {ok, []}.

%% @doc The `ping` function is undocumented in webmachine docs.
%%      Unlike in webmachine, it's optional, you don't need to implement it
-spec ping(req(), context()) -> response(pong | pang).
ping(Req, Ctx) ->
  {pong, Req, Ctx}.

-spec service_available(req(), context()) -> response(boolean()).
service_available(Req, Ctx) ->
  {true, Req, Ctx}.

-spec known_methods(req(), context()) -> response([binary()]).
known_methods(Req, Ctx) ->
  {[<<"GET">>, <<"OPTIONS">>], Req, Ctx}.

-spec uri_too_long(req(), context()) -> response(boolean()).
uri_too_long(Req, Ctx) ->
  {false, Req, Ctx}.

-spec allowed_methods(req(), context()) -> response([binary()]).
allowed_methods(Req, Ctx) ->
  {[<<"GET">>, <<"OPTIONS">>], Req, Ctx}.

%% @doc The `validate_content_checksum` function is undocumented in webmachine
%%      docs. It's triggered when Content-MD5 header is present
-spec validate_content_checksum(req(), context()) -> response(boolean()).
validate_content_checksum(Req, Ctx) ->
  {true, Req, Ctx}.

-spec malformed_request(req(), context()) -> response(boolean()).
malformed_request(Req, Ctx) ->
  {false, Req, Ctx}.

-spec is_authorized(req(), context()) -> response(boolean()).
is_authorized(Req, Ctx) ->
  {true, Req, Ctx}.

-spec forbidden(req(), context()) -> response(boolean()).
forbidden(Req, Ctx) ->
  {false, Req, Ctx}.

-spec valid_content_headers(req(), context()) -> response(boolean()).
valid_content_headers(Req, Ctx) ->
  {true, Req, Ctx}.

-spec known_content_type(req(), context()) -> response(boolean()).
known_content_type(Req, Ctx) ->
  {true, Req, Ctx}.

-spec valid_entity_length(req(), context()) -> response(boolean()).
valid_entity_length(Req, Ctx) ->
  {true, Req, Ctx}.

-spec options(req(), context()) -> response([{binary(), iolist()}]).
options(Req, Ctx) ->
  {[{<<"Custom">>, <<"Value">>}], Req, Ctx}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
