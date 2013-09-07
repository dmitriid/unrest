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
        ]
       ).

%% Content negotiation
-export([ content_types_provided/2
        , languages_provided/2
        , charsets_provided/2
        , encodings_provided/2
        , variances/2
        ]
       ).

%% Existence and redirection
-export([ resource_exists/2
        , moved_permanently/2
        , moved_temporarily/2
        , previously_existed/2
        , allow_missing_post/2
        , is_conflict/2
        ]
       ).

%% Conditional request
-export([ generate_etag/2
        , last_modified/2
        ]
       ).
%% Delete resource
-export([ delete_resource/2
        , delete_completed/2
        ]
       ).

%% Body
-export([ multiple_choices/2
        ]
       ).

%%_* Types =====================================================================

-type context() :: term().
-type req() :: cowboy_req:req().
-type response(What) :: unrest_webmachine:response(What).

%%_* API =======================================================================

%%_* init ----------------------------------------------------------------------

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

%%_* validation and auth -------------------------------------------------------
-spec service_available(req(), context()) -> response(boolean()).
service_available(Req, Ctx) ->
  {true, Req, Ctx}.

-spec known_methods(req(), context()) -> response([binary()]).
known_methods(Req, Ctx) ->
  {[<<"GET">>, <<"OPTIONS">>, <<"PUT">>, <<"POST">>, <<"DELETE">>], Req, Ctx}.

-spec uri_too_long(req(), context()) -> response(boolean()).
uri_too_long(Req, Ctx) ->
  {false, Req, Ctx}.

-spec allowed_methods(req(), context()) -> response([binary()]).
allowed_methods(Req, Ctx) ->
  {[<<"GET">>, <<"OPTIONS">>, <<"PUT">>, <<"POST">>, <<"DELETE">>], Req, Ctx}.

%% @doc The `validate_content_checksum` function is undocumented in webmachine
%%      docs. It's triggered when Content-MD5 header is present
-spec validate_content_checksum(req(), context()) -> response(boolean()).
validate_content_checksum(Req, Ctx) ->
  {true, Req, Ctx}.

-spec malformed_request(req(), context()) -> response(boolean()).
malformed_request(Req, Ctx) ->
  {false, Req, Ctx}.

-spec is_authorized(req(), context()) -> response(true | binary()).
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

%%_* content negotiation -------------------------------------------------------

%% @doc content_types_provided/2 should return a list of content types and their
%%      associated callback function as a tuple: {{Type, SubType, Params}, Fun}.
%%      Type and SubType are the media type as binary. Params is a list of
%%      Key/Value tuple, with Key and Value a binary. Fun is the name of the
%%      callback that will be used to return the content of the response. It is
%%      given as an atom.
%%
%%      An example of such return value would be:
%%         {{<<"text">>, <<"html">>, []}, to_html}
%%
%%      Note that it is also possible to return a binary content type that will
%%      then be parsed by Cowboy. However note that while this may make your
%%      resources a little more readable, this is a lot less efficient.
%%
%%      An example of such return value would be:
%%        {<<"text/html">>, to_html}
-spec content_types_provided(req(), context()) ->
                                                 response([{header(), atom()}]).
-type header() ::   binary()
                  | { Type::binary()
                    , Subtype::binary()
                    , Params::[{binary(), binary()}]
                    }.
content_types_provided(Req, Ctx) ->
  ContentTypes = [ {{<<"text">>, <<"html">>, []}, to_html}
                 , {<<"text/plain">>, to_text}
                 ],
  {ContentTypes, Req, Ctx}.

%% @doc languages_provided should return a list of binary values indicating
%%      which languages are accepted by the resource.
-spec languages_provided(req(), context()) -> response([binary()]).
languages_provided(Req, Ctx) ->
  {[<<"en-us">>, <<"ru">>], Req, Ctx}.

-spec charsets_provided(req(), context()) -> response([binary()]).
charsets_provided(Req, Ctx) ->
  {[<<"utf-8">>], Req, Ctx}.

%%
%% @doc encodings_provided/2 should return a list of encodings and their
%%      corresponding callback functions. Encodings are either a tupe
%%      {binary(), Quality::float} or just binary(). Returning a tuple
%%      is more efficient.
%%
%%      "identity;q=1.0" will be used by default, if this callback is not
%%      provided
%%
-spec encodings_provided(req(), context()) -> response([encoding()]).
-type encoding() :: {Name::binary(), Quality::float()} | binary().
encodings_provided(Req, Ctx) ->
  {[{<<"identity">>, fun(X) -> X end}], Req, Ctx}.

%% @doc variances/2 should return a list of headers that will be added
%%      to the Vary response header. The Accept, Accept-Language,
%%      Accept-Charset and Accept-Encoding headers do not need to be
%%      specified.
-spec variances(req(), context()) -> response([binary()]).
variances(Req, Ctx) ->
  {[<<"accept-charset">>], Req, Ctx}.

%%_* existence and redirection -------------------------------------------------

-spec resource_exists(req(), context()) -> response(boolean()).
resource_exists(Req, Ctx) ->
  {true, Req, Ctx}.

-spec previously_existed(req(), context()) -> response(boolean()).
previously_existed(Req, Ctx) ->
  {false, Req, Ctx}.


-spec moved_permanently(req(), context()) -> response( {true, WhereTo::iolist()}
                                                     | false
                                                     ).
moved_permanently(Req, Ctx) ->
  %% {{true, "/permanent-new-location"}, Req, Ctx}.
  {false, Req, Ctx}.

-spec moved_temporarily(req(), context()) -> response( {true, WhereTo :: iolist()}
                                                     | false
                                                     ).
moved_temporarily(Req, Ctx) ->
  %%{{true, "/temporary-new-location"}, Req, Ctx}.
  {false, Req, Ctx}.

-spec is_conflict(req(), context()) -> response(boolean()).
is_conflict(Req, Ctx) ->
  {false, Req, Ctx}.

-spec allow_missing_post(req(), context()) -> response(boolean()).
allow_missing_post(Req, Ctx) ->
  {true, Req, Ctx}.

%%_* conditional request -------------------------------------------------------

%%
%% @doc An ETag is either a tuple, containing etag strength and value or
%%      a binary containing a valid etag value (a quoted string or
%%      W/"quoted string"):
%%
%%      {strong, "some etag value"}
%%      {weak,   "another etag value"}
%%      <<"\"an etag value\"">>
%%      <<"W/\"another_value\"">>
%%      <<"*">>
%%
-spec generate_etag(req(), context()) -> response( {etag_strength(), string()}
                                                 | binary()
                                                 ).
-type etag_strength() :: strong | weak | atom().
generate_etag(Req, Ctx) ->
  {<<"\"a value\"">>, Req, Ctx}.

%% @doc Return date in the format {{Year, Month, Day}, {Hour, Minute, Second}}
-spec last_modified(req(), context()) -> response(tuple()).
last_modified(Req, Ctx) ->
  {{Y, M, D}, Time} = erlang:universaltime(),
  Yesteryear = Y - 1,
  {{{Yesteryear, M, D}, Time}, Req, Ctx}.

%%_* delete resource ----------------------------------------------------------
-spec delete_resource(req(), context()) -> response(boolean()).
delete_resource(Req, Ctx) ->
  {true, Req, Ctx}.

-spec delete_completed(req(), context()) -> response(boolean()).
delete_completed(Req, Ctx) ->
  {true, Req, Ctx}.

%%_* body ---------------------------------------------------------------------
-spec multiple_choices(req(), context()) -> response(boolean()).
multiple_choices(Req, Ctx) ->
  {false, Req, Ctx}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
