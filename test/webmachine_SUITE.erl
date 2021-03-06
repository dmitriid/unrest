%%%=============================================================================
%%% @doc Tests for the webmachine flow
%%%      See see https://twitter.com/seancribbs/status/354790811973586944/photo/1
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
-export([ validation_and_auth/1
        , content_negotiation/1
        , existence_and_redirection/1
        , conditional_requests/1
        , delete/1
        , body/1
        ]).

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
  unrest:stop(),
  Path = filename:join([code:priv_dir(unrest), "webmachine.yml"]),
  unrest:start(Path),
  Config.

end_per_suite(_Config) ->
  application:stop(unrest).

init_per_testcase(Testcase, Config) ->
  Tests = tests(Testcase),
  TestConfigs = lists:foldl(fun update_test_config/2, [], Tests),
  [{tests, lists:reverse(TestConfigs)} | Config].

end_per_testcase(_Testcase, _Config) ->
  ok.

%%_* Tests =====================================================================

validation_and_auth(Config) ->
  run(Config).

content_negotiation(Config) ->
  run(Config).

existence_and_redirection(Config) ->
  run(Config).

conditional_requests(Config) ->
  run(Config).

delete(Config) ->
  run(Config).

body(Config) ->
  run(Config).

%%_* Test definitions ==========================================================

tests(validation_and_auth) ->
  [ { ping
    , "503 for response other than pong"
    , [ {response_code, "503"}
      , {callbacks, [{ping, pang}]}
      ]
    }
  , { v3b13_service_available
    , "503 if response is `false`"
    , [ {response_code, "503"}
      , {callbacks, [{service_available, false}]}
      ]
    }
  , { v3b12_known_method
    , "501 if method is unknown"
    , [ {response_code, "501"}
      , {callbacks, [{known_methods, []}]}
      ]
    }
  , { v3b12_known_method
    , "501 if method is unknown, but a list of known_methods exists"
    , [ {response_code, "501"}
      , {callbacks, [{known_methods, [<<"POST">>]}]}
      ]
    }
  , { v3b11_uri_too_long
    , "414 if response is true"
    , [ {response_code, "414"}
      , {callbacks, [{uri_too_long, true}]}
      ]
    }
  , { v3b10_method_allowed
    , "405 if method not allowed"
    , [ {response_code, "405"}
      , {callbacks, [{allowed_methods, []}]}
      ]
    }
  , { v3b10_method_allowed
    , "405 if method not allowed"
    , [ {response_code, "405"}
      , {callbacks, [{allowed_methods, [<<"POST">>]}]}
      ]
    }
  , { v3b9_content_checksum
    , "400 if content-md5 header is present, but not validated"
    , [ {response_code, "400"}
      , {request_headers, [{"Content-MD5", "1"}]}
      , {callbacks, [{validate_content_checksum, false}]}
      ]
    }
  , { v3b9_content_checksum
    , "400 if content-md5 header is present, but invalid"
    , [ {response_code, "400"}
      , {method, "POST"}
      , {body, "test"}
      , {request_headers, [ {"Content-MD5", "YWFhCgoK"}]}
      , {callbacks, [ {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    ]
        }
      ]
    }
  , { v3b9_malformed_request
    , "403 if malformed_request returns true"
    , [ {response_code, "403"}
      , {callbacks, [{malformed_request, true}]}
      ]
    }
  , { v3b8_authorized
    , "401 and WWW-Authenticate set for non-true response"
    , [ {response_code, "401"}
      , {response_headers, [{"WWW-Authenticate", "Basic"}]}
      , {callbacks, [{is_authorized, <<"Basic">>}]}
      ]
    }
  , { v3b7_forbidden
    , "403 if response is true"
    , [ {response_code, "403"}
      , {callbacks, [{forbidden, true}]}
      ]
    }
  , { v3b6_valid_content_headers
    , "501 if response is false"
    , [ {response_code, "501"}
      , {callbacks, [{valid_content_headers, false}]}
      ]
    }
  , { v3b6_valid_content_headers
    , "501 if response is false"
    , [ {response_code, "501"}
      , {callbacks, [{valid_content_headers, false}]}
      ]
    }
  , { v3b5_known_content_type
    , "415 if response is false"
    , [ {response_code, "415"}
      , {callbacks, [{known_content_type, false}]}
      ]
    }
  , { v3b4_request_too_large
    , "413 if response from valid_entity_length is false"
    , [ {response_code, "413"}
      , {callbacks, [{valid_entity_length, false}]}
      ]
    }
  , { v3b3_options
    , "Response is 200 and headers are set"
    , [ {response_code, "200"}
      , {method, "OPTIONS"}
      , {response_headers, [ {<<"Header1">>, <<"Value 1">>}
                           , {<<"Header2">>, <<"Value 2">>}
                           ]}
      , {callbacks, [ {known_methods, [<<"OPTIONS">>]}
                    , {allowed_methods, [<<"OPTIONS">>]}
                    , {options, [ {<<"Header1">>, <<"Value 1">>}
                                , {<<"Header2">>, <<"Value 2">>}
                                ]}
                    ]
        }
      ]
    }
  ];
tests(content_negotiation) ->
  [ { v3c3_accept
    , "400 on wrong accept header"
    , [ {response_code, "400"}
      , {request_headers, [{"Accept", "invalid,,accept;;header..;"}]}
      ]
    }
  , { v3c3_accept
    , "406 on unacceptable media type"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept", "some/type"}]}
      , {callbacks, [{content_types_provided, [{<<"another/type">>, dummy}]}]}
      ]
    }
  , { v3c3_accept
    , "406 on unacceptable media type"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept", "some/type"}]}
      , {callbacks, [{content_types_provided, []}]}
      ]
    }
  , { v3d4_accept_language
    , "406 on invalid language header"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept-Language", "invalid,,accept-language;;header..;"}]}
      , {callbacks, [{languages_provided, [<<"en">>]}]}
      ]
    }
  , { v3d4_accept_language
    , "406 when no languages are accepted"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept-Language", "en"}]}
      , {callbacks, [{languages_provided, []}]}
      ]
    }
  , { v3d4_accept_language
    , "406 when desired language is not provided"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept-Language", "en"}]}
      , {callbacks, [{languages_provided, [<<"ru">>]}]}
      ]
    }
  , { v3e5_accept_charset
    , "406 on invalid charset header"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept-Charset", "invalid,,accept-charset;;header..;"}]}
      , {callbacks, [{charsets_provided, [{<<"utf-8">>, dummy}]}]}
      ]
    }
  , { v3e5_accept_charset
    , "406 when no charsets are accepted"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept-Charset", "utf-8"}]}
      , {callbacks, [{charsets_provided, []}]}
      ]
    }
  , { v3e5_accept_charset
    , "406 when desired charset is not provided"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept-Charset", "utf-8"}]}
      , {callbacks, [{charsets_provided, [{<<"latin1">>, dummy}]}]}
      ]
    }
  , { v3f6_accept_encoding
    , "406 on invalid encoding header"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept-Encoding", "invalid,,accept-charset;;header..;"}]}
      , {callbacks, [{encodings_provided, [{<<"identity">>, fun(X) -> X end}]}]}
      ]
    }
  , { v3f6_accept_encoding
    , "406 when no charsets are accepted"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept-Encoding", "identity"}]}
      , {callbacks, [{encodings_provided, []}]}
      ]
    }
  , { v3f6_accept_encoding
    , "406 when desired charset is not provided"
    , [ {response_code, "406"}
      , {request_headers, [{"Accept-Encoding", "deflate"}]}
      , {callbacks, [{encodings_provided, [[{<<"identity">>, fun(X) -> X end}]]}]}
      ]
    }
  , { v3g7_variances
    , "The vary response header must exist"
    , [ {response_code, "200"}
      , {response_headers, [{"Vary", "accept-encoding, accept-charset, accept-language, accept"}]}
      , {callbacks, [ {content_types_provided, [ {<<"some/type">>, to_html}
                                               , {<<"another/type">>, to_html}
                                               ]
                      }
                    , {languages_provided, [<<"en">>, <<"ru">>]}
                    , {charsets_provided, [ {<<"utf-8">>, fun identity/1}
                                          , {<<"latin1">>, fun identity/1}
                                          ]
                      }
                    , {encodings_provided, [ {<<"identity">>, fun identity/1}
                                           , {<<"deflate">>, fun identity/1}
                                           ]
                      }
                    , {to_html, <<"hello">>}
                    ]
        }
    ]
    }
  , { v3g7_variances
    , "Custom variance takes precedence"
    , [ {response_code, "200"}
      , {response_headers, [{"Vary", "custom, accept-encoding, accept-charset, accept-language, accept"}]}
      , {callbacks, [ {content_types_provided, [ {<<"some/type">>, to_html}
                                               , {<<"another/type">>, to_html}
                                               ]
                      }
                    , {languages_provided, [<<"en">>, <<"ru">>]}
                    , {charsets_provided, [ {<<"utf-8">>, fun identity/1}
                                          , {<<"latin1">>, fun identity/1}
                                          ]
                      }
                    , {encodings_provided, [ {<<"identity">>, fun identity/1}
                                           , {<<"deflate">>, fun identity/1}
                                           ]
                      }
                    , {variances, [<<"custom">>]}
                    , {to_html, <<"hello">>}
                    ]
        }
    ]
    }
  ];
tests(existence_and_redirection) ->
  [ { v3h7_if_match
    , "Resource doesn't exist \n"
      "If-Match header exists \n"
      "Get a 412"
    , [ {response_code, "412"}
      , {request_headers, [{"If-Match", "\"some value\""}]}
      , {callbacks, [{resource_exists, false}]
        }
      ]
    }
  , { v3i4_v3k5_moved_permanently
    , "Resource doesn't exist \n"
      "Method is PUT \n"
      "Resource is moved permanently\n"
      "Get a 301 and a location header"
    , [ {response_code, "301"}
      , {method, "PUT"}
      , {body, "test"}
      , {response_headers, [{"Location", "/new-location"}]}
      , {callbacks, [ {known_methods, [<<"PUT">>]}
                    , {allowed_methods, [<<"PUT">>]}
                    , {resource_exists, false}
                    , {moved_permanently, {true, "/new-location"}}
                    ]
        }
      ]
    }
  , { v3p3_v3o14_conflict
    , "Resource doesn't exist \n"
      "Method is PUT \n"
      "Resource is not moved \n"
      "Put is in conflict \n"
      "Get a 409"
    , [ {response_code, "409"}
      , {method, "PUT"}
      , {body, "test"}
      , {callbacks, [ {known_methods, [<<"PUT">>]}
                    , {allowed_methods, [<<"PUT">>]}
                    , {resource_exists, false}
                    , {is_conflict, true}
                    ]
        }
      ]
    }
  , { v3i4_v3k5_moved_permanently
    , "Resource doesn't exist \n"
      "Method is *not* PUT \n"
      "Resource previously existed \n"
      "Resource is moved permanently\n"
      "Get a 301 and a location header"
    , [ {response_code, "301"}
      , {method, "POST"}
      , {body, "test"}
      , {response_headers, [{"Location", "/new-location"}]}
      , {callbacks, [ {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    , {resource_exists, false}
                    , {previously_existed, true}
                    , {moved_permanently, {true, "/new-location"}}
                    ]
        }
      ]
    }
  , { v3l5_moved_temporarily
    , "Resource doesn't exist \n"
      "Method is *not* PUT \n"
      "Resource previously existed \n"
      "Resource is *not* moved permanently\n"
      "Resource is moved temporarily\n"
      "Get a 302 and a location header"
    , [ {response_code, "302"}
      , {method, "POST"}
      , {body, "test"}
      , {response_headers, [{"Location", "/new-location"}]}
      , {callbacks, [ {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    , {resource_exists, false}
                    , {previously_existed, true}
                    , {moved_temporarily, {true, "/new-location"}}
                    ]
        }
      ]
    }
  , { v3m5_is_post
    , "Resource doesn't exist \n"
      "Method is *not* PUT \n"
      "Resource previously existed \n"
      "Resource is *not* moved permanently\n"
      "Resource is *not* moved temporarily\n"
      "Method is *not* POST\n"
      "Get a 410"
    , [ {response_code, "410"}
      , {method, "GET"}
      , {callbacks, [ {resource_exists, false}
                    , {previously_existed, true}
                    ]
        }
      ]
    }
  , { v3n5_allow_post_to_missing_resource
    , "Resource doesn't exist \n"
      "Method is *not* PUT \n"
      "Resource previously existed \n"
      "Resource is *not* moved permanently\n"
      "Resource is *not* moved temporarily\n"
      "Method is POST\n"
      "We arent' allowed to POST to a missing resource\n"
      "Get a 410"
    , [ {response_code, "410"}
      , {method, "POST"}
      , {body, "test"}
      , {callbacks, [ {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    , {resource_exists, false}
                    , {previously_existed, true}
                    , {allow_missing_post, false}
                    ]
        }
      ]
    }
  , { v3m5_is_post
    , "Resource doesn't exist \n"
      "Method is *not* PUT \n"
      "Resource *didn't* exist previously \n"
      "Method is *not* POST\n"
      "Get a 404"
    , [ {response_code, "404"}
      , {method, "GET"}
      , {callbacks, [ {resource_exists, false}
                    , {previously_existed, false}
                    ]
        }
      ]
    }
  , { v3m7_allow_post_to_missing_resource
    , "Resource doesn't exist \n"
      "Method is *not* PUT \n"
      "Resource *didn't* exist previously \n"
      "Method is POST\n"
      "We arent' allowed to POST to a missing resource\n"
      "Get a 404"
    , [ {response_code, "404"}
      , {method, "POST"}
      , {body, "test"}
      , {callbacks, [ {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    , {resource_exists, false}
                    , {previously_existed, false}
                    , {allow_missing_post, false}
                    ]
        }
      ]
    }
  ];
tests(conditional_requests) ->
  [ { v3g8_if_match
    , "Resource exists \n"
      "If-Match header contains an invalid etag \n"
      "Get a 412"
    , [ {response_code, "412"}
      , {request_headers, [{"If-Match", "\"gate\""}]}
      , {callbacks, [ {resource_exists, true}
                    , {generate_etag, <<"\"etag\"">>}
                    ]
        }
      ]
    }
  , { v3h10_if_unmodified_since
    , "Resource exists \n"
      "If-Unmodified-Since is invalid \n"
      "Get a 412"
    , [ {response_code, "412"}
      , {request_headers, [{"If-Unmodified-Since", "\"invalid\""}]}
      , {callbacks, [ {resource_exists, true}
                    ]
        }
      ]
    }
  , { v3h10_if_unmodified_since
    , "Resource exists \n"
      "If-Unmodified-Since is before the resource was modified \n"
      "Get a 412"
    , [ {response_code, "412"}
      , {request_headers, [{"If-Unmodified-Since", "Tue, 11 Dec 2010 10:10:24 GMT"}]}
      , {callbacks, [ {resource_exists, true}
                    , {last_modified, {{2013, 5, 5}, {1, 1, 1}}}
                    ]
        }
      ]
    }
  , { v3i12_if_none_match
    , "Resource exists \n"
      "If-None-Match is * \n"
      "Method is GET \n"
      "Get a 304"
    , [ {response_code, "304"}
      , {request_headers, [{"If-None-Match", "*"}]}
      , {callbacks, [ {resource_exists, true}
                    ]
        }
      ]
    }
  , { v3i12_if_none_match
    , "Resource exists \n"
      "If-None-Match is * \n"
      "Method is HEAD \n"
      "Get a 304"
    , [ {response_code, "304"}
      , {request_headers, [{"If-None-Match", "*"}]}
      , {method, "HEAD"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"HEAD">>]}
                    , {allowed_methods, [<<"HEAD">>]}
                    ]
        }
      ]
    }
  , { v3i12_if_none_match
    , "Resource exists \n"
      "If-None-Match is * \n"
      "Method is *not* HEAD or GET \n"
      "Get a 412"
    , [ {response_code, "412"}
      , {request_headers, [{"If-None-Match", "*"}]}
      , {method, "POST"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    ]
        }
      ]
    }
  , { v3i12_if_none_match
    , "Resource exists \n"
      "If-None-Match header contains matching etag \n"
      "Method is GET \n"
      "Get a 304"
    , [ {response_code, "304"}
      , {request_headers, [{"If-None-Match", "\"etag\""}]}
      , {callbacks, [ {resource_exists, true}
                    , {generate_etag, <<"\"etag\"">>}
                    ]
        }
      ]
    }
  , { v3i12_if_none_match
    , "Resource exists \n"
      "If-None-Match header contains matching etag \n"
      "Method is HEAD \n"
      "Get a 304"
    , [ {response_code, "304"}
      , {request_headers, [{"If-None-Match", "\"etag\""}]}
      , {method, "HEAD"}
      , {callbacks, [ {resource_exists, true}
                    , {generate_etag, <<"\"etag\"">>}
                    , {known_methods, [<<"HEAD">>]}
                    , {allowed_methods, [<<"HEAD">>]}
                    ]
        }
      ]
    }
  , { v3i12_if_none_match
    , "Resource exists \n"
      "If-None-Match header contains matching etag \n"
      "Method is *not* HEAD or GET \n"
      "Get a 412"
    , [ {response_code, "412"}
      , {request_headers, [{"If-None-Match", "\"etag\""}]}
      , {method, "POST"}
      , {callbacks, [ {resource_exists, true}
                    , {generate_etag, <<"\"etag\"">>}
                    , {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    ]
        }
      ]
    }
  , { v3l13_if_modified_since
    , "Resource exists \n"
      "If-Modified-Since is invalid \n"
      "Get a 412"
    , [ {response_code, "412"}
      , {request_headers, [{"If-Modified-Since", "\"invalid\""}]}
      , {callbacks, [ {resource_exists, true}
                    ]
        }
      ]
    }
  , { v3l13_if_modified_since
    , "Resource exists \n"
      "If-Modified-Since is after the resource was modified \n"
      "Get a 304"
    , [ {response_code, "304"}
      , {request_headers, [{"If-Modified-Since", "Tue, 11 Dec 2010 10:10:24 GMT"}]}
      , {callbacks, [ {resource_exists, true}
                    , {last_modified, {{2009, 5, 5}, {1, 1, 1}}}
                    ]
        }
      ]
    }
  ];
tests(delete) ->
  [ { v3m20_delete_enacted
    , "Resource exists \n"
      "Method is DELETE \n"
      "Delete resource returns false \n"
      "Get a 500"
    , [ {response_code, "500"}
      , {method, "DELETE"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"DELETE">>]}
                    , {allowed_methods, [<<"DELETE">>]}
                    , {delete_resource, false}
                    ]
        }
      ]
    }
  , { v3m20_delete_completed
    , "Resource exists \n"
      "Method is DELETE \n"
      "Delete resource returns true \n"
      "Delete is not completed \n"
      "Get a 202"
    , [ {response_code, "202"}
      , {method, "DELETE"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"DELETE">>]}
                    , {allowed_methods, [<<"DELETE">>]}
                    , {delete_resource, true}
                    , {delete_completed, false}
                    ]
        }
      ]
    }
  , { v3o20_response_entity
    , "Resource exists \n"
      "Method is DELETE \n"
      "Delete resource returns true \n"
      "Delete is completed \n"
      "There is no body \n"
      "Get a 204"
    , [ {response_code, "204"}
      , {method, "DELETE"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"DELETE">>]}
                    , {allowed_methods, [<<"DELETE">>]}
                    , {delete_resource, true}
                    , {delete_completed, true}
                    ]
        }
      ]
    }
  , { v3o20_response_entity
    , "Resource exists \n"
      "Method is DELETE \n"
      "Delete resource returns true \n"
      "Delete is completed \n"
      "There is a body \n"
      "No multiple representations \n"
      "Get a 200"
    , [ {response_code, "200"}
      , {method, "DELETE"}
      , {response_body, "deleted"}
      , {set_body, "deleted"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"DELETE">>]}
                    , {allowed_methods, [<<"DELETE">>]}
                    , {delete_resource, true}
                    , {delete_completed, true}
                    ]
        }
      ]
    }
  , { v3o20_response_entity
    , "Resource exists \n"
      "Method is DELETE \n"
      "Delete resource returns true \n"
      "Delete is completed \n"
      "There is a body \n"
      "Multiple representations \n"
      "Get a 300"
    , [ {response_code, "300"}
      , {method, "DELETE"}
      , {response_body, "deleted"}
      , {set_body, "deleted"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"DELETE">>]}
                    , {allowed_methods, [<<"DELETE">>]}
                    , {delete_resource, true}
                    , {delete_completed, true}
                    , {multiple_choices, true}
                    ]
        }
      ]
    }
  ];
tests(body) ->
  [ { v3n16_post
    , "Resource exists \n"
      "Method is POST \n"
      "Post is *not* create \n"
      "process_post not implemented \n"
      "Get a 500"
    , [ {response_code, "500"}
      , {method, "POST"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    ]
        }
      ]
    }
  , { v3n16_post
    , "Resource exists \n"
      "Method is POST \n"
      "Post is *not* create \n"
      "process_post returns false \n"
      "Get a 500"
    , [ {response_code, "500"}
      , {method, "POST"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    , {process_post, false}
                    ]
        }
      ]
    }
  , { v3n16_post
    , "Resource exists \n"
      "Method is POST \n"
      "Post is *not* create \n"
      "process_post returns true and sets location header \n"
      "Get a 201 and Location header is set"
    , [ {response_code, "201"}
      , {method, "POST"}
      , {response_headers, [{"Location", "/new_resource"}]}
      , {set_headers, [{<<"Location">>, <<"/new_resource">>}]}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    , {process_post, true}
                    ]
        }
      ]
    }
  , { v3n16_post
    , "Resource exists \n"
      "Method is POST \n"
      "Post is create \n"
      "create_path not implemented \n"
      "Get a 500"
    , [ {response_code, "500"}
      , {method, "POST"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    , {post_is_create, true}
                    ]
        }
      ]
    }
  , { v3n16_post
    , "Resource exists \n"
      "Method is POST \n"
      "Post is create \n"
      "create_path does not return a path \n"
      "Get a 500"
    , [ {response_code, "500"}
      , {method, "POST"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    , {post_is_create, true}
                    , {create_path, invalid}
                    ]
        }
      ]
    }
  , { v3p11_new_resource
    , "Resource exists \n"
      "Method is POST \n"
      "Post is not create \n"
      "create_path does returns a path \n"
      "Get a 201 with Location header set"
    , [ {response_code, "201"}
      , {method, "POST"}
      , {response_headers, [{"Location", "/new_resource"}]}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"POST">>]}
                    , {allowed_methods, [<<"POST">>]}
                    , {post_is_create, true}
                    , {create_path, <<"/new_resource">>}
                    ]
        }
      ]
    }
  , { v3p3_v3o14_conflict
    , "Resource exists \n"
      "Method is PUT \n"
      "We are in conflict \n"
      "Get a 409"
    , [ {response_code, "409"}
      , {method, "PUT"}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"PUT">>]}
                    , {allowed_methods, [<<"PUT">>]}
                    , {is_conflict, true}
                    ]
        }
      ]
    }
  , { v3p11_new_resource
    , "Resource exists \n"
      "Method is PUT \n"
      "Get a 201 with Location header set"
    , [ {response_code, "201"}
      , {method, "PUT"}
      , {response_headers, [{"Location", "/new_resource"}]}
      , {set_headers, [{<<"Location">>, <<"/new_resource">>}]}
      , {callbacks, [ {resource_exists, true}
                    , {known_methods, [<<"PUT">>]}
                    , {allowed_methods, [<<"PUT">>]}
                    ]
        }
      ]
    }
  , { v3o18_body
    , "Resource exists \n"
      "Method is GET \n"
      "Get a 200 with all headers set"
    , [ {response_code, "200"}
      , {request_headers, [ {"Accept-Language", "ru"}
                          , {"Accept", "some/type, text/html"}
                          , {"Accept-Charset", "iso-8859-5, utf-8"}
                          , {"Accept-Encoding", "deflate, identity"}
                          ]
        }
      , {response_headers, [ {"Content-Type", "text/html; charset=utf-8"}
                           , {"Content-Encoding", "identity"}
                           , {"Content-Language", "ru"}
                           , {"Content-Length", "20"}
                           , {"Expires", "Fri, 01 Jan 2100 01:01:01 GMT"}
                           , {"Etag", "\"Etag\""}
                           ]
        }
      , {response_body, "hellohellohellohello"}
      , {callbacks, [ {resource_exists, true}
                    , {generate_etag, "\"Etag\""}
                    , {languages_provided, [<<"en-us">>, <<"ru">>]}
                    , {charsets_provided, [{<<"utf-8">>, fun(X) -> [X, X] end}]}
                    , {encodings_provided, [{<<"identity">>, fun(X) -> [X, X] end}]}
                    , {content_types_provided, [{{<<"text">>, <<"html">>, []}, to_html}]}
                    , {expires, {{2100, 1, 1}, {1,1,1}}}
                    , {to_html, <<"hello">>}
                    ]
        }
      ]
    }
  ].


%%_* Utility functions =========================================================

run(Config) ->
  Tests = proplists:get_value(tests, Config),
  lists:foldl(fun run_test/2, Config, Tests).

run_test({Test, Description, TestConfig}, Config) ->
  Blueprint = filename:join([proplists:get_value(priv_dir, Config)
                             , "webmachine.apib"
                            ]),
  update_blueprint(Blueprint, Test, Description, TestConfig),

  mock_webmachine_resource(TestConfig),
  Params = [{hostname, "localhost"}, {port, 8080}],
  Result = katt:run(Blueprint, Params),
  validate_webmachine_calls(TestConfig),
  unmock_webmachine_resource(),
  ?assertMatch({pass, _, _, _, _}, Result),
  Config.

update_blueprint(Blueprint, Test, Description, Config) ->
  {ok, File} = file:open(Blueprint, [write]),
  Data0 = [ "--- ", katt_util:to_list(Test), " ---"
          , $\n
          , $\n
          , "---"
          , $\n, katt_util:to_list(Description)
          , $\n, $\n, "Mocked callbacks: ", $\n
          , [  io_lib:format("~p: ~p~n", [Callback, Value])
            || {Callback, Value} <- proplists:get_value(callbacks, Config)]
          , $\n, "---"
          , $\n
          , $\n
          ,  proplists:get_value(method, Config), " /", $\n
          , [  ["> ", H, ": ", V, $\n]
            || {H, V} <- proplists:get_value(request_headers, Config)
            ]
          , case proplists:get_value(body, Config, "") of
              "" -> "";
              Body -> [Body, $\n]
            end
          ,"< ", proplists:get_value(response_code, Config), $\n
          , [  ["< ", H, ": ", V, $\n]
            || {H, V} <- proplists:get_value(response_headers, Config)
            ]
          , case proplists:get_value(response_body, Config, "") of
              "" -> "";
              Body -> [Body, $\n]
            end
          ],
  Data = list_to_binary(lists:flatten(Data0)),
  ct:pal("~s~n", [Data]),
  file:write(File, Data),
  file:close(File),
  Config.

update_test_config({Test, Description, TestConfig}, Acc) ->
  Config = lists:foldl( fun({callbacks, C}, Cfg) ->
                            Cbs0 = proplists:get_value(callbacks, Cfg),
                            Cbs = katt_util:merge_proplists(Cbs0, C),
                            lists:keystore(callbacks, 1, Cfg, {callbacks, Cbs});
                           ({Key, _} = New, Cfg) ->
                            lists:keystore(Key, 1, Cfg, New)
                        end
                      , defaults()
                      , TestConfig
                      ),
  [{Test, Description, Config} | Acc].

mock_webmachine_resource(Config) ->
  meck:new(unrest_wm_service_example),
  Callbacks = proplists:get_value(callbacks, Config),
  [  meck:expect(unrest_wm_service_example, Callback, 2, respond(Response, Config))
  || {Callback, Response} <- Callbacks
  ],
  meck:expect(unrest_wm_service_example, init, 1, {ok, []}).

unmock_webmachine_resource() ->
  meck:unload(unrest_wm_service_example).

validate_webmachine_calls(Config) ->
  Callbacks = proplists:get_value(callbacks, Config),
  [ case meck:num_calls(unrest_wm_service_example, Callback, 2) of
      N when N > 0  -> ok;
      N -> ?assertEqual({1, Callback}, {N, Callback})
    end
  || {Callback, _} <- Callbacks
  ].

respond({Type, _} = ErrorOrHalt, _) when Type =:= error
                                       ; Type =:= halt ->
  fun(_, _) -> ErrorOrHalt end;
respond(Response, Config) ->
  fun(ReqData0, Context) ->
    ReqData1 = case proplists:get_value(set_body, Config) of
                 undefined -> ReqData0;
                 Body      -> cowboy_req:set_resp_body(Body, ReqData0)
               end,
    ReqData = case proplists:get_value(set_headers, Config) of
                undefined -> ReqData1;
                Headers -> lists:foldl( fun({H, V}, Req) ->
                                          cowboy_req:set_resp_header(H, V, Req)
                                        end
                                      , ReqData1
                                      , Headers
                                      )
              end,
    {Response, ReqData, Context}
  end.

identity(X) -> X.

defaults() ->
  [ {method, "GET"}
  , {response_code, "200"}
  , {request_headers, []}
  , {response_headers, []}
  , {callbacks, []
    }
  ].
