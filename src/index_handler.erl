-module(index_handler).

-export([before_/2]).
-export([index_/4]).

before_(_X, Req) ->
    case giallo_session:exists(Req) of
        true  -> {ok, []};
        false ->
            Req1 = giallo_session:new(Req),
            giallo_session:set(<<"X">>, <<"Nothing">>, Req1),
            {ok, [], Req1}
    end.

index_(<<"GET">>, [], _Extra, Req) ->
    case giallo:query_param(<<"X">>, Req) of
        undefined -> ok;
        Val       -> giallo_session:set(<<"X">>, Val, Req)
    end,
    {ok, [{<<"session_param">>, giallo_session:get(<<"X">>, Req)}, Req]};

index_(<<"POST">>, [], _Extra, Req) ->
  Z = case giallo:post_param(<<"oi">>, Req) of
        Val -> process(Val);
        _   -> ""
      end,
  {ok, [{<<"oi">>, Z}]}.


process(Val) ->
%%  io:format("===============~n~p~n====================", [Val]),
  F = fun(File) ->
          file:write_file(File, [Val]),
          Path = filename:join([code:priv_dir(como), "run_pandoc.sh"]),
          Cmd = [ Path
                , File
                ],
          {0, Data} = mochiweb_util:cmd_status(Cmd),
          io:format("===============~n~p~n====================", [Val]),
          Data
      end,
  {ok, D} = tulib_fs:with_temp_file(F),
  D.
