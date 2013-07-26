%%%=============================================================================
%%% @doc Context is an opaque data structure used to store lots of useful data
%%%      That needs to be passed around during calls, chained flows etc.
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%=============================================================================
-module(unrest_context).

%%_* Exports ===================================================================
%%_* High-level API ------------------------------------------------------------
-export([ new/0
        , new/1
        ]).

-export([ get/2
        , get/3
        , multiget/2
        ]).

-export([ set/3
        , multiset/2
        ]).

-export([ is_error_state/1
        , errors/1
        ]).

%%_* Low-level API =------------------------------------------------------------
-export([ errors_push/2
        , callstack/1
        , callstack_push/2
        ]).

%%_* Includes ==================================================================
-include_lib("eunit/include/eunit.hrl").

%%_* Types =====================================================================

-record(context, { data   = orddict:new()
                 , errors = []
                 , callstack = []
                 }).
-opaque context() :: #context{}.
-export_type([context/0]).

%%_* API =======================================================================
-spec new() -> context().
new() ->
  #context{}.

-spec new(proplists:proplist()) -> context().
new(List) ->
  Data = orddict:from_list(List),
  #context{data = Data}.

-spec get(any(), context()) -> {ok, any()} | {error, not_found}.
get(Key, #context{data = Data}) ->
  case orddict:find(Key, Data) of
    error       -> {error, not_found};
    {ok, Value} -> {ok, Value}
  end.

-spec get(any(), context(), any()) -> {ok, any()}.
get(Key, Context, Default) ->
  case get(Key, Context) of
    {ok, Value}        -> {ok, Value};
    {error, not_found} -> {ok, Default}
  end.

-spec multiget([any()], context()) -> {ok, [any() | {error, not_found}]}.
multiget(Keys, Context) ->
  F = fun(Key, Ctx) ->
          case get(Key, Ctx) of
            {error, _} = Error -> Error;
            {ok, Value}        -> Value
          end
      end,
  {ok, [F(Key, Context) || Key <- Keys]}.

-spec set(any(), any(), context()) -> {ok, context()}.
set(Key, Value, #context{data = Data} = Context) ->
  {ok, Context#context{data = orddict:store(Key, Value, Data)}}.

-spec multiset(proplists:proplist(), context()) -> {ok, context()}.
multiset(List, Context) ->
  {ok, lists:foldl( fun({Key, Value}, Ctx) ->
                        {ok, UpdatedCtx} = set(Key, Value, Ctx),
                        UpdatedCtx
                    end
                  , Context
                  , List
                  )
  }.

-spec is_error_state(context()) -> boolean().
is_error_state(#context{errors = []}) -> false;
is_error_state(#context{})            -> true.

-spec errors(context()) -> list().
errors(#context{errors = Errors}) -> {ok, Errors}.

-spec errors_push(any(), context()) -> {ok, context()}.
errors_push(Error, #context{errors = Errors} = Context) ->
  {ok, Context#context{errors = [Error | Errors]}}.

-spec callstack(context()) -> {ok, list()}.
callstack(#context{callstack = Callstack}) ->
  {ok, Callstack}.

-spec callstack_push(any(), context()) -> {ok, context()}.
callstack_push(Call, #context{callstack = Callstack} = Context) ->
  {ok, Context#context{callstack = [Call | Callstack]}}.

%%_* Unit tests ================================================================
-ifdef(EUNIT).

%%_* Test definitions ----------------------------------------------------------
new_test_() ->
  [ { "Testing new/0"
    , fun test_new_0/0
    }
  , { "Testing new/1"
    , fun test_new_1/0
    }
  ].

get_test_() ->
  [ { "Testing get/2"
    , fun test_get_2/0
    }
  , { "Testing get/3"
    , fun test_get_3/0
    }
  , { "Testing multiget/2"
    , fun test_multiget_2/0
    }
  ].

errors_test_() ->
  [ { "Testing errors/1"
    , fun test_errors_1/0
    }
  , { "Testing errors_push/2"
    , fun test_errors_push_2/0
    }
  ].

callstack_test_() ->
  [ { "Testing callstack/1"
    , fun test_callstack_1/0
    }
  , { "Testing callstack_push/2"
    , fun test_callstack_push_2/0
    }
  ].

%%_* Test implementations ------------------------------------------------------

test_new_0() ->
  Ctx = new(),
  ?assertMatch(#context{}, Ctx),
  ?assertMatch([], Ctx#context.errors),
  ?assertEqual(orddict:new(), Ctx#context.data).

test_new_1() ->
  Data = [{test, "test"}],
  Ctx  = new(Data),
  ?assertMatch(#context{}, Ctx),
  ?assertMatch([], Ctx#context.errors),
  ?assertEqual(orddict:from_list(Data), Ctx#context.data).

test_get_2() ->
  Ctx = new([{key1, "value 1"}, {[key, 2], <<"value 2">>}]),
  ?assertEqual({ok, "value 1"}, get(key1, Ctx)),
  ?assertEqual({ok, <<"value 2">>}, get([key, 2], Ctx)).

test_get_3() ->
  Ctx = new(),
  ?assertEqual({ok, "value 1"}, get(key1, Ctx, "value 1")).

test_multiget_2() ->
  Ctx = new([{key1, "value 1"}, {[key, 2], <<"value 2">>}]),
  ?assertEqual( {ok, ["value 1", <<"value 2">>]}
              , multiget([key1, [key, 2]], Ctx)).

test_errors_1() ->
  Ctx = (new())#context{errors = [1, 3, 2]},
  {ok, Errors} = errors(Ctx),
  ?assertEqual([1, 3, 2], Errors).

test_errors_push_2() ->
  Ctx = (new())#context{errors = [1, 3, 2]},
  {ok, NewCtx} = errors_push(4, Ctx),
  {ok, Errors} = errors(NewCtx),
  ?assertEqual([4, 1, 3, 2], Errors).

test_callstack_1() ->
  Ctx = (new())#context{callstack = [1, 3, 2]},
  {ok, Callstack} = callstack(Ctx),
  ?assertEqual([1, 3, 2], Callstack).

test_callstack_push_2() ->
  Ctx = (new())#context{callstack = [1, 3, 2]},
  {ok, NewCtx} = callstack_push(4, Ctx),
  {ok, Callstack} = callstack(NewCtx),
  ?assertEqual([4, 1, 3, 2], Callstack).

-endif. %% EUNIT
%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
