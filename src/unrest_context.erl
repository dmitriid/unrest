%%%=============================================================================
%%% @doc Context is an opaque data structure used to store lots of useful data
%%%      That needs to be passed around during calls, chained flows etc.
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%% @copyright 2013 Klarna AB, API team
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
-export([ add_error/2
        ]).

%%_* Types =====================================================================

-record(context, { data   = orddict:new()
                 , errors = []
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
  try
    Value = orddict:fetch(Key, Data),
    {ok, Value}
  catch
    _:_ ->
      {error, not_found}
  end.

-spec get(any(), context(), any()) -> {ok, any()}.
get(Key, #context{data = Data}, Default) ->
  case get(Key, Data) of
    {ok, Value}        -> {ok, Value};
    {error, not_found} -> {ok, Default}
  end.

-spec multiget([any()], context()) -> {ok, [any() | {error, not_found}]}.
multiget(Keys, Context) ->
  [get(Key, Context) || Key <- Keys].

-spec set(any(), any(), context()) -> {ok, context()}.
set(Key, Value, #context{data = Data} = Context) ->
  {ok, Context#context{data = orddict:store(Key, Value, Context)}}.

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
errors(#context{errors = Errors}) -> Errors.

-spec add_error(any(), context()) -> {ok, context()}.
add_error(Error, #context{errors = Errors} = Context) ->
  {ok, Context#context{errors = [Error | Errors]}}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
