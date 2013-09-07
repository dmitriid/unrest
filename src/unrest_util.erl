%%%=============================================================================
%%% @doc Snippets of code that tend to appear all over the place
%%%
%%% @author Dmitrii Dimandt <dmitrii@dmitriid.com>
%%%=============================================================================
-module(unrest_util).

%%_* Exports ===================================================================

-export([ binary_to_atom/1
        ]).

%%_* API =======================================================================

-spec binary_to_atom(binary()) -> list().
binary_to_atom(B) when is_binary(B) ->
  list_to_atom(binary_to_list(B)).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
