-module(rlists).

-export([intersperse/2,
	 intercalate/2,
	 iterate/3,
	 take/2,
	 drop/2,
	 chunks/2,
	 init/1,
	 inits/1,
	 tails/1,
	 span/2,
	 stripprefix/2,
	 stripsuffix/2,
	 infix/2,
	 group/1]).

intersperse(_, []) ->
    [];
intersperse(_, [H]) ->
    [H];
intersperse(E, [H|T]) ->
    [H, E | intersperse(E, T)].

intercalate(Inters, Lists) ->
    lists:concat(rlists:intersperse(Inters, Lists)).

take(0, _) ->
    [];
take(_, []) ->
    [];
take(N, [H|T]) ->
    [H | take(N - 1, T)].

drop(0, L) ->
    L;
drop(_, []) ->
    [];
drop(N, [_|T]) ->
    drop(N-1, T).

chunks(_, []) ->
    [];
chunks(N, L) when N < length(L) ->
    Remainder = drop(N, L),
    [take(N, L) | chunks(N, Remainder)];
chunks(_, L) ->
    [L].

init([]) ->
    erlang:error(badarg);
init([_ | []]) ->
    [];
init([H | T]) ->
    [H | init(T)].

iterate(X, Fun, N) when N > 0 ->
    [X | iterate(Fun(X), Fun, N - 1)];
iterate(_, _, _) ->
    [].

span(Pred, List) ->
    {lists:takewhile(Pred, List),
     lists:dropwhile(Pred, List)}.

stripprefix(Prefix, List) ->
    case lists:prefix(Prefix, List) of
	true ->
	    List -- Prefix;
	false ->
	    List
    end.

stripsuffix(Suffix, List) ->
    lists:reverse(rlists:stripprefix(lists:reverse(List),
				     lists:reverse(Suffix))).

infix(Infix, List) when length(List) < length(Infix) ->
    false;
infix(Infix, List) ->
    case lists:prefix(Infix, List) of
	true ->
	    true;
	false ->
	    infix(Infix, tl(List))
    end.
			   

group([]) ->
    [];
group([H|T]) ->
    Pred = fun(X) -> X =:= H end,
    {Group, Rest} = rlists:span(Pred, T),
    [[H | Group] | group(Rest)].

inits(L) ->
    inits(L, 0, length(L)).

inits(L, Stop, Stop) ->
    [L];
inits(L, N, Stop) ->
    [rlists:take(L, N) | inits(L, N + 1, Stop)].

tails([]) ->
    [[]];
tails(L) ->
    [L | tails(tl(L))].
