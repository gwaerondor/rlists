-module(rlists_tests).
-include_lib("eunit/include/eunit.hrl").

intersperse_test_() ->
    [fun intersperse_with_empty_list_is_identity/0,
     fun intersperse_with_one_element_is_identity/0,
     fun intersperse_with_some_elements/0].

intersperse_with_empty_list_is_identity() ->
    Result = rlists:intersperse(something, []),
    ?assertEqual([], Result).

intersperse_with_one_element_is_identity() ->
    Result = rlists:intersperse(ladies, [gentlemen]),
    ?assertEqual([gentlemen], Result).

intersperse_with_some_elements() ->
    Result = rlists:intersperse($\s, "NOPE"),
    ?assertEqual("N O P E", Result).

intercalate_test_() ->
    [fun intercalate_with_empty_list_is_identity/0,
     fun intercalate_with_one_element_does_not_insert/0,
     fun intercalate_with_several_elements/0
    ].

intercalate_with_empty_list_is_identity() ->
    Result = rlists:intercalate("DOINK", []),
    ?assertEqual([], Result).

intercalate_with_one_element_does_not_insert() ->
    Result = rlists:intercalate("!", ["DOINK"]),
    ?assertEqual("DOINK", Result).

intercalate_with_several_elements() ->
    Strings = string:tokens("This is Sparta!", " "),
    Result = rlists:intercalate(" - ", Strings),
    Expected = "This - is - Sparta!",
    ?assertEqual(Expected, Result).

take_test_() ->
    [fun take_from_empty_list_is_identity/0,
     fun take_more_than_available_is_identity/0,
     fun take_entire_list_is_identity/0,
     fun take_a_few_elements_from_list/0].

take_from_empty_list_is_identity() ->
    ?assertEqual([], rlists:take(5, [])).

take_more_than_available_is_identity() ->
    List = [1, 2, 3],
    Result = rlists:take(10, List),
    ?assertEqual(List, Result).

take_entire_list_is_identity() ->
    List = [a, b, c],
    Result = rlists:take(3, List),
    ?assertEqual(List, Result).

take_a_few_elements_from_list() ->
    List = "hello world",
    ?assertEqual("hello", rlists:take(5, List)).

drop_test_() ->
    [fun drop_from_empty_list_is_identity/0,
     fun drop_more_than_available_results_in_empty_list/0,
     fun drop_entire_length_results_in_empty_list/0,
     fun drop_a_few_elements/0].

drop_from_empty_list_is_identity() ->
    ?assertEqual([], rlists:drop(100, [])).

drop_more_than_available_results_in_empty_list() ->
    ?assertEqual([], rlists:drop(42, "Answer")).

drop_entire_length_results_in_empty_list() ->
    List = "hello",
    ?assertEqual([], rlists:drop(length(List), List)).

drop_a_few_elements() ->
    List = "This is not a short list",
    ?assertEqual("short list", rlists:drop(14, List)).

chunks_test_() ->
    [fun chunking_an_empty_list_is_identity/0,
     fun chunking_more_than_entire_list_results_in_one_chunk/0,
     fun chunking_full_list_results_in_one_chunk/0,
     fun chunking_into_perfect_pieces/0,
     fun chunking_into_imperfect_pieces_has_remainder_in_final_chunk/0].

chunking_an_empty_list_is_identity() ->
    ?assertEqual([], rlists:chunks(10, [])).

chunking_more_than_entire_list_results_in_one_chunk() ->
    ?assertEqual(["abc"], rlists:chunks(10, "abc")).

chunking_full_list_results_in_one_chunk() ->
    ?assertEqual(["def"], rlists:chunks(3, "def")).

chunking_into_perfect_pieces() ->
    String = "123456789",
    Result = rlists:chunks(3, String),
    ?assertEqual(["123", "456", "789"], Result).

chunking_into_imperfect_pieces_has_remainder_in_final_chunk() ->
    String = "abc, def, ghi",
    Expected = ["abc, ", "def, ", "ghi"],
    Result = rlists:chunks(5, String),
    ?assertEqual(Expected, Result).

init_test_() ->
    [fun init_of_empty_list_results_in_badarg/0,
     fun init_of_one_element_is_empty_list/0,
     fun init_of_longer_list/0].

init_of_empty_list_results_in_badarg() ->
    ?assertError(badarg, rlists:init([])).

init_of_one_element_is_empty_list() ->
    Result = rlists:init([a]),
    ?assertEqual([], Result).

init_of_longer_list() ->
    Result = rlists:init("Hello, world!"),
    ?assertEqual("Hello, world", Result).

iterate_test_() ->
    [fun zero_iterations_returns_empty_list/0,
     fun one_iteration_returns_only_original/0,
     fun several_iterations/0].

double(X) -> X * 2.

zero_iterations_returns_empty_list() ->
    Iterations = 0,
    Result = rlists:iterate(fun double/1, 10, Iterations),
    ?assertEqual([], Result).

one_iteration_returns_only_original() ->
    Iterations = 1,
    Result = rlists:iterate(fun double/1, 10, Iterations),
    ?assertEqual([10], Result).

several_iterations() ->
    Iterations = 8,
    Result = rlists:iterate(fun double/1, 1, Iterations),
    Expected = [1, 2, 4, 8, 16, 32, 64, 128],
    ?assertEqual(Expected, Result).

span_test_() ->
    [fun span_of_empty_list_has_no_results_on_either_side/0,
     fun span_of_all_elements/0,
     fun span_of_no_elements/0,
     fun span_of_all_elements_at_beginning_of_list/0,
     fun span_of_elements_at_beginning_of_list_with_more_at_the_end/0,
     fun span_with_more_occurrences/0,
     fun span_with_elements_only_at_the_end/0].

even(X) ->
    X rem 2 == 0.

span_of_empty_list_has_no_results_on_either_side() ->
    Result = rlists:span(fun even/1, []),
    ?assertEqual({[], []}, Result).

span_of_all_elements() ->
    Result = rlists:span(fun even/1, [2, 4, 6, 8]),
    ?assertEqual({[2, 4, 6, 8], []}, Result).

span_of_no_elements() ->
    Result = rlists:span(fun even/1, [1, 3, 5, 7]),
    ?assertEqual({[], [1, 3, 5, 7]}, Result).

span_of_all_elements_at_beginning_of_list() ->
    Result = rlists:span(fun even/1, [2, 4, 1, 1]),
    ?assertEqual({[2, 4], [1, 1]}, Result).

span_of_elements_at_beginning_of_list_with_more_at_the_end() ->
    List = [2, 4, 1, 1, 2, 4],
    Result = rlists:span(fun even/1, List),
    Expected = {[2, 4], [1, 1, 2, 4]},
    ?assertEqual(Expected, Result).

span_with_more_occurrences() ->
    List = [2, 2, 1, 1, 2, 2, 1, 1],
    Result = rlists:span(fun even/1, List),
    Expected = {[2, 2], [1, 1, 2, 2, 1, 1]},
    ?assertEqual(Expected, Result).

span_with_elements_only_at_the_end() ->
    List = [1, 1, 2, 2],
    Result = rlists:span(fun even/1, List),
    Expected = {[], [1, 1, 2, 2]},
    ?assertEqual(Expected, Result).

stripprefix_test_() ->
    [fun non_matching_prefix_is_identity/0,
     fun prefix_after_beginning_does_not_count/0,
     fun prefix_matches_only_partially/0,
     fun prefix_matches_entire_list/0,
     fun prefix_matches_beginning_of_list/0].

non_matching_prefix_is_identity() ->
    Result = rlists:stripprefix("abc", "Hello"),
    ?assertEqual("Hello", Result).

prefix_after_beginning_does_not_count() ->
    Result = rlists:stripprefix("world", "Hello, world!"),
    ?assertEqual("Hello, world!", Result).

prefix_matches_only_partially() ->
    Result = rlists:stripprefix("aa", "abc"),
    ?assertEqual("abc", Result).

prefix_matches_entire_list() ->
    Result = rlists:stripprefix("abc", "abc"),
    ?assertEqual("", Result).

prefix_matches_beginning_of_list() ->
    Result = rlists:stripprefix("123", "123456"),
    ?assertEqual("456", Result).

stripsuffix_test_() ->
    [fun non_matching_suffix_is_identity/0,
     fun suffix_before_end_does_not_count/0,
     fun suffix_matches_only_partially/0,
     fun suffix_matches_entire_list/0,
     fun suffix_matches_end_of_list/0].

non_matching_suffix_is_identity() ->
    Result = rlists:stripsuffix("456", "123abc"),
    ?assertEqual("123abc", Result).

suffix_before_end_does_not_count() ->
    Result = rlists:stripsuffix("123", "123456"),
    ?assertEqual("123456", Result).

suffix_matches_only_partially() ->
    Result = rlists:stripsuffix("457", "123456"),
    ?assertEqual("123456", Result).

suffix_matches_entire_list() ->
    Result = rlists:stripsuffix("123", "123"),
    ?assertEqual([], Result).

suffix_matches_end_of_list() ->
    Result = rlists:stripsuffix("def", "abcdef"),
    ?assertEqual("abc", Result).

infix_test_() ->
    [fun infix_not_in_list/0,
     fun infix_as_prefix/0,
     fun infix_as_suffix/0,
     fun infix_as_infix/0].

infix_not_in_list() ->
    ?assertNot(rlists:infix("bye", "Hello there")).

infix_as_prefix() ->
    ?assert(rlists:infix("Hello", "Hello there")).

infix_as_suffix() ->
    ?assert(rlists:infix("there", "Hello there")).

infix_as_infix() ->
    ?assert(rlists:infix("def", "abc def ghi")).

group_test_() ->
    [fun group_of_no_elements_is_identity/0,
     fun group_of_one_element/0,
     fun group_of_many_singles/0,
     fun group_of_several/0,
     fun mixed_groups/0].

group_of_no_elements_is_identity() ->
    ?assertEqual([], rlists:group([])).

group_of_one_element() ->
    Result = rlists:group([1]),
    ?assertEqual([[1]], Result).

group_of_many_singles() ->
    Result = rlists:group("abcde"),
    ?assertEqual(["a", "b", "c", "d", "e"], Result).

group_of_several() ->
    Result = rlists:group([1, 2, 2, 3, 3, 3]),
    ?assertEqual([[1], [2, 2], [3, 3, 3]], Result).

mixed_groups() ->
    Result = rlists:group("Mississippi"),
    Expected = ["M", "i", "ss", "i", "ss", "i", "pp", "i"],
    ?assertEqual(Expected, Result).

inits_test_() ->
    [fun inits_of_empty_list_is_empty/0,
     fun inits_of_single_element_list/0,
     fun inits_of_many_element_list/0].

inits_of_empty_list_is_empty() ->
    Result = rlists:inits([]),
    ?assertEqual([[]], Result).

inits_of_single_element_list() ->
    Result = rlists:inits([1]),
    ?assertEqual([[], [1]], Result).

inits_of_many_element_list() ->
    Result = rlists:inits("abc"),
    Expected = ["", "a", "ab", "abc"],
    ?assertEqual(Expected, Result).

tails_test_() ->
    [fun tails_of_empty_list_is_empty/0,
     fun tails_of_single_element_list/0,
     fun tails_of_many_element_list/0].

tails_of_empty_list_is_empty() ->
    Result = rlists:tails([]),
    ?assertEqual([[]], Result).

tails_of_single_element_list() ->
    Result = rlists:tails([dood]),
    ?assertEqual([[dood], []], Result).

tails_of_many_element_list() ->
    Result = rlists:tails("COOL"),
    Expected = ["COOL", "OOL", "OL", "L", ""],
    ?assertEqual(Expected, Result).
