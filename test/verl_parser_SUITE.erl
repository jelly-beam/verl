-module(verl_parser_SUITE).

-compile(export_all).
-hank([{unnecessary_function_arguments, all}]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [parse_version_test, parse_requirement_test].

% match against opaque
-dialyzer({[no_opaque, no_return], parse_version_test/1}).
parse_version_test(_Cfg) ->
    {ok, {1, 2, 3, [], []}} = verl_parser:parse_version(<<"1.2.3">>),
    {ok, {1, 4, 5, [], [<<"ignore">>]}} = verl_parser:parse_version(<<"1.4.5+ignore">>),
    {ok, {0, 0, 1, [], [<<"sha">>, <<"0702245">>]}} = verl_parser:parse_version(
        <<"0.0.1+sha.0702245">>
    ),
    {ok, {1, 4, 5, [<<"6-g3318bd5">>], []}} =
        verl_parser:parse_version(<<"1.4.5-6-g3318bd5">>),
    {ok, {1, 4, 5, [6, 7, <<"eight">>], []}} =
        verl_parser:parse_version(<<"1.4.5-6.7.eight">>),
    {ok, {1, 4, 5, [<<"6-g3318bd5">>], [<<"ignore">>]}} =
        verl_parser:parse_version(<<"1.4.5-6-g3318bd5+ignore">>),
    ExpError = {error, invalid_version},
    ExpError = verl_parser:parse_version(<<"foobar">>),
    ExpError = verl_parser:parse_version(<<"2">>),
    ExpError = verl_parser:parse_version(<<"2.">>),
    ExpError = verl_parser:parse_version(<<"2.3">>),
    ExpError = verl_parser:parse_version(<<"2.3.">>),
    ExpError = verl_parser:parse_version(<<"2.3.0-">>),
    ExpError = verl_parser:parse_version(<<"2.3.0+">>),
    ExpError = verl_parser:parse_version(<<"2.3.0.">>),
    ExpError = verl_parser:parse_version(<<"2.3.0.4">>),
    ExpError = verl_parser:parse_version(<<"2.3.-rc.1">>),
    ExpError = verl_parser:parse_version(<<"2.3.+rc.1">>),
    ExpError = verl_parser:parse_version(<<"2.3.0-01">>),
    ExpError = verl_parser:parse_version(<<"2.3.00-1">>),
    ExpError = verl_parser:parse_version(<<"2.3.00">>),
    ExpError = verl_parser:parse_version(<<"2.03.0">>),
    ExpError = verl_parser:parse_version(<<"02.3.0">>),
    ExpError = verl_parser:parse_version(<<"0. 0.0">>),
    ExpError = verl_parser:parse_version(<<"0.1.0-&&pre">>).

parse_requirement_test(_Cfg) ->
    ExpSpec0 = [
        {
            {'$1', '$2', '$3', '$4', '$5'},
            [{'==', {{'$1', '$2', '$3', '$4'}}, {const, {1, 2, 3, []}}}],
            ['$_']
        }
    ],
    {ok, ExpSpec0} = verl_parser:parse_requirement(<<"1.2.3">>),
    ExpSpec1 = [
        {
            {'$1', '$2', '$3', '$4', '$5'},
            [{'/=', {{'$1', '$2', '$3', '$4'}}, {const, {1, 2, 3, []}}}],
            ['$_']
        }
    ],
    {ok, ExpSpec1} = verl_parser:parse_requirement(<<"!= 1.2.3">>),
    {ok, _} = verl_parser:parse_requirement(<<"~> 1.2.3">>),
    {ok, _} = verl_parser:parse_requirement(<<"<= 1.2.3">>),
    ExpErr = {error, invalid_requirement},
    ExpErr = verl_parser:parse_requirement(<<>>),
    ExpErr = verl_parser:parse_requirement(<<"and 2.1.0 and 2.1.1">>),
    ExpErr = verl_parser:parse_requirement(<<"2.1.1 or">>),
    ExpErr = verl_parser:parse_requirement(<<" and !">>),
    ExpErr = verl_parser:parse_requirement(<<" ! and">>).
