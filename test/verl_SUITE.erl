-module(verl_SUITE).

-compile(export_all).
-hank([{unnecessary_function_arguments, all}]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

all() ->
    [
        between_test,
        compare_test,
        eq_test,
        gt_test,
        gte_test,
        lt_test,
        lte_test,
        parse_test,
        parse_requirement_test,
        compile_requirement_test,
        is_match_test
    ].

between_test(_Config) ->
    ?assertMatch(
        true,
        verl:between(
            <<"1.0.0-alpha">>,
            <<"1.0.0-alpha.3">>,
            <<"1.0.0-alpha.2">>
        )
    ),
    ?assertMatch(
        true,
        verl:between(
            <<"1.0.0-alpha.1">>,
            <<"1.0.0-beta.2">>,
            <<"1.0.0-alpha.25">>
        )
    ),
    ?assertMatch(
        true,
        verl:between(
            <<"1.0.0-beta.2">>,
            <<"1.0.0-beta.11">>,
            <<"1.0.0-beta.7">>
        )
    ),
    ?assertMatch(
        true,
        verl:between(
            <<"1.0.0-pre-alpha.2">>,
            <<"1.0.0-pre-alpha.11">>,
            <<"1.0.0-pre-alpha.7">>
        )
    ),
    ?assertMatch(
        true,
        verl:between(
            <<"1.0.0-beta.11">>,
            <<"1.0.0-rc.3">>,
            <<"1.0.0-rc.1">>
        )
    ),
    ?assertMatch(
        true,
        verl:between(
            <<"1.0.0-rc.1">>,
            <<"1.0.0-rc.1+build.3">>,
            <<"1.0.0-rc.1+build.1">>
        )
    ),

    ?assertMatch(
        true,
        verl:between(
            <<"1.0.0-rc.1+build.1">>,
            <<"1.0.0">>,
            <<"1.0.0-rc.33">>
        )
    ),
    ?assertMatch(
        true,
        verl:between(
            <<"1.0.0">>,
            <<"1.0.0+0.3.7">>,
            <<"1.0.0+0.2">>
        )
    ),
    ?assertMatch(
        true,
        verl:between(
            <<"1.3.7+build">>,
            <<"1.3.7+build.2.b8f12d7">>,
            <<"1.3.7+build.1">>
        )
    ),
    ?assertMatch(
        true,
        verl:between(
            <<"1.3.7+build.2.b8f12d7">>,
            <<"1.3.7+build.11.e0f985a">>,
            <<"1.3.7+build.10.a36faa">>
        )
    ),
    ?assertMatch(
        true,
        verl:between(
            <<"1.0.0-alpha">>,
            <<"1.0.0-alpha">>,
            <<"1.0.0-alpha">>
        )
    ),
    ?assertMatch(
        true,
        not verl:between(
            <<"1.0.0-alpha.1">>,
            <<"1.0.0-alpha.22">>,
            <<"1.0.0">>
        )
    ),
    ?assertMatch(
        true,
        not verl:between(
            <<"1.0.0-pre-alpha.1">>,
            <<"1.0.0-pre-alpha.22">>,
            <<"1.0.0">>
        )
    ),
    ?assertMatch(
        true,
        not verl:between(
            <<"1.0.0-beta.1">>,
            <<"1.0.0-beta.11">>,
            <<"1.0.0-alpha">>
        )
    ),
    ?assertMatch(
        true,
        not verl:between(
            <<"1.0.0-beta.11">>,
            <<"1.0.0-rc.1">>,
            <<"1.0.0-rc.22">>
        )
    ).

compare_test(_Cfg) ->
    gt = verl:compare(<<"1.0.1">>, <<"1.0.0">>),
    gt = verl:compare(<<"1.1.0">>, <<"1.0.1">>),
    gt = verl:compare(<<"2.1.1">>, <<"1.2.2">>),
    gt = verl:compare(<<"1.0.0">>, <<"1.0.0-dev">>),
    gt = verl:compare(<<"1.2.3-dev">>, <<"0.1.2">>),
    gt = verl:compare(<<"1.0.0-a.b">>, <<"1.0.0-a">>),
    gt = verl:compare(<<"1.0.0-b">>, <<"1.0.0-a.b">>),
    gt = verl:compare(<<"1.0.0-a">>, <<"1.0.0-0">>),
    gt = verl:compare(<<"1.0.0-a.b">>, <<"1.0.0-a.a">>),
    lt = verl:compare(<<"1.0.0">>, <<"1.0.1">>),
    lt = verl:compare(<<"1.0.1">>, <<"1.1.0">>),
    lt = verl:compare(<<"1.2.2">>, <<"2.1.1">>),
    lt = verl:compare(<<"1.0.0-dev">>, <<"1.0.0">>),
    lt = verl:compare(<<"0.1.2">>, <<"1.2.3-dev">>),
    lt = verl:compare(<<"1.0.0-a">>, <<"1.0.0-a.b">>),
    lt = verl:compare(<<"1.0.0-a.b">>, <<"1.0.0-b">>),
    lt = verl:compare(<<"1.0.0-0">>, <<"1.0.0-a">>),
    lt = verl:compare(<<"1.0.0-a.a">>, <<"1.0.0-a.b">>),
    eq = verl:compare(<<"1.0.0">>, <<"1.0.0">>),
    eq = verl:compare(<<"1.0.0-dev">>, <<"1.0.0-dev">>),
    eq = verl:compare(<<"1.0.0-a">>, <<"1.0.0-a">>),
    {error, invalid_version} = verl:compare(<<"1.0">>, <<"1.0.0">>),
    {error, invalid_version} = verl:compare(<<"1.0.0-dev">>, <<"1.0">>),
    {error, invalid_version} = verl:compare(<<"foo">>, <<"1.0.0-a">>).

eq_test(_Config) ->
    ?assertMatch(true, verl:eq(<<"1.0.0-a">>, <<"1.0.0-a">>)),
    ?assertMatch(true, verl:eq(<<"1.0.0-alpha">>, <<"1.0.0-alpha">>)),
    ?assertMatch(true, verl:eq(<<"1.0.0-dev">>, <<"1.0.0-dev">>)),
    ?assertMatch(true, not verl:eq(<<"1.0.0">>, <<"1.0.1">>)),
    ?assertMatch(true, not verl:eq(<<"1.0.0-alpha">>, <<"1.0.1+alpha">>)),
    ?assertMatch(true, not verl:eq(<<"1.0.0+build.1">>, <<"1.0.1+build.2">>)).

gt_test(_Config) ->
    ?assertMatch(
        true,
        verl:gt(
            <<"1.0.0-alpha.1">>,
            <<"1.0.0-alpha">>
        )
    ),
    ?assertMatch(
        true,
        verl:gt(
            <<"1.0.0-beta.2">>,
            <<"1.0.0-alpha.1">>
        )
    ),
    ?assertMatch(
        true,
        verl:gt(
            <<"1.0.0-beta.11">>,
            <<"1.0.0-beta.2">>
        )
    ),
    ?assertMatch(
        true,
        verl:gt(
            <<"1.0.0-pre-alpha.14">>,
            <<"1.0.0-pre-alpha.3">>
        )
    ),
    ?assertMatch(true, verl:gt(<<"1.0.0-rc.1">>, <<"1.0.0-beta.11">>)),
    ?assertMatch(true, verl:gt(<<"1.0.0">>, <<"1.0.0-rc.1+build.1">>)),
    ?assertMatch(true, verl:gt(<<"1.3.7+build">>, <<"1.0.0+0.3.7">>)),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0-alpha">>,
            <<"1.0.0-alpha.1">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0-alpha.1">>,
            <<"1.0.0-beta.2">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0-beta.2">>,
            <<"1.0.0-beta.11">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0-beta.11">>,
            <<"1.0.0-rc.1">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0-pre-alpha.3">>,
            <<"1.0.0-pre-alpha.14">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0-rc.1">>,
            <<"1.0.0-rc.1+build.1">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0-rc.1+build.1">>,
            <<"1.0.0">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0">>,
            <<"1.0.0+0.3.7">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0+0.3.7">>,
            <<"1.3.7+build">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.3.7+build">>,
            <<"1.3.7+build.2.b8f12d7">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.3.7+build.2.b8f12d7">>,
            <<"1.3.7+build.11.e0f985a">>
        )
    ),
    ?assertMatch(
        true,
        not verl:gt(
            <<"1.0.0-alpha">>,
            <<"1.0.0-alpha">>
        )
    ).

% match against opaque
-dialyzer({[no_opaque, no_return], parse_test/1}).
parse_test(_Cfg) ->
    Exp0 = #{major => 1, minor => 2, patch => 3, pre => [], build => undefined},
    {ok, Exp0} = verl:parse(<<"1.2.3">>),
    Exp1 = #{major => 1, minor => 4, patch => 5, pre => [], build => <<"ignore">>},
    {ok, Exp1} = verl:parse(<<"1.4.5+ignore">>),
    Exp2 = #{major => 0, minor => 0, patch => 1, pre => [], build => <<"sha0702245">>},
    {ok, Exp2} = verl:parse(<<"0.0.1+sha.0702245">>),
    Exp3 = #{major => 1, minor => 4, patch => 5, pre => [<<"6-g3318bd5">>], build => undefined},
    {ok, Exp3} = verl:parse(<<"1.4.5-6-g3318bd5">>),
    Exp4 = #{major => 1, minor => 4, patch => 5, pre => [6, 7, <<"eight">>], build => undefined},
    {ok, Exp4} = verl:parse(<<"1.4.5-6.7.eight">>),
    Exp5 = #{major => 1, minor => 4, patch => 5, pre => [<<"6-g3318bd5">>], build => <<"ignore">>},
    {ok, Exp5} = verl:parse(<<"1.4.5-6-g3318bd5+ignore">>),
    ExpErr = {error, invalid_version},
    ExpErr = verl:parse(<<"foobar">>),
    ExpErr = verl:parse(<<"2">>),
    ExpErr = verl:parse(<<"2.">>),
    ExpErr = verl:parse(<<"2.3">>),
    ExpErr = verl:parse(<<"2.3.">>),
    ExpErr = verl:parse(<<"2.3.0-">>),
    ExpErr = verl:parse(<<"2.3.0+">>),
    ExpErr = verl:parse(<<"2.3.0.">>),
    ExpErr = verl:parse(<<"2.3.0.4">>),
    ExpErr = verl:parse(<<"2.3.-rc.1">>),
    ExpErr = verl:parse(<<"2.3.+rc.1">>),
    ExpErr = verl:parse(<<"2.3.0-01">>),
    ExpErr = verl:parse(<<"2.3.00-1">>),
    ExpErr = verl:parse(<<"2.3.00">>),
    ExpErr = verl:parse(<<"2.03.0">>),
    ExpErr = verl:parse(<<"02.3.0">>).

% match against opaque
-dialyzer({[no_opaque, no_return], parse_requirement_test/1}).
parse_requirement_test(_Cfg) ->
    Str = <<"1.2.3">>,
    ExpSpec = [
        {
            {'$1', '$2', '$3', '$4', '$5'},
            [{'==', {{'$1', '$2', '$3', '$4'}}, {const, {1, 2, 3, []}}}],
            ['$_']
        }
    ],
    {ok, #{string := Str, matchspec := ExpSpec, compiled := false}} =
        verl:parse_requirement(Str),
    ExpErr = {error, invalid_requirement},
    ExpErr = verl:parse_requirement(<<"1">>),
    ExpErr = verl:parse_requirement(<<"1.2">>),
    ExpErr = verl:parse_requirement(<<"1.2-3">>),
    ExpErr = verl:parse_requirement(<<"_ 1.2.3">>),
    ExpErr = verl:parse_requirement(<<"( ) 1.2.3">>).

% match against opaque
-dialyzer({[no_opaque, no_return], compile_requirement_test/1}).
compile_requirement_test(_Cfg) ->
    {ok, Req} = verl:parse_requirement(<<"1.2.3">>),
    #{compiled := true, matchspec := Ref} = verl:compile_requirement(Req),

    {Ver, []} = string:to_integer(erlang:system_info(otp_release)),
    case Ver of
        N when N < 20 ->
            true = is_binary(Ref);
        N when N >= 20 ->
            true = is_reference(Ref)
    end.

% opaque as arg
-dialyzer({[no_opaque, no_return], is_match_test/1}).
is_match_test(_Cfg) ->
    {error, invalid_version} = verl:is_match(<<"foo">>, <<"2.3.0">>),
    {error, invalid_requirement} = verl:is_match(<<"2.3.0">>, <<"foo">>),
    true = verl:is_match(<<"2.3.0">>, <<"== 2.3.0">>),
    true = verl:is_match(<<"2.3.0">>, <<"~> 2.3.0">>),
    true = verl:is_match(<<"1.2.3-alpha">>, <<"1.2.3-alpha">>),
    true = verl:is_match(<<"0.9.3">>, <<"== 0.9.3+dev">>),
    true = verl:is_match(<<"2.3.0">>, <<"2.3.0">>),
    false = verl:is_match(<<"2.4.0">>, <<"2.3.0">>),
    false = verl:is_match(<<"2.3.0">>, <<"!= 2.3.0">>),
    false = verl:is_match(<<"2.3.0">>, <<"<= 2.2.0">>),
    {ok, Ver} = verl:parse(<<"2.3.0">>),
    {ok, Req} = verl:parse_requirement(<<"2.3.0">>),
    {error, invalid_version} = verl:is_match(<<"foo">>, Req),
    true = verl:is_match(Ver, Req),
    true = verl:is_match(<<"2.3.0">>, Req),
    true = verl:is_match(Ver, <<"2.3.0">>),
    {error, invalid_requirement} = verl:is_match(Ver, <<"= 2.3.0">>),
    true = verl:is_match(Ver, Req, []),
    {error, invalid_version} = verl:is_match(<<".3.0">>, Req, []),
    true = verl:is_match(Ver, <<"== 2.3.0">>, []),
    true = verl:is_match(<<"2.3.0">>, Req, []),
    {error, invalid_version} = verl:is_match(<<"0">>, <<"== 2.3.0">>, []),
    {error, invalid_requirement} = verl:is_match(Ver, <<"= 2.3.0">>, []),
    {error, invalid_requirement} = verl:is_match(<<"2.3.0">>, <<"= 2.3.0">>, []),
    true = verl:is_match(<<"2.3.0">>, <<"== 2.3.0">>, []),
    Compiled = verl:compile_requirement(Req),
    true = verl:is_match(Ver, Compiled, []),
    true = verl:is_match(<<"2.4.0">>, <<"!2.3.0">>),
    false = verl:is_match(<<"2.3.0">>, <<"!2.3.0">>),
    true = verl:is_match(<<"2.4.0">>, <<"!= 2.3.0">>),
    false = verl:is_match(<<"2.3.0">>, <<"!= 2.3.0">>),
    true = verl:is_match(<<"2.4.0">>, <<"> 2.3.0">>),
    false = verl:is_match(<<"2.2.0">>, <<"> 2.3.0">>),
    false = verl:is_match(<<"2.3.0">>, <<"> 2.3.0">>),

    true = verl:is_match(<<"1.2.3">>, <<"> 1.2.3-alpha">>),
    true = verl:is_match(<<"1.2.3-alpha.1">>, <<"> 1.2.3-alpha">>),
    true = verl:is_match(<<"1.2.3-alpha.beta.sigma">>, <<"> 1.2.3-alpha.beta">>),
    false = verl:is_match(<<"1.2.3-alpha.10">>, <<"< 1.2.3-alpha.1">>),
    false = verl:is_match(<<"0.10.2-dev">>, <<"> 0.10.2">>),

    true = verl:is_match(<<"2.4.0">>, <<">= 2.3.0">>),
    false = verl:is_match(<<"2.2.0">>, <<">= 2.3.0">>),
    true = verl:is_match(<<"2.3.0">>, <<">= 2.3.0">>),
    true = verl:is_match(<<"2.0.0">>, <<">= 1.0.0">>),
    true = verl:is_match(<<"1.0.0">>, <<"1.0.0">>),

    true = verl:is_match(<<"2.2.0">>, <<"< 2.3.0">>),
    false = verl:is_match(<<"2.4.0">>, <<"< 2.3.0">>),
    false = verl:is_match(<<"2.3.0">>, <<"< 2.3.0">>),
    true = verl:is_match(<<"0.10.2-dev">>, <<"< 0.10.2">>),
    false = verl:is_match(<<"1.0.0">>, <<"< 1.0.0-dev">>),
    false = verl:is_match(<<"1.2.3-dev">>, <<"< 0.1.2">>),

    true = verl:is_match(<<"2.2.0">>, <<"<= 2.3.0">>),
    false = verl:is_match(<<"2.4.0">>, <<"<= 2.3.0">>),
    true = verl:is_match(<<"2.3.0">>, <<"<= 2.3.0">>),

    true = verl:is_match(<<"3.0.0">>, <<"~> 3.0">>),
    true = verl:is_match(<<"3.2.0">>, <<"~> 3.0">>),
    false = verl:is_match(<<"4.0.0">>, <<"~> 3.0">>),
    false = verl:is_match(<<"4.4.0">>, <<"~> 3.0">>),

    true = verl:is_match(<<"3.0.2">>, <<"~> 3.0.0">>),
    true = verl:is_match(<<"3.0.0">>, <<"~> 3.0.0">>),
    false = verl:is_match(<<"3.1.0">>, <<"~> 3.0.0">>),
    false = verl:is_match(<<"3.4.0">>, <<"~> 3.0.0">>),

    true = verl:is_match(<<"3.6.0">>, <<"~> 3.5">>),
    true = verl:is_match(<<"3.5.0">>, <<"~> 3.5">>),
    false = verl:is_match(<<"4.0.0">>, <<"~> 3.5">>),
    false = verl:is_match(<<"5.0.0">>, <<"~> 3.5">>),

    true = verl:is_match(<<"3.5.2">>, <<"~> 3.5.0">>),
    true = verl:is_match(<<"3.5.4">>, <<"~> 3.5.0">>),
    false = verl:is_match(<<"3.6.0">>, <<"~> 3.5.0">>),
    false = verl:is_match(<<"3.6.3">>, <<"~> 3.5.0">>),

    true = verl:is_match(<<"0.9.3">>, <<"~> 0.9.3-dev">>),
    false = verl:is_match(<<"0.10.0">>, <<"~> 0.9.3-dev">>),

    false = verl:is_match(<<"0.3.0-dev">>, <<"~> 0.2.0">>),

    false = verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0">>),
    false = verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0">>, [{allow_pre, false}]),
    false = verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0-dev">>),
    false = verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0-dev">>, [{allow_pre, false}]).

gte_test(_Config) ->
    ?assertMatch(true, verl:gte(<<"1.0.0-alpha">>, <<"1.0.0-alpha">>)),
    ?assertMatch(true, verl:gte(<<"1.0.0-pre-alpha.2">>, <<"1.0.0-pre-alpha">>)),
    ?assertMatch(true, verl:gte(<<"1.0.0-beta.2">>, <<"1.0.0-alpha.1">>)),
    ?assertMatch(true, verl:gte(<<"1.0.0-rc.1">>, <<"1.0.0-beta.11">>)),
    ?assertMatch(true, verl:gte(<<"1.0.0-rc.1+build.1">>, <<"1.0.0-rc.1">>)),
    ?assertMatch(true, verl:gte(<<"1.0.0">>, <<"1.0.0-rc.1+build.1">>)),
    ?assertMatch(true, verl:gte(<<"1.0.0+0.3.7">>, <<"1.0.0">>)),
    ?assertMatch(true, verl:gte(<<"1.3.7+build">>, <<"1.0.0+0.3.7">>)),
    ?assertMatch(true, verl:gte(<<"1.3.7+build.2.b8f12d7">>, <<"1.3.7+build">>)),
    ?assertMatch(true, verl:gte(<<"1.3.7+build.11.e0f985a">>, <<"1.3.7+build.2.b8f12d7">>)),
    ?assertMatch(true, not verl:gte(<<"1.0.0-alpha">>, <<"1.0.0-alpha.1">>)),
    ?assertMatch(true, not verl:gte(<<"1.0.0-pre-alpha">>, <<"1.0.0-pre-alpha.1">>)),
    ?assertMatch(true, not verl:gte(<<"1.0.0-alpha.1">>, <<"1.0.0-beta.2">>)),
    ?assertMatch(true, not verl:gte(<<"1.0.0-beta.2">>, <<"1.0.0-beta.11">>)),
    ?assertMatch(true, not verl:gte(<<"1.0.0-beta.11">>, <<"1.0.0-rc.1">>)),
    ?assertMatch(true, not verl:gte(<<"1.0.0-rc.1+build.1">>, <<"1.0.0">>)),
    ?assertMatch(true, not verl:gte(<<"1.0.0+0.3.7">>, <<"1.3.7+build">>)).

lt_test(_Config) ->
    ?assertMatch(
        true,
        verl:lt(
            <<"1.0.0-alpha">>,
            <<"1.0.0-alpha.1">>
        )
    ),
    ?assertMatch(
        true,
        verl:lt(
            <<"1.0.0-alpha.1">>,
            <<"1.0.0-beta.2">>
        )
    ),
    ?assertMatch(
        true,
        verl:lt(
            <<"1.0.0-beta.2">>,
            <<"1.0.0-beta.11">>
        )
    ),
    ?assertMatch(
        true,
        verl:lt(
            <<"1.0.0-pre-alpha.3">>,
            <<"1.0.0-pre-alpha.14">>
        )
    ),
    ?assertMatch(
        true,
        verl:lt(
            <<"1.0.0-beta.11">>,
            <<"1.0.0-rc.1">>
        )
    ),
    ?assertMatch(
        true,
        verl:lt(
            <<"1.0.0-rc.1+build.1">>,
            <<"1.0.0">>
        )
    ),
    ?assertMatch(
        true,
        verl:lt(
            <<"1.0.0+0.3.7">>,
            <<"1.3.7+build">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lt(
            <<"1.0.0-alpha">>,
            <<"1.0.0-alpha">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lt(
            <<"1.0.0-alpha.1">>,
            <<"1.0.0-alpha">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lt(
            <<"1.0.0-beta.2">>,
            <<"1.0.0-alpha.1">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lt(
            <<"1.0.0-beta.11">>,
            <<"1.0.0-beta.2">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lt(
            <<"1.0.0-pre-alpha.14">>,
            <<"1.0.0-pre-alpha.3">>
        )
    ),
    ?assertMatch(true, not verl:lt(<<"1.0.0-rc.1">>, <<"1.0.0-beta.11">>)),
    ?assertMatch(true, not verl:lt(<<"1.0.0-rc.1+build.1">>, <<"1.0.0-rc.1">>)),
    ?assertMatch(true, not verl:lt(<<"1.0.0">>, <<"1.0.0-rc.1+build.1">>)),
    ?assertMatch(true, not verl:lt(<<"1.0.0+0.3.7">>, <<"1.0.0">>)),
    ?assertMatch(true, not verl:lt(<<"1.3.7+build">>, <<"1.0.0+0.3.7">>)),
    ?assertMatch(
        true,
        not verl:lt(
            <<"1.3.7+build.2.b8f12d7">>,
            <<"1.3.7+build">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lt(
            <<"1.3.7+build.11.e0f985a">>,
            <<"1.3.7+build.2.b8f12d7">>
        )
    ).

lte_test(_Config) ->
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0-alpha">>,
            <<"1.0.0-alpha.1">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0-alpha.1">>,
            <<"1.0.0-beta.2">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0-beta.2">>,
            <<"1.0.0-beta.11">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0-pre-alpha.2">>,
            <<"1.0.0-pre-alpha.11">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0-beta.11">>,
            <<"1.0.0-rc.1">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0-rc.1">>,
            <<"1.0.0-rc.1+build.1">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0-rc.1+build.1">>,
            <<"1.0.0">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0">>,
            <<"1.0.0+0.3.7">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0+0.3.7">>,
            <<"1.3.7+build">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.3.7+build">>,
            <<"1.3.7+build.2.b8f12d7">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.3.7+build.2.b8f12d7">>,
            <<"1.3.7+build.11.e0f985a">>
        )
    ),
    ?assertMatch(
        true,
        verl:lte(
            <<"1.0.0-alpha">>,
            <<"1.0.0-alpha">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lte(
            <<"1.0.0-alpha.1">>,
            <<"1.0.0-alpha">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lte(
            <<"1.0.0-pre-alpha.2">>,
            <<"1.0.0-pre-alpha">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lte(
            <<"1.0.0-beta.2">>,
            <<"1.0.0-alpha.1">>
        )
    ),
    ?assertMatch(
        true,
        not verl:lte(
            <<"1.0.0-beta.11">>,
            <<"1.0.0-beta.2">>
        )
    ),
    ?assertMatch(true, not verl:lte(<<"1.0.0-rc.1">>, <<"1.0.0-beta.11">>)),
    ?assertMatch(true, not verl:lte(<<"1.0.0">>, <<"1.0.0-rc.1+build.1">>)),
    ?assertMatch(true, not verl:lte(<<"1.3.7+build">>, <<"1.0.0+0.3.7">>)).
