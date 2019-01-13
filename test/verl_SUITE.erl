-module(verl_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [parse_test, parse_requirement_test, compile_requirement_test, is_match_test].

parse_test(_Cfg) ->
    Exp0 = #{major => 1, minor => 2, patch => 3, pre => [], build => undefined},
    Exp0 = verl:parse(<<"1.2.3">>),
    Exp1 = #{major => 1, minor => 4, patch => 5, pre => [], build => <<"ignore">>},
    Exp1 = verl:parse(<<"1.4.5+ignore">>),
    Exp2 = #{major => 0, minor => 0, patch => 1, pre => [], build => <<"sha0702245">>},
    Exp2 = verl:parse(<<"0.0.1+sha.0702245">>),
    Exp3 = #{major => 1, minor => 4, patch => 5, pre => [<<"6-g3318bd5">>], build => undefined},
    Exp3 = verl:parse(<<"1.4.5-6-g3318bd5">>),
    Exp4 = #{major => 1, minor => 4, patch => 5, pre => [6, 7, <<"eight">>], build => undefined},
    Exp4 = verl:parse(<<"1.4.5-6.7.eight">>),
    Exp5 = #{major => 1, minor => 4, patch => 5, pre => [<<"6-g3318bd5">>], build => <<"ignore">>},
    Exp5 = verl:parse(<<"1.4.5-6-g3318bd5+ignore">>),
    error = verl:parse(<<"foobar">>),
    error =  verl:parse(<<"2">>),
    error =  verl:parse(<<"2.">>),
    error =  verl:parse(<<"2.3">>),
    error =  verl:parse(<<"2.3.">>),
    error =  verl:parse(<<"2.3.0-">>),
    error =  verl:parse(<<"2.3.0+">>),
    error =  verl:parse(<<"2.3.0.">>),
    error =  verl:parse(<<"2.3.0.4">>),
    error =  verl:parse(<<"2.3.-rc.1">>),
    error =  verl:parse(<<"2.3.+rc.1">>),
    error =  verl:parse(<<"2.3.0-01">>),
    error =  verl:parse(<<"2.3.00-1">>),
    error =  verl:parse(<<"2.3.00">>),
    error =  verl:parse(<<"2.03.0">>),
    error =  verl:parse(<<"02.3.0">>).

parse_requirement_test(_Cfg) ->
    Str = <<"1.2.3">>,
    ExpSpec = [{{'$1','$2','$3','$4','$5'},
                [{'==',{{'$1','$2','$3','$4'}},{const,{1,2,3,[]}}}],
                ['$_']}],
    {ok, #{string := Str, matchspec := ExpSpec, compiled := false}} =
    verl:parse_requirement(Str),
    error = verl:parse_requirement(<<"1">>),
    error = verl:parse_requirement(<<"1.2">>),
    error = verl:parse_requirement(<<"1.2-3">>),
    error = verl:parse_requirement(<<"_ 1.2.3">>),
    error = verl:parse_requirement(<<"( ) 1.2.3">>).

compile_requirement_test(_Cfg) -> 
    {ok, Req} = verl:parse_requirement(<<"1.2.3">>),
    #{compiled := true, matchspec := Ref} = verl:compile_requirement(Req),
    true = is_reference(Ref).

is_match_test(_Cfg) -> 
    {error, <<"invalid version">>} = verl:is_match(<<"foo">>, <<"2.3.0">>),
    {error, <<"invalid requirement">>} = verl:is_match(<<"2.3.0">>, <<"foo">>),
    true = verl:is_match(<<"2.3.0">>, <<"== 2.3.0">>),
    true = verl:is_match(<<"2.3.0">>, <<"~> 2.3.0">>),
    true = verl:is_match(<<"1.2.3-alpha">>, <<"1.2.3-alpha">>),
    true = verl:is_match(<<"0.9.3">>, <<"== 0.9.3+dev">>),
    true = verl:is_match(<<"2.3.0">>, <<"2.3.0">>),
    false = verl:is_match(<<"2.4.0">>, <<"2.3.0">>),
    false = verl:is_match(<<"2.3.0">>, <<"!= 2.3.0">>),
    false = verl:is_match(<<"2.3.0">>, <<"<= 2.2.0">>),
    Ver = verl:parse(<<"2.3.0">>),
    {ok, Req} = verl:parse_requirement(<<"2.3.0">>),
    {error, <<"invalid version">>} = verl:is_match(<<"foo">>, Req),
    true = verl:is_match(Ver, Req),
    true = verl:is_match(<<"2.3.0">>, Req),
    true = verl:is_match(Ver, <<"2.3.0">>),
    {error, <<"invalid requirement">>} = verl:is_match(Ver, <<"= 2.3.0">>),
    {error, <<"bad arguments">>} = verl:is_match(<<"2.3.0">>, #{}),
    true = verl:is_match(Ver, Req, []),
    false = verl:is_match(<<".3.0">>, Req, []),
    true = verl:is_match(Ver, <<"== 2.3.0">>, []),
    true = verl:is_match(<<"2.3.0">>, Req, []),
    false = verl:is_match(<<"0">>, <<"== 2.3.0">>, []),
    {error, <<"invalid requirement">>} = verl:is_match(Ver, <<"= 2.3.0">>, []),
    {error, <<"invalid requirement">>} = verl:is_match(<<"2.3.0">>, <<"= 2.3.0">>, []),
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

    true =  verl:is_match(<<"3.6.0">>, <<"~> 3.5">>),
    true =  verl:is_match(<<"3.5.0">>, <<"~> 3.5">>),
    false = verl:is_match(<<"4.0.0">>, <<"~> 3.5">>),
    false = verl:is_match(<<"5.0.0">>, <<"~> 3.5">>),

    true =  verl:is_match(<<"3.5.2">>, <<"~> 3.5.0">>),
    true =  verl:is_match(<<"3.5.4">>, <<"~> 3.5.0">>),
    false = verl:is_match(<<"3.6.0">>, <<"~> 3.5.0">>),
    false = verl:is_match(<<"3.6.3">>, <<"~> 3.5.0">>),

    true =  verl:is_match(<<"0.9.3">>, <<"~> 0.9.3-dev">>),
    false = verl:is_match(<<"0.10.0">>, <<"~> 0.9.3-dev">>),

    false = verl:is_match(<<"0.3.0-dev">>, <<"~> 0.2.0">>),

    false = verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0">>),
    false = verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0">>, [{allow_pre,
                                                             false}]),
    false = verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0-dev">>),
    false = verl:is_match(<<"2.2.0-dev">>, <<"~> 2.1.0-dev">>, [{allow_pre, false}]).
