-module(verl_parser_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [lexer_test, parse_version_test, parse_requirement_test].

lexer_test(_Cfg) ->
    Exp0 = ['==','!=','>','>=','<','<=','~>'],
    Exp0 = verl_parser:lexer(<<"== != > >= < <= ~>">>, []),
    Exp1 = ['&&','==',<<"2.1.0">>],
    Exp1 = verl_parser:lexer(<<" and 2.1.0">>, []),
    Exp2 = ['==', <<"2.3.0">>],
    Exp2 = verl_parser:lexer(<<"2.3.0">>, []),
    Exp3 = ['!=', <<"2.3.0">>],
    Exp3 = verl_parser:lexer(<<"!2.3.0">>, []),
    Exp4 = ['>', '>='],
    Exp4 = verl_parser:lexer(<<">>=">>, []),
    Exp5 = ['>', <<"2.4.0">>],
    Exp5 = verl_parser:lexer(<<">2.4.0">>, []),
    Exp5 = verl_parser:lexer(<<"> 2.4.0">>, []),
    Exp5 = verl_parser:lexer(<<"    >     2.4.0">>, []),
    Exp6 = ['>=',<<"2.0.0">>,'&&','<',<<"2.1.0">>],
    Exp6 = verl_parser:lexer(<<">= 2.0.0 and < 2.1.0">>, []),
    Exp7 = ['>=',<<"2.0.0">>,'||','<',<<"2.1.0">>],
    Exp7 = verl_parser:lexer(<<">= 2.0.0 or < 2.1.0">>, []),
    Exp8 = ['>=',<<"'2.0.0'">>,'||','<',<<"2.1.0">>],
    Exp8 = verl_parser:lexer(<<">= '2.0.0' or < 2.1.0">>, []),
    Exp9 = ['||','==',<<"2.1.0">>],
    Exp9 = verl_parser:lexer(<<" or 2.1.0">>, []).


parse_version_test(_Cfg) ->
    {ok, {1,2,3, [], []}} = verl_parser:parse_version(<<"1.2.3">>),
    {ok,{1,4,5,[],[<<"ignore">>]}} = verl_parser:parse_version(<<"1.4.5+ignore">>),
    {ok, {0,0,1,[],[<<"sha">>, <<"0702245">>]}} = verl_parser:parse_version(<<"0.0.1+sha.0702245">>),
    {ok,{1,4,5,[<<"6-g3318bd5">>],[]}} =
    verl_parser:parse_version(<<"1.4.5-6-g3318bd5">>),
    {ok, {1,4,5, [6,7, <<"eight">>], []}} =
    verl_parser:parse_version(<<"1.4.5-6.7.eight">>),
    {ok,{1,4,5,[<<"6-g3318bd5">>],[<<"ignore">>]}} =
    verl_parser:parse_version(<<"1.4.5-6-g3318bd5+ignore">>),
    error = verl_parser:parse_version(<<"foobar">>),
    error =  verl_parser:parse_version(<<"2">>),
    error =  verl_parser:parse_version(<<"2.">>),
    error =  verl_parser:parse_version(<<"2.3">>),
    error =  verl_parser:parse_version(<<"2.3.">>),
    error =  verl_parser:parse_version(<<"2.3.0-">>),
    error =  verl_parser:parse_version(<<"2.3.0+">>),
    error =  verl_parser:parse_version(<<"2.3.0.">>),
    error =  verl_parser:parse_version(<<"2.3.0.4">>),
    error =  verl_parser:parse_version(<<"2.3.-rc.1">>),
    error =  verl_parser:parse_version(<<"2.3.+rc.1">>),
    error =  verl_parser:parse_version(<<"2.3.0-01">>),
    error =  verl_parser:parse_version(<<"2.3.00-1">>),
    error =  verl_parser:parse_version(<<"2.3.00">>),
    error =  verl_parser:parse_version(<<"2.03.0">>),
    error =  verl_parser:parse_version(<<"02.3.0">>),
    error =  verl_parser:parse_version(<<"0. 0.0">>),
    error  =  verl_parser:parse_version(<<"0.1.0-&&pre">>),
    error = verl_parser:parse_requirement(<<" and !">>),
    error = verl_parser:parse_requirement(<<" ! and">>).

parse_requirement_test(_Cfg) ->
    ExpSpec0 = [{{'$1','$2','$3','$4','$5'},
                 [{'==',{{'$1','$2','$3','$4'}},{const,{1,2,3,[]}}}],
                 ['$_']}],
    {ok, ExpSpec0} = verl_parser:parse_requirement(<<"1.2.3">>),
    ExpSpec1 = [{{'$1','$2','$3','$4','$5'},
                 [{'/=',{{'$1','$2','$3','$4'}},{const,{1,2,3,[]}}}],
                 ['$_']}],
    {ok, ExpSpec1} = verl_parser:parse_requirement(<<"!= 1.2.3">>),
    {ok, _} = verl_parser:parse_requirement(<<"~> 1.2.3">>),
    {ok, _} = verl_parser:parse_requirement(<<"<= 1.2.3">>),
    error   =   verl_parser:parse_requirement(<<>>),
    error = verl_parser:parse_requirement(<<"and 2.1.0 and 2.1.1">>),
    error = verl_parser:parse_requirement(<<"2.1.1 or">>). 
