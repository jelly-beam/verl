-module(verl_parser_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [lexer, parse_version].

lexer(_Cfg) -> 
    Exp1 = ['==','!=','>','>=','<','<=','~>'],
    Exp1 = verl_parser:lexer(<<"== != > >= < <= ~>">>, []),
    Exp2 = ['==', <<"2.3.0">>],
    Exp2 = verl_parser:lexer(<<"2.3.0">>, []),
    Exp3 = ['!=', <<"2.3.0">>],
    Exp3 = verl_parser:lexer(<<"!2.3.0">>, []),
    Exp4 = ['>', '>='],
    Exp4 = verl_parser:lexer(<<">>=">>, []),
    Exp5 = ['>', <<"2.4.0">>],
    Exp5 = verl_parser:lexer(<<">2.4.0">>, []),
    Exp5 = verl_parser:lexer(<<"> 2.4.0">>, []),
    Exp5 = verl_parser:lexer(<<"    >     2.4.0">>, []).

parse_version(_Cfg) ->
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
    error =  verl_parser:parse_version(<<"02.3.0">>). 
