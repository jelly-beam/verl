-module(verl_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [lexer].


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
