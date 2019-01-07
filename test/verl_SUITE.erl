-module(verl_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [parse].


parse(_Cfg) ->
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
