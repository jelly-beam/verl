-module(verl_parser).

-export([lexer/2]).

lexer(<<">=", Rest/binary>>, Acc) ->
    lexer(Rest, ['>=' | Acc]);
lexer(<<"<=", Rest/binary>>, Acc) ->
    lexer(Rest, ['<=' | Acc]);
lexer(<<"~>", Rest/binary>>, Acc) ->
    lexer(Rest, ['~>' | Acc]);
lexer(<<">", Rest/binary>>, Acc) ->
    lexer(Rest, ['>' | Acc]);
lexer(<<"<", Rest/binary>>, Acc) ->
    lexer(Rest, ['<' | Acc]);
lexer(<<"==", Rest/binary>>, Acc) ->
    lexer(Rest, ['==' | Acc]);
lexer(<<"!=", Rest/binary>>, Acc) ->
    lexer(Rest, ['!=' | Acc]);
lexer(<<"!", Rest/binary>>, Acc) ->
    lexer(Rest, ['!=' | Acc]);
lexer(<<" or ", Rest/binary>>, Acc) ->
    lexer(Rest, ['||' | Acc]);
lexer(<<" and ", Rest/binary>>, Acc) ->
    lexer(Rest, ['&&' | Acc]);
lexer(<<" ", Rest/binary>>, Acc) ->
    lexer(Rest, Acc);
lexer(<<Char/utf8, Rest/binary>>, []) ->
    lexer(Rest, [<<Char/utf8>>, '==']);
lexer(<<Char/utf8, Body/binary>>,
      [Head | Acc]) ->
    Acc1 = case Head of
		Head when erlang:is_binary(Head) ->
		    [<<Head/binary, Char/utf8>> | Acc];
		Head
		    when Head =:= '&&' orelse Head =:= '||' ->
		    [<<Char/utf8>>, '==', Head | Acc];
		_Other -> [<<Char/utf8>>, Head | Acc]
	      end,
    lexer(Body, Acc1);
lexer(<<>>, Acc) -> 
    lists:reverse(Acc).
