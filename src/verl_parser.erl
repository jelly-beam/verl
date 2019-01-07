-module(verl_parser).

-export([lexer/2, parse_version/1, parse_version/2]).

-include("verl.hrl").

-define(ZERO_BIN, <<"0">>).
-define(EMPTY_BIN, <<>>).
-define(DOT_BIN, <<>>).


-spec lexer(requirement(), list()) -> list().
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
lexer(<<Char/utf8, Body/binary>>, [Head | Acc]) ->
    Acc1 = case Head of
		Head when is_binary(Head) ->
		    [<<Head/binary, Char/utf8>> | Acc];
		Head
		    when Head =:= '&&' orelse Head =:= '||' ->
		    [<<Char/utf8>>, '==', Head | Acc];
		_Other -> [<<Char/utf8>>, Head | Acc]
	      end,
    lexer(Body, Acc1);
lexer(<<>>, Acc) -> 
    lists:reverse(Acc).

join_bins(List, Delim) ->
    lists:foldl(fun(Bin, Acc) ->
        case bit_size(Acc) of  
            N when N > 0 -> 
                <<Acc/binary, Delim/binary, Bin/binary>>;
            _ -> 
                Bin
        end
    end, <<>>, List).

bisect(Str, Delim) ->
    [First | Rest ] =  binary:split(Str, [Delim],[global]),
    Rest1 = case Rest of 
                [] -> 
                    undefined;
                _ ->
                  join_bins(Rest, Delim)
            end,
    [First, Rest1].

split_ver(Str) ->
    case binary:split(Str, [<<".">>], [global]) of 
        [Maj, Min, P] -> 
            [Maj, Min, P, undefined];
        [Major, Minor, Patch | Rest] -> 
            [Major, Minor, Patch, Rest];
        _ ->
            [error, error, error, error]
    end.

parse_version(Str) -> parse_version(Str, false).

parse_version(Str, Approximate) when is_binary(Str) ->
    try case parse_and_convert(Str, Approximate) of 
        {ok, _} = V ->
            V;
        _ -> 
            error
        end
    catch 
        _:_ -> error
    end.

parse_and_convert(Str, Approx) ->  
    [VerPre, Build] = bisect(Str, <<"+">>),
    [Ver, Pre] = bisect(VerPre, <<"-">>),
    [Maj1, Min1, Patch1, Other] = split_ver(Ver),
    case Other of 
        undefined -> 
            {ok, Maj2} = to_digits(Maj1), 
            {ok, Min2} = to_digits(Min1), 
            {ok, Patch2} = maybe_patch(Patch1, Approx),
            {ok, PreParts} = opt_dot_separated(Pre),
            {ok, PreParts1} = parts_to_integers(PreParts, []),
            {ok, Build2} = opt_dot_separated(Build),
	        {ok, {Maj2, Min2, Patch2, PreParts1, Build2}};
        _ ->
         error
    end.

to_digits(undefined) -> error;

to_digits(Str) ->
    case has_leading_zero(Str) of
      S when S =:= undefined orelse S =:= false ->
	    parse_digits(Str, <<>>);
      _ -> 
        error
    end.

parse_digits(<<Char/integer, Rest/binary>>, Acc) 
  when is_integer(Char) andalso Char >= 48 andalso Char =< 57 ->
    parse_digits(Rest, <<Acc/binary, Char/integer>>);
parse_digits(<<>>, Acc) when byte_size(Acc) > 0 ->
    {ok, binary_to_integer(Acc)};
parse_digits(_, _) -> error.

has_leading_zero(<<48/integer, _/integer, _/binary>>) ->
    true;
has_leading_zero(_) -> 
    false.

is_valid_identifier(<<Char/integer, Rest/binary>>) 
  when is_integer(Char) andalso
	   Char >= 48 andalso Char =< 57;
	 is_integer(Char) andalso
	   Char >= 97 andalso Char=< 122;
	 is_integer(Char) andalso
	   Char >= 65 andalso Char =< 90;
	 Char == 45 ->
    is_valid_identifier(Rest);
is_valid_identifier(<<>>) -> 
    true;
is_valid_identifier(_) -> 
    false.

parts_to_integers([Part | Rest], Acc) ->
    case parse_digits(Part, <<>>) of
      {ok, Int} ->
	    case has_leading_zero(Part) of
	        P when P =:= undefined orelse P =:= false ->
		        parts_to_integers(Rest, [Int | Acc]);
	        _ -> error
	    end;
      error ->
	    parts_to_integers(Rest, [Part | Acc])
    end;
parts_to_integers([], Acc) ->
    {ok, lists:reverse(Acc)}.

opt_dot_separated(undefined) -> {ok, []};
opt_dot_separated(Str) ->
    Parts = binary:split(Str, <<".">>, [global]),
    Fun = fun(P) -> 
                  case P /= <<>> of
                      false -> false;
                      true -> is_valid_identifier(P)
                  end
          end,
    case lists:all(Fun, Parts) of
      P when P =:= undefined orelse P =:= false ->
	    error;
      _ -> 
        {ok, Parts}
    end.

maybe_patch(undefined, true) -> {ok, undefined};
maybe_patch(Patch, _) -> to_digits(Patch).
