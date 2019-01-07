-module(verl_parser).

-export([lexer/2, parse_version/1, parse_version/2]).

%% -type version() :: binary().
-type requirement() :: binary() | any().
%% -type major() :: non_neg_integer().
%% -type minor() :: non_neg_integer().
%% -type patch() :: non_neg_integer().
%% -type pre() :: [binary() | non_neg_integer()].
%% -type build() :: binary() | undefined.
%% -type t() :: #{
%%               major => major() 
%%             , minor => minor() 
%%             , patch => patch()
%%             , pre   => pre()
%%             , build => build()}.

-spec parse_requirement(binary()) -> {ok, term()} | error.
parse_requirement(Source) ->
      Lexed = lexer(Source, []),
      to_matchspec(Lexed).

to_matchspec(Lexed) ->
	try case is_valid_requirement(Lexed) of
      true ->
		First = to_condition(Lexed),
        Rest = lists:nth(2, First),
        {ok, [{{'$1', '$2', '$3', '$4', '$5'}, [to_condition(First, Rest)],
               ['$_']}]};
     false ->
        error
    end
    catch 
        invalid_matchspec -> error
    end.

to_condition(['==', Version | _]) ->
    Matchable = parse_condition(Version),
    main_condition('==', Matchable);
to_condition(['!=', Version | _]) ->
    Matchable = parse_condition(Version),
    main_condition('/=', Matchable);
to_condition(['~>', Version | _]) ->
    From = parse_condition(Version, true),
    To = approximate_upper(From),
    {'andalso',
     to_condition(['>=', matchable_to_string(From)]),
     to_condition(['<', matchable_to_string(To)])};
to_condition(['>', Version | _]) ->
    {Major, Minor, Patch, Pre} =
	parse_condition(Version),
    {'andalso',
     {'orelse',
      main_condition('>', {Major, Minor, Patch}),
      {'andalso',
       main_condition('==', {Major, Minor, Patch}),
       pre_condition('>', Pre)}},
     no_pre_condition(Pre)};
to_condition(['>=', Version | _]) ->
    Matchable = parse_condition(Version),
    {'orelse', main_condition('==', Matchable),
     to_condition(['>', Version])};
to_condition(['<', Version | _]) ->
    {Major, Minor, Patch, Pre} =
	parse_condition(Version),
    {'orelse',
     main_condition('<', {Major, Minor, Patch}),
     {'andalso',
      main_condition('==', {Major, Minor, Patch}),
      pre_condition('<', Pre)}};
to_condition(['<=', Version | _]) ->
    Matchable = parse_condition(Version),
    {'orelse', main_condition('==', Matchable),
     to_condition(['<', Version])}.


to_condition(Current, []) -> Current;
to_condition(Current,
	     ['&&', Operator, Version | Rest]) ->
    to_condition({'andalso', Current,
		  to_condition([Operator, Version])},
		 Rest);
to_condition(Current,
	     ['||', Operator, Version | Rest]) ->
    to_condition({'orelse', Current,
		  to_condition([Operator, Version])},
		 Rest).

main_condition(Op, Version)
    when erlang:tuple_size(Version) == 3 ->
    {Op, {{'$1', '$2', '$3'}}, {const, Version}};
main_condition(Op, Version)
    when erlang:tuple_size(Version) == 4 ->
    {Op, {{'$1', '$2', '$3', '$4'}},
     {const, Version}}.

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

split_two_parts(Str, Delim) ->
    [First | Rest ] =  binary:split(Str, [Delim],[global]),
    Rest1 = case Rest of 
                [] -> 
                    undefined;
                _ ->
                 binary:list_to_bin(Rest)
            end,
    [First, Rest1].

parse_version(Str) -> parse_version(Str, false).

parse_version(Str, Approximate) when erlang:is_binary(Str) ->

    [VerPre, Build] = split_two_parts(Str, <<"+">>),
    [Ver, Pre] = split_two_parts(VerPre, <<"-">>),
    [Major1, Minor1, Patch1, Next] = case binary:split(Ver, [<<".">>], [global]) of 
                                      [Maj, Min, P] -> 
                                            [Maj, Min, P, undefined];
                                      [Major, Minor, Patch | Rest] -> 
                                          [Major, Minor, Patch, Rest];
                                      _ ->
                                        [error, error, error, error]
                                  end,
    case Next of
      undefined ->
            case require_digits(Major1) of
	            {ok, Major2} ->
		            case require_digits(Minor1) of
		                {ok, Minor2} ->
		                    case maybe_patch(Patch1, Approximate) of
			                    {ok, Patch2} ->
			                        case optional_dot_separated(Pre) of
			                            {ok, PreParts} ->
				                            case convert_parts_to_integer(PreParts, []) of
    				                                {ok, PreParts1} ->
					                                case
                                                        optional_dot_separated(Build) of
					                                    {ok, Build2} ->
					                                        {ok, {Major2,
                                                                  Minor2,
                                                                  Patch2,
                                                                  PreParts1,
                                                                  Build2}};
					                                    _ ->
                                                            error
					                                end;
				                                _ ->
                                                    error
				                            end;
			                            _ ->
                                            error
			                        end;
			                    _ ->
                                    error
                            end;
		                _ ->
		                    error
                    end;
                _ ->
                    error

            end;
        _ ->
            error
    end.

parse_condition(Version) -> parse_condition(Version, false).

parse_condition(Version, Approximate) ->
    case parse_version(Version, Approximate) of
      {ok,
       {Major, Minor, Patch, Pre,
	Build}} ->
	  {Major, Minor, Patch, Pre};
      error -> erlang:throw(invalid_matchspec)
    end.

approximate_upper(Version) ->
    case Version of
      {Major, Minor, undefined, _} ->
	    {Major + 1, 0, 0, [0]};
      {Major, Minor, Patch, _} ->
	    {Major, Minor + 1, 0, [0]}
    end.

matchable_to_string({Major, Minor, Patch, Pre}) ->
    Patch2 = case Patch of
		  P when P =:= undefined orelse P =:= false ->
		      <<"0">>;
		  _ ->
            maybe_to_string(Patch)
		end,
    Pre1 = case Pre /= [] of
		false -> undefined;
		true ->
		    maybe_to_string(Pre)
	      end,
    Major1 = maybe_to_string(Major),
    Minor1 = maybe_to_string(Minor),
    Patch1 = maybe_to_string(Patch),
    <<Major1/binary, "."/binary, Minor1/binary, "."/binary, Patch1/binary, "."/binary, Pre1/binary>>.

pre_condition('>', Pre) ->
    PreLength = erlang:length(Pre),
    {'orelse',
     {'andalso', {'==', {length, '$4'}, 0},
      {const, PreLength /= 0}},
     {'andalso', {const, PreLength /= 0},
      {'orelse', {'>', {length, '$4'}, PreLength},
       {'andalso', {'==', {length, '$4'}, PreLength},
	{'>', '$4', {const, Pre}}}}}};
pre_condition('<', Pre) ->
    PreLength = erlang:length(Pre),
    {'orelse',
     {'andalso', {'/=', {length, '$4'}, 0},
      {const, PreLength == 0}},
     {'andalso', {'/=', {length, '$4'}, 0},
      {'orelse', {'<', {length, '$4'}, PreLength},
       {'andalso', {'==', {length, '$4'}, PreLength},
	{'<', '$4', {const, Pre}}}}}}.

no_pre_condition([]) ->
    {'orelse', '$5', {'==', {length, '$4'}, 0}};
no_pre_condition(_) -> {const, true}.

require_digits(undefined) -> error;

require_digits(Str) ->
    case has_leading_zero(Str) of
      S when S =:= undefined orelse S =:= false ->
	    parse_digits(Str, <<>>);
      _ -> 
        error
    end.

parse_digits(<<Char/integer, Rest/binary>>, Acc) 
  when erlang:is_integer(Char) andalso Char >= 48 andalso Char =< 57 ->
    parse_digits(Rest, <<Acc/binary, Char/integer>>);
parse_digits(<<>>, Acc) when erlang:byte_size(Acc) > 0 ->
    {ok, erlang:binary_to_integer(Acc)};
parse_digits(_, _) -> error.

has_leading_zero(<<48/integer, _/integer, _/binary>>) ->
    true;
has_leading_zero(E) -> 
    false.

is_valid_identifier(<<Char/integer, Rest/binary>>) 
  when erlang:is_integer(Char) andalso
	   Char >= 48 andalso Char =< 57;
	 erlang:is_integer(Char) andalso
	   Char >= 97 andalso Char=< 122;
	 erlang:is_integer(Char) andalso
	   Char >= 65 andalso Char =< 90;
	 Char == 45 ->
    is_valid_identifier(Rest);
is_valid_identifier(<<>>) -> 
    true;
is_valid_identifier(_) -> 
    false.

convert_parts_to_integer([Part | Rest], Acc) ->
    case parse_digits(Part, <<>>) of
      {ok, Int} ->
	    case has_leading_zero(Part) of
	        P when P =:= undefined orelse P =:= false ->
		        convert_parts_to_integer(Rest, [Int | Acc]);
	        _ -> error
	    end;
      error ->
	    convert_parts_to_integer(Rest, [Part | Acc])
    end;
convert_parts_to_integer([], Acc) ->
    {ok, lists:reverse(Acc)}.

optional_dot_separated(undefined) -> {ok, []};
optional_dot_separated(Str) ->
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
maybe_patch(Patch, _) -> require_digits(Patch).

maybe_to_string(Part) -> 
    case Part of
	    Rewrite when erlang:is_binary(Rewrite) ->
	        Rewrite;
	    Rewrite ->
	        list_to_binary(Rewrite)
    end.

is_valid_requirement([]) -> false;
is_valid_requirement([A | Next]) ->
    is_valid_requirement(A, Next).

is_valid_requirement(A, [])
    when erlang:is_binary(A) ->
    true;
is_valid_requirement(A, [B | Next])
    when (erlang:is_atom(A) andalso
	    erlang:is_atom(B))
	   andalso (A =:= '&&' orelse A =:= '||') ->
    is_valid_requirement(B, Next);
is_valid_requirement(A, [B | Next])
    when (erlang:is_binary(A) andalso
	    erlang:is_atom(B))
	   andalso (B =:= '&&' orelse B =:= '||') ->
    is_valid_requirement(B, Next);
is_valid_requirement(A, [B | Next])
    when (erlang:is_atom(A) andalso
	    erlang:is_binary(B))
	   andalso (A =:= '&&' orelse A =:= '||') ->
    is_valid_requirement(B, Next);
is_valid_requirement(A, [B | Next])
    when erlang:is_atom(A) andalso
	   erlang:is_binary(B) ->
    is_valid_requirement(B, Next);
is_valid_requirement(_, _) -> false.
