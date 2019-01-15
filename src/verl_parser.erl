-module(verl_parser).

-export([parse_requirement/1, parse_version/1, parse_version/2]).

-include("verl.hrl").

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
               Head when Head =:= '&&' orelse Head =:= '||' ->
                   [<<Char/utf8>>, '==', Head | Acc];
               _Other ->
                   [<<Char/utf8>>, Head | Acc]
           end,
    lexer(Body, Acc1);
lexer(<<>>, Acc) ->
    lists:reverse(Acc).

-spec parse_version(version()) ->
    {ok, {major(), minor(), patch(), pre(), [build()]}} | {error, invalid_version}.
parse_version(Str) -> parse_version(Str, false).

-spec parse_version(version(), boolean()) ->
    {ok,{major(),minor(), patch(), pre(),[build()]}} | {error, invalid_version}.
parse_version(Str, Approximate) when is_binary(Str) ->
    try  parse_and_convert(Str, Approximate) of
        {ok, {_, _, undefined, _, _}} ->
            {error, invalid_version};
        {ok, _} = V  ->
            V;
        {error, invalid_version} ->
            {error, invalid_version}
    catch
        error:{badmatch, _} ->
            {error, invalid_version}
    end.

parse_condition(Version) -> parse_condition(Version, false).

parse_condition(Version, Approximate) ->
    try case parse_and_convert(Version, Approximate) of
            {ok, {Major, Minor, Patch, Pre, _Bld}} ->
                {Major, Minor, Patch, Pre};
            _ ->
                throw(invalid_matchspec)
        end
    catch
        error:{badmatch, _} ->
            throw(invalid_matchspec)
    end.

approximate_upper(Version) ->
    case Version of
        {Major, _Minor, undefined, _} ->
            {Major + 1, 0, 0, [0]};
        {Major, Minor, _Patch, _Pre} ->
            {Major, Minor + 1, 0, [0]}
    end.

matchable_to_string({Major, Minor, Patch, Pre}) ->
    Patch1 = case Patch of
                 P when P =:= undefined orelse P =:= false ->
                     <<"0">>;
                 _ ->
                     maybe_to_string(Patch)
             end,
    Pre1 = case Pre == [] of
               true ->
                   <<>>;
               false ->
                   case Pre of
                       [0] ->
                           <<"-0">>;
                       _ ->
                           Pre0 = maybe_to_string(Pre),
                           << <<"-">>/binary, Pre0/binary >>
                   end
           end,
    Major1 = maybe_to_string(Major),
    Minor1 = maybe_to_string(Minor),
    Patch2 = maybe_to_string(Patch1),
    Joined = join_bins([Major1, Minor1, Patch2], <<".">>),
    << Joined/binary, Pre1/binary >>.

pre_condition('>', Pre) ->
    PreLength = length(Pre),
    {'orelse',
     {'andalso', {'==', {length, '$4'}, 0},
      {const, PreLength /= 0}},
     {'andalso', {const, PreLength /= 0},
      {'orelse', {'>', {length, '$4'}, PreLength},
       {'andalso', {'==', {length, '$4'}, PreLength},
        {'>', '$4', {const, Pre}}}}}};
pre_condition('<', Pre) ->
    PreLength = length(Pre),
    {'orelse',
     {'andalso', {'/=', {length, '$4'}, 0},
      {const, PreLength == 0}},
     {'andalso', {'/=', {length, '$4'}, 0},
      {'orelse', {'<', {length, '$4'}, PreLength},
       {'andalso', {'==', {length, '$4'}, PreLength},
        {'<', '$4', {const, Pre}}}}}}.

no_pre_condition([]) ->
    {'orelse', '$5', {'==', {length, '$4'}, 0}};
no_pre_condition(_) ->
    {const, true}.

-spec parse_requirement(binary()) -> {ok, term()} | {error, invalid_requirement}.
parse_requirement(Source) ->
    Lexed = lexer(Source, []),
    to_matchspec(Lexed).

to_matchspec(Lexed) ->
    try case is_valid_requirement(Lexed) of
            true ->
                First = to_condition(Lexed),
                Rest = lists:nthtail(2, Lexed),
                {ok, [{{'$1', '$2', '$3', '$4', '$5'}, [to_condition(First, Rest)], ['$_']}]};
            false ->
                {error, invalid_requirement}
        end
    catch
        invalid_matchspec -> {error, invalid_requirement}
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
  when tuple_size(Version) == 3 ->
    {Op, {{'$1', '$2', '$3'}}, {const, Version}};
main_condition(Op, Version)
  when tuple_size(Version) == 4 ->
    {Op, {{'$1', '$2', '$3', '$4'}},
     {const, Version}}.

bisect(Str, Delim) ->
    [First | Rest ] =  binary:split(Str, [Delim],[global]),
    Rest1 = case Rest of
                [] ->
                    undefined;
                _ ->
                    join_bins(Rest, Delim)
            end,
    [First, Rest1].

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

join_bins(List, Delim) ->
    lists:foldl(fun(Bin, Acc) ->
                        case bit_size(Acc) of
                            N when N > 0 ->
                                <<Acc/binary, Delim/binary, Bin/binary>>;
                            _ ->
                                Bin
                        end
                end, <<>>, List).

maybe_patch(undefined, true) ->
    {ok, undefined};
maybe_patch(Patch, _) ->
    to_digits(Patch).

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
            {error, invalid_version}
    end.

parse_digits(<<Char/integer, Rest/binary>>, Acc)
  when is_integer(Char) andalso Char >= 48 andalso Char =< 57 ->
    parse_digits(Rest, <<Acc/binary, Char/integer>>);
parse_digits(<<>>, Acc) when byte_size(Acc) > 0 ->
    {ok, binary_to_integer(Acc)};
parse_digits(_, _) -> error.

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

split_ver(Str) ->
    case binary:split(Str, [<<".">>], [global]) of
        [Maj0, Min0] ->
            [Maj0, Min0, undefined, undefined];
        [Maj, Min, P] ->
            [Maj, Min, P, undefined];
        [Major, Minor, Patch | Rest] ->
            [Major, Minor, Patch, Rest];
        _ ->
            [error, error, error, error]
    end.

to_digits(Str) ->
    case has_leading_zero(Str) of
        S when S =:= undefined orelse S =:= false ->
            parse_digits(Str, <<>>);
        _ ->
            error
    end.

maybe_to_string(Part) ->
    case Part of
        Rewrite when is_binary(Rewrite) ->
            Rewrite;
        Int when is_integer(Int) ->
            integer_to_binary(Int);
        Rewrite when is_list(Rewrite)->
            list_to_binary(Rewrite)
    end.

is_valid_requirement([]) -> false;
is_valid_requirement([A | Next]) ->
    is_valid_requirement(A, Next).

is_valid_requirement(A, [])
  when is_binary(A) ->
    true;
is_valid_requirement(A, [B | Next])
  when (is_atom(A) andalso
        is_atom(B))
       andalso (A =:= '&&' orelse A =:= '||') ->
    is_valid_requirement(B, Next);
is_valid_requirement(A, [B | Next])
  when (is_binary(A) andalso
        is_atom(B))
       andalso (B =:= '&&' orelse B =:= '||') ->
    is_valid_requirement(B, Next);
is_valid_requirement(A, [B | Next])
  when (is_atom(A) andalso
        is_binary(B))
       andalso (A =:= '&&' orelse A =:= '||') ->
    is_valid_requirement(B, Next);
is_valid_requirement(A, [B | Next])
  when is_atom(A) andalso
       is_binary(B) ->
    is_valid_requirement(B, Next);
is_valid_requirement(_, _) -> false.
