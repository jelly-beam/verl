-module(verl_parser).

-export([lexer/2]).

-type version() :: binary().
-type requirement() :: binary() | any().
-type major() :: non_neg_integer().
-type minor() :: non_neg_integer().
-type patch() :: non_neg_integer().
-type pre() :: [binary() | non_neg_integer()].
-type build() :: binary() | undefined.
-type t() :: #{
              major => major() 
            , minor => minor() 
            , patch => patch()
            , pre   => pre()
            , build => build()}.

%% -spec parse_requirement(binary()) :: {ok, term()} | error
%% parse_requirement(Source) ->
%%       Lexed = lexer(Source, []),
%%       to_matchspec(Lexed).
%%
%% to_matchspec(Lexed) ->
%% 	try case is_valid_requirement(Lexed) of
%%       true ->
%% 		First = to_condition(Lexed),
%%         Rest = lists:nth(2, First),
%%         {ok, [{{'$1', '$2', '$3', '$4', '$5'}, [to_condition(First, Rest)],
%%                ['$_']}]};
%%      false ->
%%         error
%%     end
%%     catch 
%%         invalid_matchspec -> error
%%     end.
%%
%% to_condition(['==', _Eversion@1 | _E]) ->
%%     _Ematchable@1 = parse_condition(_Eversion@1),
%%     main_condition('==', _Ematchable@1);
%% to_condition(['!=', _Eversion@1 | _E]) ->
%%     _Ematchable@1 = parse_condition(_Eversion@1),
%%     main_condition('/=', _Ematchable@1);
%% to_condition(['~>', _Eversion@1 | _E]) ->
%%     _Efrom@1 = parse_condition(_Eversion@1, true),
%%     _Eto@1 = approximate_upper(_Efrom@1),
%%     {'andalso',
%%      to_condition(['>=', matchable_to_string(_Efrom@1)]),
%%      to_condition(['<', matchable_to_string(_Eto@1)])};
%% to_condition(['>', _Eversion@1 | _E]) ->
%%     {_Emajor@1, _Eminor@1, _Epatch@1, _Epre@1} =
%% 	parse_condition(_Eversion@1),
%%     {'andalso',
%%      {'orelse',
%%       main_condition('>', {_Emajor@1, _Eminor@1, _Epatch@1}),
%%       {'andalso',
%%        main_condition('==', {_Emajor@1, _Eminor@1, _Epatch@1}),
%%        pre_condition('>', _Epre@1)}},
%%      no_pre_condition(_Epre@1)};
%% to_condition(['>=', _Eversion@1 | _E]) ->
%%     _Ematchable@1 = parse_condition(_Eversion@1),
%%     {'orelse', main_condition('==', _Ematchable@1),
%%      to_condition(['>', _Eversion@1])};
%% to_condition(['<', _Eversion@1 | _E]) ->
%%     {_Emajor@1, _Eminor@1, _Epatch@1, _Epre@1} =
%% 	parse_condition(_Eversion@1),
%%     {'orelse',
%%      main_condition('<', {_Emajor@1, _Eminor@1, _Epatch@1}),
%%      {'andalso',
%%       main_condition('==', {_Emajor@1, _Eminor@1, _Epatch@1}),
%%       pre_condition('<', _Epre@1)}};
%% to_condition(['<=', _Eversion@1 | _E]) ->
%%     _Ematchable@1 = parse_condition(_Eversion@1),
%%     {'orelse', main_condition('==', _Ematchable@1),
%%      to_condition(['<', _Eversion@1])}.
%%

%% to_condition(_Ecurrent@1, []) -> _Ecurrent@1;
%% to_condition(_Ecurrent@1,
%% 	     ['&&', _Eoperator@1, _Eversion@1 | _Erest@1]) ->
%%     to_condition({'andalso', _Ecurrent@1,
%% 		  to_condition([_Eoperator@1, _Eversion@1])},
%% 		 _Erest@1);
%% to_condition(_Ecurrent@1,
%% 	     ['||', _Eoperator@1, _Eversion@1 | _Erest@1]) ->
%%     to_condition({'orelse', _Ecurrent@1,
%% 		  to_condition([_Eoperator@1, _Eversion@1])},
%% 		 _Erest@1).
%%
%% main_condition(_Eop@1, _Eversion@1)
%%     when erlang:tuple_size(_Eversion@1) == 3 ->
%%     {_Eop@1, {{'$1', '$2', '$3'}}, {const, _Eversion@1}};
%% main_condition(_Eop@1, _Eversion@1)
%%     when erlang:tuple_size(_Eversion@1) == 4 ->
%%     {_Eop@1, {{'$1', '$2', '$3', '$4'}},
%%      {const, _Eversion@1}}.

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

-spec parse_version(binary()) -> {ok, t()} | error.
parse_version(_Str) -> 
    {ok, #{major => 1, minor => 2, patch => 3}}.
