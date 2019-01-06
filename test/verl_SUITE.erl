-module(verl_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
    [lexer, parse].


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

parse(_Cfg) ->
    ok.
    %% {ok, #{major := 1, minor := 2, patch := 3}} = verl_parser:parse(<<"1.2.3">>).
    %%
  %% test "parse/1" do
  %%   assert {:ok, %Version{major: 1, minor: 2, patch: 3}} = Version.parse("1.2.3")
  %%   assert {:ok, %Version{major: 1, minor: 4, patch: 5}} = Version.parse("1.4.5+ignore")
  %%   assert {:ok, %Version{major: 0, minor: 0, patch: 1}} = Version.parse("0.0.1+sha.0702245")
  %%
  %%   assert {:ok, %Version{major: 1, minor: 4, patch: 5, pre: ["6-g3318bd5"]}} =
  %%            Version.parse("1.4.5-6-g3318bd5")
  %%
  %%   assert {:ok, %Version{major: 1, minor: 4, patch: 5, pre: [6, 7, "eight"]}} =
  %%            Version.parse("1.4.5-6.7.eight")
  %%
  %%   assert {:ok, %Version{major: 1, minor: 4, patch: 5, pre: ["6-g3318bd5"]}} =
  %%            Version.parse("1.4.5-6-g3318bd5+ignore")
  %%
  %%   assert Version.parse("foobar") == :error
  %%   assert Version.parse("2") == :error
  %%   assert Version.parse("2.") == :error
  %%   assert Version.parse("2.3") == :error
  %%   assert Version.parse("2.3.") == :error
  %%   assert Version.parse("2.3.0-") == :error
  %%   assert Version.parse("2.3.0+") == :error
  %%   assert Version.parse("2.3.0.") == :error
  %%   assert Version.parse("2.3.0.4") == :error
  %%   assert Version.parse("2.3.-rc.1") == :error
  %%   assert Version.parse("2.3.+rc.1") == :error
  %%   assert Version.parse("2.3.0-01") == :error
  %%   assert Version.parse("2.3.00-1") == :error
  %%   assert Version.parse("2.3.00") == :error
  %%   assert Version.parse("2.03.0") == :error
  %%   assert Version.parse("02.3.0") == :error
  %% end
  %%
 
