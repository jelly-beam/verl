verl [![Hex Version](https://img.shields.io/hexpm/v/verl.svg)](https://hex.pm/packages/verl) [![CircleCI](https://circleci.com/gh/jelly-beam/verl.svg?style=svg)](https://circleci.com/gh/jelly-beam/verl) [![codecov](https://codecov.io/gh/jelly-beam/verl/branch/master/graph/badge.svg)](https://codecov.io/gh/jelly-beam/verl)
=====

The Elixir Version semver2 parser in Erlang. 

Build
-----

    $ rebar3 compile

Usage
------

    1> verl:parse(<<"1.2.3">>).
    #{build => undefined,major => 1,minor => 2,patch => 3,
      pre => []}
    2> verl:parse(<<"1.2.3+build">>).
    #{build => <<"build">>,major => 1,minor => 2,patch => 3,
      pre => []}
    3> verl:parse(<<"1.2.3-pre+build">>).
    #{build => <<"build">>,major => 1,minor => 2,patch => 3,
      pre => [<<"pre">>]}
    4> verl:parse(<<"1">>).
    error
    5> verl:parse(<<"2">>).
    error 

Don't want a map? Use the verl_parser module...

    1> verl_parser:parse_version(<<"1.2.3">>).
    {ok,{1,2,3,[],[]}}
    2> verl_parser:parse_version(<<"1.2.3+build">>).
    {ok,{1,2,3,[],[<<"build">>]}}
    3> verl_parser:parse_version(<<"1.2.3-pre+build">>).
    {ok,{1,2,3,[<<"pre">>],[<<"build">>]}}
    4> verl_parser:parse_version(<<"1">>).
    error

Credits
-------
  All credit goes to the Elixir team and contributors to Version and
  Version.Parser in Elixir standard lib for the algorithm and original
  implementation.
