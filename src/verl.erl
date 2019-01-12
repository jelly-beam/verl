-module(verl).

-export([is_match/2, is_match/3, parse/1, parse_requirement/1, compile_requirement/1]).

-include("verl.hrl").

-define(VER, #{major => undefined,
               minor => undefined,
               patch => undefined,
               pre   => [],
               build => []
              }).

-spec parse(version()) -> {ok, t()} | error.
parse(Str) ->
    case verl_parser:parse_version(Str) of
        {ok, {Major, Minor, Patch, Pre, Build}} ->
            Bstr = case Build of
                       [] -> undefined;
                       _ -> binary:list_to_bin(Build)
                   end,
            #{major => Major,
              minor => Minor,
              patch => Patch,
              pre   => Pre,
              build => Bstr};
        _ ->
            error
    end.

-spec parse_requirement(requirement()) -> {ok, map()} | error.
parse_requirement(Str) ->
    case verl_parser:parse_requirement(Str) of
        {ok, Spec} ->
            {ok, #{string => Str, matchspec => Spec, compiled => false}};
        _ ->
            error
    end.

-spec compile_requirement(map()) -> {ok, map()} | error.
compile_requirement(Req) when is_map(Req) ->
    Ms = ets:match_spec_compile(maps:get(matchspec, Req)),
    maps:put(compiled, true, maps:put(matchspec, Ms, Req)).

-spec is_match(any(), any()) -> {ok, boolean()} | {error, binary()}.
is_match(Version, Requirement) when is_binary(Version) andalso is_binary(Requirement) ->
    case parse(Version) of
        Ver when is_map(Ver) ->
            case parse_requirement(Requirement) of
                {ok, Req} ->
                    is_match(Ver, Req, []);
                _ ->
                    {error, <<"invalid requirement">>}
            end;
        _ ->
            {error, <<"invalid version">>}
    end;
is_match(Version, Requirement) when is_map(Version) andalso is_binary(Requirement) ->
    case parse_requirement(Requirement) of
        {ok, Req} ->
            is_match(Version, Req);
        _ ->
            {error, <<"invalid requirement">>}
    end;
is_match(Version, Requirement) when is_binary(Version) andalso is_map(Requirement) ->
    case parse(Version) of
        Ver when is_map(Ver) ->
            is_match(Ver, Requirement);
        _ ->
            {error, <<"invalid version">>}
    end;
is_match(Version, Requirement) when is_map(Version) andalso is_map(Requirement) ->
    is_match(Version, Requirement, []).

is_match(Version, Requirement, Opts)
  when is_binary(Version) andalso is_binary(Requirement) ->
    case parse(Version) of
        Ver when is_map(Ver) ->
            case parse_requirement(Requirement) of
                {ok, Req} ->
                    is_match(Ver, Req, Opts);
                _ ->
                    {error, <<"invalid requirement">>}
            end;
        _ ->
            {error, <<"invalid version">>}
    end;
is_match(Version, Requirement, Opts)
  when is_map(Version) andalso is_binary(Requirement) ->
    case parse_requirement(Requirement) of
        {ok, Req} ->
            is_match(Version, Req, Opts);
        _ ->
            {error, <<"invalid requirement">>}
    end;
is_match(Version, Requirement, Opts)
  when is_binary(Version) and is_map(Requirement) ->
    case parse(Version) of
        V when is_map(V) ->
            is_match(V, Requirement, Opts);
        _ ->
            {error, <<"invalid version">>}
    end;
is_match(Version, #{matchspec := Spec, compiled := false} = R, Opts) when
      is_map(Version) and is_map(R) ->
    AllowPre = proplists:get_value(allow_pre, Opts, false),
    {ok, Result} = ets:test_ms(to_matchable(Version, AllowPre), Spec),
    Result /= false;
is_match(Version, #{matchspec := Spec, compiled := true} = R, Opts) when
      is_map(Version) and is_map(R) ->
    AllowPre = proplists:get_value(allow_pre, Opts, false),
    ets:match_spec_run([to_matchable(Version, AllowPre)], Spec) /= [];
is_match(_, _, _) ->
    {error, <<"bad arguments">>}.
to_matchable(#{major := Major, minor := Minor, patch := Patch, pre := Pre}, AllowPre) ->
    {Major, Minor, Patch, Pre, AllowPre}.

