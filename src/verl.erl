-module(verl).

-export([compare/2, is_match/2, is_match/3, parse/1, parse_requirement/1, compile_requirement/1]).

-include("verl.hrl").

-define(VER, #{major => undefined,
               minor => undefined,
               patch => undefined,
               pre   => [],
               build => []
              }).


-spec compare(version(), version()) -> gt | eq | lt | {error, invalid_version}.
compare(Version1, Version2) ->
    ver_cmp(to_matchable(Version1,true), to_matchable(Version2, true)).

ver_cmp({Maj1, Min1, Patch1, Pre1, _}, {Maj2, Min2, Patch2, Pre2, _}) ->
    case {Maj1, Min1, Patch1} > {Maj2, Min2, Patch2} of
        true ->
            gt;
        false ->
            case {Maj1, Min1, Patch1} < {Maj2, Min2, Patch2} of
                true ->
                    lt;
                false ->
                    test_pre(Pre1, Pre2)
            end
    end;
ver_cmp(_, _) -> {error, invalid_version}.

test_pre(Pre1, Pre2) ->
    case pre_is_eq(Pre1, Pre2) of
        true ->
            gt;
        false ->
            case pre_is_eq(Pre2, Pre1) of
                true ->
                    lt;
                false ->
                    pre_cmp(Pre1, Pre2)
            end
    end.

pre_cmp(Pre1, Pre2) ->
    case Pre1 > Pre2 of
        true ->
            gt;
        false ->
            case Pre1 < Pre2 of
                true ->
                    lt;
                false ->
                    eq
            end
    end.

pre_is_eq(Pre1, Pre2) ->
    case Pre1 == [] of
        false -> false;
        true -> Pre2 /= []
    end.

-spec parse(version()) -> {ok, version_t()} | {error, invalid_version}.
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
            {error, invalid_version}
    end.

-spec parse_requirement(requirement()) ->
    {ok, requirement_t()} | {error, invalid_requirement}.
parse_requirement(Str) ->
    case verl_parser:parse_requirement(Str) of
        {ok, Spec} ->
            {ok, #{string => Str, matchspec => Spec, compiled => false}};
        _ ->
            {error, invalid_requirement}
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
                    {error, invalid_requirement}
            end;
        _ ->
            {error, invalid_version}
    end;
is_match(Version, Requirement) when is_map(Version) andalso is_binary(Requirement) ->
    case parse_requirement(Requirement) of
        {ok, Req} ->
            is_match(Version, Req);
        _ ->
            {error, invalid_requirement}
    end;
is_match(Version, Requirement) when is_binary(Version) andalso is_map(Requirement) ->
    case parse(Version) of
        Ver when is_map(Ver) ->
            is_match(Ver, Requirement);
        _ ->
            {error, invalid_version}
    end;
is_match(Version, Requirement) when is_map(Version) andalso is_map(Requirement) ->
    is_match(Version, Requirement, []).

is_match(Version, Requirement, Opts) when is_binary(Requirement) ->
    case parse_requirement(Requirement) of
        {ok, Req} ->
            is_match(Version, Req, Opts);
        _ ->
            {error, invalid_requirement}
    end;
is_match(Version, #{matchspec := Spec, compiled := false} = R, Opts) when is_map(R) ->
    AllowPre = proplists:get_value(allow_pre, Opts, true),
    {ok, Result} = ets:test_ms(to_matchable(Version, AllowPre), Spec),
    Result /= false;
is_match(Version, #{matchspec := Spec, compiled := true} = R, Opts)  when
      is_map(Version) andalso is_map(R) ->
    AllowPre = proplists:get_value(allow_pre, Opts, true),
    ets:match_spec_run([to_matchable(Version, AllowPre)], Spec) /= [];
is_match(_, _, _) ->
    {error, badarg}.

to_matchable(#{major := Major, minor := Minor, patch := Patch, pre := Pre}, AllowPre) ->
    {Major, Minor, Patch, Pre, AllowPre};
to_matchable(String, AllowPre) when is_binary(String) ->
    case verl_parser:parse_version(String) of
        {ok, {Major, Minor, Patch, Pre, _Build}} ->
            {Major, Minor, Patch, Pre, AllowPre};
        _ ->
            {error, invalid_version}
    end.
