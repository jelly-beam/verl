-module(verl).

-export([parse/1]).

-define(VER, #{major => undefined, 
       minor => undefined,
       patch => undefined,
       pre   => [],
       build => []
      }).

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
