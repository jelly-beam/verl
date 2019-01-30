-module(prop_verl).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_basic_valid_semver() ->
    ?FORALL({Maj, Min, P},{non_neg_integer(), non_neg_integer(), non_neg_integer()},
            begin
                Major = integer_to_binary(Maj),
                Minor = integer_to_binary(Min),
                Patch = integer_to_binary(P),
                V = <<Major/binary, <<".">>/binary,  Minor/binary, <<".">>/binary,  Patch/binary>>,
                {ok, #{build := undefined, major := Maj, minor := Min, patch := P, pre := []}} = verl:parse(V),
                true
            end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
