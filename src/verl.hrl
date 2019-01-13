-type version() :: binary().
-type requirement() :: binary() | any().
-type major() :: non_neg_integer().
-type minor() :: non_neg_integer().
-type patch() :: non_neg_integer().
-type pre() :: [binary() | non_neg_integer()].
-type build() :: binary() | undefined.
-type version_t() :: #{
        major => major()
        , minor => minor()
        , patch => patch()
        , pre   => pre()
        , build => build()}.
-type requirement_t() :: #{
        string => requirement(),
        matchspec => list(),
        compiled  => boolean()
       }.
