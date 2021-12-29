%% This module is ignorant about any features and thus use 'ifn',
%% 'maybe' and 'then' as ordinary atoms.

-module(feature_ignorant).

-export([foo/0]).

foo() ->
    [ifn, maybe, else].

frob(maybe) -> false.

bar() ->
    [else, maybe].

baz(ifn) ->
    true.
