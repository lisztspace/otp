%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2022. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(features).

-export([features/0,
         reserved_words/1,
         resword_add_feature/2,
         resword_add_features/2,
         resword_remove_feature/2,
         resword_remove_features/2]).

%% Currently know features
-spec features() -> [atom()].
features() ->
    [ifn_expr, maybe_expr].

%% New reserved words for a feature.  The current set is just for
%% tests and development.
-spec reserved_words(atom()) -> [atom()].
reserved_words(ifn_expr) ->
    ['ifn'];
reserved_words(maybe_expr) ->
    ['maybe', 'else'].

%% Utilities
-spec resword_add_feature(atom(), fun((atom()) -> boolean())) ->
          fun((atom()) ->
                     boolean()).
resword_add_feature(Feature, F) ->
    Words = reserved_words(Feature),
    fun(Word) ->
            lists:member(Word, Words)
                orelse F(Word)
    end.

-spec resword_remove_feature(atom(), fun((atom()) -> boolean())) ->
          fun((atom()) ->
                     boolean()).
resword_remove_feature(Feature, F) ->
    Words = reserved_words(Feature),
    fun(Word) ->
            case lists:member(Word, Words) of
                true -> false;
                false -> F(Word)
            end
    end.

-spec resword_add_features([atom()], fun((atom()) -> boolean())) ->
          fun((atom()) ->
                     boolean()).
resword_add_features(Features, F) ->
    lists:foldl(fun resword_add_feature/2, F, Features).

-spec resword_remove_features([atom()], fun((atom()) -> boolean())) ->
          fun((atom()) ->
                     boolean()).
resword_remove_features(Features, F) ->
    lists:foldl(fun resword_remove_feature/2, F, Features).
