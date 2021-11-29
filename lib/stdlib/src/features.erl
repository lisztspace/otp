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
         enabled_features/0,
         is_valid_feature/1,
         reserved_words/0,
         reserved_words/1,
         resword_add_feature/2,
         resword_add_features/2,
         resword_remove_feature/2,
         resword_remove_features/2,
         enable_feature/1,
         disable_feature/1,
         format_error/1,
         format_error/2]).

-on_load(init_features/0).

-type error() :: {?MODULE, {'invalid_features', [atom()]}}.

-define(VALID_FEATURE(Feature),
        (case is_valid_feature(Feature) of
             false ->
                 error(invalid_feature, [Feature],
                       [{error_info,
                         #{module => ?MODULE,
                           cause => #{1 => "unknown feature"}}}]);
             true -> ok
         end)).

%% Currently know features
-spec features() -> [atom()].
features() ->
    [ifn_expr, maybe_expr].

is_valid_feature(Ftr) ->
    lists:member(Ftr, features()).

%% New reserved words for a feature.  The current set is just for
%% tests and development.
-spec reserved_words(atom()) -> [atom()].
reserved_words(ifn_expr) ->
    ['ifn'];
reserved_words(maybe_expr) ->
    ['maybe', 'else'];
reserved_words(Ftr) ->
    ?VALID_FEATURE(Ftr).

%% Utilities
-spec resword_add_feature(atom(), fun((atom()) -> boolean())) ->
          {'ok', fun((atom()) -> boolean())}
              | {'error', error()}.
resword_add_feature(Feature, F) ->
    case is_valid_feature(Feature) of
        true ->
            {ok, add_feature(Feature, F)};
        false ->
            {error, {?MODULE, {invalid_features, [Feature]}}}
    end.

add_feature(Feature, F) ->
    Words = reserved_words(Feature),
    fun(Word) ->
            lists:member(Word, Words)
                orelse F(Word)
    end.

remove_feature(Feature, F) ->
    Words = reserved_words(Feature),
    fun(Word) ->
            case lists:member(Word, Words) of
                true -> false;
                false -> F(Word)
            end
    end.

-spec resword_remove_feature(atom(), fun((atom()) -> boolean())) ->
          {'ok', fun((atom()) -> boolean())}
              | {'error', error()}.
resword_remove_feature(Feature, F) ->
    case is_valid_feature(Feature) of
        true ->
            {ok, remove_feature(Feature, F)};
        false ->
            {error, {?MODULE, {invalid_features, [Feature]}}}
    end.

-spec resword_add_features([atom()], fun((atom()) -> boolean())) ->
          {'ok', fun((atom()) -> boolean())}
              | {'error', error()}.
resword_add_features(Features, F) ->
    case lists:all(fun is_valid_feature/1, Features) of
        true ->
            {ok, lists:foldl(fun add_feature/2, F, Features)};
        false ->
            IsInvalid = fun(Ftr) -> not is_valid_feature(Ftr) end,
            Invalid = lists:filter(IsInvalid, Features),
            {error, {?MODULE, {invalid_features, Invalid}}}
    end.

-spec resword_remove_features([atom()], fun((atom()) -> boolean())) ->
          {'ok', fun((atom()) -> boolean())}
              | {'error', error()}.
resword_remove_features(Features, F) ->
    case lists:all(fun is_valid_feature/1, Features) of
        true ->
            {ok, lists:foldl(fun remove_feature/2, F, Features)};
        false ->
            IsInvalid = fun(Ftr) -> not is_valid_feature(Ftr) end,
            Invalid = lists:filter(IsInvalid, Features),
            {error, {?MODULE, {invalid_features, Invalid}}}
    end.

format_error(Reason, [{_M, _F, _Args, Info}| _St]) ->
    ErrorInfo = proplists:get_value(error_info, Info, #{}),
    ErrorMap = maps:get(cause, ErrorInfo),
    ErrorMap#{reason => io_lib:format("~p: ~p", [?MODULE, Reason])}.

format_error({invalid_features, Ftrs}) ->
    case Ftrs of
        [Ftr] ->
            io_lib:fwrite("the feature ~p does not exist.", [Ftr]);
        Ftrs ->
            io_lib:fwrite("the features ~p do not exist.", [Ftrs])
    end.

%% Hold the state of which features are currently enabled.
%% This is almost static, so we go for an almost permanent state,
%% i.e., use persistent_term.
init_features() ->
    persistent_term:put(enabled_features, []),
    persistent_term:put(reserved_words, []),
    ok.

enable_feature(Feature) ->
    ?VALID_FEATURE(Feature),

    Features = persistent_term:get(enabled_features),
    case lists:member(Feature, Features) of
        true ->
            %% already there, maybe raise an error
            Features;
        false ->
            NewFeatures = [Feature| Features],
            persistent_term:put(enabled_features, NewFeatures),
            Res = persistent_term:get(reserved_words),
            New = reserved_words(Feature),
            persistent_term:put(reserved_words, New ++ Res),
            NewFeatures
    end.

disable_feature(Feature) ->
    ?VALID_FEATURE(Feature),

    Features = persistent_term:get(enabled_features),
    case lists:member(Feature, Features) of
        true ->
            NewFeatures = Features -- [Feature],
            persistent_term:put(enabled_features, NewFeatures),
            Res = persistent_term:get(reserved_words),
            Rem = reserved_words(Feature),
            persistent_term:put(reserved_words, Res -- Rem),
            NewFeatures;
        false ->
            %% Not there, possibly raise an error
            Features
    end.

enabled_features() ->
    persistent_term:get(enabled_features).

reserved_words() ->
    persistent_term:get(reserved_words).
