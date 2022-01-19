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
         feature_info/1,
         enabled_features/0,
         is_valid_feature/1,
         load_allowed/1,
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

-export([features_used/1]).

-on_load(init_features/0).

-type type() :: 'extension' | 'backwards_incompatible_change'.
-type status() :: {'remove_planned'
                   | 'inclusion_planned'
                   | 'removed'
                   | 'included'
                   | 'experimental', release()}.
-type release() :: non_neg_integer().
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
    [ifn_expr, maybe_expr, ifnot_expr, unless_expr].

is_valid_feature(Ftr) ->
    lists:member(Ftr, features()).

-spec feature_info(atom()) -> FeatureInfoMap
              when
      Description :: string(),
      FeatureInfoMap ::
        #{description := Description,
          type := type(),
          status := status()
         }.
feature_info(ifn_expr) ->
    #{description =>
          "Inclusion of expression `ifn cond -> body end`, which "
      "evaluates `body` when cond is false.  This is a truly "
      "experimental feature, present only to show and use the "
      "support for experimental features.  Not extensively tested.  "
      "Implementated by a transformation in the parser.",
      status => {experimental, 24},
      type => extension};
feature_info(ifnot_expr) ->
    #{description =>
          "Inclusion of expression `ifnot cond -> body end`, which "
      "evaluates `body` when cond is false.  This is a truly "
      "experimental feature, present only to show and use the "
      "support for experimental features.  Not extensively tested.  "
      "Similar to ifn_expr, but with a deeper implementation.",
      status => {experimental, 25},
      type => extension};
feature_info(maybe_expr) ->
    #{description =>
          "Implementation of the maybe expression proposed in EEP49 -- "
      "Value based error handling.",
      status => {experimental, 25},
      type => extension};
feature_info(unless_expr) ->
    #{description =>
          "Introduction of new expression `unless <cond> -> <body> end."
      "Truly experimental.",
      status => {experimental, 25},
      type => extension};
feature_info(Ftr) ->
    ?VALID_FEATURE(Ftr).



%% New reserved words for a feature.  The current set is just for
%% tests and development.
-spec reserved_words(atom()) -> [atom()].
reserved_words(ifn_expr) ->
    ['ifn'];
reserved_words(maybe_expr) ->
    ['maybe', 'else'];
reserved_words(ifnot_expr) ->
    ['ifnot'];
reserved_words(unless_expr) ->
    ['unless'];
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

format_error({invalid_features, Features}) ->
    Fmt = fun F([Ftr]) -> io_lib:fwrite("'~p'", [Ftr]);
              F([Ftr1, Ftr2]) ->
                  io_lib:fwrite("'~p' and '~p'", [Ftr1, Ftr2]);
              F([Ftr| Ftrs]) ->
                  io_lib:fwrite("'~p', ~s", [Ftr, F(Ftrs)])
          end,
    case Features of
        [Ftr] ->
            io_lib:fwrite("the feature ~s does not exist.", [Fmt([Ftr])]);
        Ftrs ->
            io_lib:fwrite("the features ~s do not exist.", [Fmt(Ftrs)])
    end.

%% Hold the state of which features are currently enabled.
%% This is almost static, so we go for an almost permanent state,
%% i.e., use persistent_term.
init_features() ->
    persistent_term:put(enabled_features, []),
    persistent_term:put(reserved_words, []),
    case init:get_argument('enable-feature') of
        error ->
            %% no features enabled
            ok;
        {ok, Ftrs} ->
            %% FIXME Need to be paranoid about any failures here.
            %% Only convert to an existing atom.  This will also catch
            %% a too long atom.
            F = fun(String) ->
                        case catch list_to_existing_atom(String) of
                            {'EXIT', _} -> false;
                            Atom -> {is_valid_feature(Atom), Atom}
                        end
                end,
            lists:foreach(fun enable_feature/1,
                          lists:filtermap(F, lists:append(Ftrs)))
    end,
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

-spec load_allowed(binary()) -> boolean().
load_allowed(Binary) ->
    case erts_internal:beamfile_chunk(Binary, "Meta") of
        undefined ->
            true;
        Meta ->
            MetaData = erlang:binary_to_term(Meta),
            case proplists:get_value(enabled_features, MetaData) of
                undefined ->
                    true;
                Used ->
                    Enabled = enabled_features(),
                    lists:all(fun(UFtr) ->
                                      lists:member(UFtr, Enabled)
                              end,
                              Used)
            end
    end.


%% Return features used by module or beam file
features_used(Module) when is_atom(Module) ->
    case code:get_object_code(Module) of
        error ->
            not_found;
        {_Mod, Bin, _Fname} ->
            features_in(Bin)
    end;
features_used(FName) when is_list(FName) ->
    features_in(FName).

features_in(NameOrBin) ->
    case beam_lib:chunks(NameOrBin, ["Meta"], [allow_missing_chunks]) of
        {ok, {_, [{_, missing_chunk}]}} ->
            [];
        {ok, {_, [{_, Meta}]}} ->
            MetaData = erlang:binary_to_term(Meta),
            proplists:get_value(enabled_features, MetaData, []);
        _ ->
            not_found
    end.
