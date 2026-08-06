%%% ----------------------------------------------------------------------------
%%% @author Duncan Paul Attard
%%%
%%% @doc Module description (becomes module heading).
%%%
%%% @end
%%% 
%%% Copyright (c) 2022, Duncan Paul Attard <duncanatt@gmail.com>
%%%
%%% This program is free software: you can redistribute it and/or modify it 
%%% under the terms of the GNU General Public License as published by the Free 
%%% Software Foundation, either version 3 of the License, or (at your option) 
%%% any later version.
%%%
%%% This program is distributed in the hope that it will be useful, but WITHOUT 
%%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
%%% FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
%%% more details.
%%%
%%% You should have received a copy of the GNU General Public License along with 
%%% this program. If not, see <https://www.gnu.org/licenses/>.
%%% ----------------------------------------------------------------------------
-module(maxhml_eval).
-author("Duncan Paul Attard").

%%-compile(export_all).

%%% Includes.
-include_lib("stdlib/include/assert.hrl").
%%-include_lib("syntax_tools/include/merl.hrl").
-include("log.hrl").

%%% Public API.
-export([compile/2]).
-export([parse_string/1, parse_file/1]).

%%% Callbacks/Internal.
-export([visit/2]).
-export([modularise_hml/2,generate_init_block/2,generate_verdicts/0]).
-export([generate_state_update_calls/1, generate_state_update_args/1, is_state_update_var/1]).
-export([generate_state_management/0]).
-export([generate_sys_info_function/1,generate_all_states/0,agm_generation/0]).


%%% Types.
-export_type([af_maxhml/0]).

%%% Implemented behaviors.
-behavior(gen_eval).

%%% ----------------------------------------------------------------------------
%%% Macro and record definitions.
%%% ----------------------------------------------------------------------------

%% maxHML lexer and parser modules.
-define(LEXER_MOD, maxhml_lexer).
-define(PARSER_MOD, maxhml_parser).

%% maxHML logic AST node tags.
-define(HML_TRU, tt).
-define(HML_CORR, corr).
-define(HML_FLS, ff).
-define(HML_ACT, act).
-define(HML_POS, pos).
-define(HML_NEC, nec).
-define(HML_OR, 'or').
-define(HML_AND, 'and').
-define(HML_MAX, max).
-define(HML_VAR, var).
-define(MFARGS, mfargs).

-define(IS_TERMINATING_HML(Node),
    case Node of
        {?HML_TRU, _} -> true;
        {?HML_FLS, _} -> true;
        _ -> false
    end
).

-define(IS_RECURSIVE_HML(Node),
    case Node of
        {?HML_VAR, _, _} -> true;
        _ -> false
    end
).

%% Monitor AST node tags.
-define(MON_ACC, yes).
-define(MON_CORR, corrupt).
-define(MON_REJ, no).
-define(MON_ACT, act).
-define(MON_CHS, chs).
-define(MON_OR, 'or').
-define(MON_AND, 'and').
-define(MON_REC, rec).
-define(MON_VAR, var).

%% ETS table for monitor information.
-define(MONITOR_TABLE, monitorTable).

%% Monitor environment keys.
-define(KEY_ENV, env).
-define(KEY_STR, str).
-define(KEY_VAR, var).
-define(KEY_PAT, pat).

%% State management. %% TODO : we have to normalise the var names + atoms - either use var name or atom not both.
-define(STATE_UPDATE_EXCLUDED_VARS, ['_', 'From', "_", "From"]).

%% Placeholder management.
%%-define(PH_NAMES, [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]).
-define(PH_NAMES, [a]).
-define(PH_PRF, "_@").
-define(KEY_PH_NAMES, ph_names).
-define(KEY_PH_CNT, ph_cnt).

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type line() :: erl_anno:line().
%% Line number in source.

-type with() ::
    {with, line(), gen_eval:af_mfargs()} |
    {with, line(), gen_eval:af_mfargs(), gen_eval:af_constraint()}.
%% Process instrumentation selection MFArgs.

-type spec() :: {spec, line(), with(), af_maxhml()}.
%% Instrumentation specification abstract form.

-type af_maxhml() ::
    af_hml_tt() | af_hml_ff() |
    af_hml_pos() | af_hml_nec() |
    af_hml_or() | af_hml_and() |
    af_hml_max() | af_hml_var().
%% maxHML formulae abstract form.

-type af_hml_ff() :: {ff, line()}.
-type af_hml_tt() :: {tt, line()}.
-type af_hml_pos() :: {pos, line(), gen_eval:af_sym_act(), af_maxhml()}.
-type af_hml_nec() :: {nec, line(), gen_eval:af_sym_act(), af_maxhml()}.
-type af_hml_or() :: {'or', line(), af_maxhml(), af_maxhml()}.
-type af_hml_and() :: {'and', line(), af_maxhml(), af_maxhml()}.
-type af_hml_max() :: {max, line(), af_hml_var(), af_maxhml()}.
-type af_hml_var() :: {var, line(), atom()}.
%% HML formulae abstract form.

%%% ----------------------------------------------------------------------------
%%% Public API.
%%% ----------------------------------------------------------------------------

compile(File, Opts) ->
  reset_generation_state(),
  case parse_file(File) of
    {ok, skip} ->
      gen_eval:compile(?MODULE, ?LEXER_MOD, ?PARSER_MOD, File, Opts);
    {ok, Ast} ->
      case validate_supported_fragment(Ast) of
        ok ->
          gen_eval:compile(?MODULE, ?LEXER_MOD, ?PARSER_MOD, File, Opts);
        Error = {error, Reason} ->
          ?ERROR("Refusing to synthesise '~s': ~p.", [File, Reason]),
          Error
      end;
    {error, _} ->
      % Lexing and parsing errors are reported through the usual path.
      gen_eval:compile(?MODULE, ?LEXER_MOD, ?PARSER_MOD, File, Opts)
  end.

parse_string(String) ->
  gen_eval:parse_string(?LEXER_MOD, ?PARSER_MOD, String).

parse_file(File) ->
  gen_eval:parse_file(?LEXER_MOD, ?PARSER_MOD, File).

%%% ----------------------------------------------------------------------------
%%% Supported-fragment validation.
%%%
%%% The modular generator compiles a necessity whose continuation is a verdict
%%% or a recursion variable into a wrapper function with no receive; that is
%%% sound only when a parent conjunction state has already consumed the
%%% decisive event. Shapes outside the fragment below would therefore yield
%%% monitors that emit verdicts without evidence (or consume events twice),
%%% so synthesis refuses them up front instead of generating unsound code.
%%% ----------------------------------------------------------------------------

-spec validate_supported_fragment(Ast) -> ok | {error, Reason} when
    Ast :: [spec()],
    Reason :: {unsupported_property_fragment, term()}.
validate_supported_fragment([{form, _, {sel, _, _, _}, Phi}]) ->
    validate_init_root(Phi);
validate_supported_fragment([_, _ | _]) ->
    {error, {unsupported_property_fragment, multiple_properties}};
validate_supported_fragment(Other) ->
    {error, {unsupported_property_fragment, Other}}.

%% The property must open with an initialisation modality so that the
%% generated init block owns the first receive.
validate_init_root({Mod, _, {act, _, {init, _, _, _, _}, _Guard}, Cont})
    when Mod =:= ?HML_NEC; Mod =:= ?HML_POS ->
    validate_init_continuation(Cont);
validate_init_root(Node) ->
    {error, {unsupported_property_fragment, {initial_modality, node_tag(Node)}}}.

%% After the init modality: a verdict (the init block itself receives the
%% init event), maximal recursion, a two-branch conjunction, or a chain of
%% necessities that terminates in one of those receive-owning shapes.
validate_init_continuation({Verdict, _}) when Verdict =:= ?HML_TRU; Verdict =:= ?HML_FLS ->
    ok;
validate_init_continuation(Cont) ->
    validate_receive_owner(Cont).

validate_receive_owner({?HML_MAX, _, {?HML_VAR, _, _}, Body}) ->
    validate_receive_owner(Body);
validate_receive_owner(And = {?HML_AND, _, _, _}) ->
    validate_conjunction(And);
validate_receive_owner({?HML_NEC, _, {act, _, _, _}, Cont}) ->
    case Cont of
        {Verdict, _} when Verdict =:= ?HML_TRU; Verdict =:= ?HML_FLS ->
            % Wrapper with no receive: the verdict would fire one event early.
            {error, {unsupported_property_fragment, necessity_into_verdict}};
        {?HML_VAR, _, _} ->
            % Wrapper looping without consuming an event.
            {error, {unsupported_property_fragment, necessity_into_recursion}};
        _ ->
            validate_receive_owner(Cont)
    end;
validate_receive_owner(Node) ->
    {error, {unsupported_property_fragment, node_tag(Node)}}.

%% Conjunction states own the receive for both branches, so each branch must
%% be a necessity whose continuation is a wrapper-safe verdict or recursion
%% variable. Deeper branch continuations would consume the same event twice.
validate_conjunction({?HML_AND, _, Left, Right}) ->
    case {validate_conjunction_branch(Left), validate_conjunction_branch(Right)} of
        {ok, ok} -> ok;
        {{error, _} = Error, _} -> Error;
        {_, {error, _} = Error} -> Error
    end.

validate_conjunction_branch({?HML_NEC, _, {act, _, {init, _, _, _, _}, _}, _}) ->
    {error, {unsupported_property_fragment, initial_conjunction}};
validate_conjunction_branch({?HML_NEC, _, {act, _, _, _}, Psi}) ->
    case Psi of
        {Verdict, _} when Verdict =:= ?HML_TRU; Verdict =:= ?HML_FLS -> ok;
        {?HML_VAR, _, _} -> ok;
        _ -> {error, {unsupported_property_fragment, {conjunction_branch, node_tag(Psi)}}}
    end;
validate_conjunction_branch(Node) ->
    {error, {unsupported_property_fragment, {conjunction_branch, node_tag(Node)}}}.

node_tag(Node) when is_tuple(Node), tuple_size(Node) > 0 ->
    element(1, Node);
node_tag(Node) ->
    Node.

%%% ----------------------------------------------------------------------------
%%% Generation-scoped memoisation of generated-function argument lists.
%%%
%%% Previously stored in persistent_term keyed by generated function-name
%%% atoms: entries were never erased and collided across compilations, so a
%%% second property compiled in the same VM inherited the first property's
%%% variable names (audit finding C2). The memo now lives in the process
%%% dictionary under a dedicated namespace and is erased when compilation
%%% starts.
%%% ----------------------------------------------------------------------------

-define(KEY_FUN_ARGS, maxhml_fun_args).

reset_generation_state() ->
    _ = [erase(Key) || {Key = {?KEY_FUN_ARGS, _}, _} <- get()],
    ok.

fun_args_put(Name, Args) ->
    put({?KEY_FUN_ARGS, Name}, Args),
    ok.

fun_args_get(Name) ->
    case get({?KEY_FUN_ARGS, Name}) of
        undefined -> error({missing_function_args, Name});
        Args -> Args
    end.

fun_args_get(Name, Default) ->
    case get({?KEY_FUN_ARGS, Name}) of
        undefined -> Default;
        Args -> Args
    end.

%%% ----------------------------------------------------------------------------
%%% Functions to generate the modular functions for the monitor, one time
%%% for each monitor action.
%%% This section also generates the verdict functions for the monitor and the entry (receive) block.
%%% ----------------------------------------------------------------------------
%%%
-spec generate_verdict_function({Vrd, _}, _Opts) -> erl_syntax:syntaxTree() when
    Vrd :: af_maxhml(),
    _Opts :: opts:options().
generate_verdict_function({Vrd, _}, _Opts) ->

    FromVar = erl_syntax:variable("From"), 

    case Vrd of
        ?HML_TRU ->
            erl_syntax:function(
                erl_syntax:atom(generate_function_name({Vrd, 0})),
                [erl_syntax:clause([FromVar], none, [erl_syntax:infix_expr(FromVar,erl_syntax:operator("!"),erl_syntax:atom(?MON_ACC))])]
            );
        ?HML_FLS ->
            erl_syntax:function(
                erl_syntax:atom(generate_function_name({Vrd, 0})),
                [erl_syntax:clause([FromVar], none, [erl_syntax:infix_expr(FromVar,erl_syntax:operator("!"),erl_syntax:atom(?MON_REJ))])]
            )
    end.

-spec generate_function(Node, Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    Opts :: opts:options().
generate_function(_Node = {Vrd, _LineNumber}, _Opts) when
    Vrd =:= ?HML_TRU; Vrd =:= ?HML_FLS
->
    [];
generate_function(Var = {?HML_VAR, _, _Name}, _Opts) ->

    ?TRACE("Generating function for 'var' node ~p. ~n ", [_Name]),
    _FunctionName = generate_function_name(Var),
    % FunctionArgs = lists:usort(
    %     lists:flatten([[erl_syntax:variable(V) || V <- generate_function_args(Var, [])]])
    % ),
    % ?TRACE("Function args are ~p. ~n", [FunctionArgs]),
    % [erl_syntax:function(erl_syntax:atom(FunctionName), [  erl_syntax:clause([], none, [erl_syntax:atom(?MON_VAR)])])];
    [];

generate_function(Node = {?HML_MAX, LineNumber, _Var = {?HML_VAR, _, _}, Phi}, _Opts) ->
    
    FunctionName = generate_function_name(Node),
    ?TRACE("Generating function ~p for 'max' node from src line ~p. ~n ", [FunctionName,LineNumber]),
    FunctionArgs = 
        case fun_args_get(FunctionName, empty) of
            empty ->
                fun_args_put(FunctionName, generate_function_args(Node, [])),
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(FunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(FunctionName)])
        end,

 NextFunctionName = generate_function_name(Phi),
    NextFunctionArgs = 
        case fun_args_get(NextFunctionName, empty) of
            empty ->
                fun_args_put(NextFunctionName, generate_function_args(Phi, [])),
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(NextFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(NextFunctionName)])
        end,

% ?DOUBLE CHECK THIS SECTION
    Clause = erl_syntax:clause(
        FunctionArgs,
        none,
        [erl_syntax:application(erl_syntax:atom(NextFunctionName), NextFunctionArgs)]
    ),

    Function = erl_syntax:function(
        erl_syntax:atom(FunctionName),
        [Clause]
    ),
    ReplayBridge = maxhml_agm_codegen:generate_replay_bridge(
        FunctionName,
        FunctionArgs,
        NextFunctionName,
        NextFunctionArgs
    ),
    
    ?TRACE("Generated function ~p. ~n", [FunctionName]),

    [Function, ReplayBridge | lists:flatten([generate_function(Phi, _Opts)])];
generate_function(
    OuterNode =
        {?HML_AND, _,
            InnerLeftNode =
                {ModLeft, _, _PhiLeftNode = {_, LineNumberLeft, PatPhiLeft, GuardPhiLeft}, PsiLeft},
            InnerRightNode =
                {ModRight, _, _PhiRightNode = {_, LineNumberRight, PatPhiRight, GuardPhiRight},
                    PsiRight}},
    _Opts
) when ModLeft =:= ?HML_NEC; ModRight =:= ?HML_POS; ModRight =:= ?HML_NEC; ModLeft =:=?HML_POS ->
    
    LeftNodeFunctionName = generate_function_name(InnerLeftNode),
    RightNodeFunctionName = generate_function_name(InnerRightNode),
    CompositeFunctionName = list_to_atom(
        atom_to_list(LeftNodeFunctionName) ++ atom_to_list(RightNodeFunctionName)
    ),

    ?TRACE("Generating function ~p for 'and' node from src lines ~p and ~p. ~n ", [
       CompositeFunctionName, LineNumberLeft, LineNumberRight
    ]),

    BoundVarsLeft = extract_bound_vars_from_guard(OuterNode),

    LeftNodeArgs = lists:flatten([erl_syntax:variable(V) || V <- generate_function_args(InnerLeftNode, BoundVarsLeft)]),
    PsiBoundVarsLeft = extract_bound_vars_from_guard(PsiLeft),
    PsiLeftFunctionArgs = 
        case fun_args_get(LeftNodeFunctionName, empty) of
            empty ->
                fun_args_put(LeftNodeFunctionName, generate_function_args(PsiLeft, PsiBoundVarsLeft)),
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(LeftNodeFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(LeftNodeFunctionName)])
        end,

    BoundVarsRight = extract_bound_vars_from_guard(OuterNode),
    RightNodeArgs = lists:flatten([erl_syntax:variable(V) || V <- generate_function_args(InnerRightNode, BoundVarsRight)]),
    PsiBoundVarsRight = extract_bound_vars_from_guard(PsiRight),
    PsiRightFunctionArgs = 
        case fun_args_get(RightNodeFunctionName, empty) of
            empty ->
                fun_args_put(RightNodeFunctionName, generate_function_args(PsiRight, PsiBoundVarsRight)),
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(RightNodeFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(RightNodeFunctionName)])
        end,

    CompositeFunctionArgs = 
        case fun_args_get(CompositeFunctionName, empty) of
            empty ->
                fun_args_put(CompositeFunctionName, lists:flatten([LeftNodeArgs, RightNodeArgs])),
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(CompositeFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(CompositeFunctionName)])
        end,
   
 
    BoundedVarsLeftClean= lists:usort(lists:filter(fun(Elem) -> not lists:member(Elem, ['_']) end, BoundVarsLeft)),
    BoundedVarsRightClean= lists:usort(lists:filter(fun(Elem) -> not lists:member(Elem, ['_']) end, BoundVarsRight)),

    LeftStateUpdates =
        case ?IS_TERMINATING_HML(PsiLeft) of
            true ->
                [];
            false ->
                [erl_syntax:application(
                    erl_syntax:atom(update_current_state),
                    lists:flatten([
                        erl_syntax:variable(V)
                        || V <- BoundedVarsLeftClean
                    ])
                )]
        end,
    RightStateUpdates =
        case ?IS_TERMINATING_HML(PsiRight) of
            true ->
                [];
            false ->
                [erl_syntax:application(
                    erl_syntax:atom(update_current_state),
                    lists:flatten([
                        erl_syntax:variable(V)
                        || V <- BoundedVarsRightClean
                    ])
                )]
        end,

    LeftNodeClause = erl_syntax:clause(
        [gen_eval:pat_tuple(PatPhiLeft)],
        GuardPhiLeft,
        LeftStateUpdates ++ [
            erl_syntax:application(
                erl_syntax:atom(LeftNodeFunctionName),
                PsiLeftFunctionArgs
            )
        ]
    ),

    RightNodeClause = erl_syntax:clause(
        [gen_eval:pat_tuple(PatPhiRight)],
        GuardPhiRight,
        RightStateUpdates ++ [
            erl_syntax:application(
                erl_syntax:atom(RightNodeFunctionName),
                PsiRightFunctionArgs
            )
        ]
    ),

    ReductionFun = maxhml_agm_codegen:generate_monitor_reduction_fun([
        {
            ModLeft,
            PatPhiLeft,
            GuardPhiLeft,
            PsiLeft,
            generate_function_name(PsiLeft),
            PsiLeftFunctionArgs
        },
        {
            ModRight,
            PatPhiRight,
            GuardPhiRight,
            PsiRight,
            generate_function_name(PsiRight),
            PsiRightFunctionArgs
        }
    ]),
    CaseExpression =
        maxhml_agm_codegen:generate_missing_event_recovery_case(ReductionFun),

    MissingEventClause = erl_syntax:clause(
        [gen_eval:pat_tuple({missing_event})],
        none,
        [CaseExpression]),
    ReceiveClause = erl_syntax:clause(
        CompositeFunctionArgs,
        none,
        [erl_syntax:receive_expr(
            [LeftNodeClause, RightNodeClause, MissingEventClause]
            ++ maxhml_agm_codegen:generate_checkpoint_clause(
                CompositeFunctionName,
                CompositeFunctionName,
                CompositeFunctionArgs
            )
        )]
    ),

    Function = erl_syntax:function(
        erl_syntax:atom(CompositeFunctionName),
        [ReceiveClause]
    ),
    ReplayFunctions = maxhml_agm_codegen:generate_receive_state_replay(
        CompositeFunctionName,
        CompositeFunctionArgs,
        [
            #{
                pattern => gen_eval:pat_tuple(PatPhiLeft),
                guard => GuardPhiLeft,
                ordinary_body => erl_syntax:clause_body(LeftNodeClause),
                terminal => ?IS_TERMINATING_HML(PsiLeft),
                state_updates => LeftStateUpdates,
                next_function => generate_function_name(PsiLeft),
                next_args => PsiLeftFunctionArgs
            },
            #{
                pattern => gen_eval:pat_tuple(PatPhiRight),
                guard => GuardPhiRight,
                ordinary_body => erl_syntax:clause_body(RightNodeClause),
                terminal => ?IS_TERMINATING_HML(PsiRight),
                state_updates => RightStateUpdates,
                next_function => generate_function_name(PsiRight),
                next_args => PsiRightFunctionArgs
            }
        ]
    ),

    ?TRACE("Generated function ~p. ~n", [CompositeFunctionName]),
    [Function]
    ++ ReplayFunctions
    ++ lists:flatten([
        generate_function(InnerLeftNode, _Opts),
        generate_function(InnerRightNode, _Opts)
    ]);
generate_function(Node = {?HML_NEC, LineNumber, {act, _, Pat, Guard}, Phi}, _Opts) ->
    BoundVars = extract_bound_vars_from_guard(Node),
    FunctionName = generate_function_name(Node),
    ?TRACE("Generating function ~p for 'nec' node from src line ~p. ~n ", [FunctionName, LineNumber]),
    FunctionArgs = 
        case fun_args_get(FunctionName, empty) of
            empty ->
                fun_args_put(FunctionName, generate_function_args(Node, BoundVars)),
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(FunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(FunctionName)])
        end,

    NextBoundVars = extract_bound_vars_from_guard(Phi),
    NextFunctionName = generate_function_name(Phi),
    NextFunctionArgs = 
        case fun_args_get(NextFunctionName, empty) of
            empty ->
                fun_args_put(NextFunctionName, generate_function_args(Phi, NextBoundVars)),
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(NextFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(NextFunctionName)])
        end,

    % remove extra fluff for function call 
    BoundedVars=lists:filter(fun(Elem) -> not lists:member(Elem, ['_']) end, BoundVars),
    StateUpdates = [
        erl_syntax:application(
            erl_syntax:atom(update_current_state),
            lists:flatten([
                erl_syntax:variable(V)
                || V <- BoundedVars
            ])
        )
    ],
    Clause = erl_syntax:clause(
        [gen_eval:pat_tuple(Pat)],
        Guard,
        StateUpdates ++ [
            erl_syntax:application(
                erl_syntax:atom(NextFunctionName),
                NextFunctionArgs
            )
        ]
    ),

    ReductionFun = maxhml_agm_codegen:generate_monitor_reduction_fun([
        {
            ?HML_NEC,
            Pat,
            Guard,
            Phi,
            NextFunctionName,
            NextFunctionArgs
        }
    ]),
    CaseExpression =
        maxhml_agm_codegen:generate_missing_event_recovery_case(ReductionFun),

    MissingEventClause = erl_syntax:clause(
        [gen_eval:pat_tuple({missing_event})],
        none,
        [CaseExpression]),

    ReceiveClause = erl_syntax:clause(
        FunctionArgs,
        none,
        [erl_syntax:receive_expr(
            [Clause, MissingEventClause]
            ++ maxhml_agm_codegen:generate_checkpoint_clause(
                FunctionName,
                FunctionName,
                FunctionArgs
            )
        )]
    ),

    % remove extra fluff for function call 
    {FunctionClauses, ReplayFunctions} =
        case ?IS_TERMINATING_HML(Phi) of
            true ->
                ?TRACE("Terminating function detected - Atomic termination generated. ~n"),
                {
                    [erl_syntax:clause(
                        FunctionArgs,
                        none,
                        [lists:nth(2, erl_syntax:clause_body(Clause))]
                    )],
                    []
                };
            false ->
                case ?IS_RECURSIVE_HML(Phi) of
                    true ->
                        ?TRACE("Recursive function detected - Recursive call generated. ~n"),
                        {
                            [erl_syntax:clause(
                                FunctionArgs,
                                none,
                                [lists:nth(2, erl_syntax:clause_body(Clause))]
                            )],
                            [maxhml_agm_codegen:generate_replay_bridge(
                                FunctionName,
                                FunctionArgs,
                                NextFunctionName,
                                NextFunctionArgs
                            )]
                        };
                    false ->
                        {
                            [ReceiveClause],
                            maxhml_agm_codegen:generate_receive_state_replay(
                                FunctionName,
                                FunctionArgs,
                                [
                                    #{
                                        pattern => gen_eval:pat_tuple(Pat),
                                        guard => Guard,
                                        ordinary_body =>
                                            erl_syntax:clause_body(Clause),
                                        terminal => false,
                                        state_updates => StateUpdates,
                                        next_function => NextFunctionName,
                                        next_args => NextFunctionArgs
                                    }
                                ]
                            )
                        }
                end
        end,
    Function = erl_syntax:function(
        erl_syntax:atom(FunctionName),
        FunctionClauses
    ),

    ?TRACE("Generated function ~p. ~n", [FunctionName]),

    [Function]
    ++ ReplayFunctions
    ++ lists:flatten([generate_function(Phi, _Opts)]).

%% @public Generates the receive block for the function look up. This
%% is the entry point for the look up of the function to be executed.
-spec generate_init_block(Node, _Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    _Opts :: opts:options().
generate_init_block(_OuterNode =
        {?HML_AND, _,
            _InnerLeftNode =
                {_ModLeft, _, _PhiLeftNode = {_, LineNumberLeft, PatPhiLeft = {init, _, _LeftPhiPid2, _LeftPhiPid, _LeftPhiMFArgs}, GuardPhiLeft}, PsiLeft},
            _InnerRightNode =
                {_ModRight, _, _PhiRightNode = {_, LineNumberRight, PatPhiRight = {init, _, _RightPhiPid2, _RightPhiPid, _RightPhiMFArgs}, GuardPhiRight},
                    PsiRight}},
    _Opts) ->

    ?TRACE("Generating init block for 'and' node from src lines ~p and ~p. ~n ", [
        LineNumberLeft, LineNumberRight
    ]),

    LeftNodeNextFunctionName = generate_function_name(PsiLeft),
    RightNodeNextFunctionName = generate_function_name(PsiRight),

    PsiBoundVarsLeft = extract_bound_vars_from_guard(PsiLeft),
    PsiLeftFunctionArgs = 
        case fun_args_get(LeftNodeNextFunctionName, empty) of
            empty ->
                fun_args_put(LeftNodeNextFunctionName, generate_function_args(PsiLeft, PsiBoundVarsLeft)),
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(LeftNodeNextFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(LeftNodeNextFunctionName)])
        end,

    PsiBoundVarsRight = extract_bound_vars_from_guard(PsiRight),
    PsiRightFunctionArgs = 
        case fun_args_get(RightNodeNextFunctionName, empty) of
            empty ->
                fun_args_put(RightNodeNextFunctionName, generate_function_args(PsiRight, PsiBoundVarsRight)),
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(RightNodeNextFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- fun_args_get(RightNodeNextFunctionName)])
        end,
 
    LeftNodeClause = erl_syntax:clause(
        [gen_eval:pat_tuple(PatPhiLeft)],
        GuardPhiLeft,
        [
            erl_syntax:application(
                erl_syntax:atom(LeftNodeNextFunctionName), PsiLeftFunctionArgs
            )
        ]
    ),

    RightNodeClause = erl_syntax:clause(
        [gen_eval:pat_tuple(PatPhiRight)],
        GuardPhiRight,
        [
            erl_syntax:application(
                erl_syntax:variable(RightNodeNextFunctionName), PsiRightFunctionArgs
            )
        ]
    ),

    ReceiveExpr = erl_syntax:receive_expr([LeftNodeClause, RightNodeClause]),
    ?TRACE("Generated init block for 'and' node. ~n"),

    % State Management
    % TODO: This needs a refactor... check init_block also, can be combined.
    % TODO: DOUBLE CHECK THIS START STATE -> MAYBE LOOK INTO IT ABIT -> FOR NOW WE ALWAYS ASSUME S0 as start state
    EtsInitExpr = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(new), [erl_syntax:atom(sus_state), erl_syntax:list([erl_syntax:atom(named_table),erl_syntax:atom(public),erl_syntax:atom(set)])]),
    EtsInsertCurrentState = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(insert), [erl_syntax:atom(sus_state), erl_syntax:tuple([erl_syntax:atom(current_state),erl_syntax:atom(s0)])]),
    EtsInsertPreviousState = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(insert), [erl_syntax:atom(sus_state), erl_syntax:tuple([erl_syntax:atom(previous_state),erl_syntax:atom(undefined)])]),

    lists:flatten([EtsInitExpr,EtsInsertCurrentState,EtsInsertPreviousState,ReceiveExpr]);

generate_init_block({Mod, _, {act, _, Pat = {init, _, _Pid2, _Pid, _MFArgs}, Guard}, Phi}, Opts) when Mod =:= ?HML_NEC; Mod =:= ?HML_POS->
    ?TRACE("Generating init block for ~p node. ~n", [Mod]),

    NextFunctionName = generate_function_name(Phi),
    NextFunctionArgs = generate_function_args(Phi, []),

    % The supplied-model convention treats the payload bound by the init
    % pattern as the model's first event (see the token-system walkthroughs),
    % so the init clause advances the stored state exactly like ordinary
    % event clauses. When that payload is not a model event from the initial
    % state, reachable_state/2 yields [] and every later recovery withholds
    % conservatively; the START row supplies the pre-init state.
    StateUpdateCalls = generate_state_update_calls(Pat),

    fun_args_put(NextFunctionName, NextFunctionArgs),
    AnonFunEntry =
        case Mod of
            ?HML_NEC ->
                [erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, StateUpdateCalls ++ [
                    erl_syntax:application(erl_syntax:atom(NextFunctionName), lists:flatten([erl_syntax:variable(V) || V <- NextFunctionArgs]))
            ])];
            ?HML_POS ->
                [erl_syntax:clause([gen_eval:pat_tuple(Pat)], (Guard), StateUpdateCalls ++ [
                    erl_syntax:application(erl_syntax:atom(NextFunctionName), lists:flatten([erl_syntax:variable(V) || V <- NextFunctionArgs]))
                ]),
                erl_syntax:clause([gen_eval:pat_tuple(Pat)], invert_operator(Guard), [
                erl_syntax:application(erl_syntax:atom(rejection), lists:flatten([erl_syntax:variable("From")]))
                ])
            ]
        end,


    ReceiveExpr = erl_syntax:receive_expr(AnonFunEntry),
    ?TRACE("Generated init block for ~p node. ~n",[Mod]),

    EtsInitExpr = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(new), [erl_syntax:atom(sus_state), erl_syntax:list([erl_syntax:atom(named_table),erl_syntax:atom(public),erl_syntax:atom(set)])]),
    EtsInsertCurrentState = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(insert), [erl_syntax:atom(sus_state), erl_syntax:tuple([erl_syntax:atom(current_state),erl_syntax:atom(initial_state_from_opts(Opts))])]),
    EtsInsertPreviousState = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(insert), [erl_syntax:atom(sus_state), erl_syntax:tuple([erl_syntax:atom(previous_state),erl_syntax:atom(undefined)])]),

    lists:flatten([EtsInitExpr,EtsInsertCurrentState,EtsInsertPreviousState,ReceiveExpr]);
generate_init_block(N, _) ->
    ?ERROR("Invalid node for init block generation :~p.",[N]),
    [].

%%% @private Resolves the generated monitor's initial SUS state from the
%%% supplied system-information model. The parsed START row, when present,
%%% names the state the model occupies before the first observed event;
%%% models without one fall back to the historical default `s0'.
initial_state_from_opts(Opts) ->
    try sys_info_parser:parse_file(opts:monitor_table_opt(Opts)) of
        Transitions when is_list(Transitions) ->
            case lists:keyfind('START', 1, Transitions) of
                {'START', _Event, StartState} when is_atom(StartState) ->
                    StartState;
                false ->
                    s0
            end
    catch
        % A missing or malformed model file fails loudly later, when
        % generate_sys_info_function/1 parses it for the transition list.
        _:_ ->
            s0
    end.

-spec generate_verdicts() -> [erl_syntax:syntaxTree()].
generate_verdicts() ->
    lists:flatten([
        generate_verdict_function({?HML_TRU, 0}, 0),
        generate_verdict_function({?HML_FLS, 0}, 0)
    ]).

%%% @public Modularises the functions required for the monitor, coming from `gen_eval` module
%%%
-spec modularise_hml(Node, Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    Opts :: opts:options().
modularise_hml(Node, Opts) ->
    
    Functions = generate_function(Node, Opts),
    lists:flatten([Functions]).

%%% ----------------------------------------------------------------------------
%%% Private AST manipulation functions.
%%% ----------------------------------------------------------------------------

-spec visit(Node, Opts) -> erl_syntax:syntaxTree()
  when
  Node :: af_maxhml(),
  Opts :: opts:options().
visit(Node = {Bool, _}, _Opts) when Bool =:= ?HML_TRU; Bool =:= ?HML_FLS ->
  ?TRACE("Visiting '~s' node ~p.", [Bool, Node]),

  % Get monitor meta environment for node.
  Env = get_env(Node),
  erl_syntax:tuple([erl_syntax:atom(
    if Bool =:= ?HML_TRU -> ?MON_ACC; Bool =:= ?HML_FLS -> ?MON_REJ end
  ), Env]);

visit(Var = {?HML_VAR, _, _Name}, _Opts) ->
  ?TRACE("Visiting 'var' node ~p.", [Var]),

  % Get monitor meta environment for node.
  Env = get_env(Var),
  erl_syntax:tuple([erl_syntax:atom(?MON_VAR), Env, Var]);

visit(Node = {?HML_MAX, _, Var = {?HML_VAR, _, _}, Phi}, _Opts) ->
  ?TRACE("Visiting 'max' node ~p.", [Node]),

  Clause = erl_syntax:clause(none, [visit(Phi, _Opts)]),
  Fun = erl_syntax:named_fun_expr(Var, [Clause]),

  % Get monitor meta environment for node.
  Env = get_env(Node),
  erl_syntax:tuple([erl_syntax:atom(?MON_REC), Env, Fun]);

visit(Node = {Op, _, Phi, Psi}, _Opts)
  when Op =:= ?HML_OR; Op =:= ?HML_AND ->
  ?TRACE("Visiting '~s' node ~p.", [Op, Node]),

  % Get monitor meta environment for node.
  Env = get_env(Node),
  erl_syntax:tuple(
    [erl_syntax:atom(Op), Env, visit(Phi, _Opts), visit(Psi, _Opts)]
  );

visit(Node = {Mod, _, {act, _, Pat, Guard}, Phi}, _Opts)
  when Mod =:= ?HML_POS; Mod =:= ?HML_NEC ->
  ?TRACE("Visiting '~s' node ~p.", [Mod, Node]),

  % Encode the predicate functions for the action and its inverse. The predicate
  % functions are mutually-exclusive. This means that for any pattern and guard
  % combination, and any value the pattern data variables may be mapped to,
  % these two predicate functions will always return the negated truth value of
  % of each other.
  Pred = erl_syntax:fun_expr([
    erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [erl_syntax:atom(true)]),
    erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(false)])
  ]),

  InvPred = erl_syntax:fun_expr([
    erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [erl_syntax:atom(false)]),
    erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(true)])
  ]),

  % Encode the action bodies. The normal (left) action body consists of the
  % pattern with variables, and the continuation monitor. The inverse (right)
  % action consists of the verdict when the inverse pattern and guard test is
  % successful.
  CntBody = erl_syntax:fun_expr([
    erl_syntax:clause([gen_eval:pat_tuple(Pat)], none, [visit(Phi, _Opts)])
  ]),

  VrdBody = erl_syntax:fun_expr([
    erl_syntax:clause([erl_syntax:underscore()], none, [
      if Mod =:= pos ->
        erl_syntax:tuple([erl_syntax:atom(?MON_REJ), get_env({ff, 0})]);
        Mod =:= nec ->
          erl_syntax:tuple([erl_syntax:atom(?MON_ACC), get_env({tt, 0})])
      end
    ])
  ]),

  % Get a new unique placeholder for this monitor action.
  Ph = new_ph(),

  % Encode left and right action nodes.
  LeftAct = erl_syntax:tuple(
    [erl_syntax:atom(act), get_env(Node, Ph, true), Pred, CntBody]),
  RightAct = erl_syntax:tuple(
    [erl_syntax:atom(act), get_env(Node, Ph, false), InvPred, VrdBody]),

  % Encode the mutually-exclusive choice consisting of the left and right
  % summands.
  erl_syntax:tuple([erl_syntax:atom(chs), get_chs_env(), LeftAct, RightAct]).


%%% ----------------------------------------------------------------------------
%%% Private monitor helper functions for modularisation.
%%% ----------------------------------------------------------------------------

%%% @private Generates the state update calls (`update_current_state` calls) for the given node and its continuation (in scope).
-spec generate_state_update_calls(Pat) -> [erl_syntax:syntaxTree()] when
    Pat :: gen_eval:af_sym_act().
generate_state_update_calls(Pat) ->
    case generate_state_update_args(Pat) of
        [] ->
            [];
        Args ->
            [erl_syntax:application(erl_syntax:atom(update_current_state), Args)]
    end.

-spec generate_state_update_args(Pat) -> [erl_syntax:syntaxTree()] when
    Pat :: gen_eval:af_sym_act().
generate_state_update_args(Pat) ->
    case lists:filter(fun is_state_update_var/1, extract_vars(Pat, [])) of
        [EventVar | _] ->
            [erl_syntax:variable(EventVar)];
        [] ->
            []
    end.

-spec is_state_update_var(Var) -> boolean() when
    Var :: atom() | string().
is_state_update_var(Var) ->
    not lists:member(Var, ?STATE_UPDATE_EXCLUDED_VARS).

%%% @private Generates the function name for the given node.
-spec generate_function_name(Node) -> atom() when
    Node :: af_maxhml().
generate_function_name({?HML_VAR, _LineNumber, Name}) ->
    list_to_atom(string:lowercase(atom_to_list(Name)));
generate_function_name(_Node = {?HML_MAX, _LineNumber, Var = {_, _, _Name}, _}) ->
    generate_function_name(Var);
generate_function_name(
    {?HML_AND, _, _Phi = {_, PhiLineNumber, PhiPat, _}, _Psi = {_, PsiLineNumber, PsiPat, _}}
) ->

    Action1 = element(1, element(3, PhiPat)),
    Action2 = element(1, element(3, PsiPat)),
    list_to_atom(
        (string:lowercase(atom_to_list(Action1)) ++ integer_to_list(PhiLineNumber)) ++
            string:lowercase(atom_to_list(Action2) ++ integer_to_list(PsiLineNumber))
    );
generate_function_name({?HML_NEC, _NecLineNumber, {_, PhiLineNumber, Pat, _}, _}) ->
    Action = element(1, Pat),
    list_to_atom(atom_to_list(Action) ++ integer_to_list(PhiLineNumber));
generate_function_name({Verdict, _}) when Verdict =:= ?HML_TRU; Verdict =:= ?HML_FLS ->
    case Verdict of
        ?HML_TRU -> acceptance;
        ?HML_FLS -> rejection
    end.

% %% @private Generates the function arguments for the given node and its continuation (in scope).
-spec generate_function_args(Node, BoundVars) -> [erl_syntax:syntaxTree()] when
    Node :: af_maxhml(),
    BoundVars :: [atom()].
generate_function_args(Node = {?HML_VAR, _LineNumber, Name}, _BoundVars) ->
    % Recursive variable encountered; no further recursion
    ?TRACE("Searching for function arguments for 'var' node ~p.~n", [Name]),
    fun_args_get(generate_function_name(Node), []);
    % _BoundVars;
    
generate_function_args(_Node = {?HML_MAX, _, {?HML_VAR, _, _Name}, Phi}, BoundVars) ->
    % `max X` recursive construct, recursively process Phi
    generate_function_args(Phi, BoundVars);

generate_function_args(
    _OuterNode = {?HML_AND, _,
        InnerLeftNode = {nec, _, {_, _, _PatPhi, _GuardPhi}, _Psi}, InnerRightNode}, BoundVars
) ->
    % `and` compound node with `nec` on the left
    % Extract variables from guard and pattern, treat them as bound in this scope
    
    RightVars = generate_function_args(InnerRightNode, BoundVars),
    LeftVars = generate_function_args(InnerLeftNode, BoundVars),

    ?TRACE("Right vars are ~p.~n", [RightVars]),
    ?TRACE("Left vars are ~p.~n", [LeftVars]),
    FreeVars = lists:usort(RightVars ++ LeftVars),
    FreeVars;

generate_function_args(Node = {?HML_NEC, _, {_, _, Pat, Guard}, Phi}, BoundVars) ->

    FreeVarsInPattern = extract_free_vars_from_guard(Guard, Pat),
    UpdatedBoundVars = lists:usort(BoundVars ++ extract_bound_vars_from_guard(Node)),
    ContinuationVars = generate_function_args(Phi, UpdatedBoundVars),  

    TotalFreeVars = lists:usort(FreeVarsInPattern ++ ContinuationVars),
    % ?TRACE("Total free vars are ~p.~n", [TotalFreeVars]),
    FreeVars = lists:usort(TotalFreeVars -- UpdatedBoundVars),  % Exclude bound vars in this scope
    FreeVars;

generate_function_args({Verdict, _}, _BoundVars) when Verdict =:= ?HML_TRU; Verdict =:= ?HML_FLS ->
    ["From"].

%%% ----------------------------------------------------------------------------
%%% AGM code-generation callbacks.
%%% ----------------------------------------------------------------------------

generate_sys_info_function(Opts) ->
    maxhml_agm_codegen:generate_sys_info_function(Opts).

generate_all_states() ->
    maxhml_agm_codegen:generate_all_states().

generate_state_management() ->
    maxhml_agm_codegen:generate_state_management().

agm_generation() ->
    maxhml_agm_codegen:agm_generation().

%%% ----------------------------------------------------------------------------
%%% Private monitor environment creation functions.
%%% ----------------------------------------------------------------------------

%%% @private Returns an Erlang AST representation of the monitor environment
%%% used to manage the monitor meta information such as the substitution and its
%%% stringified representation.
-spec get_env(Node) -> erl_syntax:syntaxTree()
  when
  Node :: af_hml_tt() | af_hml_ff() | af_hml_or() | af_hml_and() |
  af_hml_max() | af_hml_var().
get_env(Node = {Bool, _}) when Bool =:= ?HML_TRU; Bool =:= ?HML_FLS ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  new_env([Str]);
get_env(Node = {Op, _, _, _}) when Op =:= ?HML_OR; Op =:= ?HML_AND ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  new_env([Str]);
get_env(Node = {?HML_MAX, _, {?HML_VAR, _, Name}, _}) ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  Var = new_env_kv(?KEY_VAR, erl_syntax:atom(Name)),
  new_env([Str, Var]);
get_env(Node = {?HML_VAR, _, Name}) ->
  Str = new_env_kv(?KEY_STR, get_str(Node)),
  Var = new_env_kv(?KEY_VAR, erl_syntax:atom(Name)),
  new_env([Str, Var]).

%%% @private Returns an Erlang AST representation of the monitor environment
%%% for monitor parallel disjunction and conjunction.
-spec get_env(Node, Ph, Inv) -> erl_syntax:syntaxTree()
  when
  Node :: af_hml_pos() | af_hml_nec(),
  Ph :: string(),
  Inv :: boolean().
get_env(Node = {Mod, _, _Act, _Phi}, Ph, Inv)
  when Mod =:= ?HML_POS; Mod =:= ?HML_NEC ->

  % Get stringified representation of the monitor, variable placeholder and
  % pattern used to help stringify the monitor.
  Str = new_env_kv(?KEY_STR, get_str(Node, Ph, Inv)),
  Var = new_env_kv(?KEY_VAR, erl_syntax:atom(Ph)),
  Pat = new_env_kv(?KEY_PAT, get_pat(Node)),
  new_env([Str, Var, Pat]).

%%% @private Returns an Erlang AST representation of the monitor environment for
%%% choice.
-spec get_chs_env() -> erl_syntax:syntaxTree().
get_chs_env() ->
  Str = new_env_kv(?KEY_STR, get_chs_str()),
  new_env([Str]).


%%% @private Returns an Erlang AST representation of a new key-value pair.
-spec new_env_kv(Key, Val) -> erl_syntax:syntaxTree()
  when
  Key :: atom(),
  Val :: erl_syntax:syntaxTree().
new_env_kv(Key, Val) ->
  erl_syntax:tuple([erl_syntax:atom(Key), Val]).

%%% @private Returns an Erlang AST representation of a new monitor environment,
%%% with the specified list elements.
-spec new_env(List :: [erl_syntax:syntaxTree()]) -> erl_syntax:syntaxTree().
new_env(List) ->
  erl_syntax:tuple([erl_syntax:atom(?KEY_ENV), erl_syntax:list(List)]).


%%% ----------------------------------------------------------------------------
%%% Private monitor stringifying and functions.
%%% ----------------------------------------------------------------------------

%%% @private Returns an Erlang ASP representation of the stringified monitor
%%% verdicts, parallel Boolean connectives, and recursion.
-spec get_str(Node) -> erl_syntax:syntaxTree()
  when
  Node :: af_hml_tt() | af_hml_ff() | af_hml_or() | af_hml_and() |
  af_hml_max() | af_hml_var().
get_str({?HML_TRU, _}) ->
  erl_syntax:string("yes");
get_str({?HML_FLS, _}) ->
  erl_syntax:string("no");
get_str({Op, _, _, _}) when Op =:= ?HML_OR; Op =:= ?HML_AND ->
  erl_syntax:string(atom_to_list(Op));
get_str({?HML_MAX, _, {?HML_VAR, _, Name}, _}) ->
  erl_syntax:string(lists:flatten("rec ", atom_to_list(Name)));
get_str({?HML_VAR, _, Name}) ->
  erl_syntax:string(atom_to_list(Name)).

%%% @private Returns an Erlang AST representation of the stringified monitor
%%% actions.
%%%
%%% {@par The action expects a variable placeholder and can generate the action
%%%       or inverse action based on the flag Inv.
%%% }
-spec get_str(Node, Ph, Inv) -> erl_syntax:syntaxTree()
  when
  Node :: af_hml_pos() | af_hml_nec(),
  Ph :: string(),
  Inv :: boolean().
get_str({Mod, _, {?HML_ACT, _, Pat, Guard}, _}, Ph, Inv)
  when Mod =:= ?HML_POS; Mod =:= ?HML_NEC ->

  % Stringify placeholder and the internal representation of the pattern as an
  % Erlang trace event.
  IoList = [Ph, $/, erl_pp:expr(erl_syntax:revert(gen_eval:pat_tuple(Pat)))],

  % Stringify guard only if present.
  IoList_ = if Guard =:= [] -> IoList; true -> [IoList, $ , erl_pp:guard(Guard)] end,

  % Add the stringified negation if the branch is the inverse one (called the)
  % negative branch of mutually-exclusive choice.
  IoList__ = if Inv -> IoList_; true -> ["NOT(", IoList_, ")"] end,

  erl_syntax:string(lists:flatten(IoList__)).

%%% @private Returns an Erlang AST representation of the stringified monitor
%%% mutually-exclusive choice.
-spec get_chs_str() -> erl_syntax:syntaxTree().
get_chs_str() ->
  erl_syntax:string("+").


%%% @private Returns an Erlang AST representation of the native Erlang trace
%%% event patterns with all the variables and 'don't care' patterns replaced by
%%% `undefined'. This is used by the monitoring algorithm to unwrap the monitor
%%% function enclosing monitor actions and compute the stringified
%%% representation of the monitor on the fly.
%%%
%%% {@par The current implementation works, but is inelegant since it piggybacks
%%%       on the Erlang parsing mechanism. The function first converts the
%%%       abstract pattern to an IoList, replaces the variables and 'don't care'
%%%       patterns with `undefined', and parses the result back to an Erlang AST
%%%       representation. The alternative and (perhaps?) more elegant way is to
%%%       implement a replace feature that mutates an Erlang AST. This takes
%%%       time, and must be made to support all the Erlang syntax (unless
%%%       someone else has done it.
%%% }
-spec get_pat(Node :: af_hml_pos() | af_hml_nec()) -> erl_syntax:syntaxTree().
get_pat({Mod, _, {?HML_ACT, _, Pat, _Guard}, _})
  when Mod =:= ?HML_POS; Mod =:= ?HML_NEC ->

  Str = erl_pp:expr(erl_syntax:revert(gen_eval:pat_tuple(Pat))),
  Replaced = re:replace(Str, "\\b([A-Z_][a-zA-Z0-9_@]*)\\b", "undefined", [{return, list}, global]),

  {ok, Tokens, _EndLine} = erl_scan:string(Replaced ++ "."),
  {ok, [AbsForm]} = erl_parse:parse_exprs(Tokens),
  AbsForm.


%%% @private Initializes the variable placeholder generator.
-spec init_ph() -> ok.
init_ph() ->

  put(?KEY_PH_NAMES, ?PH_NAMES), % list of available variable placeholder names.
  put(?KEY_PH_CNT, 0), % 0-based index.
  ok.

%%% @private Checks whether the variable placeholder generator is initialized
%%% and initializes it if not.
-spec check_ph() -> ok.
check_ph() ->
  case get(?KEY_PH_NAMES) of
    undefined ->

      % Placeholder token name generator not initialized.
      init_ph();
    _ ->
      ok
  end.

%%% @private Returns the next unique variable placeholder name.
-spec new_ph() -> string().
new_ph() ->

  % Ensure that placeholder token name generator is initialized.
  check_ph(),

  % Get last placeholder counter and increment it.
  Cnt = put(?KEY_PH_CNT, get(?KEY_PH_CNT) + 1),

  % Get next placeholder token name. Calculation wraps around the counter when
  % the it goes beyond the number of available token names. Access to the list
  % of token names is 1-based.
  Tok = lists:nth((Cnt rem length(?PH_NAMES)) + 1, ?PH_NAMES),

  % Calculate the token name suffix, to generate a unique placeholder token. The
  % suffix is incremented once the counter goes beyond the number of available
  % token names.
  Idx = Cnt div length(?PH_NAMES),

  % Generate unique placeholder name.
  lists:flatten(io_lib:format("~s~s~2..0B", [?PH_PRF, Tok, Idx])).

%%% @private Returns the free variables from a guard expression and funcation definition. If a variable is found in both the guard and the pattern,
%%% the variable is not included, as it will shadow the variable in the pattern.
-spec extract_free_vars_from_guard(Guard, Pat) -> string() when
    Guard :: gen_eval:af_guard(),
    Pat :: gen_eval:af_pattern().
extract_free_vars_from_guard(Guard, Pat) ->
    BoundVars = extract_vars(Pat, []),
    extract_vars_guard(Guard, BoundVars, []).
-spec extract_bound_vars_from_guard(Node) -> string() when
    Node :: af_maxhml().
extract_bound_vars_from_guard(_Node = {_Vrd, _LineNumber}) ->
    [];
extract_bound_vars_from_guard(_Node = {?HML_VAR, _, _Name}) ->
    [];
extract_bound_vars_from_guard(_Node = {?HML_MAX, _, {?HML_VAR, _, _Name}, Phi}) ->
    extract_bound_vars_from_guard(Phi);
extract_bound_vars_from_guard(_OuterNode =
        {?HML_AND, _,
            _InnerLeftNode =
                {?HML_NEC, _, _PhiLeftNode = {_, _LineNumberLeft, PatPhiLeft, _GuardPhiLeft}, _PsiLeft},
            _InnerRightNode =
                {?HML_NEC, _, _PhiRightNode = {_, _LineNumberRight, PatPhiRight, _GuardPhiRight},
                    _PsiRight}}) ->
    BoundVarsLeft = extract_vars(PatPhiLeft, []),
    BoundVarsRight = extract_vars(PatPhiRight, []),
    BoundVars = BoundVarsLeft ++ BoundVarsRight,
    BoundVars;
extract_bound_vars_from_guard(_Node = {?HML_NEC, _LineNumber, {act, _, Pat, _Guard}, _Phi}) ->
    BoundVars = extract_vars(Pat, []),
    BoundVars.


%%% @private Returns the variables from a guard expression.
-spec extract_vars_guard(Guard, PatVars, Acc) -> string() when
    Guard :: gen_eval:af_guard(),
    PatVars :: string(),
    Acc :: string().
% Base Case
extract_vars_guard({var, _, Variable}, PatVars, Acc) ->
    case lists:member(Variable, PatVars) of
        true -> Acc;
        false -> [Variable | Acc]
    end;
% Recurse through the guard list
extract_vars_guard([H | T], PatVars, Acc) ->
    extract_vars_guard(T, PatVars, extract_vars_guard(H, PatVars, Acc));
% Recurse through the guard tuple
extract_vars_guard(Tuple, PatVars, Acc) when is_tuple(Tuple) ->
    TupleList = tuple_to_list(Tuple),
    extract_vars_guard(TupleList, PatVars, Acc);
% Catch-All
extract_vars_guard(_, _, Acc) ->
    Acc.

%%% @private Returns the variables from a pattern expression.
-spec extract_vars(Pat, Acc) -> string() when
    Pat :: gen_eval:af_pattern(),
    Acc :: string().
% Base Case
extract_vars({var, _, Variable}, Acc) ->
    [Variable | Acc];
% Recurse through the pattern list
extract_vars([H | T], Acc) ->
    extract_vars(T, extract_vars(H, Acc));
% Recurse through the pattern tuple
extract_vars(Tuple, Acc) when is_tuple(Tuple) ->
    TupleList = tuple_to_list(Tuple),
    extract_vars(TupleList, Acc);
% Catch-All
extract_vars(_, Acc) ->
    Acc.

invert_operator(Guard) ->
    case Guard of
        [[{op, Line, '=:=', Left, Right}]] ->
            [[{op, Line, '=/=', Left, Right}]];
        [[{op, Line, '=:=', Left, Right} | Rest]] ->
            [[{op, Line, '=/=', Left, Right} | Rest]];
        _ ->
            Guard
    end.
