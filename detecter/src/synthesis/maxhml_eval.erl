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

-type with() :: {with, line(), gen_eval:af_mfargs()} |
{with, line(), gen_eval:af_mfargs(), gen_eval:af_constraint()}.
%% Process instrumentation selection MFArgs.

-type spec() :: {spec, line(), with(), af_maxhml()}.
%% Instrumentation specification abstract form.

-type af_maxhml() :: af_hml_tt() | af_hml_ff() | af_hml_pos() | af_hml_nec() |
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
  gen_eval:compile(?MODULE, ?LEXER_MOD, ?PARSER_MOD, File, Opts).

parse_string(String) ->
  gen_eval:parse_string(?LEXER_MOD, ?PARSER_MOD, String).

parse_file(File) ->
  gen_eval:parse_file(?LEXER_MOD, ?PARSER_MOD, File).

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
generate_function(Node = {Vrd, LineNumber}, _Opts) when
    Vrd =:= ?HML_TRU; Vrd =:= ?HML_FLS
->
    [];
generate_function(Var = {?HML_VAR, _, _Name}, _Opts) ->

    ?TRACE("Generating function for 'var' node ~p. ~n ", [_Name]),
    FunctionName = generate_function_name(Var),
    % FunctionArgs = lists:usort(
    %     lists:flatten([[erl_syntax:variable(V) || V <- generate_function_args(Var, [])]])
    % ),
    % ?TRACE("Function args are ~p. ~n", [FunctionArgs]),
    % [erl_syntax:function(erl_syntax:atom(FunctionName), [  erl_syntax:clause([], none, [erl_syntax:atom(?MON_VAR)])])];
    [];

generate_function(Node = {?HML_MAX, LineNumber, Var = {?HML_VAR, _, _}, Phi}, _Opts) ->
    
    FunctionName = generate_function_name(Node),
    ?TRACE("Generating function ~p for 'max' node from src line ~p. ~n ", [FunctionName,LineNumber]),
    FunctionArgs = 
        case persistent_term:get(FunctionName, empty) of
            empty ->
                persistent_term:put(FunctionName, generate_function_args(Node, [])),
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(FunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(FunctionName)])
        end,

 NextFunctionName = generate_function_name(Phi),
    NextFunctionArgs = 
        case persistent_term:get(NextFunctionName, empty) of
            empty ->
                persistent_term:put(NextFunctionName, generate_function_args(Phi, [])),
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(NextFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(NextFunctionName)])
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
    
    ?TRACE("Generated function ~p. ~n", [FunctionName]),

    [Function | lists:flatten([generate_function(Phi, _Opts)])];
generate_function(
    OuterNode =
        {?HML_AND, _,
            InnerLeftNode =
                {ModLeft, _, PhiLeftNode = {_, LineNumberLeft, PatPhiLeft, GuardPhiLeft}, PsiLeft},
            InnerRightNode =
                {ModRight, _, PhiRightNode = {_, LineNumberRight, PatPhiRight, GuardPhiRight},
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
        case persistent_term:get(LeftNodeFunctionName, empty) of
            empty ->
                persistent_term:put(LeftNodeFunctionName, generate_function_args(PsiLeft, PsiBoundVarsLeft)),
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(LeftNodeFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(LeftNodeFunctionName)])
        end,

    BoundVarsRight = extract_bound_vars_from_guard(OuterNode),
    RightNodeArgs = lists:flatten([erl_syntax:variable(V) || V <- generate_function_args(InnerRightNode, BoundVarsRight)]),
    PsiBoundVarsRight = extract_bound_vars_from_guard(PsiRight),
    PsiRightFunctionArgs = 
        case persistent_term:get(RightNodeFunctionName, empty) of
            empty ->
                persistent_term:put(RightNodeFunctionName, generate_function_args(PsiRight, PsiBoundVarsRight)),
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(RightNodeFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(RightNodeFunctionName)])
        end,

    CompositeFunctionArgs = 
        case persistent_term:get(CompositeFunctionName, empty) of
            empty ->
                persistent_term:put(CompositeFunctionName, lists:flatten([LeftNodeArgs, RightNodeArgs])),
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(CompositeFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(CompositeFunctionName)])
        end,
   
 
    LeftNodeClause = erl_syntax:clause(
        [gen_eval:pat_tuple(PatPhiLeft)],
        GuardPhiLeft,

        case ?IS_TERMINATING_HML(PsiLeft) of
            true ->
                [
                    erl_syntax:application(
                        erl_syntax:atom(LeftNodeFunctionName), PsiLeftFunctionArgs
                    )
                ];
            _->
                [
                    erl_syntax:application(erl_syntax:atom(update_current_state), PsiLeftFunctionArgs),    
                    erl_syntax:application(
                        erl_syntax:atom(LeftNodeFunctionName), PsiLeftFunctionArgs
                    )
                ]
            end
    ),

    RightNodeClause = erl_syntax:clause(
        [gen_eval:pat_tuple(PatPhiRight)],
        GuardPhiRight,

         case ?IS_TERMINATING_HML(PsiRight) of
            true ->
                [erl_syntax:application(
                        erl_syntax:variable(RightNodeFunctionName), PsiRightFunctionArgs
                    )
                ];
            _ -> 
                [erl_syntax:application(erl_syntax:atom(update_current_state), CompositeFunctionArgs),
                    erl_syntax:application(
                        erl_syntax:variable(RightNodeFunctionName), PsiRightFunctionArgs
                    )
                ]
            end
    ),

    MissingEventClause = erl_syntax:clause(
        [gen_eval:pat_tuple({missing_event})],
        none,
        [erl_syntax:atom(ok)]
        ),

    ReceiveClause = erl_syntax:clause(
        CompositeFunctionArgs,
        none,
        [erl_syntax:receive_expr([LeftNodeClause, RightNodeClause, MissingEventClause])]
    ),

    Function = erl_syntax:function(
        erl_syntax:atom(CompositeFunctionName),
        [ReceiveClause]
    ),

    ?TRACE("Generated function ~p. ~n", [CompositeFunctionName]),
    [
        Function
        | lists:flatten([
            generate_function(InnerLeftNode, _Opts), generate_function(InnerRightNode, _Opts)
        ])
    ];
generate_function(Node = {?HML_NEC, LineNumber, {act, _, Pat, Guard}, Phi}, _Opts) ->
    BoundVars = extract_bound_vars_from_guard(Node),
    FunctionName = generate_function_name(Node),
    ?TRACE("Generating function ~p for 'nec' node from src line ~p. ~n ", [FunctionName, LineNumber]),
    FunctionArgs = 
        case persistent_term:get(FunctionName, empty) of
            empty ->
                persistent_term:put(FunctionName, generate_function_args(Node, BoundVars)),
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(FunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(FunctionName)])
        end,


    ?TRACE("Function Args ~p", [FunctionArgs]),

    NextBoundVars = extract_bound_vars_from_guard(Phi),
    NextFunctionName = generate_function_name(Phi),
    NextFunctionArgs = 
        case persistent_term:get(NextFunctionName, empty) of
            empty ->
                persistent_term:put(NextFunctionName, generate_function_args(Phi, NextBoundVars)),
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(NextFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(NextFunctionName)])
        end,

    % TODO: should really remove the "From" var when calling update_current_state... there are better ways of doing this -> find out how

    Clause = erl_syntax:clause(
        [gen_eval:pat_tuple(Pat)],
        Guard,
        [erl_syntax:application(erl_syntax:atom(update_current_state), NextFunctionArgs),erl_syntax:application(erl_syntax:atom(NextFunctionName), NextFunctionArgs)]
    ),

    MissingEventClause = erl_syntax:clause(
        [gen_eval:pat_tuple({missing_event})],
        none,
        [erl_syntax:atom(ok)]
        ),

    ReceiveClause = erl_syntax:clause(
        FunctionArgs,
        none,
        [erl_syntax:receive_expr([Clause,MissingEventClause])]
    ),

    Function = erl_syntax:function(
        erl_syntax:atom(FunctionName),
        case ?IS_TERMINATING_HML(Phi) of
            true ->
                ?TRACE("Terminating function detected - Atomic termination generated. ~n"),
                % ! Using lists:nth here cause of the update_state -> we do not need to update state when giving a verdict...
                [erl_syntax:clause(FunctionArgs, none, [lists:nth(2,erl_syntax:clause_body(Clause))])];
            _ ->
                case ?IS_RECURSIVE_HML(Phi) of
                    true ->
                        ?TRACE("Recursive function detected - Recursive call generated. ~n"),
                        % ! Using lists:nth here cause of the update_state -> we do not need to update state during internal transitions...
                        [erl_syntax:clause(FunctionArgs, none, [lists:nth(2,erl_syntax:clause_body(Clause))])];
                    _ ->
                        [ReceiveClause]
                    end
        end
    ),

    ?TRACE("Generated function ~p. ~n", [FunctionName]),

    [Function | lists:flatten([generate_function(Phi, _Opts)])].

%% @public Generates the receive block for the function look up. This
%% is the entry point for the look up of the function to be executed.
-spec generate_init_block(Node, _Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    _Opts :: opts:options().
generate_init_block(OuterNode =
        {?HML_AND, _,
            InnerLeftNode =
                {ModLeft, _, PhiLeftNode = {_, LineNumberLeft, PatPhiLeft = {init, _, LeftPhiPid2, LeftPhiPid, LeftPhiMFArgs}, GuardPhiLeft}, PsiLeft},
            InnerRightNode =
                {ModRight, _, PhiRightNode = {_, LineNumberRight, PatPhiRight = {init, _, RightPhiPid2, RightPhiPid, RightPhiMFArgs}, GuardPhiRight},
                    PsiRight}},
    _Opts) ->

    ?TRACE("Generating init block for 'and' node from src lines ~p and ~p. ~n ", [
        LineNumberLeft, LineNumberRight
    ]),

    LeftNodeNextFunctionName = generate_function_name(PsiLeft),
    RightNodeNextFunctionName = generate_function_name(PsiRight),

    PsiBoundVarsLeft = extract_bound_vars_from_guard(PsiLeft),
    PsiLeftFunctionArgs = 
        case persistent_term:get(LeftNodeNextFunctionName, empty) of
            empty ->
                persistent_term:put(LeftNodeNextFunctionName, generate_function_args(PsiLeft, PsiBoundVarsLeft)),
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(LeftNodeNextFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(LeftNodeNextFunctionName)])
        end,

    PsiBoundVarsRight = extract_bound_vars_from_guard(PsiRight),
    PsiRightFunctionArgs = 
        case persistent_term:get(RightNodeNextFunctionName, empty) of
            empty ->
                persistent_term:put(RightNodeNextFunctionName, generate_function_args(PsiRight, PsiBoundVarsRight)),
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(RightNodeNextFunctionName)]);
            _ ->
                lists:flatten([erl_syntax:variable(V) || V <- persistent_term:get(RightNodeNextFunctionName)])
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
    % TODO: This needs a refactor... check init_block also, can be combined
    EtsInitExpr = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(new), [erl_syntax:atom(sus_state), erl_syntax:list([erl_syntax:atom(named_table),erl_syntax:atom(public),erl_syntax:atom(set)])]),
    EtsInsertCurrentState = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(insert), [erl_syntax:atom(sus_state), erl_syntax:tuple([erl_syntax:atom(current_state),erl_syntax:atom(start)])]),
    EtsInsertPreviousState = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(insert), [erl_syntax:atom(sus_state), erl_syntax:tuple([erl_syntax:atom(previous_state),erl_syntax:atom(undefined)])]),

    lists:flatten([EtsInitExpr,EtsInsertCurrentState,EtsInsertPreviousState,ReceiveExpr]);

generate_init_block({Mod, _, {act, _, Pat = {init, _, Pid2, Pid, MFArgs}, Guard}, Phi}, _Opts) when Mod =:= ?HML_NEC; Mod =:= ?HML_POS->
    ?TRACE("Generating init block for ~p node. ~n", [Mod]),

    NextFunctionName = generate_function_name(Phi),
    NextFunctionArgs = generate_function_args(Phi, []),
    
    persistent_term:put(NextFunctionName, NextFunctionArgs),
    AnonFunEntry = 
        case Mod of 
            ?HML_NEC ->
                [erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [
                erl_syntax:application(erl_syntax:atom(NextFunctionName), lists:flatten([erl_syntax:variable(V) || V <- NextFunctionArgs]))
                ])];
            ?HML_POS ->
                [erl_syntax:clause([gen_eval:pat_tuple(Pat)], (Guard), [
                erl_syntax:application(erl_syntax:atom(NextFunctionName), lists:flatten([erl_syntax:variable(V) || V <- NextFunctionArgs]))
                ]),
                erl_syntax:clause([gen_eval:pat_tuple(Pat)], invert_operator(Guard), [
                erl_syntax:application(erl_syntax:atom(rejection), lists:flatten([erl_syntax:variable("From")]))
                ])
                ]
        end,


    ReceiveExpr = erl_syntax:receive_expr(AnonFunEntry),
    ?TRACE("Generated init block for ~p node. ~n",[Mod]),
    
    % State Management

    % TODO: this needs a refactor...check above

    EtsInitExpr = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(new), [erl_syntax:atom(sus_state), erl_syntax:list([erl_syntax:atom(named_table),erl_syntax:atom(public),erl_syntax:atom(set)])]),
    EtsInsertCurrentState = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(insert), [erl_syntax:atom(sus_state), erl_syntax:tuple([erl_syntax:atom(current_state),erl_syntax:atom(start)])]),
    EtsInsertPreviousState = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(insert), [erl_syntax:atom(sus_state), erl_syntax:tuple([erl_syntax:atom(previous_state),erl_syntax:atom(undefined)])]),

    lists:flatten([EtsInitExpr,EtsInsertCurrentState,EtsInsertPreviousState,ReceiveExpr]);
generate_init_block(N, _) ->
    ?ERROR("Invalid node for init block generation :~p.",[N]),
    [].

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
%%% @private Generates the function name for the given node.
-spec generate_function_name(Node) -> atom() when
    Node :: af_maxhml().
generate_function_name({?HML_VAR, LineNumber, Name}) ->
    list_to_atom(string:lowercase(atom_to_list(Name)));
generate_function_name(Node = {?HML_MAX, LineNumber, Var = {_, _, Name}, _}) ->
    generate_function_name(Var);
generate_function_name(
    {?HML_AND, _, Phi = {_, PhiLineNumber, PhiPat, _}, Psi = {_, PsiLineNumber, PsiPat, _}}
) ->

    Action1 = element(1, element(3, PhiPat)),
    Action2 = element(1, element(3, PsiPat)),
    list_to_atom(
        (string:lowercase(atom_to_list(Action1)) ++ integer_to_list(PhiLineNumber)) ++
            string:lowercase(atom_to_list(Action2) ++ integer_to_list(PsiLineNumber))
    );
generate_function_name({?HML_NEC, NecLineNumber, {_, PhiLineNumber, Pat, _}, _}) ->
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
generate_function_args(Node = {?HML_VAR, LineNumber, Name}, _BoundVars) ->
    % Recursive variable encountered; no further recursion
    ?TRACE("Searching for function arguments for 'var' node ~p.~n", [Name]),
    persistent_term:get(generate_function_name(Node), []);
    % _BoundVars;
    
generate_function_args(Node = {?HML_MAX, _, {?HML_VAR, _, Name}, Phi}, BoundVars) ->
    % `max X` recursive construct, recursively process Phi
    generate_function_args(Phi, BoundVars);

generate_function_args(
    OuterNode = {?HML_AND, _, 
        InnerLeftNode = {nec, _, {_, _, PatPhi, GuardPhi}, Psi}, InnerRightNode}, BoundVars
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
%%% System Info functions
%%% 
%%% refer to module sys_info_parser
%%% ----------------------------------------------------------------------------

% init_transitions/0
generate_sys_info_function(Opts) ->
    SourceFile = opts:monitor_table_opt(Opts),
    SysInfo = sys_info_parser:parse_file(SourceFile),
    Map = generate_sys_info_transition(SysInfo),
    [erl_syntax:function(
        erl_syntax:atom(init_transitions),
        [
            erl_syntax:clause(
                [],
                [],
                [Map]
            )
        ]
    )].

generate_sys_info_transition(TransitionList)->
    % Create AST for the key: {Source, Destination}
    Fields = lists:map(fun sys_info_to_map/1, TransitionList),
    erl_syntax:map_expr(Fields).
    % Map.

sys_info_to_map({Source, EventTuple, Destination}) ->    
    % Key Map
    KeyAST = erl_syntax:tuple([
        erl_syntax:atom(Source),
        erl_syntax:atom(Destination)
    ]),
    % Fun Map
    io:format("EventTuple is: ~p~n", [EventTuple]),
    ValueAST = parse_sys_info_event(EventTuple),
    erl_syntax:map_field_assoc(KeyAST, ValueAST).
    

parse_sys_info_event({EventType, EventPayload}) when EventType =:= is_integer ->

    EventBody = erl_syntax:infix_expr(
        erl_syntax:variable("Event"),
        erl_syntax:operator('=:='),
        erl_syntax:integer(EventPayload)
    ),

    erl_syntax:fun_expr(
        [erl_syntax:clause(
            [erl_syntax:variable("Event")],[],[EventBody]
            )]
    );

parse_sys_info_event({EventType, EventPayload}) when EventType =:= atom ->
        EventBody = erl_syntax:infix_expr(
        erl_syntax:variable("Event"),
        erl_syntax:operator('=:='),
        erl_syntax:atom(EventPayload)
    ),

    erl_syntax:fun_expr(
        [erl_syntax:clause(
            [erl_syntax:variable("Event")],[],[EventBody]
            )]
    );

parse_sys_info_event({{EventType, EventPayload}, AdditionalGuards}) when EventType =:= 'fun' ->
    
    EventVar = erl_syntax:variable("Event"),
    % Generate body of map + additional guards in spec
    EventBody = generate_sys_info_guard(EventPayload,AdditionalGuards),

    erl_syntax:fun_expr(
            [erl_syntax:clause(
            [EventVar],[],[EventBody]
            )]
        ).

generate_sys_info_guard(EventPayload, AdditionalGuards) ->
    EventVar = erl_syntax:variable("Event"),
    
    % Generate general guards 
    IsIntegerGuard = erl_syntax:application(
        erl_syntax:atom(is_integer),
        [EventVar]
    ),

    IsRealGuard = erl_syntax:application(
        erl_syntax:atom(is_number),
        [EventVar]
    ),

    MainGuard = case EventPayload of 

            null -> erl_syntax:atom(null);
        
            is_natural_integer -> 
                
                NaturalIntegerGuard = erl_syntax:infix_expr(
                    EventVar,
                    erl_syntax:operator(">"),
                    erl_syntax:integer(0)
                ),

                CombinedGuard = erl_syntax:infix_expr(
                    IsIntegerGuard,
                    erl_syntax:operator("andalso"),
                    NaturalIntegerGuard
                ),

                CombinedGuard;

                % TODO: SORT OUT THE OTHER GUARDS
            is_any_integer -> IsIntegerGuard;
            
            is_real_number -> IsRealGuard
        end,

        case AdditionalGuards of 
            [Operator | {GuardPayloadType, GuardPayload} ] ->
                io:format("Operator Guards ~p~n", [Operator]),
                io:format("Payload Guards ~p~n", [GuardPayload]),
                AdditionalGuard = 
                    case Operator of
                        setminus ->
                            erl_syntax:infix_expr(
                                EventVar,
                                erl_syntax:operator("=/="),
                                case GuardPayloadType of 
                                    is_integer -> erl_syntax:integer(GuardPayload);
                                    is_atom -> erl_syntax:atom(GuardPayload)
                                end
                            );
                        _ ->
                            []
                    end,
                
                erl_syntax:infix_expr(
                    MainGuard,
                    erl_syntax:operator("andalso"),
                    AdditionalGuard);

            % No additional guards, just return main body
            [] -> MainGuard
        end.

% get_system_states/1
generate_all_states() ->
    % vars
    SysInfoVar = erl_syntax:variable('StateTransitionTable'),
    SrcVar = erl_syntax:variable('Src'),
    ConditionVar = erl_syntax:variable('_Condition'),
    DstVar = erl_syntax:variable('Dst'),
    AccVar = erl_syntax:variable('Acc'),
    StatesVar = erl_syntax:variable('States'),
    UniqueStatesVar = erl_syntax:variable('UniqueStates'),

    TransitionsAssignment = erl_syntax:infix_expr(
        SysInfoVar,
        erl_syntax:operator('='),
        erl_syntax:application(erl_syntax:atom(init_transitions),[])
        ),

    FoldBody = erl_syntax:fun_expr([
        erl_syntax:clause(
              [erl_syntax:tuple([erl_syntax:tuple([SrcVar, DstVar]),ConditionVar]),AccVar],
            [],
            [erl_syntax:cons(SrcVar, erl_syntax:list([DstVar,AccVar]))])
    ]),

    FoldExpression = erl_syntax:application(
        erl_syntax:atom(lists),
        erl_syntax:atom(foldl),
        [FoldBody,erl_syntax:list([]), erl_syntax:application(erl_syntax:atom(maps),erl_syntax:atom(to_list),[SysInfoVar])]),

    StateAssignment = erl_syntax:infix_expr(StatesVar,erl_syntax:operator('='),FoldExpression),

    FunctionReturn = erl_syntax:application(
        erl_syntax:atom(lists),
        erl_syntax:atom(usort),
        [erl_syntax:application(erl_syntax:atom(lists),erl_syntax:atom(flatten),[StatesVar])]),

     [erl_syntax:function(
        erl_syntax:atom(get_system_states),
        [
            erl_syntax:clause(
                [],
                [],
                [TransitionsAssignment,StateAssignment,FunctionReturn]
            )
        ]
    )].

%%% ----------------------------------------------------------------------------
%%% State Management
%%% ----------------------------------------------------------------------------

generate_state_management() ->
    generate_update_system_state_function().    

generate_update_system_state_function()->
    ParamEvent = erl_syntax:variable("Event"),
    OutdatedStateVariable = erl_syntax:variable("OutdatedState"),
    UpdatedStateVariable = erl_syntax:variable("UpdatedState"),

    OutdatedClause = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(lookup_element),[erl_syntax:atom(sus_state),erl_syntax:atom(current_state),erl_syntax:integer(2)]),

    UpdatedClause = erl_syntax:application(erl_syntax:atom(reachable_states), [OutdatedStateVariable,ParamEvent]),

    OutdatedClauseAssignment = erl_syntax:infix_expr(OutdatedStateVariable,erl_syntax:operator('='),OutdatedClause),
    UpdatedClauseAssignment = erl_syntax:infix_expr(UpdatedStateVariable,erl_syntax:operator('='),UpdatedClause),

    UpdateStateClause = erl_syntax:application(erl_syntax:atom(ets),erl_syntax:atom(insert),[erl_syntax:atom(sus_state),erl_syntax:tuple([erl_syntax:atom(current_state),UpdatedStateVariable])]),

    [erl_syntax:function(
        erl_syntax:atom(update_current_state),
        [
            % TODO: When understanding how to pass the correct vars, remove the underscore
            erl_syntax:clause(
                [ParamEvent, erl_syntax:underscore()],
                [],
                [OutdatedClauseAssignment,UpdatedClauseAssignment,UpdateStateClause]
            )
        ]
    )].


%%% ----------------------------------------------------------------------------
%%% Automaton Guided Monitoring functions.
%%% ----------------------------------------------------------------------------

agm_generation()->
    lists:flatten([generate_reachable_states_function(), generate_preceeding_states_from_state_function()]).

% reachable_states/2
generate_reachable_states_function()->
    
    % vars
    ParamState = erl_syntax:variable('State'),
    ParamEvent = erl_syntax:variable('Event'),
    TransitionsVar = erl_syntax:variable('StateTransitionTable'),
    SrcVar = erl_syntax:variable('Src'),
    DstVar = erl_syntax:variable('Dst'),
    ConditionVar = erl_syntax:variable('Condition'),
    AccVar = erl_syntax:variable('Acc'),

    TransitionsAssignment = erl_syntax:infix_expr(
        TransitionsVar,
        erl_syntax:operator('='),
        erl_syntax:application(erl_syntax:atom(init_transitions),[])
        ),

    % For Case Expression
    CaseCondition1 = erl_syntax:infix_expr(SrcVar, erl_syntax:operator('=:='), ParamState),
    CaseCondition2 = erl_syntax:application(ConditionVar,[ParamEvent]),
    CaseCondition = erl_syntax:infix_expr(CaseCondition1, erl_syntax:operator('andalso'), CaseCondition2),

    CaseConditionTrueBody = erl_syntax:cons(DstVar,AccVar),
    CaseConditionFalseBody = AccVar,

    CaseExpression = erl_syntax:case_expr(
        CaseCondition,
        [
            erl_syntax:clause(
                [erl_syntax:atom(true)],
                [],
                [CaseConditionTrueBody]
            ),
            erl_syntax:clause(
                [erl_syntax:atom(false)],
                [],
                [CaseConditionFalseBody]
            )
            ]
        ),

    % For Fold Expression 
    FoldBody = erl_syntax:fun_expr([
        erl_syntax:clause(
            [erl_syntax:tuple([erl_syntax:tuple([SrcVar, DstVar]),ConditionVar]),AccVar],
            [],
            [CaseExpression])
    ]),

    FoldExpression = erl_syntax:application(erl_syntax:atom(lists),erl_syntax:atom(foldl),
                                [FoldBody, erl_syntax:list([]), erl_syntax:application(erl_syntax:atom(maps),erl_syntax:atom(to_list),[TransitionsVar])]
                                ),

    [erl_syntax:function(
        erl_syntax:atom(reachable_states),
        [
            erl_syntax:clause(
                [ParamState,ParamEvent],
                [],
                [TransitionsAssignment,FoldExpression]
            )
        ]
    )].

% preceeding_states/1
generate_preceeding_states_from_state_function() ->
   % vars
    ParamState = erl_syntax:variable('State'),
    % ParamEvent = erl_syntax:variable('Event'),
    TransitionsVar = erl_syntax:variable('StateTransitionTable'),
    SrcVar = erl_syntax:variable('Src'),
    DstVar = erl_syntax:variable('Dst'),
    ConditionVar = erl_syntax:variable('_Condition'),
    AccVar = erl_syntax:variable('Acc'),

    TransitionsAssignment = erl_syntax:infix_expr(
        TransitionsVar,
        erl_syntax:operator('='),
        erl_syntax:application(erl_syntax:atom(init_transitions),[])
        ),
    

    % For If Expression 
    IfClauseMatch = 
        erl_syntax:clause([],
        [erl_syntax:infix_expr(DstVar,erl_syntax:operator('=:='),ParamState)],
        [erl_syntax:application(erl_syntax:atom(lists), erl_syntax:atom(usort), [erl_syntax:cons(SrcVar, AccVar)])]),
    
    IfClauseNoMatch = 
        erl_syntax:clause([],
        [erl_syntax:atom(true)],
        [AccVar]),

    IfExpression = erl_syntax:if_expr([IfClauseMatch,IfClauseNoMatch]),

    FoldBody = erl_syntax:fun_expr([
        erl_syntax:clause(
            [erl_syntax:tuple([erl_syntax:tuple([SrcVar, DstVar]),ConditionVar]),AccVar],
            [],
            [IfExpression])
    ]),

    FoldExpression = erl_syntax:application(erl_syntax:atom(lists),erl_syntax:atom(foldl),
                            [FoldBody, erl_syntax:list([]), erl_syntax:application(erl_syntax:atom(maps),erl_syntax:atom(to_list),[TransitionsVar])]
                            ),
    
    [erl_syntax:function(
        erl_syntax:atom(preceeding_states_from_state),
        [
            erl_syntax:clause(
                [ParamState],
                [],
                [TransitionsAssignment,FoldExpression]
            )
        ]
    )].

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
get_pat({Mod, _, {?HML_ACT, _, Pat, Guard}, _})
  when Mod =:= ?HML_POS; Mod =:= ?HML_NEC ->

  Str = erl_pp:expr(erl_syntax:revert(gen_eval:pat_tuple(Pat))),
  Replaced = re:replace(Str, "\\b([A-Z_][a-zA-Z0-9_@]*)\\b", "undefined", [{return, list}, global]),

  {ok, Tokens, _EndLine} = erl_scan:string(Replaced ++ "."),
  {ok, [AbsForm]} = erl_parse:parse_exprs(Tokens),
  AbsForm.


%%% @private Initializes the variable placeholder generator.
-spec init_ph() -> ok.
init_ph() ->

  % Placeholder token list must at least contain one name.
  if length(?PH_NAMES) < 1 -> error("Empty token token names"); true -> ok end,

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
extract_bound_vars_from_guard(Node = {Vrd, LineNumber}) ->
    [];
extract_bound_vars_from_guard(Node = {?HML_VAR, _, _Name}) ->
    [];
extract_bound_vars_from_guard(Node = {?HML_MAX, _, {?HML_VAR, _, _Name}, Phi}) ->
    extract_bound_vars_from_guard(Phi);
extract_bound_vars_from_guard(OuterNode =
        {?HML_AND, _,
            InnerLeftNode =
                {?HML_NEC, _, PhiLeftNode = {_, LineNumberLeft, PatPhiLeft, GuardPhiLeft}, PsiLeft},
            InnerRightNode =
                {?HML_NEC, _, PhiRightNode = {_, LineNumberRight, PatPhiRight, GuardPhiRight},
                    PsiRight}}) ->
    BoundVarsLeft = extract_vars(PatPhiLeft, []),
    BoundVarsRight = extract_vars(PatPhiRight, []),
    BoundVars = BoundVarsLeft ++ BoundVarsRight,
    BoundVars;
extract_bound_vars_from_guard(Node = {?HML_NEC, LineNumber, {act, _, Pat, Guard}, Phi}) ->
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