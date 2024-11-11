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

-export([generate_monitor_table/1]).
-export([modularise_hml/2, generate_init_block/2]).

%%% Callbacks/Internal.
-export([visit/2]).

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
        % {?HML_VAR,_,_} -> true;
        _ -> false
    end
).

-define(IS_RECURSIVE_HML(Node),
    case Node of
        % {?HML_MAX, _, _, _} -> true;
        {?HML_VAR, _, _} -> true;
        _ -> false
    end
).

%% Monitor AST node tags.
-define(MON_ACC, yes).
-define(MON_REJ, no).
-define(MON_ACT, act).
-define(MON_CHS, chs).
-define(MON_OR, 'or').
-define(MON_AND, 'and').
-define(MON_REC, rec).
-define(MON_VAR, var).
-define(MON_CORR, corrupt).

%% NOTE:  THIS IS A VARIABLE IN ERLANG AST: {var, 5, 'Corrupt'}
%%

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
-define(PH_COMPOSITE_FUNCTION, "_composite_function_").

%%% ----------------------------------------------------------------------------
%%% Type definitions.
%%% ----------------------------------------------------------------------------

-type line() :: erl_anno:line().
%% Line number in source.

-type with() ::
    {with, line(), gen_eval:af_mfargs()}
    | {with, line(), gen_eval:af_mfargs(), gen_eval:af_constraint()}.
%% Process instrumentation selection MFArgs.

-type spec() :: {spec, line(), with(), af_maxhml()}.
%% Instrumentation specification abstract form.

-type af_maxhml() ::
    af_hml_tt()
    | af_hml_ff()
    | af_hml_pos()
    | af_hml_nec()
    | af_hml_or()
    | af_hml_and()
    | af_hml_max()
    | af_hml_var().
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
%%% Functions to generate the higher order functions for the monitor, one time
%%% for each monitor action.
%%% This section also generates the verdict functions for the monitor and the entry (receive) block.
%%% ----------------------------------------------------------------------------
%%%
-spec generate_verdict_function({Vrd, _}, _Opts) -> erl_syntax:syntaxTree() when
    Vrd :: af_maxhml(),
    _Opts :: opts:options().
generate_verdict_function({Vrd, _}, _Opts) ->
    case Vrd of
        ?HML_TRU ->
            erl_syntax:function(
                erl_syntax:atom(generate_function_name({Vrd, 0})),
                [erl_syntax:clause([], none, [erl_syntax:atom(?MON_ACC)])]
            );
        ?HML_FLS ->
            erl_syntax:function(
                erl_syntax:atom(generate_function_name({Vrd, 0})),
                [erl_syntax:clause([], none, [erl_syntax:atom(?MON_REJ)])]
            );
        ?HML_CORR ->
            erl_syntax:function(
                erl_syntax:atom("corrupt"),
                [erl_syntax:clause([], none, [erl_syntax:atom(?MON_CORR)])]
            )
    end.

-spec generate_function(Node, Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    Opts :: opts:options().
generate_function(Node = {Vrd, LineNumber}, _Opts) when
    Vrd =:= ?HML_TRU; Vrd =:= ?HML_FLS
->
    [];
generate_function(
    Node = {?HML_NEC, NecLineNumber, Phi = {act, _, Pat = {init, _, _, _, _}, Guard}, Psi}, _Opts
) ->
    generate_function(Psi, _Opts);
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
                {?HML_NEC, _, PhiLeftNode = {_, LineNumberLeft, PatPhiLeft, GuardPhiLeft}, PsiLeft},
            InnerRightNode =
                {?HML_NEC, _, PhiRightNode = {_, LineNumberRight, PatPhiRight, GuardPhiRight},
                    PsiRight}},
    _Opts
) ->
    
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
        [
            erl_syntax:application(
                erl_syntax:atom(LeftNodeFunctionName), PsiLeftFunctionArgs
            )
        ]
    ),

    RightNodeClause = erl_syntax:clause(
        [gen_eval:pat_tuple(PatPhiRight)],
        GuardPhiRight,
        [
            erl_syntax:application(
                erl_syntax:variable(RightNodeFunctionName), PsiRightFunctionArgs
            )
        ]
    ),

    ReceiveClause = erl_syntax:clause(
        CompositeFunctionArgs,
        none,
        [erl_syntax:receive_expr([LeftNodeClause, RightNodeClause])]
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

    Clause = erl_syntax:clause(
        [gen_eval:pat_tuple(Pat)],
        Guard,
        [erl_syntax:application(erl_syntax:atom(NextFunctionName), NextFunctionArgs)]
    ),

    ReceiveClause = erl_syntax:clause(
        FunctionArgs,
        none,
        [erl_syntax:receive_expr([Clause])]
    ),

    Function = erl_syntax:function(
        erl_syntax:atom(FunctionName),
        case ?IS_TERMINATING_HML(Phi) of
            true ->
                ?TRACE("Terminating function detected - Atomic termination generated. ~n"),
                [erl_syntax:clause([], none, erl_syntax:clause_body(Clause))];
            _ ->
                case ?IS_RECURSIVE_HML(Phi) of
                    true ->
                        ?TRACE("Recursive function detected - Recursive call generated. ~n"),
                        [erl_syntax:clause(FunctionArgs, none, erl_syntax:clause_body(Clause))];
                    _ ->
                        [ReceiveClause]
                    end
        end
    ),

    ?TRACE("Generated function ~p. ~n", [FunctionName]),

    [Function | lists:flatten([generate_function(Phi, _Opts)])].

    % CorruptGuard = erl_syntax:infix_expr(
    % %     erl_syntax:application(
    % %         erl_syntax:atom(element),
    % %         [erl_syntax:integer(1), Args]
    % %     ),
    % %     erl_syntax:operator('=:='),
    % %     erl_syntax:atom('corrupt_payload')
    % % ),


%% @public Generates the receive block for the function look up. This
%% is the entry point for the look up of the function to be executed.
-spec generate_init_block(Node, _Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    _Opts :: opts:options().
generate_init_block({?HML_NEC, _, {act, _, Pat = {init, _, Pid2, Pid, MFArgs}, Guard}, Phi}, _Opts) ->
    ?TRACE("Generating init block for 'nec' node. ~n"),

    NextFunctionName = generate_function_name(Phi),
    NextFunctionArgs = generate_function_args(Phi, []),

    persistent_term:put(NextFunctionName, NextFunctionArgs),
    AnonFunEntry = erl_syntax:clause([gen_eval:pat_tuple(Pat)], none, [
        erl_syntax:application(erl_syntax:atom(NextFunctionName), lists:flatten([erl_syntax:variable(V) || V <- NextFunctionArgs]))
    ]),

    ReceiveExpr = erl_syntax:receive_expr([AnonFunEntry]),
    ?TRACE("Generated init block for 'nec' node. ~n"),
    
    [ReceiveExpr];
generate_init_block(_, _) ->
    ?ERROR("Invalid node for init block generation.").

%%% @public Modularises the functions required for the monitor, coming from `gen_eval` module
%%%
-spec modularise_hml(Node, Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    Opts :: opts:options().
modularise_hml(Node, Opts) ->

    Functions = generate_function(Node, Opts),

    VrdTrue = generate_verdict_function({tt, 0}, 0),
    VrdFalse = generate_verdict_function({ff, 0}, 0),
    VrdCorrupt = generate_verdict_function({corr, 0}, 0),

    lists:flatten([VrdTrue, VrdFalse, VrdCorrupt, Functions]).

%%% ----------------------------------------------------------------------------
%%% Private AST manipulation functions.
%%% ----------------------------------------------------------------------------

-spec visit(Node, Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    Opts :: opts:options().
visit(Node = {Bool, _}, _Opts) when Bool =:= ?HML_TRU; Bool =:= ?HML_FLS ->
    ?TRACE("Visiting '~s' node ~p.", [Bool, Node]),

    % Get monitor meta environment for node.
    Env = get_env(Node),
    erl_syntax:tuple([
        erl_syntax:atom(
            if
                Bool =:= ?HML_TRU -> ?MON_ACC;
                Bool =:= ?HML_FLS -> ?MON_REJ
            end
        ),
        Env
    ]);
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
visit(Node = {Op, _, Phi, Psi}, _Opts) when
    Op =:= ?HML_OR; Op =:= ?HML_AND
->
    ?TRACE("Visiting '~s' node ~p.", [Op, Node]),

    % Get monitor meta environment for node.
    Env = get_env(Node),
    erl_syntax:tuple(
        [erl_syntax:atom(Op), Env, visit(Phi, _Opts), visit(Psi, _Opts)]
    );
visit(Node = {Mod, _, {act, _, Pat, Guard}, Phi}, _Opts) when
    Mod =:= ?HML_POS; Mod =:= ?HML_NEC
->
    ?TRACE("Visiting '~s' node ~p.", [Mod, Node]),

    % CorrGuard = erl_syntax:infix_expr(
    %     erl_syntax:application(
    %         erl_syntax:atom(element),
    %         [erl_syntax:integer(1), erl_syntax:variable('_Pat')]
    %     ),
    %     erl_syntax:operator('=:='),
    %     erl_syntax:atom(corrupt_payload)
    % ),

    % Encode the predicate functions for the action and its inverse. The predicate
    % functions are mutually-exclusive. This means that for any pattern and guard
    % combination, and any value the pattern data variables may be mapped to,
    % these two predicate functions will always return the negated truth value of
    % of each other.
    %
    % In addition, there is also a thrid predicate function that encodes the case when the
    % pattern is corrupted.

    % Pred = erl_syntax:named_fun_expr(erl_syntax:atom('String')
    %     ,[
    %     erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [erl_syntax:atom(true)]),
    %     erl_syntax:clause([gen_eval:corrupted_pat_tuple()], CorrGuard, [erl_syntax:atom(?MON_CORR)]),
    %     erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(false)])
    % ]),

    Pred = erl_syntax:fun_expr([
        erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [erl_syntax:atom(true)]),
        % erl_syntax:clause([gen_eval:corrupted_pat_tuple()], CorrGuard, [erl_syntax:atom(?MON_CORR)]),
        erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(false)])
    ]),

    InvPred = erl_syntax:fun_expr([
        erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [erl_syntax:atom(false)]),
        % erl_syntax:clause([gen_eval:corrupted_pat_tuple()], CorrGuard, [erl_syntax:atom(?MON_CORR)]),
        erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(true)])
    ]),

    % Encode the action bodies. The normal (left) action body consists of the
    % pattern with variables, and the continuation monitor. The inverse (right)
    % action consists of the verdict when the inverse pattern and guard test is
    % successful.
    CntBody = erl_syntax:fun_expr([
        erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [visit(Phi, _Opts)])
    ]),

    % CorrBody = erl_syntax:named_fun_expr(erl_syntax:variable('CorruptBody'), [
    %     erl_syntax:clause([gen_eval:corrupted_pat_tuple()], CorrGuard, [
    %         erl_syntax:tuple([erl_syntax:atom(?MON_CORR), gen_corr_env()])
    %     ])
    % ]),

    VrdBody = erl_syntax:fun_expr([
        erl_syntax:clause([erl_syntax:underscore()], none, [
            if
                Mod =:= pos ->
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
        % [erl_syntax:atom(act), get_env(Node, Ph, true), Pred, CorrBody, CntBody]
        [erl_syntax:atom(act), get_env(Node, Ph, true), Pred, CntBody]
    ),
    RightAct = erl_syntax:tuple(
        % [erl_syntax:atom(act), get_env(Node, Ph, false), InvPred, CorrBody, VrdBody]
        [erl_syntax:atom(act), get_env(Node, Ph, false), InvPred, VrdBody]
    ),

    % CorruptAct = erl_syntax:tuple(
    %   [erl_syntax:atom(act), get_env(Node, Ph, false), CorruptPred, CorruptBody]),

    % Encode the mutually-exclusive choice consisting of the left and right
    % summands.
    erl_syntax:tuple([erl_syntax:atom(?MON_CHS), get_chs_env(), LeftAct, RightAct]).
% erl_syntax:tuple([erl_syntax:atom(this_is_one), get_chs_env(), LeftAct, RightAct]).

%%% ----------------------------------------------------------------------------
%%% Private monitor helper functions for corruption detection.
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
    ?TRACE("Free vars are ~p for pattern ~p.~n", [FreeVarsInPattern, Pat]),
    UpdatedBoundVars = lists:usort(BoundVars ++ extract_bound_vars_from_guard(Node)),
    ContinuationVars = generate_function_args(Phi, UpdatedBoundVars),  
    ?TRACE("Continuation vars are ~p.~n", [ContinuationVars]),
    ?TRACE("Bound vars are ~p.~n", [UpdatedBoundVars]),

    TotalFreeVars = lists:usort(FreeVarsInPattern ++ ContinuationVars),
    % ?TRACE("Total free vars are ~p.~n", [TotalFreeVars]),
    FreeVars = lists:usort(TotalFreeVars -- UpdatedBoundVars),  % Exclude bound vars in this scope
    FreeVars;

generate_function_args({Verdict, _}, _BoundVars) when Verdict =:= ?HML_TRU; Verdict =:= ?HML_FLS ->
    % Terminating verdict node encountered
    ?TRACE("No function arguments for terminating verdict node ~p.~n", [Verdict]),
    [].


%%% @private The logic that handles corrupt event patterns. When the event pattern
%%% is corrupt, the monitor must branch into a set of mutually-exclusive choices,
%%% one for each possible correct event pattern.

%%% ----------------------------------------------------------------------------
%%% Private monitor functions to generate the monitor table information.
%%% ----------------------------------------------------------------------------

-spec generate_monitor_table(Dir) -> EventTable when
    Dir :: string(),
    EventTable :: [gen_eval:af_event()].
generate_monitor_table(Dir) ->
    Table = event_table_parser:parse_table(Dir),
    sys_info_eval:create_sys_info(Table),
    sys_info_eval:return_sys_info().

%%% ----------------------------------------------------------------------------
%%% Private monitor environment creation functions.
%%% ----------------------------------------------------------------------------

%%% @private Returns an Erlang AST representation of the monitor environment
%%% used to manage the monitor meta information such as the substitution and its
%%% stringified representation.
-spec get_env(Node) -> erl_syntax:syntaxTree() when
    Node ::
        af_hml_tt()
        | af_hml_ff()
        | af_hml_or()
        | af_hml_and()
        | af_hml_max()
        | af_hml_var().
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

-spec gen_corr_env() -> erl_syntax:syntaxTree().
gen_corr_env() ->
    Str = new_env_kv(?KEY_STR, erl_syntax:string("corrupt")),
    new_env([Str]).

%%% @private Returns an Erlang AST representation of the monitor environment
%%% for monitor parallel disjunction and conjunction.
-spec get_env(Node, Ph, Inv) -> erl_syntax:syntaxTree() when
    Node :: af_hml_pos() | af_hml_nec(),
    Ph :: string(),
    Inv :: boolean().
get_env(Node = {Mod, _, _Act, _Phi}, Ph, Inv) when
    Mod =:= ?HML_POS; Mod =:= ?HML_NEC
->
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
-spec new_env_kv(Key, Val) -> erl_syntax:syntaxTree() when
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
-spec get_str(Node) -> erl_syntax:syntaxTree() when
    Node ::
        af_hml_tt()
        | af_hml_ff()
        | af_hml_or()
        | af_hml_and()
        | af_hml_max()
        | af_hml_var().
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
-spec get_str(Node, Ph, Inv) -> erl_syntax:syntaxTree() when
    Node :: af_hml_pos() | af_hml_nec(),
    Ph :: string(),
    Inv :: boolean().
get_str({Mod, _, {?HML_ACT, _, Pat, Guard}, _}, Ph, Inv) when
    Mod =:= ?HML_POS; Mod =:= ?HML_NEC
->
    % Stringify placeholder and the internal representation of the pattern as an
    % Erlang trace event.
    IoList = [Ph, $/, erl_pp:expr(erl_syntax:revert(gen_eval:pat_tuple(Pat)))],

    % Stringify guard only if present.
    IoList_ =
        if
            Guard =:= [] -> IoList;
            true -> [IoList, $\s, erl_pp:guard(Guard)]
        end,

    % Add the stringified negation if the branch is the inverse one (called the)
    % negative branch of mutually-exclusive choice.
    IoList__ =
        if
            Inv -> IoList_;
            true -> ["NOT(", IoList_, ")"]
        end,

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
get_pat({Mod, _, {?HML_ACT, _, Pat, Guard}, _}) when
    Mod =:= ?HML_POS; Mod =:= ?HML_NEC
->
    Str = erl_pp:expr(erl_syntax:revert(gen_eval:pat_tuple(Pat))),
    Replaced = re:replace(Str, "\\b([A-Z_][a-zA-Z0-9_@]*)\\b", "undefined", [{return, list}, global]),

    {ok, Tokens, _EndLine} = erl_scan:string(Replaced ++ "."),
    {ok, [AbsForm]} = erl_parse:parse_exprs(Tokens),
    AbsForm.

%%% @private Initializes the variable placeholder generator.
-spec init_ph() -> ok.
init_ph() ->
    % Placeholder token list must at least contain one name.
    if
        length(?PH_NAMES) < 1 -> error("Empty token token names");
        true -> ok
    end,

    % list of available variable placeholder names.
    put(?KEY_PH_NAMES, ?PH_NAMES),
    % 0-based index.
    put(?KEY_PH_CNT, 0),
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

-spec unpack_vars(Vars) -> var when
    Vars :: [{tree,variable, _, VarName :: string()}].
unpack_vars(Vars) ->
    [VarName || {tree,variable, _, VarName} <- Vars].


%%% @private Returns the type of the node.
-spec node_type(Node) -> atom() when
    Node :: af_maxhml().
node_type(Node) -> element(1, Node).
