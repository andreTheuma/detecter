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
-export([hof_generation/2]).

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
            erl_syntax:infix_expr(
                erl_syntax:variable("Acceptance"),
                erl_syntax:operator('='),
                erl_syntax:fun_expr([
                    erl_syntax:clause([], none, [erl_syntax:atom(?MON_ACC)])
                ])
            );
        ?HML_FLS ->
            erl_syntax:infix_expr(
                erl_syntax:variable("Rejection"),
                erl_syntax:operator('='),
                erl_syntax:fun_expr([
                    erl_syntax:clause([], none, [erl_syntax:atom(?MON_REJ)])
                ])
            );
        ?HML_CORR ->
            erl_syntax:infix_expr(
                erl_syntax:variable("Corrupt"),
                erl_syntax:operator('='),
                erl_syntax:fun_expr([
                    erl_syntax:clause([], none, [erl_syntax:atom(?MON_CORR)])
                ])
            )
    end.

-spec generate_higher_order_function(Node, Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    Opts :: opts:options().
generate_higher_order_function(Node = {Vrd, LineNumber}, _Opts) when
    Vrd =:= ?HML_TRU; Vrd =:= ?HML_FLS
->
    ?TRACE("Verdict node from src line ~p is consumed. ~n", [LineNumber]),
    [];
generate_higher_order_function(
    Node = {?HML_NEC, NecLineNumber, Phi = {act, _, Pat = {init, _, _, _, _}, Guard}, Psi}, _Opts
) ->
    ?TRACE("init node from src line ~p is consumed. ~n", [NecLineNumber]),
    generate_higher_order_function(Psi, _Opts);
generate_higher_order_function(Var = {?HML_VAR, _, _Name}, _Opts) ->
    ?TRACE("Generating higher order function for 'var' node ~p. ~n ", [_Name]),
    Env = get_env(Var),
    erl_syntax:tuple([erl_syntax:atom(?MON_VAR), Env, Var]);
generate_higher_order_function(Node = {?HML_MAX, _, Var = {?HML_VAR, _, _}, Phi}, _Opts) ->
    ?TRACE("Generating higher order function for 'max' node ~p. ~n ", [Node]),
    ?TRACE("Phi for max: ~p ~n", [Phi]),

    FunctionName = generate_function_name(Node),
    % FunctionArgs = lists:usort(
    %     lists:flatten([[erl_syntax:variable(V) || V <- generate_function_args(Phi)]])
    % ),

    NextFunctionName = generate_function_name(Phi),
    NextFunctionArgs = lists:usort(
        lists:flatten([[erl_syntax:variable(V) || V <- generate_function_args(Phi)]])
    ),

    Clause = erl_syntax:clause(
        % [gen_eval:pat_tuple(Pat)],
        [],
        none,
        [erl_syntax:application(erl_syntax:variable(NextFunctionName), NextFunctionArgs)]
    ),
    Fun = erl_syntax:named_fun_expr(erl_syntax:variable("X"), [Clause]),
    FunctionClosure = erl_syntax:infix_expr(
        erl_syntax:variable(FunctionName),
        erl_syntax:operator('='),
        Fun
    ),
    [FunctionClosure | lists:flatten([generate_higher_order_function(Phi, _Opts)])];
generate_higher_order_function(
    OuterNode =
        {?HML_AND, _,
            InnerLeftNode =
                {?HML_NEC, _, PhiLeftNode = {_, LineNumberLeft, PatPhiLeft, GuardPhiLeft}, PsiLeft},
            InnerRightNode =
                {?HML_NEC, _, PhiRightNode = {_, LineNumberRight, PatPhiRight, GuardPhiRight},
                    PsiRight}},
    _Opts
) ->
    ?TRACE("Generating higher order functions for 'and' node from src lines ~p and ~p. ~n ", [
        LineNumberLeft, LineNumberRight
    ]),

    LeftNodeFunctionName = generate_function_name(InnerLeftNode),
    LeftNodeArgs = lists:usort(
        lists:flatten([[erl_syntax:variable(V) || V <- generate_function_args(InnerLeftNode)]])
    ),
    PsiLeftFunctionArgs = lists:usort(
        lists:flatten([[erl_syntax:variable(V) || V <- generate_function_args(PsiLeft)]])
    ),

    RightNodeFunctionName = generate_function_name(InnerRightNode),
    RightNodeArgs = lists:usort(
        lists:flatten([[erl_syntax:variable(V) || V <- generate_function_args(InnerRightNode)]])
    ),
    PsiRightFunctionArgs = lists:usort(
        lists:flatten([[erl_syntax:variable(V) || V <- generate_function_args(PsiRight)]])
    ),

    %? removes duplicated variables...this is temporary workaround and should be proved. It could
    %? be that the variables also change order...not too sure yet

    CompositeFunctionArgs = lists:usort(lists:flatten([LeftNodeArgs, RightNodeArgs])),
    CompositeFunctionName = LeftNodeFunctionName ++ RightNodeFunctionName,

    LeftNodeClause = erl_syntax:clause(
        [gen_eval:pat_tuple(PatPhiLeft)],
        GuardPhiLeft,
        [
            erl_syntax:application(
                erl_syntax:variable(generate_function_name(InnerLeftNode)), PsiLeftFunctionArgs
            )
        ]
    ),

    RightNodeClause = erl_syntax:clause(
        [gen_eval:pat_tuple(PatPhiRight)],
        GuardPhiRight,
        [
            erl_syntax:application(
                erl_syntax:variable(generate_function_name(InnerRightNode)), PsiRightFunctionArgs
            )
        ]
    ),

    ReceiveExpr = erl_syntax:receive_expr([LeftNodeClause, RightNodeClause]),

    Fun = erl_syntax:fun_expr([
        erl_syntax:clause(
            CompositeFunctionArgs,
            none,
            [ReceiveExpr]
        )
    ]),

    FunctionClosure = erl_syntax:infix_expr(
        erl_syntax:variable(CompositeFunctionName),
        erl_syntax:operator('='),
        Fun
    ),

    ?TRACE("Generated function ~p. ~n", [CompositeFunctionName]),
    [
        FunctionClosure
        | lists:flatten([
            generate_higher_order_function(InnerLeftNode, _Opts),
            generate_higher_order_function(InnerRightNode, _Opts)
        ])
    ];
generate_higher_order_function(Node = {?HML_NEC, LineNumber, {act, _, Pat, Guard}, Phi}, _Opts) ->
    ?TRACE("Generating higher order function for 'nec' node from src line ~p. ~n ", [LineNumber]),

    FunctionName = generate_function_name(Node),

    NextFunctionName = generate_function_name(Phi),
    NextFunctionArgs = lists:usort(
        lists:flatten([[erl_syntax:variable(V) || V <- generate_function_args(Phi)]])
    ),

    % CorruptGuard = erl_syntax:infix_expr(
    %     erl_syntax:application(
    %         erl_syntax:atom(element),
    %         [erl_syntax:integer(1), Args]
    %     ),
    %     erl_syntax:operator('=:='),
    %     erl_syntax:atom('corrupt_payload')
    % ),

    Clause = erl_syntax:clause(
        [gen_eval:pat_tuple(Pat)],
        Guard,
        [erl_syntax:application(erl_syntax:variable(NextFunctionName), NextFunctionArgs)]
    ),

    ReceiveExpr = erl_syntax:receive_expr([Clause]),

    Fun = erl_syntax:fun_expr(
        case ?IS_TERMINATING_HML(Phi) of
            true ->
                ?TRACE("Terminating function - Atomic termination generated. ~n"),
                [
                    erl_syntax:clause([], none, erl_syntax:clause_body(Clause))
                ];
            _ ->
                [
                    erl_syntax:clause(
                        if
                            Guard =:= [] ->
                                [gen_eval:pat_tuple(Pat)];
                            true ->
                                lists:flatten([
                                    [gen_eval:pat_tuple(Pat)],
                                    [
                                        erl_syntax:variable(V)
                                     || V <- extract_vars_from_guard(Guard, Pat)
                                    ]
                                ])
                        end,
                        none,
                        [ReceiveExpr]
                    )
                ]
        end
    ),

    FunctionClosure = erl_syntax:infix_expr(
        erl_syntax:variable(FunctionName),
        erl_syntax:operator('='),
        Fun
    ),

    ?TRACE("Generated function ~p. ~n", [FunctionName]),
    [FunctionClosure | lists:flatten([generate_higher_order_function(Phi, _Opts)])].

%% @private Generates the receive block for the function look up. This
%% is the entry point for the look up of the function to be executed.
-spec generate_init_block(Node, _Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    _Opts :: opts:options().
generate_init_block({?HML_NEC, _, {act, _, Pat = {init, _, Pid2, Pid, MFArgs}, Guard}, Phi}, _Opts) ->
    ?TRACE("Generating init block for 'nec' node. ~n"),
    Var = erl_syntax:variable(generate_function_name(Phi)),
    Args = [],

    AnonFunEntry = erl_syntax:clause([gen_eval:pat_tuple(Pat)], none, [
        erl_syntax:application(Var, Args)
    ]),

    ReceiveExpr = erl_syntax:receive_expr([AnonFunEntry]),

    [ReceiveExpr];
generate_init_block(_, _) ->
    ?ERROR("Invalid node for init block generation.").

%%% @public Entry point for the generation of higher order functions for the monitor, coming from `gen_eval` module
%%%
-spec hof_generation(Node, Opts) -> erl_syntax:syntaxTree() when
    Node :: af_maxhml(),
    Opts :: opts:options().
hof_generation(Node, Opts) ->
    % Important to reverse the list of functions, as the functions cannot have any forward references.
    FunctionRelations = lists:reverse(generate_higher_order_function(Node, Opts)),

    InitBlock = generate_init_block(Node, Opts),

    VrdTrue = generate_verdict_function({tt, 0}, 0),
    VrdFalse = generate_verdict_function({ff, 0}, 0),
    VrdCorrupt = generate_verdict_function({corr, 0}, 0),

    [VrdTrue, VrdFalse, VrdCorrupt] ++ FunctionRelations ++ InitBlock.

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

    CorrGuard = erl_syntax:infix_expr(
        erl_syntax:application(
            erl_syntax:atom(element),
            [erl_syntax:integer(1), erl_syntax:variable('_Pat')]
        ),
        erl_syntax:operator('=:='),
        erl_syntax:atom(corrupt_payload)
    ),

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
        erl_syntax:clause([gen_eval:corrupted_pat_tuple()], CorrGuard, [erl_syntax:atom(?MON_CORR)]),
        erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(false)])
    ]),

    InvPred = erl_syntax:fun_expr([
        erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [erl_syntax:atom(false)]),
        erl_syntax:clause([gen_eval:corrupted_pat_tuple()], CorrGuard, [erl_syntax:atom(?MON_CORR)]),
        erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(true)])
    ]),

    % Encode the action bodies. The normal (left) action body consists of the
    % pattern with variables, and the continuation monitor. The inverse (right)
    % action consists of the verdict when the inverse pattern and guard test is
    % successful.
    CntBody = erl_syntax:fun_expr([
        erl_syntax:clause([gen_eval:pat_tuple(Pat)], Guard, [visit(Phi, _Opts)])
    ]),

    CorrBody = erl_syntax:named_fun_expr(erl_syntax:variable('CorruptBody'), [
        erl_syntax:clause([gen_eval:corrupted_pat_tuple()], CorrGuard, [
            erl_syntax:tuple([erl_syntax:atom(?MON_CORR), gen_corr_env()])
        ])
    ]),

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
        [erl_syntax:atom(act), get_env(Node, Ph, true), Pred, CorrBody, CntBody]
    ),
    RightAct = erl_syntax:tuple(
        [erl_syntax:atom(act), get_env(Node, Ph, false), InvPred, CorrBody, VrdBody]
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
-spec generate_function_name(Node) -> string() when
    Node :: af_maxhml().
generate_function_name({?HML_VAR, LineNumber, Name}) ->
    string:titlecase(atom_to_list(Name) ++ integer_to_list(LineNumber));
generate_function_name(Node = {?HML_MAX, LineNumber, Var = {_, _, Name}, _}) ->
    string:titlecase(atom_to_list(Name) ++ integer_to_list(LineNumber));
generate_function_name(
    {?HML_AND, _, Phi = {_, PhiLineNumber, PhiPat, _}, Psi = {_, PsiLineNumber, PsiPat, _}}
) ->
    ?TRACE("Generating composite function name for 'and' nodes from src lines ~p and ~p. ~n ", [
        PhiLineNumber, PsiLineNumber
    ]),
    Action1 = element(1, element(3, PhiPat)),
    Action2 = element(1, element(3, PsiPat)),
    string:titlecase(atom_to_list(Action1) ++ integer_to_list(PhiLineNumber)) ++
        string:titlecase(atom_to_list(Action2) ++ integer_to_list(PsiLineNumber));
generate_function_name({?HML_NEC, NecLineNumber, {_, PhiLineNumber, Pat, _}, _}) ->
    ?TRACE("Generating function name for 'nec' node from src line ~p. ~n ", [NecLineNumber]),
    Action = element(1, Pat),
    string:titlecase(atom_to_list(Action) ++ integer_to_list(PhiLineNumber));
generate_function_name({Verdict, _}) when Verdict =:= ?HML_TRU; Verdict =:= ?HML_FLS ->
    string:titlecase(
        if
            Verdict =:= ?HML_TRU -> "Acceptance";
            Verdict =:= ?HML_FLS -> "Rejection"
        end
    ).

%% @private Generates the function arguments for the given node.
-spec generate_function_args(Node) -> [erl_syntax:syntaxTree()] when
    Node :: af_maxhml().
generate_function_args({?HML_VAR,LineNumber,Name}) ->
    ?TRACE("Consuming function arguments for 'var' node ~p on line ~p. ~n ", [Name, LineNumber]),
    [];
generate_function_args(
    OuterNode = {?HML_AND, _, Node = {nec, _, Phi = {_, LineNumber, PatPhi, GuardPhi}, Psi}, RightNode}
) ->
    % ?TRACE("Generating compound function arguments for 'nec' node from src lines ~p and ~p. ~n " , [LineNumber, PsiLineNumber]),
    extract_vars_from_guard(GuardPhi, PatPhi);
generate_function_args({?HML_NEC, _, {_, LineNumber, Pat, Guard}, _}) ->
    ?TRACE("Generating function arguments for 'nec' node from src line ~p. ~n ", [LineNumber]),
    extract_vars_from_guard(Guard, Pat);
generate_function_args({Verdict, _}) when Verdict =:= ?HML_TRU; Verdict =:= ?HML_FLS ->
    ?TRACE("No function arguments for verdict node ~p. ~n", [Verdict]),
    [].

%%% @private Takes the Monitor state table along with the corrupt event pattern, returning a
%%% set of the possible states that the monitor can branch into.
-spec get_possible_states(Pat, MonitorStateTable) -> [MonitorStateTable] when
    Pat :: gen_eval:af_pattern(),
    MonitorStateTable :: tuple().
get_possible_states(Pat, MonitorStateTable) ->
    pass.

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

%%% @private Returns the variables from a guard expression. If a variable is found in both the guard and the pattern,
%%% the variable is not included, as it will shadow the variable in the pattern.
-spec extract_vars_from_guard(Guard, Pat) -> string() when
    Guard :: gen_eval:af_guard(),
    Pat :: gen_eval:af_pattern().
extract_vars_from_guard(Guard, Pat) ->
    PatVars = extract_vars(Pat, []),
    extract_vars_guard(Guard, PatVars, []).

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
extract_vars({var, _, Variable}, Acc) ->
    [Variable | Acc];
extract_vars([H | T], Acc) ->
    extract_vars(T, extract_vars(H, Acc));
extract_vars(Tuple, Acc) when is_tuple(Tuple) ->
    TupleList = tuple_to_list(Tuple),
    extract_vars(TupleList, Acc);
extract_vars(_, Acc) ->
    Acc.

%%% @private Returns the type of the node.
-spec node_type(Node) -> atom() when
    Node :: af_maxhml().
node_type(Node) -> element(1, Node).
