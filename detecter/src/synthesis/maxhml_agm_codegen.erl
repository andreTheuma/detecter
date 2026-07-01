%%% ----------------------------------------------------------------------------
%%% maxHML-specific Automaton-Guided Monitoring code generation.
%%% ----------------------------------------------------------------------------
-module(maxhml_agm_codegen).
-author("André Theuma").

-export([
    generate_missing_event_recovery_case/1,
    generate_monitor_reduction_fun/1,
    generate_sys_info_function/1,
    generate_all_states/0,
    generate_state_management/0,
    agm_generation/0
]).

%% maxHML logic AST node tags (from [maxhml_eval.erl]). Consider including in some definition .hrl file

-define(HML_TRU, tt).
-define(HML_FLS, ff).
-define(HML_NEC, nec).
-define(MON_ACC, yes).
-define(MON_REJ, no).

generate_missing_event_recovery_case(ReductionFun) ->
    RecoveryVar = erl_syntax:variable('Recovery'),
    RecoveryReasonVar = erl_syntax:variable('RecoveryReason'),
    ConsequenceReasonVar = erl_syntax:variable('ConsequenceReason'),
    ContinuationVar = erl_syntax:variable('Continuation'),
    ConsequenceCase = erl_syntax:case_expr(
        erl_syntax:application(
            erl_syntax:atom(resolve_monitoring_consequence),
            [RecoveryVar, ReductionFun]
        ),
        [
            erl_syntax:clause(
                [erl_syntax:tuple([
                    erl_syntax:atom(ok),
                    erl_syntax:map_expr([
                        erl_syntax:map_field_exact(
                            erl_syntax:atom(continuation),
                            ContinuationVar
                        )
                    ])
                ])],
                [],
                [erl_syntax:application(ContinuationVar, [])]
            ),
            erl_syntax:clause(
                [erl_syntax:tuple([
                    erl_syntax:atom(withhold),
                    ConsequenceReasonVar
                ])],
                [],
                [withhold_tuple(ConsequenceReasonVar)]
            )
        ]
    ),
    erl_syntax:case_expr(
        erl_syntax:application(
            erl_syntax:atom(handle_missing_event),
            [erl_syntax:variable('From')]
        ),
        [
            erl_syntax:clause(
                [erl_syntax:tuple([
                    erl_syntax:atom(ok),
                    RecoveryVar
                ])],
                [],
                [ConsequenceCase]
            ),
            erl_syntax:clause(
                [erl_syntax:tuple([
                    erl_syntax:atom(withhold),
                    RecoveryReasonVar
                ])],
                [],
                [withhold_tuple(RecoveryReasonVar)]
            )
        ]
    ).

generate_monitor_reduction_fun(Branches) ->
    SupportedModalities = lists:all(
        fun({Mod, _, _, _, _, _}) -> Mod =:= ?HML_NEC end,
        Branches
    ),
    ActionTypes = lists:usort([
        element(1, Pat)
        || {_, Pat, _, _, _, _} <- Branches
    ]),
    StateUpdateArgs = [
        maxhml_eval:generate_state_update_args(Pat)
        || {_, Pat, _, _, _, _} <- Branches
    ],
    SupportedStateArgs = lists:all(
        fun(Args) -> length(Args) =:= 1 end,
        StateUpdateArgs
    ),
    case SupportedModalities
        andalso length(ActionTypes) =:= 1
        andalso SupportedStateArgs
    of
        true ->
            generate_supported_monitor_reduction_fun(
                Branches,
                StateUpdateArgs
            );
        false ->
            generate_unknown_monitor_reduction_fun(
                unsupported_monitor_pattern
            )
    end.

generate_supported_monitor_reduction_fun(Branches, StateUpdateArgs) ->
    MissingEventVar = erl_syntax:variable('MissingEvent'),
    LiteralClauses = lists:zipwith(
        fun generate_literal_monitor_reduction_clause/2,
        Branches,
        StateUpdateArgs
    ),
    SymbolicClauses = generate_symbolic_monitor_reduction_clauses(
        Branches,
        StateUpdateArgs
    ),
    LiteralCase = erl_syntax:case_expr(
        MissingEventVar,
        LiteralClauses ++ [
            erl_syntax:clause(
                [erl_syntax:underscore()],
                [],
                [unknown_monitor_reduction(unmatched_literal)]
            )
        ]
    ),
    erl_syntax:fun_expr([
        erl_syntax:clause(
            [erl_syntax:tuple([
                erl_syntax:atom(literal),
                MissingEventVar
            ])],
            [],
            [LiteralCase]
        )
        | SymbolicClauses
    ] ++ [
        erl_syntax:clause(
            [erl_syntax:underscore()],
            [],
            [unknown_monitor_reduction(unsupported_event_spec)]
        )
    ]).

generate_literal_monitor_reduction_clause(
    {_, _, Guard, Phi, NextFunctionName, NextFunctionArgs},
    [EventPattern]
) ->
    erl_syntax:clause(
        [EventPattern],
        Guard,
        [successful_monitor_reduction(
            Phi,
            NextFunctionName,
            NextFunctionArgs
        )]
    ).

successful_monitor_reduction(Phi, NextFunctionName, NextFunctionArgs) ->
    erl_syntax:tuple([
        erl_syntax:atom(ok),
        erl_syntax:list([
            monitor_reduction_pair(
                Phi,
                NextFunctionName,
                NextFunctionArgs
            )
        ])
    ]).

successful_monitor_reductions(Branches) ->
    erl_syntax:tuple([
        erl_syntax:atom(ok),
        erl_syntax:list([
            monitor_reduction_pair(
                Phi,
                NextFunctionName,
                NextFunctionArgs
            )
            || {
                _,
                _,
                _,
                Phi,
                NextFunctionName,
                NextFunctionArgs
            } <- Branches
        ])
    ]).

monitor_reduction_pair(Phi, NextFunctionName, NextFunctionArgs) ->
    Signature =
        case Phi of
            {?HML_TRU, _} ->
                erl_syntax:tuple([
                    erl_syntax:atom(verdict),
                    erl_syntax:atom(?MON_ACC)
                ]);
            {?HML_FLS, _} ->
                erl_syntax:tuple([
                    erl_syntax:atom(verdict),
                    erl_syntax:atom(?MON_REJ)
                ]);
            _ ->
                erl_syntax:tuple([
                    erl_syntax:atom(continue),
                    erl_syntax:atom(NextFunctionName),
                    erl_syntax:list(NextFunctionArgs)
                ])
        end,
    Continuation = erl_syntax:fun_expr([
        erl_syntax:clause(
            [],
            [],
            [erl_syntax:application(
                erl_syntax:atom(NextFunctionName),
                NextFunctionArgs
            )]
        )
    ]),
    erl_syntax:tuple([Signature, Continuation]).

generate_symbolic_monitor_reduction_clauses(
    [{_, _, [], Phi, NextFunctionName, NextFunctionArgs}],
    [[EventPattern]]
) ->
    case consequence_depends_on_event(EventPattern, NextFunctionArgs) of
        true ->
            [];
        false ->
            SymbolicReduction = successful_monitor_reduction(
                Phi,
                NextFunctionName,
                NextFunctionArgs
            ),
            symbolic_event_spec_clauses(
                fun(_) -> SymbolicReduction end
            )
    end;
generate_symbolic_monitor_reduction_clauses(Branches, StateUpdateArgs) ->
    case classify_complementary_event_guards(
        Branches,
        StateUpdateArgs
    ) of
        {ok, EqualBranch, NotEqualBranch, ComparedValue} ->
            symbolic_event_spec_clauses(
                fun(EventSpec) ->
                    generate_symbolic_membership_case(
                        EventSpec,
                        ComparedValue,
                        EqualBranch,
                        NotEqualBranch
                    )
                end
            );
        unsupported ->
            []
    end.

symbolic_event_spec_clauses(BodyFun) ->
    RangeVar = erl_syntax:variable('SymbolicRange'),
    ExcludedVar = erl_syntax:variable('Excluded'),
    TwoFieldSpec = erl_syntax:tuple([
        erl_syntax:atom(symbolic),
        RangeVar
    ]),
    ThreeFieldSpec = erl_syntax:tuple([
        erl_syntax:atom(symbolic),
        RangeVar,
        ExcludedVar
    ]),
    [
        erl_syntax:clause([TwoFieldSpec], [], [BodyFun(TwoFieldSpec)]),
        erl_syntax:clause([ThreeFieldSpec], [], [BodyFun(ThreeFieldSpec)])
    ].

classify_complementary_event_guards(Branches, StateUpdateArgs) ->
    Classified = lists:zipwith(
        fun(Branch = {_, _, Guard, _, _, _}, [EventPattern]) ->
            case classify_event_guard(
                Guard,
                erl_syntax:variable_name(EventPattern)
            ) of
                {ok, Operator, ComparedValue} ->
                    {Operator, ComparedValue, Branch, EventPattern};
                unsupported ->
                    unsupported
            end
        end,
        Branches,
        StateUpdateArgs
    ),
    Equal = lists:keyfind('=:=', 1, Classified),
    NotEqual = lists:keyfind('=/=', 1, Classified),
    case {Equal, NotEqual, length(Classified)} of
        {
            {'=:=', EqualValue, EqualBranch, EqualEventPattern},
            {'=/=', NotEqualValue, NotEqualBranch, NotEqualEventPattern},
            2
        } ->
            EqualArgs = element(6, EqualBranch),
            NotEqualArgs = element(6, NotEqualBranch),
            case guard_operands_equal(EqualValue, NotEqualValue)
                andalso not consequence_depends_on_event(
                    EqualEventPattern,
                    EqualArgs
                )
                andalso not consequence_depends_on_event(
                    NotEqualEventPattern,
                    NotEqualArgs
                )
            of
                true ->
                    {ok, EqualBranch, NotEqualBranch, EqualValue};
                false ->
                    unsupported
            end;
        _ ->
            unsupported
    end.

classify_event_guard(
    [[{op, _, Operator, Left, Right}]],
    EventVariable
) when Operator =:= '=:=';
       Operator =:= '=/=' ->
    case {
        is_guard_event_variable(Left, EventVariable),
        is_guard_event_variable(Right, EventVariable)
    } of
        {true, false} ->
            {ok, Operator, Right};
        {false, true} ->
            {ok, Operator, Left};
        _ ->
            unsupported
    end;
classify_event_guard(_, _) ->
    unsupported.

is_guard_event_variable({var, _, Variable}, Variable) ->
    true;
is_guard_event_variable(_, _) ->
    false.

guard_operands_equal({var, _, Left}, {var, _, Right}) ->
    Left =:= Right;
guard_operands_equal({integer, _, Left}, {integer, _, Right}) ->
    Left =:= Right;
guard_operands_equal({atom, _, Left}, {atom, _, Right}) ->
    Left =:= Right;
guard_operands_equal(_, _) ->
    false.

consequence_depends_on_event(EventPattern, NextFunctionArgs) ->
    EventVariable = erl_syntax:variable_name(EventPattern),
    lists:any(
        fun(Arg) ->
            erl_syntax:type(Arg) =:= variable
                andalso erl_syntax:variable_name(Arg) =:= EventVariable
        end,
        NextFunctionArgs
    ).

generate_symbolic_membership_case(
    EventSpec,
    ComparedValue,
    EqualBranch,
    NotEqualBranch
) ->
    erl_syntax:case_expr(
        remote_call(
            agm_engine,
            event_spec_membership,
            [EventSpec, ComparedValue]
        ),
        [
            erl_syntax:clause(
                [erl_syntax:atom(true)],
                [],
                [successful_monitor_reductions([
                    EqualBranch,
                    NotEqualBranch
                ])]
            ),
            erl_syntax:clause(
                [erl_syntax:atom(false)],
                [],
                [successful_monitor_reductions([NotEqualBranch])]
            ),
            erl_syntax:clause(
                [erl_syntax:atom(unknown)],
                [],
                [unknown_monitor_reduction(
                    unsupported_symbolic_guard
                )]
            )
        ]
    ).

generate_unknown_monitor_reduction_fun(Reason) ->
    erl_syntax:fun_expr([
        erl_syntax:clause(
            [erl_syntax:underscore()],
            [],
            [unknown_monitor_reduction(Reason)]
        )
    ]).

unknown_monitor_reduction(Reason) ->
    erl_syntax:tuple([
        erl_syntax:atom(unknown),
        erl_syntax:atom(Reason)
    ]).

generate_sys_info_function(Opts) ->
    SourceFile = opts:monitor_table_opt(Opts),
    SysInfo = sys_info_parser:parse_file(SourceFile),
    [erl_syntax:function(
        erl_syntax:atom(init_transitions),
        [
            erl_syntax:clause(
                [],
                [],
                [erl_syntax:list([
                    sys_info_to_transition(Transition)
                    || Transition <- SysInfo
                ])]
            )
        ]
    )].

sys_info_to_transition({Source, EventTuple, Destination}) ->
    erl_syntax:tuple([
        erl_syntax:atom(Source),
        erl_syntax:atom(Destination),
        generate_sys_info_event_spec(EventTuple),
        generate_sys_info_event_condition(EventTuple)
    ]).

generate_sys_info_event_spec({is_integer, Event}) ->
    literal_event_spec(erl_syntax:integer(Event));
generate_sys_info_event_spec({atom, Event}) ->
    literal_event_spec(erl_syntax:atom(Event));
generate_sys_info_event_spec({{'fun', null}, []}) ->
    literal_event_spec(erl_syntax:atom(null));
generate_sys_info_event_spec({{'fun', is_natural_integer}, []}) ->
    symbolic_event_spec(natural_integer);
generate_sys_info_event_spec({{'fun', is_any_integer}, []}) ->
    symbolic_event_spec(any_integer);
generate_sys_info_event_spec({{'fun', is_real_number}, []}) ->
    symbolic_event_spec(real_number);
generate_sys_info_event_spec(
    {{'fun', is_any_integer}, [setminus | {is_integer, ExcludedEvent}]}
) ->
    erl_syntax:tuple([
        erl_syntax:atom(symbolic),
        erl_syntax:atom(any_integer_except),
        erl_syntax:integer(ExcludedEvent)
    ]).

literal_event_spec(Event) ->
    erl_syntax:tuple([erl_syntax:atom(literal), Event]).

symbolic_event_spec(Range) ->
    erl_syntax:tuple([
        erl_syntax:atom(symbolic),
        erl_syntax:atom(Range)
    ]).

generate_sys_info_event_condition({is_integer, EventPayload}) ->
    event_condition(
        erl_syntax:infix_expr(
            erl_syntax:variable('Event'),
            erl_syntax:operator('=:='),
            erl_syntax:integer(EventPayload)
        )
    );
generate_sys_info_event_condition({atom, EventPayload}) ->
    event_condition(
        erl_syntax:infix_expr(
            erl_syntax:variable('Event'),
            erl_syntax:operator('=:='),
            erl_syntax:atom(EventPayload)
        )
    );
generate_sys_info_event_condition({{'fun', EventPayload}, AdditionalGuards}) ->
    event_condition(generate_sys_info_guard(EventPayload, AdditionalGuards)).

event_condition(Body) ->
    erl_syntax:fun_expr([
        erl_syntax:clause(
            [erl_syntax:variable('Event')],
            [],
            [Body]
        )
    ]).

generate_sys_info_guard(EventPayload, AdditionalGuards) ->
    EventVar = erl_syntax:variable('Event'),
    IsInteger = erl_syntax:application(
        erl_syntax:atom(is_integer),
        [EventVar]
    ),
    MainGuard =
        case EventPayload of
            null ->
                erl_syntax:infix_expr(
                    EventVar,
                    erl_syntax:operator('=:='),
                    erl_syntax:atom(null)
                );
            is_natural_integer ->
                erl_syntax:infix_expr(
                    IsInteger,
                    erl_syntax:operator('andalso'),
                    erl_syntax:infix_expr(
                        EventVar,
                        erl_syntax:operator('>'),
                        erl_syntax:integer(0)
                    )
                );
            is_any_integer ->
                IsInteger;
            is_real_number ->
                erl_syntax:application(
                    erl_syntax:atom(is_number),
                    [EventVar]
                )
        end,
    case AdditionalGuards of
        [setminus | {GuardPayloadType, GuardPayload}] ->
            GuardValue =
                case GuardPayloadType of
                    is_integer -> erl_syntax:integer(GuardPayload);
                    is_atom -> erl_syntax:atom(GuardPayload)
                end,
            erl_syntax:infix_expr(
                MainGuard,
                erl_syntax:operator('andalso'),
                erl_syntax:infix_expr(
                    EventVar,
                    erl_syntax:operator('=/='),
                    GuardValue
                )
            );
        [] ->
            MainGuard
    end.

generate_all_states() ->
    [zero_arity_engine_adapter(
        get_system_states,
        get_system_states,
        [erl_syntax:application(
            erl_syntax:atom(init_transitions),
            []
        )]
    )].

generate_state_management() ->
    EventVar = erl_syntax:variable('Event'),
    PreviousVar = erl_syntax:variable('PreviousState'),
    CurrentVar = erl_syntax:variable('CurrentState'),
    [erl_syntax:function(
        erl_syntax:atom(update_current_state),
        [
            erl_syntax:clause(
                [EventVar],
                [],
                [
                    match(
                        PreviousVar,
                        ets_lookup(current_state)
                    ),
                    match(
                        CurrentVar,
                        erl_syntax:application(
                            erl_syntax:atom(reachable_state),
                            [PreviousVar, EventVar]
                        )
                    ),
                    ets_insert(previous_state, PreviousVar),
                    ets_insert(current_state, CurrentVar)
                ]
            )
        ]
    )].

agm_generation() ->
    lists:flatten([
        generate_transition_adapters(),
        generate_handle_missing_event_function()
    ]).

generate_transition_adapters() ->
    StateVar = erl_syntax:variable('State'),
    EventVar = erl_syntax:variable('Event'),
    StatesVar = erl_syntax:variable('States'),
    SourceVar = erl_syntax:variable('SourceState'),
    DestinationVar = erl_syntax:variable('DestinationState'),
    RecoveryVar = erl_syntax:variable('Recovery'),
    ReductionFunVar = erl_syntax:variable('ReductionFun'),
    Transitions = erl_syntax:application(
        erl_syntax:atom(init_transitions),
        []
    ),
    [
        engine_adapter(
            reachable_state,
            [StateVar, EventVar],
            reachable_state,
            [Transitions, StateVar, EventVar]
        ),
        engine_adapter(
            reachable_states_from_state,
            [StateVar],
            reachable_states_from_state,
            [Transitions, StateVar]
        ),
        engine_adapter(
            preceeding_states_from_state,
            [StateVar],
            preceding_states_from_state,
            [Transitions, StateVar]
        ),
        engine_adapter(
            preceeding_states_from_event,
            [EventVar],
            preceding_states_from_event,
            [Transitions, EventVar]
        ),
        engine_adapter(
            resolve_singleton_state,
            [StatesVar],
            resolve_singleton_state,
            [StatesVar]
        ),
        engine_adapter(
            candidate_event_specs,
            [SourceVar, DestinationVar],
            candidate_event_specs,
            [Transitions, SourceVar, DestinationVar]
        ),
        engine_adapter(
            resolve_monitoring_consequence,
            [RecoveryVar, ReductionFunVar],
            resolve_monitoring_consequence,
            [RecoveryVar, ReductionFunVar]
        ),
        engine_adapter(
            event_spec_membership,
            [EventVar, StateVar],
            event_spec_membership,
            [EventVar, StateVar]
        )
    ].

generate_handle_missing_event_function() ->
    FromVar = erl_syntax:variable('From'),
    SourceVar = erl_syntax:variable('SourceState'),
    PayloadVar = erl_syntax:variable('Payload'),
    RecoveryVar = erl_syntax:variable('Recovery'),
    InferredVar = erl_syntax:variable('InferredState'),
    ReasonVar = erl_syntax:variable('Reason'),
    RecoverCall = remote_call(
        agm_engine,
        recover_missing_event,
        [
            erl_syntax:application(
                erl_syntax:atom(init_transitions),
                []
            ),
            SourceVar,
            PayloadVar
        ]
    ),
    RecoveryCase = erl_syntax:case_expr(
        RecoverCall,
        [
            erl_syntax:clause(
                [erl_syntax:tuple([
                    erl_syntax:atom(ok),
                    RecoveryVar
                ])],
                [],
                [
                    match(
                        InferredVar,
                        remote_call(
                            maps,
                            get,
                            [
                                erl_syntax:atom(inferred_state),
                                RecoveryVar
                            ]
                        )
                    ),
                    ets_insert(previous_state, SourceVar),
                    ets_insert(current_state, InferredVar),
                    erl_syntax:tuple([
                        erl_syntax:atom(ok),
                        RecoveryVar
                    ])
                ]
            ),
            erl_syntax:clause(
                [erl_syntax:tuple([
                    erl_syntax:atom(withhold),
                    ReasonVar
                ])],
                [],
                [withhold_tuple(ReasonVar)]
            )
        ]
    ),
    TracePattern = erl_syntax:tuple([
        erl_syntax:tuple([
            erl_syntax:atom(trace),
            erl_syntax:underscore(),
            erl_syntax:atom(send),
            PayloadVar,
            erl_syntax:underscore()
        ]),
        FromVar
    ]),
    ReceiveExpression = erl_syntax:receive_expr([
        erl_syntax:clause(
            [TracePattern],
            [],
            [RecoveryCase]
        )
    ]),
    [erl_syntax:function(
        erl_syntax:atom(handle_missing_event),
        [
            erl_syntax:clause(
                [FromVar],
                [],
                [
                    match(SourceVar, ets_lookup(current_state)),
                    ReceiveExpression
                ]
            )
        ]
    )].

zero_arity_engine_adapter(Name, EngineFunction, EngineArgs) ->
    engine_adapter(Name, [], EngineFunction, EngineArgs).

engine_adapter(Name, Args, EngineFunction, EngineArgs) ->
    erl_syntax:function(
        erl_syntax:atom(Name),
        [
            erl_syntax:clause(
                Args,
                [],
                [remote_call(agm_engine, EngineFunction, EngineArgs)]
            )
        ]
    ).

remote_call(Module, Function, Args) ->
    erl_syntax:application(
        erl_syntax:module_qualifier(
            erl_syntax:atom(Module),
            erl_syntax:atom(Function)
        ),
        Args
    ).

ets_lookup(Key) ->
    remote_call(
        ets,
        lookup_element,
        [
            erl_syntax:atom(sus_state),
            erl_syntax:atom(Key),
            erl_syntax:integer(2)
        ]
    ).

ets_insert(Key, Value) ->
    remote_call(
        ets,
        insert,
        [
            erl_syntax:atom(sus_state),
            erl_syntax:tuple([
                erl_syntax:atom(Key),
                Value
            ])
        ]
    ).

match(Pattern, Expression) ->
    erl_syntax:infix_expr(
        Pattern,
        erl_syntax:operator('='),
        Expression
    ).

withhold_tuple(Reason) ->
    erl_syntax:tuple([
        erl_syntax:atom(withhold),
        Reason
    ]).
