-module(fault_gen).
-author("André Theuma").

-include("event.hrl").

-export([generate_faults_from_trace/2]).

-spec generate_faults_from_trace(Trace, NumberOfFaults, CorruptedTrace) -> CorruptedTrace 
    when
    Trace :: list(event:evm_event()) | list(event:int_event()),
    NumberOfFaults :: integer(),
    CorruptedTrace :: list(event:evm_event()) | list(event:int_event()) | _.
generate_faults_from_trace([], NumberOfFaults, CorruptedTrace) ->
    io:format("Number of faults NOT generated: ~p~n", [NumberOfFaults]),
    CorruptedTrace;
generate_faults_from_trace(TraceTail, 0, CorruptedTrace) -> 
    % Append the remaining trace to the corrupted trace
    CorruptedTrace ++ TraceTail;
generate_faults_from_trace(Trace, NumberOfFaults, CorruptedTrace) ->
    % Generate a random whole number between 10 and 100
    RandNum = rand:uniform(100),
    [TraceHead|TraceTail] = Trace,
    
    case RandNum rem 2 of
        0 ->
            % if the previous event is a corrupted event ignore this 
            % event and do not generate a fault as we cannot resolve two corrupted events in a row 
            case CorruptedTrace of 
                [?CORRUPTED_EVENT|_] ->
                    generate_faults_from_trace(TraceTail, NumberOfFaults, [TraceHead|CorruptedTrace]);
                _ ->
                    % generate a fault
                generate_faults_from_trace(TraceTail, NumberOfFaults - 1, [?CORRUPTED_EVENT|CorruptedTrace])
            end;
        1 ->
            % do not generate a fault
            generate_faults_from_trace(TraceTail, NumberOfFaults, [TraceHead|CorruptedTrace])
    end.

generate_faults_from_trace(Trace, NumberOfFaults)-> 
    generate_faults_from_trace(Trace, NumberOfFaults, []).
    



