% define empty event macro
-define(EMPTY_EVENT, "E").
-define(CORRUPTED_EVENT, "X").
-define(CORRUPTED_PAYLOAD, corrupt_payload).

%% Guard macro that checks if the event is empty ("E").
% -spec is_empty(Event) -> boolean()
-define(is_empty(Event), Event =:= ?EMPTY_EVENT).

%% Guard macro that checks if the event is missing ("X").
% -spec is_corrupt(Event) -> boolean() 
%   when 
% Event :: event:int_event() | event:evm_event().
-define(is_corrupt(Event), element(1,Event) =:= ?CORRUPTED_PAYLOAD).

% -spec is_previous_corrupt(EventQueue) -> boolean()
%   when
%   EventQueue :: {EventN, EventN1, EventN2},
%   EventN :: event:int_event(),
%   EventN1 :: event:int_event(),
%   EventN2 :: event:int_event().
-define(is_previous_corrupt(EventQueue), ?is_corrupt(element(1, EventQueue))).

% -spec is_mfa(Mfa) -> boolean()
%   when
%   Mfa :: mfa().
-define(is_mfa(Module, Function, Arity), is_atom(Module) andalso is_atom(Function) andalso lists:all(fun is_integer/1, Arity)).
