%%% ----------------------------------------------------------------------------
%%% Grammar definitions.
%%% ----------------------------------------------------------------------------

Nonterminals

trace delay event

mfa

list tail tuple items item.



Terminals

% Punctuation symbols.
'(' ')' '[' ']' '{' '}' ',' '.'

% Data types.
pid ref atom int float string

% Keywords.
delayed by

% Event declaration symbols.
fork init exit send recv

% Corrupt event symbol.
corrupt_event

% Corrupt payload symbol.
corrupt_payload.

Rootsymbol delay.

delay -> event                                  : '$1'.
delay -> event delayed by int                   : {delay, '$4', '$1'}.

event -> fork '(' pid ',' pid ',' mfa ')'       : {fork, '$3', '$5', '$7'}.
event -> init '(' pid ',' pid ',' mfa ')'       : {init, '$3', '$5', '$7'}.
event -> exit '(' pid ',' item ')'              : {exit, '$3', '$5'}.
event -> send '(' pid ',' pid ',' item ')'      : {send, '$3', '$5', '$7'}.
event -> recv '(' pid ',' item ')'              : {recv, '$3', '$5'}.

% Corrupt events - > returns the corrupt event and the pid of the event that was corrupted.
% event -> corrupt_payload '(' pid ',' pid ',' mfa ')' : {corrupt_payload, '$3', '$5', '$7'}.
event -> corrupt_payload '(' pid ',' item ')'         : {corrupt_payload, '$3', '$5'}.
event -> corrupt_payload '(' pid ',' pid ',' item ')' : {corrupt_payload, '$3', '$5', '$7'}.

mfa -> '{' atom ',' atom ',' list '}'           : {mfa, '$2', '$4', '$6'}.


%%list -> '[' items ']'
%%items -> '$empty' : nil.
%%items -> item ',' items

%%list -> '[' ']'                                 : {list, []}.
%%list -> '[' items ']'                           : {list, '$2'}.

list -> '[' ']'                                   : nil.
list -> '[' tail ']'                              : '$2'.
tail -> item                                      : {cons, '$1', nil}.
tail -> item ',' tail                             : {cons, '$1', '$3'}.

tuple -> '{' '}'                                : {tuple, []}.
tuple -> '{' items '}'                          : {tuple, '$2'}.
items -> item                                   : ['$1'].
items -> item ',' items                         : ['$1' | '$3'].

item -> pid                                     : '$1'.
item -> ref                                     : '$1'.
item -> atom                                    : '$1'.
item -> int                                     : '$1'.
item -> float                                   : '$1'.
item -> string                                  : '$1'.
item -> list                                    : '$1'.
item -> tuple                                   : '$1'.

Erlang code.

%% keep track of annotation info in tokens
-define(anno(Tup), element(2, Tup)).