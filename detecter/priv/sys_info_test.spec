fork | {}; {init}
init | {fork}; {recv}
send | {init, recv}; {recv, send}
recv | {send}; {send, init}
exit | {recv}; {}