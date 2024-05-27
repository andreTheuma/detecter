spawn | {}; {spawned}
spawned | {spawn}; {recv}
send | {spawned, recv}; {recv, send}
recv | {send}; {send, spawned}
exit | {recv}; {}