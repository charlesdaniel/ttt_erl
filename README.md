## Tic Tac Toe in Erlang
#### by Charles Daniel

This is my first attempt at an actual program while trying to learn Erlang.
It's a basic text based tic tac toe game in Erlang.


To Run it get into an Erlang shell and compile `ttt` then run `ttt:start_game()`

```
$ erl
Erlang/OTP 18 [erts-7.1] [source-2882b0c] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> c(ttt).
{ok,ttt}
2> ttt:start_game().
1 | 2 | 3
---------
4 | 5 | 6
---------
7 | 8 | 9
Your Move ->
```

You are "x", simply enter a number from 1..9 and hit Enter. The computer will then take a turn and we repeat until there is a winner or there are no more spots left.

###TODO
 - Make the computer actually play somewhat intelligently rather than picking random spots.
 - Rework the code to be more Erlangy
 - Better GUI