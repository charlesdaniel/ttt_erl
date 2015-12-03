-module ttt.

-author "Charles Daniel <charles.sam.daniel@gmail.com>".
-vsn "0.2".

-export([start_game/0]).

-define(USER, "X").
-define(COMPUTER, "O").

% Print the board, the board looks like [{1,2,3},{4,5,6},{7,8,9}] (any of those can also be the values of ?USER or ?COMPUTER)
print_board({A,B,C}) ->
    % Print a single row
    % Note if it's an integer we'll need to convert it to the equivalent string before printing
    io:format("~s | ~s | ~s ~n", [ case is_integer(X) of true -> integer_to_list(X); _->X end || X <- [A, B, C] ]);

print_board([]) ->
    ok;

print_board([H|T]) ->
    print_board(H),
    if
        length(T) > 0 ->
            io:format("---------~n"),
            print_board(T);
        true ->
            print_board(T)
    end.


% Return a new Board with the piece in the Move position if valid otherwise return an invalid_move
place_piece(Board, Piece, Move) ->
    case lists:any(fun(X) -> case X of {Move, _, _} -> true; {_, Move, _}->true; {_, _, Move}-> true; _->false end end, Board) of
        true ->
            {ok, [ case { X, Y, Z} of {Move, _, _} -> {Piece, Y, Z}; {_, Move, _} -> {X, Piece, Z}; {_, _, Move} -> {X, Y, Piece}; _ -> {X, Y, Z} end || {X, Y, Z} <- Board]};
        _ ->
            {invalid_move, Board}
    end.


% Prompt the user for a move after printing the board out
get_move(Board, ?USER) ->
    print_board(Board),
    {ok, [Move]} = io:fread("Your Move -> ", "~d"),
    Move;

% Get a move for the computer (We just choose a random location for now)
% TODO: Make this smarter
get_move(_Board, ?COMPUTER) ->
    random:uniform(9).


% Call a get_move for the player until they give us a valid empty spot
process_move(Board, Piece) ->
    Move = get_move(Board, Piece),
    case place_piece(Board, Piece, Move) of
        {ok, NewBoard} ->
            NewBoard;
        {invalid_move, _} ->
            io:format("Illegal Move ~n"),
            process_move(Board, Piece)
    end.


% Figure out if there was a winner and return who it was
calculate_winner(Board) ->
    case Board of
        [{A, A, A}, {_, _, _}, {_, _, _}] -> A;
        [{_, _, _}, {A, A, A}, {_, _, _}] -> A;
        [{_, _, _}, {_, _, _}, {A, A, A}] -> A;
        [{A, _, _}, {A, _, _}, {A, _, _}] -> A;
        [{_, A, _}, {_, A, _}, {_, A, _}] -> A;
        [{_, _, A}, {_, _, A}, {_, _, A}] -> A;
        [{A, _, _}, {_, A, _}, {_, _, A}] -> A;
        [{_, _, A}, {_, A, _}, {A, _, _}] -> A;
        _ -> not_done
    end.


% Quick function to flip the turn to the other player
whos_next(?USER) -> ?COMPUTER;
whos_next(?COMPUTER) -> ?USER.


% Main driver, runs the game and checks for maximum number of moves (to call it a Tie)
run_game(Board, _, Moves) when Moves > 9 ->
    print_board(Board),
    io:format("It's a Tie! ~n");

run_game(Board, WhosTurn, Moves) ->
    NewBoard = process_move(Board, WhosTurn),
    case calculate_winner(NewBoard) of
        not_done ->
            run_game(NewBoard, whos_next(WhosTurn), Moves + 1);
        A ->
            print_board(NewBoard),
            io:format("Winner is ~s! ~n", [A])
    end.

run_game(Board, WhosTurn) ->
    run_game(Board, WhosTurn, 1).


% Entry point into the game
start_game() ->
    run_game([{1, 2, 3}, {4, 5, 6}, {7, 8, 9}], ?USER).
