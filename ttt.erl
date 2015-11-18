-module ttt.

-author "Charles Daniel <charles.sam.daniel@gmail.com>".
-vsn "0.1".

-export([start_game/0]).

%% Print the board, the board looks like [{1,2,3},{4,5,6},{7,8,9}] (any of those can also be the atom x or o)
print_board({A,B,C}) ->
    io:format("~p | ~p | ~p ~n", [A, B, C]);

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


%% Return a new Board with the piece in the Move position if valid otherwise return an invalid_move
place_piece(Board, Piece, Move) ->
    case lists:any(fun(X) -> case X of {Move, _, _} -> true; {_, Move, _}->true; {_, _, Move}-> true; _->false end end, Board) of
        true ->
            {ok, [ case { X, Y, Z} of {Move, _, _} -> {Piece, Y, Z}; {_, Move, _} -> {X, Piece, Z}; {_, _, Move} -> {X, Y, Piece}; _ -> {X, Y, Z} end || {X, Y, Z} <- Board]};
        _ ->
            {invalid_move, Board}
    end.


%% Prompt the user for a move after printing the board out
get_user_move(Board, Piece) ->
    print_board(Board),
    {ok, [Move]} = io:fread("Your Move -> ", "~d"),
    case place_piece(Board, Piece, Move) of
        {ok, NewBoard} ->
            NewBoard;
        {invalid_move, _} ->
            io:format("Illegal Move\n"),
            get_user_move(Board, Piece)
    end.


%% Get a move from the computer (it's just random for now)
%% TODO: Make computer smarter
get_computer_move(Board, Piece) ->
    Move = rand:uniform(9),
    case place_piece(Board, Piece, Move) of
        {ok, NewBoard} ->
            NewBoard;
        {invalid_move, _} ->
            get_computer_move(Board, Piece)
    end.


%% Figure out if there was a winner and return who it was
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


%% Recursively run the game (take a turn each time)
%% If we're at more than 9 moves it's a tie
run_game(Board, WhosTurn, Moves) ->
    if
        Moves > 9 ->
            print_board(Board),
            io:format("It's a Tie\n");
        true ->
            {NewBoard, WhosTurnNext} = case WhosTurn of
                user1 -> {get_user_move(Board, x), computer};
                computer -> {get_computer_move(Board, o), user1};
                _ -> {invalid_move}
            end,
            case calculate_winner(NewBoard) of
                x ->
                    print_board(NewBoard),
                    io:format("Winner is X (you)\n");
                o ->
                    print_board(NewBoard),
                    io:format("Winner is O (computer)\n");
                _ -> run_game(NewBoard, WhosTurnNext, Moves + 1)
            end
    end.

run_game(Board, WhosTurn) ->
    run_game(Board, WhosTurn, 1).


%% Entry point into the game
start_game() ->
    run_game([{1, 2, 3}, {4, 5, 6}, {7, 8, 9}], user1).
