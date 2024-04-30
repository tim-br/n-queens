:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(between)).
:- use_module(library(ordsets)).
max_outer_bound(5, 5).
min_outer_bound(1, 1).

/*
  write("new x"),
  write("\n"),
  write(NewX),
  write("\n"),
  write("new y"),
  write(NewY),
  write("\n"),
*/

check_in_bounds(X, Y):-
  min_outer_bound(MinBoundX, MinBoundY),
  max_outer_bound(MaxBoundX, MaxBoundY),
  X =< MaxBoundX,
  Y =< MaxBoundY,
  X >= MinBoundX,
  Y >= MinBoundY. 

move((X1, Y1), Res, Res) :-
    \+ check_in_bounds(X1, Y1), 
    !.

move((X1, Y1), Res, Res3):-
  NewX is X1 + 1,
  NewY is Y1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_right((NewX, NewY), Res2, Res3).

move((X1, Y1), Res, Res3):-
  NewX is X1,
  NewY is Y1 + 1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_right((NewX, NewY), Res2, Res3).

move((X1, Y1), Res, Res3):-
  NewX is X1 - 1,
  NewY is Y1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_right((NewX, NewY), Res2, Res3).

move((X1, Y1), Res, Res3):-
  NewX is X1,
  NewY is Y1 - 1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_right((NewX, NewY), Res2, Res3).

move((X1, Y1), Res, Res3):-
  NewX is X1 + 1,
  NewY is Y1 + 1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_right((NewX, NewY), Res2, Res3).

move((X1, Y1), Res, Res3):-
  NewX is X1 + 1,
  NewY is Y1 - 1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_right((NewX, NewY), Res2, Res3).

move((X1, Y1), Res, Res3):-
  NewX is X1 - 1,
  NewY is Y1 + 1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_right((NewX, NewY), Res2, Res3).

move((X1, Y1), Res, Res3):-
  NewX is X1 - 1,
  NewY is Y1 - 1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_right((NewX, NewY), Res2, Res3).

move((X1, Y1), Res, Res):-
  NewX is X1 + 1,
  NewY is Y1,
  \+ check_in_bounds(NewX, NewY).

move_right((X1, Y1), Res, Res) :-
    \+ check_in_bounds(X1, Y1), 
    !.

move_right((X1, Y1), Res, Res3):-
  %%check_in_bounds(X1, Y1),
  NewX is X1 + 1,
  NewY is Y1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_right((NewX, NewY), Res2, Res3).

move_right((X1, Y1), Res, Res):-
  %%check_in_bounds(X1, Y1),
  NewX is X1 + 1,
  NewY is Y1,
  \+ check_in_bounds(NewX, NewY).

move_left((X1, Y1), Res, Res) :-
    \+ check_in_bounds(X1, Y1), 
    !.

move_left((X1, Y1), Res, Res3):-
  %check_in_bounds(X1, Y1),
  NewX is X1 - 1,
  NewY is Y1,
  check_in_bounds(NewX, NewY),
  Res2 = [(NewX, NewY) | Res],
  move_left((NewX, NewY), Res2, Res3).

move_left((X1, Y1), Res, Res):-
  %check_in_bounds(X1, Y1),
  NewX is X1 - 1,
  NewY is Y1,
  \+ check_in_bounds(NewX, NewY).

move_up((X1, Y1), Res, Res) :-
    \+ check_in_bounds(X1, Y1), 
    !.

move_up((X1, Y1), Res, Res3):-
  %check_in_bounds(X1, Y1),
  NewX is X1,
  NewY is Y1 + 1,
  check_in_bounds(NewX, NewY),
  Res2 = [(NewX, NewY) | Res],
  move_up((NewX, NewY), Res2, Res3).

move_up((X1, Y1), Res, Res):-
  %check_in_bounds(X1, Y1),
  NewX is X1,
  NewY is Y1 + 1,
  \+ check_in_bounds(NewX, NewY).

move_down((X1, Y1), Res, Res) :-
    \+ check_in_bounds(X1, Y1), 
    !.

move_down((X1, Y1), Res, Res3):-
  %check_in_bounds(X1, Y1),
  NewX is X1,
  NewY is Y1 - 1,
  check_in_bounds(NewX, NewY),
  Res2 = [(NewX, NewY) | Res],
  move_down((NewX, NewY), Res2, Res3).

move_down((X1, Y1), Res, Res):-
  %check_in_bounds(X1, Y1),
  NewX is X1,
  NewY is Y1 - 1,
  \+ check_in_bounds(NewX, NewY).

move_diag_right((X1, Y1), Res, Res) :-
    \+ check_in_bounds(X1, Y1), 
    !.

move_diag_right((X1, Y1), Res, Res3):-
  %check_in_bounds(X1, Y1),
  NewX is X1 + 1,
  NewY is Y1 + 1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_diag_right((NewX, NewY), Res2, Res3).

move_diag_right((X1, Y1), Res, Res):-
  %check_in_bounds(X1, Y1),
  NewX is X1 + 1,
  NewY is Y1 + 1,
  \+ check_in_bounds(NewX, NewY).

move_diag_right_down((X1, Y1), Res, Res) :-
    \+ check_in_bounds(X1, Y1), 
    !.

move_diag_right_down((X1, Y1), Res, Res3):-
  %check_in_bounds(X1, Y1),
  NewX is X1 + 1,
  NewY is Y1 - 1,
  check_in_bounds(NewX, NewY),
  append(Res, [(NewX, NewY)], Res2),
  move_diag_right_down((NewX, NewY), Res2, Res3).

move_diag_right_down((X1, Y1), Res, Res):-
  %check_in_bounds(X1, Y1),
  NewX is X1 + 1,
  NewY is Y1 - 1,
  \+ check_in_bounds(NewX, NewY).

move_diag_left((X1, Y1), Res, Res) :-
    \+ check_in_bounds(X1, Y1), 
    !.

move_diag_left((X1, Y1), Res, Res3):-
  % check_in_bounds(X1, Y1),
  NewX is X1 - 1,
  NewY is Y1 - 1,

  check_in_bounds(NewX, NewY),
  Res2 = [(NewX, NewY) | Res],
  move_diag_left((NewX, NewY), Res2, Res3).

move_diag_left((X1, Y1), Res, Res):-
  % check_in_bounds(X1, Y1),
  NewX is X1 - 1,
  NewY is Y1 - 1,
  \+ check_in_bounds(NewX, NewY).

move_diag_left_up((X1, Y1), Res, Res) :-
    \+ check_in_bounds(X1, Y1), 
    !.

move_diag_left_up((X1, Y1), Res, Res3):-
  % check_in_bounds(X1, Y1),
  NewX is X1 - 1,
  NewY is Y1 + 1,
  check_in_bounds(NewX, NewY),
  Res2 = [(NewX, NewY) | Res],
  move_diag_left((NewX, NewY), Res2, Res3).

move_diag_left_up((X1, Y1), Res, Res):-
  % check_in_bounds(X1, Y1),
  NewX is X1 - 1,
  NewY is Y1 + 1,
  \+ check_in_bounds(NewX, NewY).

all_moves(Coord, Moves):-
  move_right(Coord, [], Moves1),
  move_left(Coord, [], Moves2),
  move_up(Coord, [], Moves3),
  move_down(Coord, [], Moves4),
  move_diag_left(Coord, [], Moves5),
  move_diag_left_up(Coord, [], Moves6),
  move_diag_right(Coord, [], Moves7),
  move_diag_right_down(Coord, [], Moves8),
  append(Moves1, Moves2, Temp1),
  append(Temp1, Moves3, Temp2),
  append(Temp2, Moves4, Temp3),
  append(Temp3, Moves5, Temp4),
  append(Temp4, Moves6, Temp5),
  append(Temp5, Moves7, Temp6),
  append(Temp6, Moves8, Moves).

/*search_pred((X, Y), IllegalMoves, Res1):-
  write("search pred 1\n"),

  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX > MaxX, 
  NewY is Y - 1,
  search_pred((1, NewY), IllegalMoves, Res1).

search_pred((X, Y), IllegalMoves, Res1):-
  write("search pred 2\n"),

  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX =< MaxX, 
  NewY is Y,
  check_in_bounds(NewX, NewY),
  member((NewX, NewY), IllegalMoves),
  search_pred((NewX, NewY), IllegalMoves, Res1).

search_pred((X, Y), IllegalMoves, Res2):-
  write("search pred 3\n"),
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX =< MaxX, 
  NewY is Y,
  check_in_bounds(NewX, NewY),
  \+ member((NewX, NewY), IllegalMoves),
  %ReturnRes = [(NewX, NewY) | Res1],
  all_moves((NewX, NewY), NewIllegalMoves),
  Res2 = (NewX, NewY),
  append(IllegalMoves, NewIllegalMoves, ReturnIllegalMoves),
  search_pred((NewX, NewY), ReturnIllegalMoves, Res2).*/

/*search_pred((X, Y), IllegalMoves, Res1, Res2):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX =< MaxX, 
  NewY is Y,
  member((NewX, NewY), IllegalMoves),
  search_pred((NewX, NewY), IllegalMoves, Res1, Res2).*/

search_row(Y, IllegalMoves, Res):-
  max_outer_bound(MaxX, _),
  between(1, MaxX, X),
  \+ member((X, Y), IllegalMoves),
  %%all_moves(X, Y, IllegalMoves2),
  %%append(IllegalMoves, IllegalMoves2, NewIllegalMoves), 
  Res = (X, Y).

search_pred((X, Y), _, _, _):-
  max_outer_bound(MaxX, _),
  min_outer_bound(_, MinY),
  X > MaxX,
  Y < MinY,
  !.

search_pred((X, Y), IllegalMoves, Res):-
  write("search pred 1 X\n"),
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX > MaxX,
  write("X is greater move to next row\n"),
  NewY is Y - 1,
  search_pred((0, NewY), IllegalMoves, Res).

search_pred((X, Y), IllegalMoves, Res):-
  %write("search pred 1 X\n"),
  NewX is X + 1,
  NewY is Y,
  max_outer_bound(MaxX, _),
  NewX =< MaxX,
  member((NewX, NewY), IllegalMoves),
  write("CALLS AGINT!!\n"),
  write("New X 1\n"),
  write(NewX),
  write("New Y 1\n"),
  write(NewY),
  search_pred((NewX, NewY), IllegalMoves, Res).

search_pred((X, Y), IllegalMoves, Res):-
  NewX is X + 1,
  NewY is Y,
  max_outer_bound(MaxX, _),
  NewX =< MaxX,
  \+ member((NewX, NewY), IllegalMoves),
  write("New X foo\n"),
  write(NewX),
  write("New Y foo\n"),
  write(NewY),
  Res = (NewX, NewY),
  search_pred((NewX, NewY), IllegalMoves, Res).

/*search((X,Y), Res):-
  all_moves((X, Y), IllegalMoves),

  search_pred((1, 3), [(X, Y)| IllegalMoves], Res).*/

search((X,Y), Res2):-
  all_moves((X, Y), IllegalMoves),
  max_outer_bound(_, MaxY),
  %min_outer_bound(_, MinY),
  between(1, MaxY, Row),
  search_row(Row, [(X,Y) | IllegalMoves], Res),
  Res2 = [(X, Y) | [Res]].

next_move((X, Y), IllegalMoves, _):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX >= MaxX,
  NewY is Y - 1,
  min_outer_bound(_, MinY),
  NewY =< MinY,
  fail.

next_move((X, Y), IllegalMoves, Move):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX =< MaxX,
  Move = (NewX, Y).

next_move((X, Y), IllegalMoves, Move):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX > MaxX,
  NewY is Y - 1,
  min_outer_bound(_, MinY),
  NewY >= MinY,
  Move = (1, NewY).

next_legal_move((X, Y), _, _):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX >= MaxX,
  NewY is Y - 1,
  min_outer_bound(_, MinY),
  NewY =< MinY,
  fail.

next_legal_move((X, Y), IllegalMoves, Move):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX =< MaxX,
  member((NewX, Y), IllegalMoves),
  next_legal_move((NewX, Y), IllegalMoves, Move).

next_legal_move((X, Y), IllegalMoves, Move):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX =< MaxX,
  \+ member((NewX, Y), IllegalMoves),
  Move = (NewX, Y).

next_legal_move((X, Y), IllegalMoves, Move):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX > MaxX,
  NewY is Y - 1,
  min_outer_bound(_, MinY),
  NewY >= MinY,
  member((1, NewY), IllegalMoves),
  next_legal_move((1, NewY), IllegalMoves, Move).

next_legal_move((X, Y), IllegalMoves, Move):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX > MaxX,
  NewY is Y - 1,
  min_outer_bound(_, MinY),
  NewY >= MinY,
  \+ member((1, NewY), IllegalMoves),
  Move = (1, NewY).

/*next_legal_move((X, Y), IllegalMoves, NextMove):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX > MaxX,
  NewY is Y - 1,
  next_legal_move((0, NewY), IllegalMoves, NextMove).

next_legal_move((X, Y), IllegalMoves, NextMove):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX =< MaxX,
  \+ member((NewX, Y), IllegalMoves),
  NextMove = (NewX, Y).

next_legal_move((X, Y), IllegalMoves, NextMove):-
  NewX is X + 1,
  max_outer_bound(MaxX, _),
  NewX =< MaxX,
  member((NewX, Y), IllegalMoves),
  next_legal_move((NewX, Y), IllegalMoves, NextMove).*/

my_search(Move, IllegalMoves, Move) :-
    % Check that no next move is possible.
   \+ next_legal_move(Move, IllegalMoves, _).

my_search((X, Y), IllegalMoves, Move):-
  all_moves((X, Y), IllegalMovesX),
  IllegalMovesN = [(X, Y) | IllegalMovesX],
  write(IllegalMovesN),
  ord_union(IllegalMoves, IllegalMovesN, NewIllegalMoves),
  next_legal_move((X, Y), NewIllegalMoves, NextMove),
  (   Move = NextMove  % Return this move and attempt to find more
  ;   my_search(NextMove, NewIllegalMoves, Move)
  ).


final_legal_move(Move, Move) :-
    % Check that no next move is possible.
    \+ next_legal_move(Move, [], _).

final_legal_move((X, Y), FinalMove):-
  next_legal_move((X, Y), [], NextMove),
  final_legal_move(NextMove, FinalMove).

write_square_and_traverse_wrapper(X, Y, QueenCoords):-
  format("~s", ["#\n\n"]),
  write_square_and_traverse(X, Y, QueenCoords).

write_square_and_traverse(X,Y, QueenCoords):-
  \+ member((X,Y), QueenCoords),
  max_outer_bound(X, _),
  min_outer_bound(_, Y),
  format("~s", ["#\n\n"]),
  !.

write_square_and_traverse(X,Y, QueenCoords):-
  member((X,Y), QueenCoords),
  max_outer_bound(X, _),
  min_outer_bound(_, Y),
  format("~s", ["β\n\n"]),
  !.

write_square_and_traverse(X,Y,QueenCoords):-
  max_outer_bound(MaxX, _),
  X > MaxX, 
  format("~s", ["\n"]),
  NewY is Y - 1,
  write_square_and_traverse(1 ,NewY, QueenCoords).

write_square_and_traverse(X,Y, QueenCoords):-
  \+ member((X,Y), QueenCoords),
  max_outer_bound(MaxX, _),
  X =< MaxX, 
  format("~s ", ["#"]),
  NewX is X + 1,
  write_square_and_traverse(NewX,Y, QueenCoords).

write_square_and_traverse(X,Y, QueenCoords):-
  member((X,Y), QueenCoords),
  max_outer_bound(MaxX, _),
  X =< MaxX, 
  format("~s ", ["β"]),
  NewX is X + 1,
  write_square_and_traverse(NewX, Y, QueenCoords).


app([], Res, Res).

app([X | Y], Res, Res3):-
  NewRes = [X | Res],
  app(Y, NewRes, Res3).