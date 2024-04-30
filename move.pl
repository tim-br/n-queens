:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(between)).
:- use_module(library(ordsets)).

:-dynamic(queen_position/2).
:-dynamic(queen_set/1).

max_outer_bound(6, 6).
min_outer_bound(1, 1).

check_in_bounds(X, Y):-
  min_outer_bound(MinBoundX, MinBoundY),
  max_outer_bound(MaxBoundX, MaxBoundY),
  X =< MaxBoundX,
  Y =< MaxBoundY,
  X >= MinBoundX,
  Y >= MinBoundY. 

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
  move_diag_left_up((NewX, NewY), Res2, Res3).

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

available_positions((X, Y), QueensAcc, IllegalMoves):-
  max_outer_bound(MaxX, MaxY),
  min_outer_bound(MinX, MinY),
  between(MinX, MaxX, X),
  between(MinY, MaxY, Y),
  \+ member((X, Y), IllegalMoves),
  \+ member((X, Y), QueensAcc).

search((X,Y), Queens):-
  all_moves((X, Y), IllegalMoves),
  max_outer_bound(_, MaxY),
  %min_outer_bound(_, MinY),
  between(1, MaxY, Row),
  search_row(Row, [(X,Y) | IllegalMoves], Res),
  Res2 = [Res | Queens],
  NewX = X + 1,
  search((NewX, Y), Res2).

all_in_list([], _).
all_in_list([H|T], List2) :-
    member(H, List2),
    all_in_list(T, List2).

% Predicate to check set equality by mutual inclusion
set_equal(Set1, Set2) :-
    all_in_list(Set1, Set2),
    all_in_list(Set2, Set1).

all_lists_unique([]).
all_lists_unique([Head|Tail]) :-
    not(member(Head, Tail)),
    all_lists_unique(Tail).

queen_position_memo(X,Y) :-
    (    queen_position(X,Y) -> true
    ;    (assertz(queen_position(X,Y)), false)
    ).

queen_set_memo(Set) :-
    (    sort(Set, Sorted1), queen_set(Sorted1) -> true
    ;    (sort(Set, Sorted1), assertz(queen_set(Sorted1)), false)
    ).

nsearch_((X, Y), IllegalMoves, Queens, Queens):-
  \+ available_positions((X, Y), Queens, IllegalMoves),
  \+ queen_set_memo(Queens),
  retractall(queen_position(_, _)).
  
nsearch_((X, Y), IllegalMoves, QueensAcc, QueensRes):-
  all_moves((X, Y), ZIllegalMoves),
  ord_union(IllegalMoves, ZIllegalMoves, NextIllegalMoves),
  available_positions(NextMove, QueensAcc, NextIllegalMoves),
  NextQueens = [NextMove | QueensAcc],
  NNextIllegal = [NextMove | NextIllegalMoves],
  nsearch_(NextMove, NNextIllegal, NextQueens, QueensRes).

nsearch(Queens):-
  max_outer_bound(MaxX, MaxY),
  min_outer_bound(MinX, MinY),
  between(MinX, MaxX, X),
  between(MinY, MaxY, Y),
  nsearch_((X, Y), [(X, Y)], [(X, Y)], Queens),
  length(Queens, Len),
  Len >= MaxX.

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