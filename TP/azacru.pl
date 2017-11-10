/* encoding(utf8). */
/* Use font DejaVu Sans Mono, size 10 */

:-use_module(library(lists)).

/* =================== */
/* ===== Options ===== */
/* =================== */

:-dynamic
    board/1,
    turn/1.

start_game(Mode):-
    clear_game,
    assertz(board([['420','000','510','000','000','000','520','000','000'],
                    ['000','000','000','000','000','000','000','000','000'],
                    ['000','000','000','000','000','000','000','000','000'],
                    ['000','000','000','000','000','000','000','000','000'],
                    ['310','000','000','000','000','000','000','000','720'],
                    ['000','000','000','000','000','000','000','000','000'],
                    ['000','000','000','000','000','000','000','000','000'],
                    ['000','000','000','000','000','000','000','000','000'],
                    ['000','000','110','000','000','000','120','000','810']])),
    assertz(turn(1)),
    call_cleanup(game_cycle(Mode), clear_game).

/* to do */
continue_game.

clear_game:-
    retractall(board(_)),
    retractall(turn(_)).

/* ================= */
/* ===== Logic ===== */
/* ================= */

/* Game cycles for different modes */
game_cycle(pvp):-
    repeat,
    turn(Turn),
    display_board(Turn),
    input(Turn, _P, _FP).
/*  update_board(). */
    /* to do */

/* to do */
game_cycle(pvc).

/* to do */
game_cycle(cvc).

game_cycle(_):-
    write('Error: No such game mode.'), fail.

/* =================== */
/* ===== Display ===== */
/* =================== */

display_board(Turn):-
    board(Board),
    print_board(Board),
    write('It\'s your turn, player '),
    write(Turn), nl.

/* Wrapper for repeating several put_code calls with same argument (may change, use compounds) */
write_code(C):-
    put_code(C).
write_code(1, C):-
    put_code(C).
write_code(N, C):-
    put_code(C),
    Next is N - 1,
    write_code(Next, C).

/* Display board */
print_board(M):-
    write_code(9487), write_code(7, 9473), write_code(9519), write_code(7, 9473), write_code(9519), write_code(7, 9473),
    write_code(9523), write_code(7, 9473), write_code(9519), write_code(7, 9473), write_code(9519), write_code(7, 9473),
    write_code(9523), write_code(7, 9473), write_code(9519), write_code(7, 9473), write_code(9519), write_code(7, 9473),
    write_code(9491), nl,
    print_matrix(M, 0, 1).

print_matrix([H1|[H2|[H3|T]]], 0, C1):-
    C2 is C1 + 1,
    C3 is C2 + 1,
    CNext is C3 + 1,
    print_list(H1, C1),
    write_code(9504), write_code(7, 9472), write_code(9532), write_code(7, 9472), write_code(9532), write_code(7, 9472),
    write_code(9538), write_code(7, 9472), write_code(9532), write_code(7, 9472), write_code(9532), write_code(7, 9472),
    write_code(9538), write_code(7, 9472), write_code(9532), write_code(7, 9472), write_code(9532), write_code(7, 9472),
    write_code(9512), nl,
    print_list(H2, C2),
    write_code(9504), write_code(7, 9472), write_code(9532), write_code(7, 9472), write_code(9532), write_code(7, 9472),
    write_code(9538), write_code(7, 9472), write_code(9532), write_code(7, 9472), write_code(9532), write_code(7, 9472),
    write_code(9538), write_code(7, 9472), write_code(9532), write_code(7, 9472), write_code(9532), write_code(7, 9472),
    write_code(9512), nl,
    print_list(H3, C3),
    print_matrix(T, 1, CNext).
print_matrix([H|T], 1, CNext):-
    write_code(9507), write_code(7, 9473), write_code(9535), write_code(7, 9473), write_code(9535), write_code(7, 9473),
    write_code(9547), write_code(7, 9473), write_code(9535), write_code(7, 9473), write_code(9535), write_code(7, 9473),
    write_code(9547), write_code(7, 9473), write_code(9535), write_code(7, 9473), write_code(9535), write_code(7, 9473),
    write_code(9515), nl,
    print_matrix([H|T], 0, CNext).
print_matrix([], _Section, _CNext):-
    write_code(9495), write_code(7, 9473), write_code(9527), write_code(7, 9473), write_code(9527), write_code(7, 9473),
    write_code(9531), write_code(7, 9473), write_code(9527), write_code(7, 9473), write_code(9527), write_code(7, 9473),
    write_code(9531), write_code(7, 9473), write_code(9527), write_code(7, 9473), write_code(9527), write_code(7, 9473),
    write_code(9499), nl,
    write('    A       B       C       D       E       F       G       H       I'), nl.

print_list([H1|[H2|[H3|[H4|[H5|[H6|[H7|[H8|[H9|[]]]]]]]]]], C):-
    write_code(9475), print_element(H1, 1), write_code(9474), print_element(H2, 1), write_code(9474), print_element(H3, 1),
    write_code(9475), print_element(H4, 1), write_code(9474), print_element(H5, 1), write_code(9474), print_element(H6, 1),
    write_code(9475), print_element(H7, 1), write_code(9474), print_element(H8, 1), write_code(9474), print_element(H9, 1),
    write_code(9475), nl,
    write_code(9475), print_element(H1, 2), write_code(9474), print_element(H2, 2), write_code(9474), print_element(H3, 2),
    write_code(9475), print_element(H4, 2), write_code(9474), print_element(H5, 2), write_code(9474), print_element(H6, 2),
    write_code(9475), print_element(H7, 2), write_code(9474), print_element(H8, 2), write_code(9474), print_element(H9, 2),
    write_code(9475), write(C), nl,
    write_code(9475), print_element(H1, 3), write_code(9474), print_element(H2, 3), write_code(9474), print_element(H3, 3),
    write_code(9475), print_element(H4, 3), write_code(9474), print_element(H5, 3), write_code(9474), print_element(H6, 3),
    write_code(9475), print_element(H7, 3), write_code(9474), print_element(H8, 3), write_code(9474), print_element(H9, 3),
    write_code(9475), nl.

print_element(E, R):-
    atom_chars(E, [OC, PC, MC]),
    number_chars(O, [OC]),
    number_chars(P, [PC]),
    number_chars(M, [MC]),
    print_element(O, P, M, R).
print_element(1, P, _M, 1):-
    write('   '), print_piece(1, P), write('   ').
print_element(2, P, _M, 1):-
    write('     '), print_piece(2, P), write(' ').
print_element(8, P, _M, 1):-
    write(' '), print_piece(8, P), write('     ').
print_element(3, P, M, 2):-
    write('   '), print_marker(M), write(' '), print_piece(3, P), write(' ').
print_element(7, P, M, 2):-
    write(' '), print_piece(7, P), write(' '), print_marker(M), write('   ').
print_element(_O, _P, M, 2):-
    write('   '), print_marker(M), write('   ').
print_element(4, P, _M, 3):-
    write('     '), print_piece(4, P), write(' ').
print_element(5, P, _M, 3):-
    write('   '), print_piece(5, P), write('   ').
print_element(6, P, _M, 3):-
    write(' '), print_piece(6, P), write('     ').
print_element(_O, _P, _M, _R):-
    write('       ').

print_piece(1, P):-
    C is 9652 - P,
    write_code(C).
print_piece(2, P):-
    C is 9666 - P,
    write_code(C).
print_piece(3, P):-
    C is 9656 - P,
    write_code(C).
print_piece(4, P):-
    C is 9666 - P,
    write_code(C).
print_piece(5, P):-
    C is 9662 - P,
    write_code(C).
print_piece(6, P):-
    C is 9656 - P,
    write_code(C).
print_piece(7, P):-
    C is 9666 - P,
    write_code(C).
print_piece(8, P):-
    C is 9656 - P,
    write_code(C).

print_marker(0):-
    write(' ').
print_marker(M):-
    C is 9897 + M,
    write_code(C).

/* ================= */
/* ===== Input ===== */
/* ================= */

input(Turn, P, FP):-
    /* maybe define predicate for each of these so that if user wants to change piece, we can specify it in movement input */
    repeat,
    write('Select piece'), nl,
    read(RawP),
    validate_position_input(RawP, P),
    evaluate_position_input(Turn, P),
    repeat,
    write('Define movement'), nl,
    read(RawM),
    validate_movement_input(RawM, M),
    evaluate_movement_input(Turn, M, FP, _Placeholder, _Placeholder).

/* maybe to do */
position_input.

/* Input validation */
validate_position_input(RawP, [Cx, Cy]):-
    atom(RawP),
    atom_chars(RawP, RefinedP),
    length(RefinedP, 2),
    [RawCx, RawCy] = RefinedP,
    member(RawCx, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']),
    nth1(Cx, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'], RawCx),
    member(RawCy, ['1', '2', '3', '4', '5', '6', '7', '8', '9']),
    number_chars(Cy, [RawCy]),
    !.
validate_position_input(_, _):-
    write('Invalid input!'), nl,
    fail.

/* maybe to do */
movement_input.

validate_movement_input(RawM, [Dir, Dis]):-
    atom(RawM),
    atom_chars(RawM, RefinedM),
    length(RefinedM, 2),
    [Dir, RawDis] = RefinedM,
    member(Dir, ['l', 'f', 'r']),
    member(RawDis, ['1', '2', '3', '4', '5', '6', '7', '8']),
    number_chars(Dis, [RawDis]),
    !.
validate_movement_input(_, _):-
    write('Invalid input!'), nl,
    fail.

/* maybe maybe to do */
rotation_input.

validate_rotation_input(RawR, R):-
    atom(RawR),
    atom_chars(RawR, R),
    length(R, 1),
    member(R, ['l', 'f', 'r']).
validate_rotation_input(_, _):-
    write('Invalid input!'), nl,
    fail.

/* Input evaluation */
evaluate_position_input(Turn, [Cx, Cy]):-
    board(Board),
    Count is 1,
    evaluate_position_input_1(Turn, Cx, Cy, Count, Board).
evaluate_position_input_1(Turn, Cx, Cy, Cy, [Row|_]):-
    Count is 1,
    evaluate_position_input_2(Turn, Cx, Count, Row).
evaluate_position_input_1(Turn, Cx, Cy, Count, [_|Rest]):-
    CountNext is Count + 1,
    evaluate_position_input_1(Turn, Cx, Cy, CountNext, Rest).
evaluate_position_input_2(Turn, Cx, Cx, [Column|_]):-
    atom_chars(Column, [_OC, PC, _MC]),
    number_chars(Turn, [PC]).
evaluate_position_input_2(Turn, Cx, Count, [_|Rest]):-
    CountNext is Count + 1,
    evaluate_position_input_2(Turn, Cx, CountNext, Rest).
evaluate_position_input_2(_, Cx, Cx, _):-
    write('Invalid play!'), nl,
    fail.

/* to do */
evaluate_movement_input(_Turn, _M, _FP, _TypeMovement, _CallRotation).
    /* return coordinates of final position to FP */
    /* also check if rotation needs to be called */
    /* maybe return type of movement, according to game rules */
