/* :-encoding(utf8). */
/* Use font DejaVu Sans Mono, size 10 */

/* ===================== */
/* ===== Utilities ===== */
/* ===================== */

safecall(Call, FailCall):-
    catch((Call; FailCall), _, (FailCall, nl, write('Exception!'), nl)).

saferead(Read):-
    catch(read(Read), _Throw, (write('Invalid input!'), nl, !, fail)).

assertz_board([]).
assertz_board([H|T]):-
    H = [Coord, Info],
    assertz(board(Coord, Info)),
    assertz_board(T).
assertz_sector([]).
assertz_sector([H|T]):-
    H = [Sector, Player, Power],
    assertz(sector(Sector, Player, Power)),
    assertz_sector(T).

writecode(C):-
    put_code(C).
writecode(1, C):-
    put_code(C).
writecode(N, C):-
    put_code(C),
    Next is N - 1,
    writecode(Next, C).

nth(_, [], 0, _).
nth(E, [E|_], Nth, Nth).
nth(E, [_|L], Nth, N):-
    Next is N + 1,
    nth(E, L, Nth, Next).
nth(E, L, N):-
    Next is 1,
    nth(E, L, N, Next).

condition(Cond, True):-
    condition(Cond, True, true).
condition(Cond, True, False):-
    (Cond,!,
        True
    );(
        False
    ).

/* =================== */
/* ===== Options ===== */
/* =================== */

:-dynamic
    debug/1,
    player/1,
    board/2,
    sector/3.

start_game:-
    clear_game,
    assertz(player([1, 2])),
    assertz_board([[[1, 1], [4, 2, 0]], [[2, 1], [0, 0, 0]], [[3, 1], [5, 1, 0]], [[4, 1], [0, 0, 0]], [[5, 1], [0, 0, 0]], [[6, 1], [0, 0, 0]], [[7, 1], [5, 2, 0]], [[8, 1], [0, 0, 0]], [[9, 1], [0, 0, 0]],
                    [[1, 2], [0, 0, 0]], [[2, 2], [0, 0, 0]], [[3, 2], [0, 0, 0]], [[4, 2], [0, 0, 0]], [[5, 2], [0, 0, 0]], [[6, 2], [0, 0, 0]], [[7, 2], [0, 0, 0]], [[8, 2], [0, 0, 0]], [[9, 2], [0, 0, 0]],
                    [[1, 3], [0, 0, 0]], [[2, 3], [0, 0, 0]], [[3, 3], [0, 0, 0]], [[4, 3], [0, 0, 0]], [[5, 3], [0, 0, 0]], [[6, 3], [0, 0, 0]], [[7, 3], [0, 0, 0]], [[8, 3], [0, 0, 0]], [[9, 3], [0, 0, 0]],
                    [[1, 4], [0, 0, 0]], [[2, 4], [0, 0, 0]], [[3, 4], [0, 0, 0]], [[4, 4], [0, 0, 0]], [[5, 4], [0, 0, 0]], [[6, 4], [0, 0, 0]], [[7, 4], [0, 0, 0]], [[8, 4], [0, 0, 0]], [[9, 4], [0, 0, 0]],
                    [[1, 5], [3, 1, 0]], [[2, 5], [0, 0, 0]], [[3, 5], [0, 0, 0]], [[4, 5], [0, 0, 0]], [[5, 5], [0, 0, 0]], [[6, 5], [0, 0, 0]], [[7, 5], [0, 0, 0]], [[8, 5], [0, 0, 0]], [[9, 5], [7, 2, 0]],
                    [[1, 6], [0, 0, 0]], [[2, 6], [0, 0, 0]], [[3, 6], [0, 0, 0]], [[4, 6], [0, 0, 0]], [[5, 6], [0, 0, 0]], [[6, 6], [0, 0, 0]], [[7, 6], [0, 0, 0]], [[8, 6], [0, 0, 0]], [[9, 6], [0, 0, 0]],
                    [[1, 7], [0, 0, 0]], [[2, 7], [0, 0, 0]], [[3, 7], [0, 0, 0]], [[4, 7], [0, 0, 0]], [[5, 7], [0, 0, 0]], [[6, 7], [0, 0, 0]], [[7, 7], [0, 0, 0]], [[8, 7], [0, 0, 0]], [[9, 7], [0, 0, 0]],
                    [[1, 8], [0, 0, 0]], [[2, 8], [0, 0, 0]], [[3, 8], [0, 0, 0]], [[4, 8], [0, 0, 0]], [[5, 8], [0, 0, 0]], [[6, 8], [0, 0, 0]], [[7, 8], [0, 0, 0]], [[8, 8], [0, 0, 0]], [[9, 8], [0, 0, 0]],
                    [[1, 9], [0, 0, 0]], [[2, 9], [0, 0, 0]], [[3, 9], [1, 1, 0]], [[4, 9], [0, 0, 0]], [[5, 9], [0, 0, 0]], [[6, 9], [0, 0, 0]], [[7, 9], [1, 2, 0]], [[8, 9], [0, 0, 0]], [[9, 9], [8, 1, 0]]]),
    assertz_sector([[1, 1, 0], [2, 1, 0], [3, 1, 0], [4, 1, 0], [5, 1, 0], [6, 1, 0], [7, 1, 0], [8, 1, 0], [9, 1, 0], [1, 2, 0], [2, 2, 0], [3, 2, 0], [4, 2, 0], [5, 2, 0], [6, 2, 0], [7, 2, 0], [8, 2, 0], [9, 2, 0]]),
    condition((\+ debug(on)), safecall(game_engine, clear_game)).

continue_game:-
    safecall(game_engine, clear_game).

clear_game:-
    retractall(player(_)),
    retractall(board(_, _)),
    retractall(sector(_, _, _)).

/* ================== */
/* ===== Engine ===== */
/* ================== */

game_engine(Player, NextTurnPlayer):-
    display_board(Player),
    input(Player, Position, FinalPosition, Orientation, FinalOrientation, Type, NextTurnPlayer),
    update_board(Player, Position, FinalPosition, Orientation, FinalOrientation, Type),
    !.

game_engine:-
    repeat,
    player([Player, NextPlayer]),
    condition((Player \== 0),
    (
        game_engine(Player, NextTurnPlayer),
        retractall(player(_)),
        assertz(player([NextPlayer, NextTurnPlayer])),
        fail
    )),
    display_board(Player).

/* =================== */
/* ===== Display ===== */
/* =================== */

display_board(0):-
    print_board,
    write('Game over!'), nl, write('It\'s '), nl,
    sector(1, 1, WS1),
    sector(2, 1, WS2),
    sector(3, 1, WS3),
    sector(4, 1, WS4),
    sector(5, 1, WS5),
    sector(6, 1, WS6),
    sector(7, 1, WS7),
    sector(8, 1, WS8),
    sector(9, 1, WS9),
    sector(1, 2, BS1),
    sector(2, 2, BS2),
    sector(3, 2, BS3),
    sector(4, 2, BS4),
    sector(5, 2, BS5),
    sector(6, 2, BS6),
    sector(7, 2, BS7),
    sector(8, 2, BS8),
    sector(9, 2, BS9),
    WhiteScore is WS1 + WS2 + WS3 + WS4 + WS5 + WS6 + WS7 + WS8 + WS9,
    BlackScore is BS1 + BS2 + BS3 + BS4 + BS5 + BS6 + BS7 + BS8 + BS9,
    condition((WhiteScore > BlackScore),
    (
        write('your victory, WHITE player!'), nl, nl, write('Stats:'), nl,
        write('WHITE player scored '), write(WhiteScore), write(' points.'), nl,
        write('BLACK player scored '), write(BlackScore), write(' points.'), nl
    ),
    condition((BlackScore > WhiteScore),
    (
        write('your victory, BLACK player!'), nl, nl, write('Stats:'), nl,
        write('BLACK player scored '), write(BlackScore), write(' points.'), nl,
        write('WHITE player scored '), write(WhiteScore), write(' points.'), nl
    ),
    (
        write('a draw!'), nl, nl, write('Stats:'), nl,
        write('BLACK player scored '), write(BlackScore), write(' points.'), nl,
        write('WHITE player scored '), write(WhiteScore), write(' points.'), nl
    ))).

display_board(Player):-
    print_board,
    write('It\'s your turn, '),
    condition((Player == 1),
    (
        write('WHITE')
    ),
    (
        write('BLACK')
    )),
    write(' player!'), nl.

print_board:-
    writecode(9487), writecode(7, 9473), writecode(9519), writecode(7, 9473), writecode(9519), writecode(7, 9473),
    writecode(9523), writecode(7, 9473), writecode(9519), writecode(7, 9473), writecode(9519), writecode(7, 9473),
    writecode(9523), writecode(7, 9473), writecode(9519), writecode(7, 9473), writecode(9519), writecode(7, 9473),
    writecode(9491), nl,
    print_board(1, 0).

print_board(10, _):-
    writecode(9495), writecode(7, 9473), writecode(9527), writecode(7, 9473), writecode(9527), writecode(7, 9473),
    writecode(9531), writecode(7, 9473), writecode(9527), writecode(7, 9473), writecode(9527), writecode(7, 9473),
    writecode(9531), writecode(7, 9473), writecode(9527), writecode(7, 9473), writecode(9527), writecode(7, 9473),
    writecode(9499), nl,
    write('    A       B       C       D       E       F       G       H       I'), nl.
print_board(R1, 0):-
    R2 is R1 + 1,
    R3 is R2 + 1,
    RNext is R3 + 1,
    print_board(R1),
    writecode(9504), writecode(7, 9472), writecode(9532), writecode(7, 9472), writecode(9532), writecode(7, 9472),
    writecode(9538), writecode(7, 9472), writecode(9532), writecode(7, 9472), writecode(9532), writecode(7, 9472),
    writecode(9538), writecode(7, 9472), writecode(9532), writecode(7, 9472), writecode(9532), writecode(7, 9472),
    writecode(9512), nl,
    print_board(R2),
    writecode(9504), writecode(7, 9472), writecode(9532), writecode(7, 9472), writecode(9532), writecode(7, 9472),
    writecode(9538), writecode(7, 9472), writecode(9532), writecode(7, 9472), writecode(9532), writecode(7, 9472),
    writecode(9538), writecode(7, 9472), writecode(9532), writecode(7, 9472), writecode(9532), writecode(7, 9472),
    writecode(9512), nl,
    print_board(R3),
    print_board(RNext, 1).
print_board(RNext, 1):-
    writecode(9507), writecode(7, 9473), writecode(9535), writecode(7, 9473), writecode(9535), writecode(7, 9473),
    writecode(9547), writecode(7, 9473), writecode(9535), writecode(7, 9473), writecode(9535), writecode(7, 9473),
    writecode(9547), writecode(7, 9473), writecode(9535), writecode(7, 9473), writecode(9535), writecode(7, 9473),
    writecode(9515), nl,
    print_board(RNext, 0).

print_board(R):-
    writecode(9475), print_element(1, R, 1), writecode(9474), print_element(2, R, 1), writecode(9474), print_element(3, R, 1),
    writecode(9475), print_element(4, R, 1), writecode(9474), print_element(5, R, 1), writecode(9474), print_element(6, R, 1),
    writecode(9475), print_element(7, R, 1), writecode(9474), print_element(8, R, 1), writecode(9474), print_element(9, R, 1),
    writecode(9475), nl,
    writecode(9475), print_element(1, R, 2), writecode(9474), print_element(2, R, 2), writecode(9474), print_element(3, R, 2),
    writecode(9475), print_element(4, R, 2), writecode(9474), print_element(5, R, 2), writecode(9474), print_element(6, R, 2),
    writecode(9475), print_element(7, R, 2), writecode(9474), print_element(8, R, 2), writecode(9474), print_element(9, R, 2),
    writecode(9475), write(R), nl,
    writecode(9475), print_element(1, R, 3), writecode(9474), print_element(2, R, 3), writecode(9474), print_element(3, R, 3),
    writecode(9475), print_element(4, R, 3), writecode(9474), print_element(5, R, 3), writecode(9474), print_element(6, R, 3),
    writecode(9475), print_element(7, R, 3), writecode(9474), print_element(8, R, 3), writecode(9474), print_element(9, R, 3),
    writecode(9475), nl.

print_element(X, Y, R):-
    board([X, Y], [O, P, M]),
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
    writecode(C).
print_piece(2, P):-
    C is 9666 - P,
    writecode(C).
print_piece(3, P):-
    C is 9656 - P,
    writecode(C).
print_piece(4, P):-
    C is 9666 - P,
    writecode(C).
print_piece(5, P):-
    C is 9662 - P,
    writecode(C).
print_piece(6, P):-
    C is 9656 - P,
    writecode(C).
print_piece(7, P):-
    C is 9666 - P,
    writecode(C).
print_piece(8, P):-
    C is 9656 - P,
    writecode(C).

print_marker(0):-
    write(' ').
print_marker(M):-
    C is 9897 + M,
    writecode(C).

/* ================= */
/* ===== Input ===== */
/* ================= */

input(Player, Position, FinalPosition, Orientation, FinalOrientation, Type, NextTurnPlayer):-
    repeat,
    piece_input(Player, Position, Power, Pass),
    condition((Pass == 1),
    (
        NextTurnPlayer is 0
    ),
    (
        movement_input(Player, Position, Power, FinalPosition, Orientation, Type, Rotate, Back1),
        condition((Back1 == 0),
        (
            condition((Rotate == 0),
            (
                FinalOrientation is Orientation,
                NextTurnPlayer is Player
            ),
            (
                orientation_input(Orientation, FinalOrientation, Back2),
                condition((Back2 == 0),
                (
                    NextTurnPlayer is Player
                ),
                (
                    fail
                ))
            ))
        ),
        (
            fail
        ))
    )).

piece_input(Player, Position, Power, Pass):-
    repeat,
    write('Select piece'), nl,
    saferead(RawPosition),
    validate_piece_input(RawPosition, Player, Position, Pass),
    condition((Pass == 0),
    (
        evaluate_piece_input(Player, Position, Power)
    )),
    !.

movement_input(Player, Position, Power, FinalPosition, Orientation, Type, Rotate, Back):-
    repeat,
    write('Define movement'), nl,
    saferead(RawMovement),
    validate_movement_input(RawMovement, Movement, Back),
    condition((Back == 0),
    (
        evaluate_movement_input(Player, Movement, Position, Power, FinalPosition, Orientation, Type, Rotate)
    )),
    !.

orientation_input(Orientation, FinalOrientation, Back):-
    repeat,
    write('Define orientation'), nl,
    saferead(RawOrientation),
    validate_orientation_input(RawOrientation, Back),
    condition((Back == 0),
    (
        check_orientation(Orientation, RawOrientation, FinalOrientation)
    )),
    !.

/* ===== Input validation ===== */

validate_piece_input(pass, Player, _, Pass):-
    \+ check_piece(Player),
    Pass is 1,
    !.
validate_piece_input(pass, _, _, _):-
    write('Invalid play!'), nl,
    !,
    fail.
validate_piece_input(RawPosition, _, [PositionX, PositionY], Pass):-
    atom(RawPosition),
    atom_chars(RawPosition, Position),
    length(Position, 2),
    [RawPositionX, RawPositionY] = Position,
    nth(RawPositionX, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'], PositionX),
    PositionX \== 0,
    number_chars(PositionY, [RawPositionY]),
    PositionY \== 0,
    board([PositionX, PositionY], [_, P, _]),
    P \== 0,
    Pass is 0,
    !.
validate_piece_input(_, _, _, _):-
    write('Invalid input!'), nl,
    !,
    fail.

validate_movement_input(back, _, Back):-
    Back is 1,
    !.
validate_movement_input(RawMovement, [Direction, Distance], Back):-
    atom(RawMovement),
    atom_chars(RawMovement, Movement),
    length(Movement, 2),
    [Direction, RawDistance] = Movement,
    member(Direction, ['l', 'f', 'r']),
    member(RawDistance, ['1', '2', '3', '4', '5', '6', '7', '8']),
    number_chars(Distance, [RawDistance]),
    Back is 0,
    !.
validate_movement_input(_, _, _):-
    write('Invalid input!'), nl,
    !,
    fail.

validate_orientation_input(back, Back):-
    Back is 1,
    !.
validate_orientation_input(RawOrientation, Back):-
    atom(RawOrientation),
    member(RawOrientation, ['l', 'f', 'r']),
    Back is 0,
    !.
validate_orientation_input(_, _):-
    write('Invalid input!'), nl,
    !,
    fail.

/* ===== Input evaluation ===== */

evaluate_piece_input(Player, [PositionX, PositionY], Power):-
    board([PositionX, PositionY], [OForward, Player, _]), %% if either this
    check_orientation(OForward, 'l', OLeft),
    check_orientation(OForward, 'r', ORight),
    check_limit(PositionX, PositionY, OLeft, LLeft),
    check_limit(PositionX, PositionY, OForward, LForward),
    check_limit(PositionX, PositionY, ORight, LRight),
    \+ sort([LLeft, LForward, LRight], [0]), %% or this can't be proven, the selection isn't valid
    check_sector(PositionX, PositionY, S),
    sector(S, Player, SPower),
    Power is max(SPower, 1), %% disallow Power == 0
    check_piece(PositionX, PositionY, OForward, Player, Power), %% additional check to see if any play is possible
    !.
evaluate_piece_input(_, _, _):-
    write('Invalid piece!'), nl,
    !,
    fail.

evaluate_movement_input(Player, [Direction, Distance], [PositionX, PositionY], Power, [FinalPositionX, FinalPositionY], Orientation, Type, Rotate):-
    Distance =< Power, %% if either this
    board([PositionX, PositionY], [O, _, _]),
    check_orientation(O, Direction, Orientation),
    check_limit(PositionX, PositionY, Orientation, Limit),
    Distance =< Limit, %% or this
    check_movement(PositionX, PositionY, Distance, Orientation, Player, FinalPositionX, FinalPositionY, Type), %% or this can't be proven, the selection isn't valid
    check_sector(PositionX, PositionY, Sector),
    check_sector(FinalPositionX, FinalPositionY, FinalSector),
    condition((FinalSector \== Sector),
    (
        Rotate is 1
    ),
    (
        Rotate is 0
    )),
    !.
evaluate_movement_input(_, _, _, _, _, _, _, _):-
    write('Invalid movement!'), nl,
    !,
    fail.

/* ================== */
/* ===== Update ===== */
/* ================== */

update_coords(X, Y, X, NY, 1):-
    NY is Y - 1.
update_coords(X, Y, NX, NY, 2):-
    NX is X + 1,
    NY is Y - 1.
update_coords(X, Y, NX, Y, 3):-
    NX is X + 1.
update_coords(X, Y, NX, NY, 4):-
    NX is X + 1,
    NY is Y + 1.
update_coords(X, Y, X, NY, 5):-
    NY is Y + 1.
update_coords(X, Y, NX, NY, 6):-
    NX is X - 1,
    NY is Y + 1.
update_coords(X, Y, NX, Y, 7):-
    NX is X - 1.
update_coords(X, Y, NX, NY, 8):-
    NX is X - 1,
    NY is Y - 1.

update_board(_, FinalPosition, FinalPosition, _).
update_board(Player, [PositionX, PositionY], [FinalPositionX, FinalPositionY], Orientation):-
    retract(board([PositionX, PositionY], [_, _, M])),
    assertz(board([PositionX, PositionY], [0, 0, Player])),
    check_sector(PositionX, PositionY, S),
    sector(S, Player, Power),
    NewPower is Power + 1,
    retract(sector(S, Player, Power)),
    assertz(sector(S, Player, NewPower)),
    condition((M \== 0, M \== Player),
    (
        check_sector(PositionX, PositionY, OS),
        sector(OS, M, OtherPower),
        NewOtherPower is OtherPower - 1,
        retract(sector(OS, M, OtherPower)),
        assertz(sector(OS, M, NewOtherPower))
    )),
    update_coords(PositionX, PositionY, PositionXNext, PositionYNext, Orientation),
    update_board(Player, [PositionXNext, PositionYNext], [FinalPositionX, FinalPositionY], Orientation).
    
update_board(Player, Position, FinalPosition, Orientation, FinalOrientation, rush):-
    update_board(Player, Position, FinalPosition, Orientation),
    retract(board(FinalPosition, [_, _, _])),
    assertz(board(FinalPosition, [FinalOrientation, Player, Player])).
update_board(Player, Position, FinalPosition, Orientation, _, drop):-
    update_board(Player, Position, FinalPosition, Orientation).
update_board(Player, Position, FinalPosition, _, FinalOrientation, Type):-
    retract(board(Position, [_, _, M])),
    assertz(board(Position, [0, 0, M])),
    retract(board(FinalPosition, [_, _, _])),
    assertz(board(FinalPosition, [FinalOrientation, Player, Player])),
    condition((Type == move),
    (
        [FX, FY] = FinalPosition,
        check_sector(FX, FY, S),
        sector(S, Player, Power),
        NewPower is Power + 1,
        retract(sector(S, Player, Power)),
        assertz(sector(S, Player, NewPower))
    )).

/* ================== */
/* ===== Checks ===== */
/* ================== */

check_piece(P):-
    board([X, Y], [O, P, _]), %% must test all of these
    check_sector(X, Y, S),
    sector(S, P, SPower),
    Power is max(SPower, 1),
    check_piece(X, Y, O, P, Power).

check_piece(X, Y, O1, P, Power):-
    check_orientation(O1, 'l', O2),
    check_orientation(O1, 'r', O3),
    check_limit(X, Y, O1, L1),
    check_limit(X, Y, O2, L2),
    check_limit(X, Y, O3, L3),
    Limit1 is min(Power, L1),
    Limit2 is min(Power, L2),
    Limit3 is min(Power, L3),
    condition((Limit1 == 0; \+ check_piece(X, Y, 0, O1, P, Limit1)),
    (
        condition((Limit2 == 0; \+ check_piece(X, Y, 0, O2, P, Limit2)),
        (
            condition((Limit3 == 0; \+ check_piece(X, Y, 0, O3, P, Limit3)),
            (
                !,
                fail
            ))
        ))
    )).
check_piece(X, Y, L, O, P, L):-
    !,
    check_movement(X, Y, L, O, P, _, _, _).
check_piece(X, Y, D, O, P, L):-
    \+ check_movement(X, Y, D, O, P, _, _, _),
    DNext is D + 1,
    !,
    check_piece(X, Y, DNext, O, P, L).
check_piece(_, _, _, _, _, _).

check_sector(X, Y, S):-
    S is div(X + 2, 3) + div(Y - 1, 3) * 3.

check_orientation(O, 'l', NewO):-
    NewO is mod(O + 6, 8) + 1.
check_orientation(O, 'f', O).
check_orientation(O, 'r', NewO):-
    NewO is mod(O, 8) + 1.

check_limit(_, Y, 1, L):- %% check if cuts aren't needed
    L is Y - 1.
check_limit(X, Y, 2, L):-
    L is min(9 - X, Y - 1).
check_limit(X, _, 3, L):-
    L is 9 - X.
check_limit(X, Y, 4, L):-
    L is min(9 - X, 9 - Y).
check_limit(_, Y, 5, L):-
    L is 9 - Y.
check_limit(X, Y, 6, L):-
    L is min(X - 1, 9 - Y).
check_limit(X, _, 7, L):-
    L is X - 1.
check_limit(X, Y, 8, L):-
    L is min(X - 1, Y - 1).

check_movement(X, Y, D, 1, P, X, FY, T):-
    FY is Y - D,
    check_final_field(X, FY, P, OldT),
    YNext is Y - 1,
    DNext is D - 1,
    check_movement(X, YNext, DNext, 1, P, OldT, T).
check_movement(X, Y, D, 2, P, FX, FY, T):-
    FX is X + D,
    FY is Y - D,
    check_final_field(FX, FY, P, OldT),
    XNext is X + 1,
    YNext is Y - 1,
    DNext is D - 1,
    check_movement(XNext, YNext, DNext, 2, P, OldT, T).
check_movement(X, Y, D, 3, P, FX, Y, T):-
    FX is X + D,
    check_final_field(FX, Y, P, OldT),
    XNext is X + 1,
    DNext is D - 1,
    check_movement(XNext, Y, DNext, 3, P, OldT, T).
check_movement(X, Y, D, 4, P, FX, FY, T):-
    FX is X + D,
    FY is Y + D,
    check_final_field(FX, FY, P, OldT),
    XNext is X + 1,
    YNext is Y + 1,
    DNext is D - 1,
    check_movement(XNext, YNext, DNext, 4, P, OldT, T).
check_movement(X, Y, D, 5, P, X, FY, T):-
    FY is Y + D,
    check_final_field(X, FY, P, OldT),
    YNext is Y + 1,
    DNext is D - 1,
    check_movement(X, YNext, DNext, 5, P, OldT, T).
check_movement(X, Y, D, 6, P, FX, FY, T):-
    FX is X - D,
    FY is Y + D,
    check_final_field(FX, FY, P, OldT),
    XNext is X - 1,
    YNext is Y + 1,
    DNext is D - 1,
    check_movement(XNext, YNext, DNext, 6, P, OldT, T).
check_movement(X, Y, D, 7, P, FX, Y, T):-
    FX is X - D,
    check_final_field(FX, Y, P, OldT),
    XNext is X - 1,
    DNext is D - 1,
    check_movement(XNext, Y, DNext, 7, P, OldT, T).
check_movement(X, Y, D, 8, P, FX, FY, T):-
    FX is X - D,
    FY is Y - D,
    check_final_field(FX, FY, P, OldT),
    XNext is X - 1,
    YNext is Y - 1,
    DNext is D - 1,
    check_movement(XNext, YNext, DNext, 8, P, OldT, T).

check_movement(_, _, 0, _, _, T, T).
check_movement(X, Y, D, 1, P, OldT, T):-
    check_field(X, Y, OldT, NewT),
    YNext is Y - 1,
    DNext is D - 1,
    check_movement(X, YNext, DNext, 1, P, NewT, T).
check_movement(X, Y, D, 2, P, OldT, T):-
    check_field(X, Y, OldT, NewT),
    XNext is X + 1,
    YNext is Y - 1,
    DNext is D - 1,
    check_movement(XNext, YNext, DNext, 2, P, NewT, T).
check_movement(X, Y, D, 3, P, OldT, T):-
    check_field(X, Y, OldT, NewT),
    XNext is X + 1,
    DNext is D - 1,
    check_movement(XNext, Y, DNext, 3, P, NewT, T).
check_movement(X, Y, D, 4, P, OldT, T):-
    check_field(X, Y, OldT, NewT),
    XNext is X + 1,
    YNext is Y + 1,
    DNext is D - 1,
    check_movement(XNext, YNext, DNext, 4, P, NewT, T).
check_movement(X, Y, D, 5, P, OldT, T):-
    check_field(X, Y, OldT, NewT),
    YNext is Y + 1,
    DNext is D - 1,
    check_movement(X, YNext, DNext, 5, P, NewT, T).
check_movement(X, Y, D, 6, P, OldT, T):-
    check_field(X, Y, OldT, NewT),
    XNext is X - 1,
    YNext is Y + 1,
    DNext is D - 1,
    check_movement(XNext, YNext, DNext, 6, P, NewT, T).
check_movement(X, Y, D, 7, P, OldT, T):-
    check_field(X, Y, OldT, NewT),
    XNext is X - 1,
    DNext is D - 1,
    check_movement(XNext, Y, DNext, 7, P, NewT, T).
check_movement(X, Y, D, 8, P, OldT, T):-
    check_field(X, Y, OldT, NewT),
    XNext is X - 1,
    YNext is Y - 1,
    DNext is D - 1,
    check_movement(XNext, YNext, DNext, 8, P, NewT, T).

check_final_field(X, Y, Player, T):-
    board([X, Y], [_, 0, M]),
    condition((M == Player),
    (
        T = rush
    ),
    condition((M == 0),
    (
        T = move
    ),
    (
        fail
    ))).
check_field(X, Y, OldT, NewT):-
    board([X, Y], [_, P, M]),
    condition(((OldT == rush; OldT == drop), P \== 0),
    (
        NewT = jump
    ),
    condition((OldT == rush, M \== P, M \== 0),
    (
        NewT = drop
    ),
    condition((OldT == move),
    (
        P == 0,
        NewT = OldT
    ),
    (
        NewT = OldT
    )))).
