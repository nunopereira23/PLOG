:-use_module(library(lists)).
:-use_module(library(clpfd)).

%% Main

/*
trid(5, [sum(2, 4, 5, 12), sum(4, 7, 8, 9), sum(6, 9, 10, 6), sum(7, 11, 12, 6), sum(8, 12, 13, 12), sum(9, 13, 14, 8)]).
                      __
                     /  \
                    | 05 |
                     \__/
                     /  \
                    /    \
                 __/      \__
                /  \      /  \
               | 04 |----| 01 |
                \__/      \__/
                /  \      /  \
               /    \    /    \
            __/  12  \__/      \__
           /  \      /  \      /  \
          | 03 |----| 05 |----| 02 |
           \__/      \__/      \__/
           /  \      /  \      /  \
          /    \    /    \    /    \
       __/  09  \__/      \__/  06  \__
      /  \      /  \      /  \      /  \
     | 02 |----| 04 |----| 01 |----| 03 |
      \__/      \__/      \__/      \__/
      /  \      /  \      /  \      /  \
     /    \    /    \    /    \    /    \
  __/  06  \__/  12  \__/  08  \__/      \__
 /  \      /  \      /  \      /  \      /  \
| 01 |----| 03 |----| 05 |----| 02 |----| 04 |
 \__/      \__/      \__/      \__/      \__/
*/

trid(Layer, Sums):-
    trid_check(Layer, Sums),
    trid_logic(Layer, Sums, Numbers),
    trid_display(Layer, Numbers),
    trid_clear.
trid(_Layer, _Sums):-
    trid_clear,
    fail.

trid_check(Layer, Sums):-
    Layer >= 5, %% Layer == 1 is trivial, Layer == 2 is impossible, Layer == 3 is Sums-trivial, and Layer == 4 is impossible as well
    Layer =< 99, %% My screen holds up to 18, my CPU... not so many
    (length(Sums, 0);
    setof(Sum, get_sums(Sums, Sum), [MinSum|SumsRest]),
    MinSum >= 6,
    length(SumsRest, MaxSumIndex),
    nth1(MaxSumIndex, SumsRest, MaxSum),
    MaxSum =< Layer + (Layer - 1) + (Layer - 2)),
    set_sums(Sums),
    !.

trid_logic(Layer, Sums, Numbers):-
    Size is div(Layer * (Layer + 1), 2),
    length(Numbers, Size),
    domain(Numbers, 1, Layer),
    distinct_horizontal(Layer, Numbers),
    distinct_vertical(Layer, Numbers),
    distinct_diagonal(Layer, Numbers),
    constraint_sums(Sums, Numbers),
    labeling([], Numbers),
    !.

trid_display(Layer, Numbers):-
    display_grid(Layer, Numbers).

trid_clear:-
    clear_sums.

%% Logic

distinct_horizontal(Layer, Numbers):-
    distinct_horizontal(Layer, 1, Numbers).
distinct_horizontal(Layer, I, Numbers):-
    horizontal_numbers(Numbers, I, Horizontal, NumbersNext),
    all_distinct(Horizontal),
    (I == Layer;
    INext is I + 1,
    distinct_horizontal(Layer, INext, NumbersNext)).

distinct_vertical(Layer, Numbers):-
    distinct_vertical(Layer, Layer, Numbers).
distinct_vertical(Layer, I, Numbers):-
    vertical_numbers(Numbers, I, Vertical, NumbersNext),
    all_distinct(Vertical),
    (I == 1;
    INext is I - 1,
    distinct_vertical(Layer, INext, NumbersNext)).

distinct_diagonal(Layer, Numbers):-
    distinct_diagonal(Layer, Layer, Numbers).
distinct_diagonal(Layer, I, Numbers):-
    diagonal_numbers(Numbers, I, Diagonal, NumbersNext),
    all_distinct(Diagonal),
    (I == 1;
    INext is I - 1,
    distinct_diagonal(Layer, INext, NumbersNext)).

constraint_sums([], _).
constraint_sums([sum(V1, V2, V3, S)|Sums], Numbers):-
    element(V1, Numbers, N1),
    element(V2, Numbers, N2),
    element(V3, Numbers, N3),
    S #= N1 + N2 + N3,
    constraint_sums(Sums, Numbers).

%% Display

display_grid(Layer, [Number|Numbers]):-
    Y is 1,
    N0 is (Layer - Y) * 5,
    N1 is N0 + 1,
    N2 is N0 + 2,
    number_chars(Number, [NumberChar1|NumberChar2]),
    (length([NumberChar1|NumberChar2], 2),
    atom_chars(N, [NumberChar1|NumberChar2]);
    atom_chars(N, ['0', NumberChar1])),
    writeN(' ', N2), write('__'), nl,
    writeN(' ', N1), write('/  \\'), nl,
    writeN(' ', N0), write('| '), write(N), write(' |'), nl,
    writeN(' ', N1), write('\\__/'), nl,
    writeN(' ', N1), write('/  \\'), nl,
    writeN(' ', N0), write('/    \\'), nl,
    YNext is Y + 1,
    N0Next is N0 - 5,
    N1Next is N1 - 5,
    N2Next is N2 - 5,
    display_grid(Layer, YNext, N0Next, N1Next, N2Next, Numbers).
display_grid(Layer, Y, N0, N1, N2, [Number|Numbers]):-
    number_chars(Number, [NumberChar1|NumberChar2]),
    (length([NumberChar1|NumberChar2], 2),
    atom_chars(N, [NumberChar1|NumberChar2]);
    atom_chars(N, ['0', NumberChar1])),
    writeN(' ', N2), write('__'), display_layer_1(1, Y), nl,
    writeN(' ', N1), write('/  \\'), display_layer_2(1, Y), nl,
    writeN(' ', N0), write('| '), write(N), write(' |'), display_layer_3(1, Y, Numbers, NumbersNext), nl,
    writeN(' ', N1), write('\\__/'), display_layer_4(1, Y), nl,
    (Y == Layer;
    writeN(' ', N1), write('/  \\'), display_layer_5(1, Y), nl,
    writeN(' ', N0), write('/    \\'), display_layer_6(1, Y), nl,
    YNext is Y + 1,
    N0Next is N0 - 5,
    N1Next is N1 - 5,
    N2Next is N2 - 5,
    display_grid(Layer, YNext, N0Next, N1Next, N2Next, NumbersNext)).

display_layer_1(_Y, _Y).
display_layer_1(X, Y):-
    XNext is X + 1,
    YPrev is Y - 1,
    vertex(V1, [X, YPrev]),
    vertex(V2, [X, Y]),
    vertex(V3, [XNext, Y]),
    (sum(V1, V2, V3, Sum),
    number_chars(Sum, [SumChar1|SumChar2]),
    (length([SumChar1|SumChar2], 2),
    atom_chars(S, [SumChar1|SumChar2]);
    atom_chars(S, ['0', SumChar1]));
    S = '  '),
    write('/  '), write(S), write('  \\__'),
    display_layer_1(XNext, Y).

display_layer_2(_Y, _Y).
display_layer_2(X, Y):-
    write('      /  \\'),
    XNext is X + 1,
    display_layer_2(XNext, Y).

display_layer_3(_Y, _Y, Numbers, Numbers).
display_layer_3(X, Y, [Number|Numbers], NumbersNext):-
    number_chars(Number, [NumberChar1|NumberChar2]),
    (length([NumberChar1|NumberChar2], 2),
    atom_chars(N, [NumberChar1|NumberChar2]);
    atom_chars(N, ['0', NumberChar1])),
    write('----| '), write(N), write(' |'),
    XNext is X + 1,
    display_layer_3(XNext, Y, Numbers, NumbersNext).

display_layer_4(_Y, _Y).
display_layer_4(X, Y):-
    write('      \\__/'),
    XNext is X + 1,
    display_layer_4(XNext, Y).

display_layer_5(_Y, _Y).
display_layer_5(X, Y):-
    XNext is X + 1,
    YNext is Y + 1,
    vertex(V1, [X, Y]),
    vertex(V2, [XNext, Y]),
    vertex(V3, [XNext, YNext]),
    (sum(V1, V2, V3, Sum),
    number_chars(Sum, [SumChar1|SumChar2]),
    (length([SumChar1|SumChar2], 2),
    atom_chars(S, [SumChar1|SumChar2]);
    atom_chars(S, ['0', SumChar1]));
    S = '  '),
    write('  '), write(S), write('  /  \\'),
    display_layer_5(XNext, Y).

display_layer_6(_Y, _Y).
display_layer_6(X, Y):-
    write('    /    \\'),
    XNext is X + 1,
    display_layer_6(XNext, Y).

%% Utilities

writeN(_, 0):-
    !.
writeN(C, N):-
    Next is N - 1,
    write(C),
    writeN(C, Next).

vertex(V, C):-
    vertex(V, 1, [1, 1], C),
    !.
vertex(V, V, C, C).
vertex(V, I, [X, Y], C):-
    INext is I + 1,
    (X =\= Y,
    XNext is X + 1,
    YNext is Y;
    XNext is 1,
    YNext is Y + 1),
    vertex(V, INext, [XNext, YNext], C).

cell(V1, V2, V3):-
    vertex(V1, [X0, Y0]),
    vertex(V2, [X0v1, Y1v0]),
    vertex(V3, [X1, Y1]),
    X1 =:= X0 + 1,
    Y1 =:= Y0 + 1,
    (X0v1 =:= X0, Y1v0 =:= Y1;
    X0v1 =:= X1, Y1v0 =:= Y0),
    !.

get_sums([sum(_, _, _, Sum)], Sum):-
    !.
get_sums([sum(_, _, _, Sum)|_], Sum).
get_sums([_|Sumslist], Sum):-
    get_sums(Sumslist, Sum).

set_sums([]).
set_sums([sum(V1, V2, V3, S)|Sums]):-
    cell(V1, V2, V3),
    assertz(sum(V1, V2, V3, S)),
    !,
    set_sums(Sums).

clear_sums:-
    retractall(sum(_, _, _, _)).

horizontal_numbers(Numbers, 0, [], Numbers).
horizontal_numbers([HorizontalNumber|NextNumbers], I, Horizontal, NumbersNext):-
    INext is I - 1,
    horizontal_numbers(NextNumbers, INext, HorizontalNumbersNext, NumbersNext),
    Horizontal = [HorizontalNumber|HorizontalNumbersNext].

vertical_numbers(Numbers, N, Vertical, NumbersNext):-
    vertical_numbers(Numbers, N, 1, Vertical, NumbersNext).
vertical_numbers(Numbers, N, N, Vertical, NumbersNext):-
    horizontal_numbers(Numbers, N, [VerticalNumber|NumbersNext], _),
    Vertical = [VerticalNumber].
vertical_numbers(Numbers, N, I, Vertical, NumbersNext):-
    INext is I + 1,
    horizontal_numbers(Numbers, I, [VerticalNumber|HorizontalNumbersNext], NextNumbers),
    vertical_numbers(NextNumbers, N, INext, VerticalNext, VerticalNumbersNext),
    Vertical = [VerticalNumber|VerticalNext],
    append(HorizontalNumbersNext, VerticalNumbersNext, NumbersNext).

diagonal_numbers(Numbers, N, Diagonal, NumbersNext):-
    diagonal_numbers(Numbers, N, 1, Diagonal, NumbersNext).
diagonal_numbers(Numbers, N, N, Diagonal, NumbersNext):-
    horizontal_numbers(Numbers, N, HorizontalNumbers, _),
    reverse(HorizontalNumbers, [DiagonalNumber|_NumbersNext]),
    reverse(_NumbersNext, NumbersNext),
    Diagonal = [DiagonalNumber].
diagonal_numbers(Numbers, N, I, Diagonal, NumbersNext):-
    INext is I + 1,
    horizontal_numbers(Numbers, I, HorizontalNumbers, NextNumbers),
    reverse(HorizontalNumbers, [DiagonalNumber|_HorizontalNumbersNext]),
    reverse(_HorizontalNumbersNext, HorizontalNumbersNext),
    diagonal_numbers(NextNumbers, N, INext, DiagonalNext, DiagonalNumbersNext),
    Diagonal = [DiagonalNumber|DiagonalNext],
    append(HorizontalNumbersNext, DiagonalNumbersNext, NumbersNext).

/*
    horizontal_numbers([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o], 1, H1, _N1), horizontal_numbers(_N1, 2, H2, _N2), horizontal_numbers(_N2, 3, H3, _N3), horizontal_numbers(_N3, 4, H4, _N4), horizontal_numbers(_N4, 5, H5, N).
    H1 = [a],
    H2 = [b,c],
    H3 = [d,e,f],
    H4 = [g,h,i,j],
    H5 = [k,l,m,n,o],
    N = []

    vertical_numbers([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o], 5, V1, _N1), vertical_numbers(_N1, 4, V2, _N2), vertical_numbers(_N2, 3, V3, _N3), vertical_numbers(_N3, 2, V4, _N4), vertical_numbers(_N4, 1, V5, N).
    V1 = [a,b,d,g,k],
    V2 = [c,e,h,l],
    V3 = [f,i,m],
    V4 = [j,n],
    V5 = [o],
    N = []

    diagonal_numbers([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o], 5, D1, _N1), diagonal_numbers(_N1, 4, D2, _N2), diagonal_numbers(_N2, 3, D3, _N3), diagonal_numbers(_N3, 2, D4, _N4), diagonal_numbers(_N4, 1, D5, N).
    D1 = [a,c,f,j,o],
    D2 = [b,e,i,n],
    D3 = [d,h,m],
    D4 = [g,l],
    D5 = [k],
    N = []
*/
