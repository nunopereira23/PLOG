/* :-use_module(library(lists)). */
/* ver doc sicstus 10.21 */

print_matrix([]):-
    write('o ----- o ----- o ----- o ----- o ----- o ----- o ----- o ----- o ----- o'), nl.
print_matrix([H1|[H2|[H3|T]]]):-
    write('o ----- o ----- o ----- o ----- o ----- o ----- o ----- o ----- o ----- o'), nl,
    print_list(H1),
    write('o ----- + ----- + ----- o ----- + ----- + ----- o ----- + ----- + ----- o'), nl,
    print_list(H2),
    write('o ----- + ----- + ----- o ----- + ----- + ----- o ----- + ----- + ----- o'), nl,
    print_list(H3),
    print_matrix(T).

print_list([H1|[H2|[H3|[H4|[H5|[H6|[H7|[H8|[H9|[]]]]]]]]]]):- /* There may be a better way to do this */
    print_element(H1, 1),
    print_element(H2, 1),
    print_element(H3, 1),
    print_element(H4, 1),
    print_element(H5, 1),
    print_element(H6, 1),
    print_element(H7, 1),
    print_element(H8, 1),
    print_element(H9, 1),
    write('|'), nl,
    print_element(H1, 2),
    print_element(H2, 2),
    print_element(H3, 2),
    print_element(H4, 2),
    print_element(H5, 2),
    print_element(H6, 2),
    print_element(H7, 2),
    print_element(H8, 2),
    print_element(H9, 2),
    write('|'), nl,
    print_element(H1, 3),
    print_element(H2, 3),
    print_element(H3, 3),
    print_element(H4, 3),
    print_element(H5, 3),
    print_element(H6, 3),
    print_element(H7, 3),
    print_element(H8, 3),
    print_element(H9, 3),
    write('|'), nl.

print_element(0, 0, 0, _R):-
    write('|       ').

print_element(0, 0, _F, 1):-
    write('|       ').
print_element(1, P, _F, 1):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AP, ' ', ' ', ' ']),
    write(A).
print_element(2, P, _F, 1):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AP, ' ', AP, ' ']),
    write(A).
print_element(3, P, _F, 1):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AP, ' ', ' ', ' ']),
    write(A).
print_element(4, _P, _F, 1):-
    write('|       ').
print_element(5, _P, _F, 1):-
    write('|       ').
print_element(6, _P, _F, 1):-
    write('|       ').
print_element(7, P, _F, 1):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AP, ' ', ' ', ' ']),
    write(A).
print_element(8, P, _F, 1):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', AP, ' ', AP, ' ', ' ', ' ']),
    write(A).

print_element(0, 0, F, 2):-
    number_chars(F, [AF|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AF, ' ', ' ', ' ']),
    write(A).
print_element(1, P, F, 2):-
    number_chars(P, [AP|[]]),
    number_chars(F, [AF|[]]),
    atom_chars(A, ['|', ' ', AP, ' ', AF, ' ', AP, ' ']),
    write(A).
print_element(2, P, F, 2):-
    number_chars(P, [AP|[]]),
    number_chars(F, [AF|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AF, ' ', AP, ' ']),
    write(A).
print_element(3, P, F, 2):-
    number_chars(P, [AP|[]]),
    number_chars(F, [AF|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AF, ' ', AP, ' ']),
    write(A).
print_element(4, P, F, 2):-
    number_chars(P, [AP|[]]),
    number_chars(F, [AF|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AF, ' ', AP, ' ']),
    write(A).
print_element(5, P, F, 2):-
    number_chars(P, [AP|[]]),
    number_chars(F, [AF|[]]),
    atom_chars(A, ['|', ' ', AP, ' ', AF, ' ', AP, ' ']),
    write(A).
print_element(6, P, F, 2):-
    number_chars(P, [AP|[]]),
    number_chars(F, [AF|[]]),
    atom_chars(A, ['|', ' ', AP, ' ', AF, ' ', ' ', ' ']),
    write(A).
print_element(7, P, F, 2):-
    number_chars(P, [AP|[]]),
    number_chars(F, [AF|[]]),
    atom_chars(A, ['|', ' ', AP, ' ', AF, ' ', ' ', ' ']),
    write(A).
print_element(8, P, F, 2):-
    number_chars(P, [AP|[]]),
    number_chars(F, [AF|[]]),
    atom_chars(A, ['|', ' ', AP, ' ', AF, ' ', ' ', ' ']),
    write(A).

print_element(0, 0, _F, 3):-
    write('|       ').
print_element(1, _P, _F, 3):-
    write('|       ').
print_element(2, _P, _F, 3):-
    write('|       ').
print_element(3, P, _F, 3):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AP, ' ', ' ', ' ']),
    write(A).
print_element(4, P, _F, 3):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AP, ' ', AP, ' ']),
    write(A).
print_element(5, P, _F, 3):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AP, ' ', ' ', ' ']),
    write(A).
print_element(6, P, _F, 3):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', AP, ' ', AP, ' ', ' ', ' ']),
    write(A).
print_element(7, P, _F, 3):-
    number_chars(P, [AP|[]]),
    atom_chars(A, ['|', ' ', ' ', ' ', AP, ' ', ' ', ' ']),
    write(A).
print_element(8, _P, _F, 3):-
    write('|       ').

print_element(E, Line):-
    Orientation is floor(E / 100),
    Piece is floor((E - floor(E / 100) * 100) / 10),
    Field is E - floor(E / 10) * 10,
    print_element(Orientation, Piece, Field, Line).
