% Turns a list into a conjunction of n terms
list_to_comma_functor([X],X).
list_to_comma_functor([X,Y|Zs], (X,C)) :- list_to_comma_functor([Y|Zs], C).

% Throws an exception with the concatanation of n terms
throw_atoms(Atms) :- atomic_list_concat(Atms, Msg), throw(Msg).

% Creates a list of n unique temps
tmp_lst(0, []).
tmp_lst(N, [T|Ts]) :- N >= 0, Nd is N-1, atom_concat('V',N,T), tmp_lst(Nd, Ts).

% Writes out a list of terms seperated by .\n
write_list(_, []).
write_list(Out, [L|LS]) :- write(Out, L), write(Out, '.\n'), write_list(Out, LS).

% A map function that simply removes elements that do not satisfy the rule
filter(Rule, [D|Defs],[N|Names]) :- call(Rule, D, N), !, filter(Rule, Defs, Names).
filter(Rule, [_|Defs], Names) :- filter(Rule, Defs, Names).
filter(_, [], []).

% Add qoutes to an atom if needed
add_qoutes(X, X) :- number(X), !.
add_qoutes(X,Y) :- atomic_list_concat(['\'', X, '\''], Y).