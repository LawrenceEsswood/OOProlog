% Sums a list
sum([], 0).
sum([N|Ns], Sum) :- sum(Ns, SumA), Sum is SumA + N.

% Counts occurences of terms
count_occurences(X, X, 1).
count_occurences(X, Y, 0) :- X \= Y, atomic(Y).
count_occurences(X, Y, Sum) :- X \= Y, 
	not(atomic(Y)), Y =.. [_|Ys], 
	maplist(count_occurences(X), Ys, Ns),
	sum(Ns, Sum).

% Add args to a compound
add_functor_args(F, Args, F2) :- 
	F =.. [Nm|Args1],
	append(Args1, Args, AllArgs),
	F2 =.. [Nm|AllArgs].


% Turns a list into a conjunction of n terms
list_to_comma_functor([X],X).
list_to_comma_functor([X,Y|Zs], (X,C)) :- list_to_comma_functor([Y|Zs], C).

% Throws an exception with the concatanation of n terms
throw_atoms(Atms) :- atomic_list_concat(Atms, Msg), throw(Msg).

% Creates a list of n unique temps
tmp_lst(0, []).
tmp_lst(N, [T|Ts]) :- N >= 0, Nd is N-1, atom_concat('V',N,T), tmp_lst(Nd, Ts).

write_standard(Out, Term) :- op(0, fx, class), write_term(Out, Term, []), op(99, fx, class).

% Writes out a list of terms seperated by .\n
write_list(_, []).
write_list(Out, [L|LS]) :-
	write_standard(Out, L),
	write(Out, '.\n'),
	write_list(Out, LS).

% A map function that simply removes elements that do not satisfy the rule
filter(Rule, [D|Defs],[N|Names]) :- call(Rule, D, N), !, filter(Rule, Defs, Names).
filter(Rule, [_|Defs], Names) :- filter(Rule, Defs, Names).
filter(_, [], []).

partition(_,[],[],[]).
partition(Rule, [X|Xs], [X|L], R) :- call(Rule, X), !, partition(Rule, Xs, L, R).
partition(Rule, [X|Xs], L, [X|R]) :- partition(Rule, Xs, L, R).

escapes(['\'','\\']).

add_escapes_list([],[]).
add_escapes_list([X|LX], ['\\',X| RX]) :- escapes(E), member(X, E), add_escapes_list(LX, RX).
add_escapes_list([X|LX], [X|RX]) :- add_escapes_list(LX, RX).

add_escapes(X , Y) :- atom_chars(X, LX), add_escapes_list(LX, RX), atom_chars(Y , RX).

% Add qoutes to an atom if needed
add_qoutes(X, X) :- number(X), !.
add_qoutes([],[]) :- !.
add_qoutes(X,Y) :- add_escapes(X, Z), atomic_list_concat(['\'', Z, '\''], Y).

copy(File1,File2) :-
	open(File1,read,Stream1),
	open(File2,write,Stream2),
	copy_stream_data(Stream1,Stream2),
	close(Stream1),
	close(Stream2).