:- ['./parse.pl'].
:- ['./generate.pl'].
:- ['../src/utils/utils.pl'].

call_list([]).
call_list([X|Xs]) :- call(X), call_list(Xs).
call_all(X) :- X =.. [',' | Xs], call_list(Xs).
call_all(X) :- functor(X , Nm, _), Nm \= ',', call(X).

interpret :- write('OOPL query interpeter.\n'),
	defined_names(Names),
	load_scope(Names, Scope),
	riwloop(Scope).

riwloop(Scope) :-
	write('?-'),
	read_term(user_input, X, [variable_names(Eqs)]),
	(parse_clause(_, X, fact(P)) ; throw('Only currently allowing facts at runtime.\n')),
	(resolve_args_comma_functor(P, Scope, [], _, Res) ; throw('Failed to interpret query')),
	((call_list(Res), write('true.\n')) ; write('false.\n')),
	%write(Res),
	write_list(user_output, Eqs),
 	riwloop(Scope).