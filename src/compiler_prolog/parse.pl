%%%%%%%%%%
% Parser %
%%%%%%%%%%

% Define some operators to help parsing %

:- op(100, xfy, ::).
:- op(99, fx, class).
:- op(100, xfx, extends).

% Helper predicates %

% Appends a list to a disjunction
comma_list_append([], P, P).
comma_list_append([X|Xs],P,','(X,Pd)) :- comma_list_append(Xs, P, Pd).

% Flattens a list of tuples into a single list %
flatten_single([],[]).
flatten_single([(V, NstLs)|Rest], Res) :- 
	flatten_single(Rest, Restd), append(NstLs, [V|Restd], Res).

% Flattens a list of tuples into two lists %
flatten_pair([],[],[]).
flatten_pair([(V, NstLs)|Rest],[V|RestV], Res) :- 
	flatten_pair(Rest, RestV, RestNstLs), append(NstLs, RestNstLs, Res).

bind_vars_to_name([=(X,var(X))|XS]) :- bind_vars_to_name(XS).
bind_vars_to_name([]).

in_stream((IS,_,_), IS).
out_stream((_,OS,_), OS).

% Reads defs until eof or endclass %

parse_defs(S, [Def|DefLs], End) :- in_stream(S, IS),
	read_term(IS, T, [variable_names(Eqs)]),
	bind_vars_to_name(Eqs),
	T \= End,
	!,
	(T \= end_of_file ; throw('Unexpected end of stream')),
	(parse_def(S, T, Def) ; throw('Could not parse definition')),
	parse_defs(S, DefLs, End).
parse_defs(_, [], _).

% Parses a single definition %
						
parse_def(S, T, Def) :- parse_class(S, T, Def).
parse_def(S, T, Def) :- parse_field(S, T, Def).
parse_def(S, T, Def) :- parse_clause(S, T, Def).

% Parses a field %
parse_field(_, var(X), field(X)) :- !.

% Parse a class header and body %
parse_class(S, T, class_def(H, Defs)) :-
	parse_header(S, T, H),
	parse_defs(S, Defs, endclass).
						
parse_header(_, class X extends Y, class_head(X,[Y])) :- atomic(X), atomic(Y), !.
parse_header(_, class X, class_head(X,[])) :- atomic(X).

% Parses a normal prolog clause %
parse_clause(_, P :- Xs, rule(Pd, Ys)) :- !,
	tag_predicate(P, Pd),
	tag_atoms(Xs, Ys).

parse_clause(_, P, fact(Pd)) :- tag_predicate(P, Pd).

tag_predicate(P, P) :- atomic(P).
tag_predicate(P, Pd) :- not(atomic(P)), tag_atoms(P, Pd).

tag_atoms(var(X), var(X)) :- !.
tag_atoms(X, atom(X)) :- atomic(X), !.
tag_atoms(P, P2) :- 
	functor(P, _, _), P =.. [Nm|Args], 
	maplist(tag_atoms, Args, Args2),
	P2 =.. [Nm|Args2].