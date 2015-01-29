%%%%%%%%%%
% Parser %
%%%%%%%%%%

% Define some operators to help parsing %

:- op(100, yfx, ::).
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

bind_vars_to_name([=(X,X)|XS]) :- bind_vars_to_name(XS).
bind_vars_to_name([]).

in_stream((IS,_,_), IS).
out_stream((_,OS,_), OS).

% Reads defs until eof or endclass %

not_unified(A,B) :- not(unifiable(A,B,[])).

parse_defs(S, DefLs, End) :- in_stream(S, IS),
	read_term(IS, T, [variable_names(Eqs)]),
	not_unified(T, End),
	!,
	(not_unified(T, end_of_file) ; throw('Unexpected end of stream')),
	(parse_def(S, Eqs, T, DefL) ; throw('Could not parse definition')),
	bind_vars_to_name(Eqs),
	append(DefL, DefR, DefLs),
	parse_defs(S, DefR, End).
parse_defs(_, [], _).

% Parses a single definition %
parse_def(S, Eqs, T, [Def]) :- var(T), !, parse_field(S, T, Def).
parse_def(S, Eqs, T, Def) :- parse_include(S, T, Def).						
parse_def(S, Eqs, T, [Def]) :- parse_class(S, T, Def).
parse_def(S, Eqs, T, [Def]) :- parse_clause(S, Eqs, T, Def).

% Parses an oopl include, a bit of a hack for bootstrapping (implemented properly in oopl)
parse_include(S, :- oopl_include(Path), Defs) :-
	out_stream(S, Out), 
	open(Path, read, In), 
	parse_defs((In, Out, _), Defs, end_of_file), 
	close(In).

% Parses a field %
parse_field(_, X, field(X)) :- !.

% Parse a class header and body %
parse_class(S, T, class_def(H, Defs)) :-
	parse_header(S, T, H),
	parse_defs(S, Defs, endclass).
						
parse_header(_, class X extends Y, class_head(X,[Y])) :- atomic(X), atomic(Y), !.
parse_header(_, class X, class_head(X,[])) :- atomic(X).

% Parses a normal prolog clause %
parse_clause(_, Eqs, :- Xs, rule('', Ys)) :- !,
	tag_atoms(Eqs, Xs, Ys).

parse_clause(_, Eqs, P :- Xs, rule(Pd, Ys)) :- !,
	tag_predicate(Eqs, P, Pd),
	tag_atoms(Eqs, Xs, Ys).

parse_clause(_, Eqs, P, fact(Pd)) :- tag_predicate(Eqs, P, Pd).

tag_predicate(Eqs, P, P) :- atomic(P).
tag_predicate(Eqs, P, P2) :- not(atomic(P)),
	P =.. [Nm|Args],
	maplist(tag_atoms(Eqs), Args, Args2),
	P2 =.. [Nm|Args2].

is_named_member(X, [=(_,Y)|_]) :- X == Y.
is_named_member(X, [_|YS]) :- is_named_member(X, YS).

name_dont_care(Eqs, X) :- is_named_member(X, Eqs).
name_dont_care(Eqs, '_').

tag_atoms(Eqs, X, var(X)) :- var(X), !, name_dont_care(Eqs, X).
tag_atoms(Eqs, X, atom(X)) :- atomic(X), !.

tag_atoms(Eqs, P, compound(P2)) :- 
	functor(P, _, _), P =.. [Nm|Args], 
	maplist(tag_atoms(Eqs), Args, Args2),
	P2 =.. [Nm|Args2].