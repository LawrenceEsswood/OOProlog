:- op(100, xfy, ::).
:- op(99, fx, class).
:- op(100, xfx, extends).

test(Defs) :- open('./test.oopl', read ,S) , parse_defs((S, user_output, _), Defs, end_of_file).

%%%%%%%%%%
% Parser %
%%%%%%%%%%

% Helper predicates %

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

parse_defs(S, [Def|DefLs], End) :- in_stream(S, IS),
	read_term(IS, T, [variable_names(Eqs)]),
	nl,
	T \= End,
	!,
	bind_vars_to_name(Eqs),
	parse_def(S, T, Def),
	parse_defs(S, DefLs, End).
parse_defs(_, [], _).

% Parses a single definition %
						
parse_def(S, T, Def) :- parse_class(S, T, Def).
parse_def(S, T, Def) :- parse_clause(S, T, Def).

% Parses a normal prolog clause %
parse_clause(S, P :- Xs, rule(Pd, Ys)) :-
	parse_predicate(S, P, Pd), 
	parse_functor_top(S, Xs, Ys).

parse_clause(S, P, fact(Pd)) :- parse_predicate(S, P, Pd).

% Parses the rule part of a clause %
parse_predicate(_, P, P) :- functor(P, _, _).

% Parses a compound or atom %

parse_functor_top(S, P, Pd) :- functor(P, ',', _) , parse_functor_args_together(S, P, Pd).
parse_functor_top(S, P, Pdd) :- 
	functor(P, X, _), X \= ',', 
	parse_functor(S, P, Pd, [], Nst),
	append(Nst, [Pd], Lst),
	Pdd =.. [','|Lst].

% In this case we have to rewrite infix ::, un-nest as a predicate % TODO
parse_functor(S, X::Y, Z, Nested, Nested2) :- 
	parse_functor(S, X, Xd, [::(Xd, Yd, Z)|Nested], Nested1),
	parse_functor(S, Y, Yd, Nested1, Nested2).
	
% Atom case %
parse_functor(_, P, P, Nst, Nst) :- functor(P, P, 0).

% Normal predicate %
parse_functor(S, P, Pd, Nst, Nstdd) :- 
	P \= _::_, functor(P, _, N), N \= 0,
	parse_functor_args_seperate(S, P, Pd, Nstd),
	append(Nst, Nstd, Nstdd).
	
% Go through each argument and process it in the same way %

% Keep the infix operations we lift out seperate %
parse_functor_args_seperate(S, P, Pd, NstDs) :-
	findall((Vd, NstD), parse_functor_args(S, P, Vd, NstD), Result),
	flatten_pair(Result, Lst, NstDs),
	functor(P, X, _),
	Pd =.. [X|Lst].

% Interleave the lifted out operations %
parse_functor_args_together(S, P, Pd) :-
	findall((Vd, NstD), parse_functor_args(S, P, Vd, NstD), Result),
	flatten_single(Result, Lst),
	functor(P, X, _),
	Pd =.. [X|Lst].

parse_functor_args(S, P, Vd, NstD) :-
	arg(_, P, V),
	parse_functor(S, V, Vd, [], NstD).

% Parse a class header and body %
parse_class(S, T, class_def(H, Defs)) :-
	parse_header(S, T, H),
	parse_defs(S, Defs, endclass).
						
parse_header(_, class X extends Y, class_head(X,[Y])).
parse_header(_, class X, class_head(X,[])).