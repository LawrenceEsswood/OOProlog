:- op(100, xfy, ::).
:- op(99, fx, class).
:- op(100, xfx, extends).

test(Defs) :- open('./test.oopl', read ,S) , parse_defs((S, user_output, Eqs), [], Defs).

%%%%%%%%%%
% Parser %
%%%%%%%%%%

% Helper predicates %

%Might not need these%
var_name(X, [=(X,Y)|_], Y).
var_name(X, [_|Eqs], Y) :- var_name(X, Eqs, Y).
var_name_scope(X, [L|_], Y) :- var_name(X, L, Y).
var_name_scope(X, [_|LS], Y) :- var_name_scope(X, LS, Y).
bindings((_,_,EqsLs), EqsLs).
append_bindings((IS,OS,EqsLs), Eqs, (IS,OS,[Eqs|EqsLs]).
get_name((_,_,E), Var, Name) :- var_name_scope(Var, E, Name).

bind_vars_to_name([=(X,X)|XS]) :- bind_vars_to_name(XS).
bind_vars_to_name([]).

in_stream((IS,_,_), IS).
out_stream((_,OS,_), OS).

% Reads defs until eof or endclass %

parse_defs(S,DefLs,[Def|DefLs]) :- in_stream(S, IS),
	read_term(IS, T, [variable_names(Eqs)]),
	nl,
	T \= end_of_file, T \= endclass, 
	!,
	bind_vars_to_name(Eqs),
	append_bindings(S, Eqs, Sd),
	parse_def(Sd, T, ),
	parse_defs(S).
parse_defs(S, DefLs, DefLs).

% Parses a single definition %
						
parse_def(S, T, Def) :- parse_class(S, T, Def).
parse_def(S, T, Def) :- parse_clause(S, T, Def).

% Parses a normal prolog clause %
parse_clause(S, P :- Xs, rule(PD, Ys)) :-
	parse_predicate(S, P, PD), 
	parse_functor(S, Xs, Ys).

parse_clause(S, P, fact(PD)) :- parse_predicate(S, P, PD).

% Parses the rule part of a clause %
parse_predicate(S, P, P) :- functor(P, _, _).

% Parses a compound or atom %

% In this case we have to rewrite infix ::, un-nest as a predicate % TODO
parse_functor(S, X::Y, ::(XD,YD,Z)) :- parse_
	
% Atom case %
parse_functor(_, P, P) :- functor(P, P, 0).

% List case %
parse_functor(S, P, Pd) :- functor(P, ,, N).

% Normal predicate %
parse_functor(S, P, P_) :- 
	P \= _::_, functor(P, X, N), N \= 0,
	functor(P_,X,N),
	parse_functor_args(S, 1, N, P, P_).

% Go through each argument and process it in the same way %
parse_functor_args(S, Ns, Ne, P, Pd) :-
	Ns =< Ne, arg(Ns, P, V),
	parse_functor(S, V, Vd),
	arg(Ns, Pd, Vd),
	Nsd is Ns + 1,
	parse_functor_args(S, Nsd, Ne, P, Pd).

% Parse a class header and body %
parse_class(S, T, class_def(H, Defs)) :-
	parse_header(S, T, H),
	parse_defs(S,[],Defs),
	in_stream(S, IS),
	(read(IS, endclass);throw('Unmatched class header')).
						
parse_header(S, class X extends Y, class_head(X,[Y])).
parse_header(S, class X, class_head(X,[])).