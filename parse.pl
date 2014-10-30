:- op(100, xfy, ::).
:- op(99, fx, class).
:- op(100, xfx, extends).
test :- open('./test.oopl', read ,S) , parse_defs((S, user_output, Eqs)).

%%%%%%%%%%
% Parser %
%%%%%%%%%%

% Helper predicates %

var_name(X, [=(X,Y)|_], Y).
var_name(X, [_|Eqs], Y) :- var_name(X, Eqs, Y).
var_name_scope(X, [L|_], Y) :- var_name(X, L, Y).
var_name_scope(X, [_|LS], Y) :- var_name_scope(X, LS, Y).

in_stream((IS,_,_), IS).
out_stream((_,OS,_), OS).
bindings((_,_,EqsLs), EqsLs).
append_bindings((IS,OS,EqsLs), Eqs, (IS,OS,[Eqs|EqsLs]).


% Reads defs until eof or endclass %

parse_defs(S,DefLs,[Def|DefLs]) :- in_stream(S, IS),
						read_term(IS, T, [variable_names(Eqs)]),
						nl,
						T \= end_of_file, T \= endclass, 
						append_bindings(S, Eqs, Sd),
						!, parse_def(Sd, T, ),
						parse_defs(S).
parse_defs(S, DefLs, DefLs).

% Parses a single definition %
						
parse_def(S, T, Def) :- parse_class(S, T).
parse_def(S, T, Def) :- parse_clause(S, T).

% Parses a normal prolog clause %	
parse_clause((_, OS, _), P :- Xs) :- 	parse_predicate(S, P), 
									parse_functor(S, Xs, Ys), write(OS, Ys).

parse_clause(S, P) :- parse_predicate(S, P).

% Parses the rule part of a clause %
parse_predicate((_, OS, _), P) :- write(OS, P), write(OS, ' :- ').

% Parses a compound or atom %
% In this case we have to rewrite infix :: as a predicate %
parse_functor((_, OS, _), X::Y, Z) :- write(OS, '::('),
								write(OS, X),
								write(OS, ','),
								write(OS, Y),
								write(OS, ', '),
								write(OS, Z ),
								write(OS, '),').
% Atom case %						
parse_functor(_, P, P) :- functor(P, P, 0).

% Normal predicate %
parse_functor(S, P, P_) :- P \= _::_, functor(P, X, N), N \= 0,
								functor(P_,X,N),
								parse_functor_args(S, 1, N, P, P_).

% Go through each argument and process it in the same way %								
parse_functor_args(S, Ns, Ne, P, Pd) :- Ns =< Ne, arg(Ns, P, V),
										parse_functor(S, V, Vd),
										arg(Ns, Pd, Vd),
										Nsd is Ns + 1, 
										parse_functor_args(S, Nsd, Ne, P, Pd).

% Parse a class header and body %
parse_class((IS, OS, E), T) :- 	parse_header((IS, OS, E), T, H),
						parse_defs((IS, OS, E)),
						(read(IS, endclass);throw('Unmatched class header')).
						
parse_header(S, class X extends Y, class_def(X,[Y])).
parse_header(S, class X, class_def(X,[])).


