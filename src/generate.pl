:- ['./parse.pl'].

main :- open('./tests/test.oopl', read ,S) , parse_defs((S, user_output, _), Defs, end_of_file), !,
		open('./tests/test.out.pl', write ,S2) generate_defs_top((user_input, S2, _), Defs, top).

% TODO
make_class(S, Def, Scope) :- 
	get_class_name(Def, Scope, Name),
	make_class_type_functor(Name, Scope, F),
	make_class_type_body(Name, Scope, Bdy),
	P =.. [F, Bdy].

make_predicate_name(PredicateName, Scope, Result) :- scope_name(Scope, Prefix), atomic_list_concat([Prefix, PredicateName], Result).

% The name of the rule for getting a classes type info
make_class_type_functor(ClassName, Scope, Result) :- scope_name(Scope, Prefix), atomic_list_concat([class_def, Prefix, ClassName], Result).
% The compound the represents a class type TODO
make_class_type_body(Name, Scope, blah_blah).

% Generates prolog from a single definition
make_from_fact(Nm, ArgLst, [], Res) :- Res =.. [Nm|ArgLst].
% FIXME this might not print properly
make_from_fact(Nm, ArgLst, Commands, (P1 :- P2)) :- Commands \= [], P1 =.. [Nm|ArgLst], P2 =.. [','|Commands].

% Gets a name from a particular definition
get_class_name(class_def(class_head(X,_),_), X).
get_fact_name(fact(P), Name) :- functor(P, Name, _).
get_rule_name(rule(P,_), Name) :- functor(P, Name, _).
get_field_name(field(Name), Name).

% Gets a name from a definition
get_name(Def, class(Name)) :- get_class_name(Def, Name).
get_name(Def, pred(Name)) :- get_fact_name(Def, Name).
get_name(Def, pred(Name)) :- get_rule_name(Def, Name).
get_name(Def, field(Name)) :- get_field_name(Def, Name).

% Gets all the names defined at this scope
get_defined_names([],[]).
get_defined_names([D|Ds],[N|Ns]) :- get_name(D,N), get_defined_names(Ds, Ns).

%Gets a scopes prefix
scope_name(scp(Name,_,_), Name).
scope_name(top,'_').
% Creates a new scope
new_scope(Defs, OldScope, Name, scp(NewName,Defined, OldScope)) :- 
	scope_name(OldScope, OldName),
	atomic_list_concat(['_', Name, OldName], NewName), 
	get_defined_names(Defs, Defined).

% Top level predicate for generating prolog
generate_defs_top(S, Defs, Scope) :- new_scope(Defs, Scope, '', NewScope), generate_defs(S, Defs, NewScope).

% Generates prolog from a list of definitions
generate_defs(S, [Def|Defs], Scope) :- generate_def(S, Def, Scope), generate_defs(S, Defs, Scope).
generate_defs(_, [], _).

% looks up a name to get a prefix
lookup(scp(Prefix, Names, Next), Arg, Prefix) :- contains(Names, Arg), !.
lookup(scp(Prefix, Names, Next), Arg, Res) :- lookup(Next, Arg, Res).

%Atoms might refer to a class
resolve_arg(Scope, atm(X), Tmp, [C]) :- 
	lookup(Scope, class(X), Prefix),!,
	make_class_type_functor(X, Scope, F),
	C ..= [F,'This',Tmp].

resolve_arg(_, atm(X), X, []).

%Variables might be members
resolve_arg(Scope, var(Arg), Tmp, [::(This, Arg, Tmp)]) :-
	lookup(Scope, field(Arg), X), !.
resolve_arg(_, var(Arg), Arg, []).

% Tmps will never mean anything in context
resolve_arg(Scope, tmp(Arg), Arg, []).

% This is the case where we have a compound, might be need to add a prefix
resolve_arg(Scope, P, Pd, Commands) :- 
	P =.. [Nm|Args],
	lookup(Scope, pred(Nm), Prefix), !,
	make_predicate_name(Nm, Prefix, NewName),
	resolve_args(Scope, Args, Argsd, Commands),
	Pd ..= [NewName|Argsd].
resolve_arg(Scope, P, Pd, Commands) :-
	P =.. [Nm|Args],
	resolve_args(Scope, Args, Argsd, Commands),
	Pd ..= [Nm|Argsd].

resolve_args(Scope, [Arg|Args],[NewArg|NewArgs],AllCommands) :-
	resolve_arg(Scope, Arg, NewArg,Commands), 
	resolve_args(Scope, Args, NewArgs, Commands2),
	append(Commands, Commands2, AllCommands).

resolve_args([],[],[]).

generate_def(S, fact(P), Scope) :- 
	P =.. [Nm|ArgLstIn], resolve_args(Scope, ArgLstIn, ArgLst, Commands),
	make_predicate_name(Nm, Scope, NewName),
	make_from_fact(NewName, ArgLst, Commands, Res),
	out_stream(S, Out), write(Out, Res), write(Out, '\n').

resolve_args_comma_functor(S, B, Scope, Res) :- 
	functor(B, ',', _),
	arg(1, B, Arg), arg(2, B, Bdy),
	resolve_arg(Scope, Arg, NewArg, Commands1),
	resolve_args_comma_functor(S, Bdy, Scope, Res1),
	append(Commands1, [NewArg|Res1], Res).

resolve_args_comma_functor(S, B, Scope, Res) :-
	functor(B, Nm, _),
	Nm /= ',',
	resolve_arg(Scope, B, Bd, Commands),
	append(Commands, Bd, Res).

generate_def(S, rule(P, B), Scope)) :- 
	P =.. [Nm|ArgLstIn], resolve_args(Scope, ArgLstIn, ArgLst, Commands),
	resolve_args_comma_functor(S, B, Scope, Res),
	append(Commands, Res, Body),
	make_predicate_name(Nm, Scope, NewName),
	Pd =.. [NewName|ArgLst],
	CBdy =.. [','|Body], %Again this might not work
	out_stream(S, Out), write(Out, (Pd :- CBdy), write(Out, '\n').

generate_def(S, class_def(H,B), Scope) :- 
	make_class(S, class_def(H,B), Scope), 
	get_class_name(class_def(H,B), N), 
	new_scope(B, Scope, N, NewScope), 
	generate_defs(S, B, NewScope).

%Fields don't generate anything
generate_def(S, field(F), Scope).