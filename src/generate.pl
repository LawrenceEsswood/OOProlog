:- ['./parse.pl'].

main :-
	open('./tests/test2.oopl', read , S), parse_defs((S, user_output, _), Defs, end_of_file), !,
	open('./tests/test2.out.pl', write ,S2), generate_defs_top((user_input, S2, _), Defs, top).
 
% Helper predicates (these should be moved into their own file)
list_to_comma_functor([X],X).
list_to_comma_functor([X,Y|Zs], (X,C)) :- list_to_comma_functor([Y|Zs], C).

add_qoutes(X,Y) :- atomic_list_concat(['\'', X, '\''], Y).

% Creates a tuple of the original name of a predicate, and its scoped name
pair_predicate(Scope, Name, (ID, Resolved)) :-
	make_predicate_name(Name, Scope, Resolved),
	add_qoutes(Name, ID).

get_predicate_pair(Scope, fact(P), Pair) :- functor(P, Name, _), pair_predicate(Scope, Name, Pair).
get_predicate_pair(Scope, rule(P, _), Pair) :- functor(P, Name, _), pair_predicate(Scope, Name, Pair).

% Generates the rule that constructs a class type
make_class(S, Def, Scope) :- 
	get_class_name(Def, Name),
	get_class_body(Def, Defs),
	get_all_names_of(get_field_name, Defs, Fields),
	get_all_names_of(get_predicate_pair(Scope), Defs, Predicates),
	make_class_type_functor(Name, Scope, F),
	make_class_type_args(Scope, Args),
	make_class_type_body(Name, Fields, Predicates, Bdy),
	P =.. [F | Args],
	out_stream(S, Out), write(Out, (P :- Bdy)), write(Out, '.\n').

% The name of a predicate after generation
make_predicate_name(PredicateName, Scope, Result) :- scope_prefix(Scope, Prefix), atomic_list_concat([Prefix, PredicateName], Result).
% The name of the rule for getting a classes type info
make_class_type_functor(ClassName, Scope, Result) :- scope_prefix(Scope, Prefix), atomic_list_concat([class_def, Prefix, ClassName], Result).
% The compound the represents a class type 
make_class_type_args(Scope, Bdy) :- add_this(['T'], Scope, Bdy).
% Easier to just use new
make_class_type_body(Name, Fields, Predicates, Bdy, Constructor) :- 
	expand_new(new('X', 'T',Name, Fields, Predicates, none, none, Constructor), NewComs), 
	list_to_comma_functor([class_def_classType('X') | NewComs], Bdy). 

expand_new(Command, [build_class(Type, Class), ::(Type, Constructor, Con) , Call]) :- 
	add_qoutes('Constructor', Constructor),
	Command =.. [new, Type, Class | Args],
	Call =.. [call, Con, Class | Args].

% Generates prolog from a single definition
make_from_fact(Nm, ArgLst, [], Res) :- Res =.. [Nm|ArgLst].
% FIXME this might not print properly
make_from_fact(Nm, ArgLst, Commands, (P1 :- P2)) :- Commands \= [], P1 =.. [Nm|ArgLst], list_to_comma_functor(Commands, P2).

% Gets the definitions for a class
get_class_body(class_def(_,X),X).

% Gets a name from a particular definition
get_class_name(class_def(class_head(X,_),_), X).
get_fact_name(fact(P), Name) :- functor(P, Name, _).
get_rule_name(rule(P,_), Name) :- functor(P, Name, _).
get_field_name(field(Name), Name).
get_all_names_of(Rule, [D|Defs],[N|Names]) :- call(Rule, D, N), !, get_all_names_of(Rule, Defs, Names).
get_all_names_of(Rule, [_|Defs], Names) :- get_all_names_of(Rule, Defs, Names).
get_all_names_of(_, [], []).

% Gets a name from a definition
get_name(Def, class(Name)) :- get_class_name(Def, Name).
get_name(Def, pred(Name)) :- get_fact_name(Def, Name).
get_name(Def, pred(Name)) :- get_rule_name(Def, Name).
get_name(Def, field(Name)) :- get_field_name(Def, Name).

% Gets all the names defined at this scope
get_defined_names([],[]).
get_defined_names([D|Ds],[N|Ns]) :- get_name(D,N), get_defined_names(Ds, Ns).

%Gets a scopes prefix
scope_prefix(scp(Name,_,_), Name).
scope_prefix(top,'_').
scope_defines(scp(_,Names,_), Names).
scope_parent(scp(_,_,P), P).
scope_is_top(scp(_,_,top)).

% looks up a name to get its containing scope
lookup(Scope, Arg, Scope) :- scope_defines(Scope, Names), member(Arg, Names), !.
lookup(Scope, Arg, Res) :- scope_parent(Scope, Next), lookup(Next, Arg, Res).

% Creates a new scope
new_scope(Defs, OldScope, Name, scp(NewName,Defined, OldScope)) :- 
	scope_prefix(OldScope, OldName),
	atomic_list_concat(['_', Name, OldName], NewName), 
	get_defined_names(Defs, Defined).

% Top level predicate for generating prolog
generate_defs_top(S, Defs, Scope) :- new_scope(Defs, Scope, '', NewScope), generate_defs(S, Defs, NewScope).

% Generates prolog from a list of definitions
generate_defs(S, [Def|Defs], Scope) :- generate_def(S, Def, Scope), generate_defs(S, Defs, Scope).
generate_defs(_, [], _).



%Atoms might refer to a class
resolve_arg(Scope, atom(X), Tmp, [C]) :-
	lookup(Scope, class(X), _),!,
	make_class_type_functor(X, Scope, F),
	add_this([Tmp], Scope, Ls),
	C =.. [F|Ls].

resolve_arg(_, atom(X), X, []) :- !.

%Variables might be members
resolve_arg(Scope, var(Arg), Tmp, [::('This', ArgC, Tmp)]) :-
	lookup(Scope, field(Arg), _), !,
	add_qoutes(Arg, ArgC).
resolve_arg(_, var(Arg), Arg, []) :- !.

% Tmps will never mean anything in context
resolve_arg(_, tmp(Arg), Arg, []) :- !.

% This is the case where we have a compound, might be need to add a prefix and/or 'This'
resolve_arg(Scope, P, Pd, Commands) :- 
	P =.. [Nm|Args],
	lookup(Scope, pred(Nm), PredScope), !,
	scope_prefix(PredScope, Prefix),
	atom_concat(Prefix, Nm, NewName),
	resolve_args(Scope, Args, Args2, Commands),
	add_this(Args2, PredScope, Args3),
	Pd =.. [NewName|Args3].
resolve_arg(Scope, P, Pd, Commands) :-
	P =.. [Nm|Args],
	resolve_args(Scope, Args, Argsd, Commands),
	Pd =.. [Nm|Argsd].

resolve_args(Scope, [Arg|Args],[NewArg|NewArgs],AllCommands) :-
	resolve_arg(Scope, Arg, NewArg,Commands), 
	resolve_args(Scope, Args, NewArgs, Commands2),
	append(Commands, Commands2, AllCommands).

resolve_args(_,[],[],[]).

resolve_args_comma_functor(B, Scope, Res) :-
	functor(B, ',', _),
	arg(1, B, Arg), arg(2, B, Bdy),
	resolve_arg(Scope, Arg, NewArg, Commands1),
	resolve_args_comma_functor(S, Bdy, Scope, Res1),
	append(Commands1, [NewArg|Res1], Res).

resolve_args_comma_functor(B, Scope, Res) :-
	functor(B, Nm, _),
	(Nm \= ','),
	resolve_arg(Scope, B, Bd, Commands),
	append([Bd], Commands, Res).

add_this(Ls, Scope, Ls) :- scope_is_top(Scope).
add_this(Ls, Scope, ['This'|Ls]) :- not(scope_is_top(Scope)).

generate_def(S, fact(P), Scope) :- 
	P =.. [Nm|ArgLstIn], resolve_args(Scope, ArgLstIn, ArgLst, Commands),
	make_predicate_name(Nm, Scope, NewName),
	add_this(ArgLst, Scope, ArgLst2),
	make_from_fact(NewName, ArgLst2, Commands, Res),
	out_stream(S, Out), write(Out, Res), write(Out, '.\n').

generate_def(S, rule(P, B), Scope) :-
	P =.. [Nm|ArgLstIn], resolve_args(Scope, ArgLstIn, ArgLst, Commands),
	resolve_args_comma_functor(B, Scope, Res),
	append(Commands, Res, Body),
	make_predicate_name(Nm, Scope, NewName),
	add_this(ArgLst, Scope, ArgLst2),
	Pd =.. [NewName | ArgLst2],
	list_to_comma_functor(Body, CBdy),
	out_stream(S, Out), write(Out, (Pd :- CBdy)), write(Out, '.\n').

generate_def(S, class_def(H,B), Scope) :-
	make_class(S, class_def(H,B), Scope),
	get_class_name(class_def(H,B), N),
	new_scope(B, Scope, N, NewScope),
	generate_defs(S, B, NewScope).

%Fields don't generate anything
generate_def(_, field(_), Scope) :- not(scope_is_top(Scope)); throw('Fields must occur within a class').