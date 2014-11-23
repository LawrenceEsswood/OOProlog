:- ['./parse.pl'].

%main :- quick('./tests/test2',':- [\'standard.out.pl\'].\n').
main :- quick('./standard').

quick(X, Extras) :- atom_concat(X, '.oopl', X1), atom_concat(X, '.out.pl', X2), compile(X1, X2, Extras).
quick(X) :- quick(X,'').

compile(In, Out, Extras) :- 
	open(In, read , S), parse_defs((S, user_output, _), Defs, end_of_file), !,
	open(Out, write , S2), write(S2, Extras), generate_defs_top((user_input, S2, _), Defs, top), !.

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

%gets a field name and qoutes its
get_field_name_qoute(X, Y) :- get_field_name(X, Z), add_qoutes(Z, Y).

% Generates the rule that constructs a class type
make_class(S, Def, ScopeAbove, Scope) :- 
	get_class_name(Def, Name),
	get_class_body(Def, Defs),
	get_all_names_of(get_field_name_qoute, Defs, Fields),
	get_all_names_of(get_predicate_pair(Scope), Defs, Predicates),
	make_class_type_functor(Name, ScopeAbove, F),
	make_class_type_args(ScopeAbove, Args),
	make_class_type_body(Name, Fields, Predicates, Bdy, none),
	P =.. [F | Args],
	out_stream(S, Out), write(Out, (P :- Bdy)), write(Out, '.\n').

% The name of a predicate after generation
make_predicate_name(PredicateName, Scope, Result) :- scope_prefix(Scope, Prefix), atomic_list_concat(['p',Prefix, PredicateName], Result).
% The name of the rule for getting a classes type info
make_class_type_functor(ClassName, Scope, Result) :- scope_prefix(Scope, Prefix), atomic_list_concat([class_def, Prefix, ClassName], Result).
% The compound the represents a class type 
make_class_type_args(Scope, Bdy) :- add_this(['T'], Scope, Bdy).
% Easier to just use new
make_class_type_body(Name, Fields, Predicates, Bdy, Constructor) :- 
	expand_new(new('X', 'T', Name, Fields, Predicates, none, none, Constructor), _, NewComs),
	list_to_comma_functor([class_def_classType('X') | NewComs], Bdy). 

expand_new(Command, Con, [build_class(Type, Class), ::(Type, Constructor, Con) , ((Con = none) ; Call)]) :- 
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

% new is expanded into other predicates
resolve_arg_new(Scope, Reps, RepsOut, P, AllCommands) :-
	functor(P, new, _), !,
	expand_new(P, tmp(_), Commands),
	resolve_args(Scope, Reps, RepsOut, Commands, Commands1, Commands2),
	append(Commands2, Commands1, AllCommands).

%We may already have a replacement
resolve_arg(_,Reps, Reps, T, Td, []) :- member((T, Td), Reps), !.

%Might be an infix operator that will need upwrapping
resolve_arg(Scope, Reps, Reps3, X::Y, Tmp, Cmds3) :- 
	!, 
	resolve_arg(Scope, [(Xd::Yd,Tmp)|Reps], Reps2, X, Xd, Cmds1),
	resolve_arg(Scope, Reps2, Reps3, Y, Yd, Cmds2),
	append(Cmds2, [::(Xd,Yd,Tmp)|Cmds1], Cmds3).

%Atoms might refer to a class FIXME: also predicates when used with first order
resolve_arg(Scope, Reps, [(atom(X),Tmp)|Reps], atom(X), Tmp, [C]) :-
	lookup(Scope, class(X), _),!,
	make_class_type_functor(X, Scope, F),
	add_this([Tmp], Scope, Ls),
	C =.. [F|Ls].

resolve_arg(_, R, R, atom(X), Y, []) :- !, add_qoutes(X, Y).

%Variables might be members
resolve_arg(Scope, Reps, [(var(Arg),Tmp)|Reps], var(Arg), Tmp, [C]) :-
	lookup(Scope, field(Arg), _), !,
	C = ::('This', ArgC, Tmp),
	add_qoutes(Arg, ArgC).
resolve_arg(_, R, R, var(Arg), Arg, []) :- !.

% Tmps will never mean anything else in context
resolve_arg(_, R, R, tmp(Arg), Arg, []) :- !.

% This is the case where we have a compound, might be need to add a prefix and/or 'This'
resolve_arg(Scope, Reps, RepsOut, P, Pd, Commands) :- 
	P =.. [Nm|Args],
	lookup(Scope, pred(Nm), PredScope), !,
	make_predicate_name(Nm, Scope, NewName),
	resolve_args(Scope, Reps, RepsOut, Args, Args2, Commands),
	add_this(Args2, PredScope, Args3),
	Pd =.. [NewName|Args3].
resolve_arg(Scope, Reps, RepsOut, P, Pd, Commands) :-
	P =.. [Nm|Args],
	resolve_args(Scope, Reps, RepsOut, Args, Argsd, Commands),
	Pd =.. [Nm|Argsd].

resolve_args(Scope, Reps, RepsOut2, [Arg|Args],[NewArg|NewArgs],AllCommands) :-
	resolve_arg(Scope, Reps, RepsOut, Arg, NewArg,Commands),
	resolve_args(Scope, RepsOut, RepsOut2, Args, NewArgs, Commands2),
	append(Commands, Commands2, AllCommands).

resolve_args(_, R, R,[],[],[]).

resolve_args_comma_functor(B, Scope, Reps, RepsOut2, Res) :-
	functor(B, ',', _),
	arg(1, B, Arg), arg(2, B, Bdy),
	resolve_args_comma_functor(Bdy, Scope, Reps, RepsOut, Res1),
	((	
		resolve_arg_new(Scope, RepsOut, RepsOut2, Arg, CommandsNew), !,
		append(CommandsNew, Res1, Res)
	);(
		resolve_arg(Scope, RepsOut, RepsOut2, Arg, NewArg, Commands1),
		append(Commands1, [NewArg|Res1], Res)
	)).

resolve_args_comma_functor(B, Scope, Reps, RepsOut, Res) :-
	functor(B, Nm, _),
	(Nm \= ','),
	((
		resolve_arg_new(Scope, Reps, RepsOut, B, Res), !
	);(
		resolve_arg(Scope, Reps, RepsOut, B, Bd, Commands),
		append(Commands, [Bd], Res)
	)).

add_this(Ls, Scope, Ls) :- scope_is_top(Scope).
add_this(Ls, Scope, ['This'|Ls]) :- not(scope_is_top(Scope)).

generate_def(S, fact(P), Scope) :- 
	P =.. [Nm|ArgLstIn], 
	resolve_args(Scope, [], _, ArgLstIn, ArgLst, Commands),
	make_predicate_name(Nm, Scope, NewName),
	add_this(ArgLst, Scope, ArgLst2),
	make_from_fact(NewName, ArgLst2, Commands, Res),
	out_stream(S, Out), write(Out, Res), write(Out, '.\n').

%FIXME, we can look through all the replacements to work out if we need this
generate_def(S, rule(P, B), Scope) :-
	P =.. [Nm|ArgLstIn], 
	resolve_args(Scope, [], OutReps, ArgLstIn, ArgLst, Commands),
	resolve_args_comma_functor(B, Scope, OutReps, _, Res),
	append(Commands, Res, Body),
	make_predicate_name(Nm, Scope, NewName),
	add_this(ArgLst, Scope, ArgLst2),
	Pd =.. [NewName | ArgLst2],
	list_to_comma_functor(Body, CBdy),
	out_stream(S, Out), write(Out, (Pd :- CBdy)), write(Out, '.\n').

generate_def(S, class_def(H,B), Scope) :-
	get_class_name(class_def(H,B), N),
	new_scope(B, Scope, N, NewScope),
	make_class(S, class_def(H,B), Scope, NewScope),
	generate_defs(S, B, NewScope).

%Fields don't generate anything
generate_def(_, field(_), Scope) :- not(scope_is_top(Scope)); throw('Fields must occur within a class').