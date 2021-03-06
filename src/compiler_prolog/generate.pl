:- ['./parse.pl'].
:- ['../src/utils/utils.pl'].

%%%%%%%%%%%%
% Compiler %
%%%%%%%%%%%%

%Compilation main entry point. Takes an in file, and an out file. Extras will be appended to the start. Current options are:
% [no_munge, no_commas, no_interpret]

compile(In, Out, Extras, Opts) :- 
	open(In, read , S), parse_defs((S, user_output, _), Defs, end_of_file), !,
	open(Out, write, S2), write_standard(S2, Extras), generate_defs_top((user_input, S2, _), Defs, Opts), !,
	close(S), close(S2).

% An oopl file with name X, outputs it to X.out.pl
quick(X, Extras, Opts) :- atom_concat(X, '.oopl', X1), atom_concat(X, '.out.pl', X2), compile(X1, X2, Extras, Opts).
% With no extras
quick(X, Opts) :- quick(X,'',Opts).
% With no options
quick(X) :- quick(X, []).

% Compile with the standard heading and options
generic_compile(In, Out) :- generic_compile(In, Out, []).

% Compile with the standard heading
generic_compile(In, Out, Opts) :- compile(In, Out,
':- [''./standard.out.pl''].
', Opts).

quick_generic(In) :- atom_concat(In, '.oopl', X1), atom_concat(In, '.out.pl', X2), generic_compile(X1, X2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checks if a predicate is a constructor for the class defined at this scope
is_constructor(Scope, pred(Name, N), pred(Name, N)) :-
	scope_header(Scope, Hd),
	get_header_name(Hd, ClassNm),
	Name = ClassNm.

is_not_constructor(Scope, P, P) :- not(is_constructor(Scope, P, _)).

% Creates a tuple of the original predicate, and its scoped name.
pair_predicate(Scope, Name, (Name, Resolved)) :-
	make_predicate_name(Name, Scope, Resolved).

%gets a field name and qoutes its //TODO redundant
get_field_name_qoute(X, Y) :- get_field_name(X, Y).

%TODO allow more than one parent
find_parents_definitions(_, [], [],[],[]).
find_parents_definitions(Scope, [Parent], Fields, Predicates, SuperPreds) :-
	(lookup(Scope, class(Parent), PDefScope) ; throw_atoms(['Could not find a class of name ', Parent,'.'])),
	(scope_of(PDefScope, Parent, ParentScope) ; throw_atoms([Parent, ' is not a class.'])),
	scope_header(ParentScope, ParentHeader),
	find_class_definitions(ParentScope, ParentHeader, Fields, Predicates, SuperPreds).

make_super_call(CRes, PRes, N , L :- R) :- 
	tmp_lst(N, Ts),
	L =.. [CRes, 'This'|Ts],
	R =.. [PRes, 'This'|Ts].

predicate_inherit([], TotalPairs, TotalPairs, []).
predicate_inherit([(pred(CName,NC), CRes)|Ps], ParentPs, TotalPairs, [Sup|SuperPreds]) :- 
	member((pred(CName,NP), PRes), ParentPs),
	!,
	delete(ParentPs, (pred(CName,NP), PRes), ParentPsWO),
	make_super_call(CRes, PRes, NP, Sup),
	predicate_inherit([(pred(CName,NC), CRes)|Ps], ParentPsWO, TotalPairs, SuperPreds).
predicate_inherit([(CName, CRes)|Ps], ParentPs, [(CName, CRes)|TotalPairs], SuperPreds) :-
	predicate_inherit(Ps, ParentPs, TotalPairs, SuperPreds).


find_class_definitions(Scope, Hchild, Fields, Predicates, SuperPreds) :-
	scope_defines(Scope, Names),
	filter(get_field_name_qoute,Names,ThisFields),
	filter(is_not_constructor(Scope), Names, Names2),
	filter(pair_predicate(Scope), Names2, ThisPredicates),
	get_header_parents(Hchild, Parents),
	find_parents_definitions(Scope, Parents, ParentFields, ParentPredicates, ParentSuperPreds),
	(intersection(ThisFields, ParentFields,[]), ! ; throw('Cannot redefine a parents field.')),  %Might allow redefinition, use union
	append(ParentFields, ThisFields, Fields),
	predicate_inherit(ThisPredicates, ParentPredicates, Predicates, ThisSuperPreds),
	append(ParentSuperPreds, ThisSuperPreds, SuperPreds).

not_same_pred_name(Nm, (pred(Nm2,N), Res), (pred(Nm2,N), Res)) :- Nm \= Nm2.

get_unique_predicates([],[]).
get_unique_predicates([(pred(Nm,_),Res)|Ps], [(Nm,Res)|OPs]) :-
	filter(not_same_pred_name(Nm),Ps,FPs),
	get_unique_predicates(FPs, OPs).

none_if_empty(_, [], none).
none_if_empty(Scope, [X|_], Y) :- make_predicate_name(X, Scope, Y).

% Generates the rule that constructs a class type
make_class(S, Def, ScopeAbove, Scope, SuperPreds) :-
	get_class_name(Def, Name),
	get_class_header(Def, Hdr),
	find_class_definitions(Scope, Hdr, Fields, Predicates, SuperPreds),
	get_unique_predicates(Predicates, UPredicates),
	make_class_type_functor(Name, ScopeAbove, F),
	make_class_type_args(ScopeAbove, Args),
	scope_opts(Scope, Opts),
	scope_defines(Scope, Names),
	filter(is_constructor(Scope), Names, Constructors),
	none_if_empty(Scope, Constructors, Constructor),
	make_class_type_body(Opts, Name, Fields, UPredicates, Bdy, Constructor),
	P =.. [F | Args],
	out_stream(S, Out), write_standard(Out, (P :- Bdy)), write(Out, '.\n').

% The name of a predicate after generation
append_if_not_blank(Prefix, PredicateName, Result) :- Prefix \= '', atomic_list_concat(['p',Prefix, PredicateName], Result).
append_if_not_blank('', PredicateName, PredicateName).

make_predicate_name(pred('', _), _, '').
make_predicate_name(pred(PredicateName, _), Scope, Result) :- scope_prefix(Scope, Prefix), append_if_not_blank(Prefix, PredicateName, Result).

% The name of the rule for getting a classes type info
make_class_type_functor(ClassName, Scope, Result) :- scope_prefix(Scope, Prefix), atomic_list_concat([class_def, Prefix, ClassName], Result).
% The compound the represents a class type 
make_class_type_args(Scope, Bdy) :- add_this(['T'], Scope, Bdy).
% Easier to just use new
make_class_type_body(Opts, Name, Fields, Predicates, Bdy, Constructor) :- 
	maplist(add_qoutes, Fields, QFields),
	expand_new(Opts, new('X', 'T', Name, QFields, Predicates, none, none, Constructor), _, NewComs),
	list_to_comma_functor([class_def__classType('X') | NewComs], Bdy).

expand_new(Opts, Command, Con, [build_class(Type, Class), ::(Type, Constructor, Con), ((Con = none) ; Call)]) :- 
	Command =.. [new, Type, Class | Args],
	add_qoutes('Constructor', Constructor),
	Call =.. [call, Con, Class | Args].

% Generates prolog from a single definition
make_from_fact(Nm, ArgLst, [], Res) :- Res =.. [Nm|ArgLst].
% FIXME this might not print properly
make_from_fact(Nm, ArgLst, Commands, (P1 :- P2)) :- Commands \= [], P1 =.. [Nm|ArgLst], list_to_comma_functor(Commands, P2).

% Gets the definitions for a class
get_class_body(class_def(_,X),X).

% Gets a name from a particular definition
get_class_name(class_def(class_head(X,_),_), X).
get_class_header(class_def(Hd, _), Hd).
get_header_parents(class_head(_,P), P).
get_header_name(class_head(N,_), N).
get_fact_name(fact(P), Name/N) :- functor(P, Name, N), Name \= ''.
get_rule_name(rule(P,_), Name/N) :- functor(P, Name, N), Name \= ''.
get_field_name(field(Name), Name).

% Gets a name from a definition
get_name(Def, class(Name)) :- get_class_name(Def, Name).
get_name(Def, pred(Name,N)) :- get_fact_name(Def, Name/N).
get_name(Def, pred(Name,N)) :- get_rule_name(Def, Name/N).
get_name(Def, field(Name)) :- get_field_name(Def, Name).

% Gets all the names defined at this scope
get_defined_names([],[]).
get_defined_names([D|Ds],[N|Ns]) :- get_name(D,N), !, get_defined_names(Ds, Ns).
get_defined_names([_|Ds], Ns) :- get_defined_names(Ds, Ns).

% Getters for scope
scope_prefix(scp(_,Name,_,_,_,_), Name).
scope_prefix(top,'_').
scope_opts(scp(Opts,_,_,_,_,_), Opts).
scope_defines(scp(_,_,_,Names,_,_), Names).
scope_parent(scp(_,_,_,_,_,P), P).
scope_is_top(scp(_,_,_,_,_,top)).
scope_header(scp(_,_,Hdr,_,_,_), Hdr).
scope_class_scopes(scp(_,_,_,_,CS,_), CS).

% Creates a new scope
new_scope(Opts, Defs, Hd, OldScope, Name, S) :- 
	S = scp(Opts, NewName, Hd, Defined, ClassScopes, OldScope),
	scope_prefix(OldScope, OldName),
	(	(member('no_munge', Opts), NewName = '');
		atomic_list_concat(['_', Name, OldName], NewName)
	),
	get_defined_names(Defs, Defined),
	all_new_class_scopes(Opts, Defs, S, ClassScopes).

% Save the top of the scope to the output for the interpreter
dump_scope(S, scp(_,_,_,Names,_,_)) :- 
	out_stream(S, Out), 
	write_canonical(Out, defined_names(Names)),
	write_canonical(Out, '.
:- [''./interpret.pl''].
:- interpret.
').

% Recreates the top scope from given names	
load_scope(Names, scp(['no_qoutes'],'__','top',Names,[],top)).

all_new_class_scopes(Opts, Defs, Scope, CSS) :- filter(new_class_scope(Opts, Scope), Defs, CSS).
new_class_scope(Opts, ScopeAbove, C, cs(N, ClassScope)) :-
	get_class_name(C, N),
	get_class_header(C, H),
	get_class_body(C, B),
	new_scope(Opts, B, H, ScopeAbove, N, ClassScope).

% looks up a name to get its containing scope
lookup(Scope, Arg, ScopeRes) :- lookup_nesting(Scope, Arg, ScopeRes), !.
lookup(Scope, Arg, ScopeRes) :- lookup_parent(Scope, Arg, ScopeRes).

lookup_here(Scope, Arg, Scope) :- scope_defines(Scope, Names), member(Arg, Names).
lookup_nesting(Scope, Arg, Res) :- scope_parent(Scope, Next), lookup_nesting(Next, Arg, Res).
lookup_nesting(Scope, Arg, Res) :- lookup_here(Scope, Arg, Res), !.
lookup_parent(Scope, Arg, Res) :-
	scope_header(Scope, Hdr),
	get_header_parents(Hdr, [P]),
	lookup(Scope, class(P), PDefScope),
	scope_of(PDefScope, P, PScope),
	lookup_parent(PScope, Arg, Res).
lookup_parent(Scope, Arg, Res) :- lookup_here(Scope, Arg, Res), !.

%trys to find a scope of a certain class
scope_of(Scope, Pname, Scp) :- scope_class_scopes(Scope, CS), member(cs(Pname, Scp), CS).

%Walks the parse tree finding out names to create scope

% Top level predicate for generating prolog
generate_defs_top(S, Defs, Opts) :- new_scope(Opts, Defs, top, top, '', NewScope), generate_defs(S, Defs, NewScope), (member('no_interpret',Opts);dump_scope(S, NewScope)).

% Generates prolog from a list of definitions
generate_defs(S, [Def|Defs], Scope) :- (generate_def(S, Def, Scope), ! ; 
										throw('Failed to generate from def.')),
										generate_defs(S, Defs, Scope).
generate_defs(_, [], _).

% new is expanded into other predicates
resolve_arg_new(Scope, Reps, RepsOut, compound(P), AllCommands) :-
	functor(P, new, _), !,
	scope_opts(Scope, Opts),
	expand_new(Opts, P, tmp(_), Commands),
	resolve_args(Scope, Reps, RepsOut, Commands, Commands1, Commands2, false),
	append(Commands2, Commands1, AllCommands).

%We may already have a replacement
resolve_arg(_,Reps, Reps, T, Td, [], _) :- member((T, Td), Reps), !.

%Might be an infix operator that will need upwrapping
resolve_arg(Scope, Reps, Reps3, compound(X::Y), Tmp, Cmds4, _) :-
	!, 
	resolve_arg(Scope, [(compound(X::Y),Tmp)|Reps], Reps2, X, Xd, Cmds1, false),
	resolve_arg(Scope, Reps2, Reps3, Y, Yd, Cmds2, false),
	append(Cmds1, Cmds2, Cmds3),
	append(Cmds3, [::(Xd,Yd,Tmp)], Cmds4).

%Atoms refers to predicate
resolve_arg(Scope, Reps, Reps, atom(X), P, [], true) :-
	lookup(Scope, pred(X, 0), PredScope), !,
	make_predicate_name(pred(X, 0), PredScope, Nm),
	add_this([], PredScope, Args),
	P =.. [Nm|Args].

% Atom refers to class
resolve_arg(Scope, Reps, [(atom(X),Tmp)|Reps], atom(X), Tmp, [C], _) :-
	lookup(Scope, class(X), DefScp),!,
	make_class_type_functor(X, DefScp, F),
	add_this([Tmp], DefScp, Ls),
	C =.. [F|Ls].

resolve_arg(S, R, R, atom(X), Y, [], _) :- scope_opts(S, Opts), !, add_qoutes(X, Y).

%Variables might be members
resolve_arg(Scope, Reps, [(var(Arg),Tmp)|Reps], var(Arg), Tmp, [::('This', ArgQ, Tmp)], _) :-
	lookup(Scope, field(Arg), _), !,
	scope_opts(Scope, Opts),
	add_qoutes(Arg, ArgQ).

resolve_arg(_, R, R, var(Arg), Arg, [], _) :- !.

% Tmps will never mean anything else in context
resolve_arg(_, R, R, tmp(Arg), Arg, [], _) :- !.

% might need to add a prefix and/or 'This'
resolve_arg(Scope, Reps, RepsOut, compound(P), Pd, Commands, true) :-
	P =.. [Nm|Args],
	length(Args, N),
	lookup(Scope, pred(Nm, N), PredScope), !,
	(is_not_constructor(PredScope, pred(Nm, N), _) ; throw('Cannot directly call a constructor.')),
	make_predicate_name(pred(Nm, N), PredScope, NewName),
	resolve_args(Scope, Reps, RepsOut, Args, Args2, Commands, false),
	add_this(Args2, PredScope, Args3),
	Pd =.. [NewName|Args3].

% This is the case where we have a compound we cant lookup, 
% FIXME, replace with access/call for correct semantics
resolve_arg(Scope, Reps, RepsOut, compound(P), Pd, Commands, Chge) :-
	P =.. [Nm|Args],
	contin_change(Nm, Chge, Chge2),
	resolve_args(Scope, Reps, RepsOut, Args, Argsd, Commands, Chge2),
	Pd =.. [Nm|Argsd].

% FIXME this is a hack, comes when using resolve_arg from new etc.
resolve_arg(Scope, Reps, RepsOut, P, Pd, Commands, Chge) :-
	functor(P, Nm, _),
	Nm \= 'compound',
	resolve_arg(Scope, Reps, RepsOut, compound(P), Pd, Commands, Chge).

resolve_args(Scope, Reps, RepsOut2, [Arg|Args],[NewArg|NewArgs],AllCommands,Chge) :-
	resolve_arg(Scope, Reps, RepsOut, Arg, NewArg,Commands,Chge),
	resolve_args(Scope, RepsOut, RepsOut2, Args, NewArgs, Commands2,Chge),
	append(Commands, Commands2, AllCommands).

resolve_args(_, R, R,[],[],[],_).

goal_funcs([',',';','not']).

contin_change(_, false, false).
contin_change(Nm, true, true) :- goal_funcs(L), member(Nm, L).
contin_change(Nm, true, false) :- goal_funcs(L), not(member(Nm, L)).

resolve_args_comma_functor(compound(B), Scope, Reps, RepsOut2, Res) :-
	functor(B, ',', _), !,
	arg(1, B, Arg), arg(2, B, Bdy),
	((	
		resolve_arg_new(Scope, Reps, RepsOut, Arg, CommandsNew), !,
		append(CommandsNew, Res1, Res)
	);(
		resolve_arg(Scope, Reps, RepsOut, Arg, NewArg, Commands1, true),
		append(Commands1, [NewArg|Res1], Res)
	)),
	resolve_args_comma_functor(Bdy, Scope, RepsOut, RepsOut2, Res1).

resolve_args_comma_functor(B, Scope, Reps, RepsOut, Res) :-
	((
		resolve_arg_new(Scope, Reps, RepsOut, B, Res), !
	);(
		resolve_arg(Scope, Reps, RepsOut, B, Bd, Commands, true),
		append(Commands, [Bd], Res)
	)).

add_this(Ls, Scope, Ls) :- scope_is_top(Scope).
add_this(Ls, Scope, ['This'|Ls]) :- not(scope_is_top(Scope)).

generate_def(S, fact(P), Scope) :- 
	P =.. [Nm|ArgLstIn],
	length(ArgLstIn, N),
	resolve_args(Scope, [], _, ArgLstIn, ArgLst, Commands, false),
	make_predicate_name(pred(Nm, N), Scope, NewName),
	add_this(ArgLst, Scope, ArgLst2),
	make_from_fact(NewName, ArgLst2, Commands, Res),
	out_stream(S, Out), write_standard(Out, Res), write(Out, '.\n').

%FIXME, we can look through all the replacements to work out if we need this
generate_def(S, rule(P, B), Scope) :- 
	P =.. [Nm|ArgLstIn],
	length(ArgLstIn, N),
	resolve_args(Scope, [], OutReps, ArgLstIn, ArgLst, Commands, false),
	resolve_args_comma_functor(B, Scope, OutReps, _, Res),
	append(Commands, Res, Body),
	make_predicate_name(pred(Nm, N), Scope, NewName),
	add_this(ArgLst, Scope, ArgLst2),
	Pd =.. [NewName | ArgLst2],
	list_to_comma_functor(Body, CBdy),
	out_stream(S, Out),
	write_standard(Out, Pd :- CBdy),
	write(Out, '.\n').

generate_def(S, C, Scope) :-
	get_class_name(C, N),
	scope_of(Scope, N, NewScope),
	make_class(S, C, Scope, NewScope, Supers),
	get_class_body(C, B),
	generate_defs(S, B, NewScope),
	out_stream(S, Out),
	write_list(Out, Supers).

%Fields don't generate anything
generate_def(_, field(_), Scope) :- not(scope_is_top(Scope)), !; throw('Fields must occur within a class').