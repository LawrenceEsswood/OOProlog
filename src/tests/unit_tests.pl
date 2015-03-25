%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%BEGIN BLATENT COPY KLUDGE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(100, yfx, ::).

:- set_prolog_flag(generate_debug_info, false).

:- dynamic
	entered/1,			% clauses entered
	exited/1.			% clauses completed

:- meta_predicate
	show_coverage(0).

show_coverage2(Goal) :-
	setup_call_cleanup(
	    setup_trace(State),
	    once(Goal),
	    cleanup_trace(State)).

setup_trace(state(Visible, Leash, Ref)) :-
	asserta((user:prolog_trace_interception(Port, Frame, _, continue) :-
			prolog_cover:assert_cover(Port, Frame)), Ref),
	port_mask([unify,exit], Mask),
	'$visible'(Visible, Mask),
	'$leash'(Leash, Mask),
	trace.

port_mask([], 0).
port_mask([H|T], Mask) :-
	port_mask(T, M0),
	'$syspreds':port_name(H, Bit),
	Mask is M0 \/ Bit.

cleanup_trace(state(Visible, Leash, Ref)) :-
	nodebug,
	'$visible'(_, Visible),
	'$leash'(_, Leash),
	erase(Ref),
	covered(Succeeded, Failed),
	file_coverage(Succeeded, Failed).


%%	assert_cover(+Port, +Frame) is det.
%
%	Assert coverage of the current clause. We monitor two ports: the
%	_unify_ port to see which  clauses   we  entered, and the _exit_
%	port to see which completed successfully.

assert_cover(unify, Frame) :-
	running_static_pred(Frame),
	prolog_frame_attribute(Frame, clause, Cl), !,
	assert_entered(Cl).
assert_cover(exit, Frame) :-
	running_static_pred(Frame),
	prolog_frame_attribute(Frame, clause, Cl), !,
	assert_exited(Cl).
assert_cover(_, _).

%%	running_static_pred(+Frame) is semidet.
%
%	True if Frame is not running a dynamic predicate.

running_static_pred(Frame) :-
	prolog_frame_attribute(Frame, goal, Goal),
	\+ predicate_property(Goal, dynamic).

%%	assert_entered(+Ref) is det.
%%	assert_exited(+Ref) is det.
%
%	Add Ref to the set of entered or exited	clauses.

assert_entered(Cl) :-
	entered(Cl), !.
assert_entered(Cl) :-
	assert(entered(Cl)).

assert_exited(Cl) :-
	exited(Cl), !.
assert_exited(Cl) :-
	assert(exited(Cl)).

%%	covered(+Ref, +VisibleMask, +LeashMask, -Succeeded, -Failed) is	det.
%
%	Restore state and collect failed and succeeded clauses.

covered(Succeeded, Failed) :-
	findall(Cl, (entered(Cl), \+exited(Cl)), Failed0),
	findall(Cl, retract(exited(Cl)), Succeeded0),
	retractall(entered(Cl)),
	sort(Failed0, Failed),
	sort(Succeeded0, Succeeded).


		 /*******************************
		 *	     REPORTING		*
		 *******************************/

%%	file_coverage(+Succeeded, +Failed) is det.
%
%	Write a report on  the  clauses   covered  organised  by file to
%	current output.

file_coverage(Succeeded, Failed) :-
	format('~N~n~`=t~78|~n'),
	format('~tCoverage by File~t~78|~n'),
	format('~`=t~78|~n'),
	format('~w~t~w~64|~t~w~72|~t~w~78|~n',
	       ['File', 'Clauses', '%Cov', '%Fail']),
	format('~`=t~78|~n'),
	forall(source_file(File),
	       file_coverage(File, Succeeded, Failed)),
	format('~`=t~78|~n').

file_coverage(File, Succeeded, Failed) :-
	findall(Cl, clause_source(Cl, File, _), Clauses),
	sort(Clauses, All),
	(   ord_intersect(All, Succeeded)
	->  true
	;   ord_intersect(All, Failed)
	), !,
	ord_intersection(All, Failed, FailedInFile),
	ord_intersection(All, Succeeded, SucceededInFile),
	ord_subtract(All, SucceededInFile, UnCov1),
	ord_subtract(UnCov1, FailedInFile, Uncovered),
	length(All, AC),
	length(Uncovered, UC),
	length(FailedInFile, FC),
	CP is 100-100*UC/AC,
	FCP is 100*FC/AC,
	summary(File, 56, SFile),
	format('~w~t ~D~64| ~t~1f~72| ~t~1f~78|~n', [SFile, AC, CP, FCP]).
file_coverage(_,_,_).


summary(Atom, MaxLen, Summary) :-
	atom_length(Atom, Len),
	(   Len < MaxLen
	->  Summary = Atom
	;   SLen is MaxLen - 5,
	    sub_atom(Atom, _, SLen, 0, End),
	    atom_concat('...', End, Summary)
	).


%%	clause_source(+Clause, -File, -Line) is det.
%%	clause_source(-Clause, +File, -Line) is det.

clause_source(Clause, File, Line) :-
	nonvar(Clause), !,
	clause_property(Clause, file(File)),
	clause_property(Clause, line_count(Line)).
clause_source(Clause, File, Line) :-
	source_file(Pred, File),
	\+ predicate_property(Pred, multifile),
	nth_clause(Pred, _Index, Clause),
	clause_property(Clause, line_count(Line)).
clause_source(Clause, File, Line) :-
	Pred = _:_,
	predicate_property(Pred, multifile),
	nth_clause(Pred, _Index, Clause),
	clause_property(Clause, file(File)),
	clause_property(Clause, line_count(Line)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%END BLATENT COPY KLUDGE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%


















setup :-
	working_directory(_, '..'),
	['./build.pl'],
	load_interpreter.

test :- setup, set_test_options([]), show_coverage2(run_tests).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(bootstrap).
	
	test(build_compiler,  [nondet]) :- build_from_ooprolog.

	test(build_interpreter,  [nondet]) :- build_interpret.

:- end_tests(bootstrap).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	is_even(X) :- 0 is X mod 2.
	even_decrement(X, Y) :- 0 is X mod 2, Y is X - 1.

:- begin_tests(utils).

	test(sum) :- 
		sum([],0),
		sum([10], 10),
		sum([1,2,3,4], 10).

	test(count_occurences, [nondet]) :-
		count_occurences(foo, bar, 0),
		count_occurences(foo, foo, 1),
		count_occurences(foo, (foo,(foo,foo)), 3).

	test(add_functor_args) :-
		add_functor_args(foo, [], foo),
		add_functor_args(foo(1,2), [3,4], foo(1,2,3,4)),
		add_functor_args(foo, [1,2], foo(1,2)).

	test(list_to_comma_functor, [nondet]) :-
		list_to_comma_functor([1],1),
		list_to_comma_functor([1,2],(1,2)),
		list_to_comma_functor([1,2,3,4,5,6],(1,2,3,4,5,6)).

	%test(filter) :-
	%	filter(even_decrement, [1,2,4,7,20], [1,3,19]),
	%	filter(even_decrement, [], []).

	test(partition, [nondet]) :-
		partition(is_even, [1,2,3,4,5],[2,4],[1,3,5]),
		partition(is_even, [], [], []).

	test(add_qoutes, [nondet]) :-
		add_qoutes(1,1),
		add_qoutes([],[]),
		add_qoutes(hello,'\'hello\''),
		add_qoutes('a\\a','\'a\\\\a\'').

:- end_tests(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(parse).
	
	test(bind_vars_to_name, [nondet]) :-
		Ls = [=('X', X),=('Y', Y), =('Z', Z)],
		bind_vars_to_name(Ls),
		not(not_unified(X, 'X')),
		not(not_unified(Y, 'Y')),
		not(not_unified(Z, 'Z')).

	test(not_unified, [nondet]) :-
		not_unified(_,_),
		A = B,
		not(not_unified(A, B)),
		not_unified(a, b),
		not(not_unified(C, C)).

	get_parse_method(N, C) :-
		interpret_query(new(parser, P, _)),
		::(P, N, C).

	test(is_named_member, [nondet]) :-
		get_parse_method(is_named_member, C),
		call(C, X, [=('X',X)]),
		\+ call(C, X, [=('X',_)]),
		\+ call(C, X, []).

	test(name_dont_care, [nondet]) :-
		get_parse_method(name_dont_care, C),
		Eqs = [=('X',X),=('Y',Y)],
		call(C, Eqs, Y),
		not_unified(X, '_'),
		call(C, Eqs, Z),
		not(not_unified(Z, '_')).

	test(tag_predicate, [nondet]) :-
		get_parse_method(tag_predicate, C),
		Eqs = [=('X', X)],
		call(C, Eqs, foo(X,_,blah(a)), Res),
		not(not_unified(Res, foo(var(X),var('_'),compound(blah(atom(a)))))),
		call(C, Eqs, foo, foo).

:- end_tests(parse).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(generate).
	setup_generator(G, Scope) :-
		interpret_query((
			new(generator, G, [], './tests/unit_test_file.oopl', './tests/unit_test_file.out.pl', ''),
			call(G::g_compile),
			Scope = G::'TopScope'
			)).
		

		%resolve_arg(Scope, RepsIn, RepsOut, ArgIn, ArgOut, Commands, Cge)

	test(resolve_args) :-
		setup_generator(G, Scope),
		::(G, resolve_arg, C),
		call(C, Scope, [], _, atom(x), '\'x\'', [], true),
		!,
		call(C, Scope, [], _, var('X'), 'X', [], true),
		!,
		call(C, Scope, [], _, atom(foo), p_foo, [], true),
		!,
		call(C, Scope, [], _, atom(foo), '\'foo\'', [], false),
		!,
		call(C, Scope, [], _, compound(bar(atom(x),atom(y))), p_bar('\'x\'', '\'y\''), [], true),
		!,
		call(C, Scope, [], _, var('X')::var('Y'), Z, [::('X','Y',Z)], true),
		!,
		call(C, Scope, [], _, atom(a), A, Ls, false),
		A \=  '\'a\'',
		Ls \= [],
		!.
:- end_tests(generate).

