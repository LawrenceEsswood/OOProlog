:- op(100, yfx, ::).

setup :-
	working_directory(_, '..'),
	['./build.pl'],
	load_interpreter.

test :- setup, run_tests.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(bootstrap).
	
	test(cycle3) :- cycle_compiler, cycle_compiler, cycle_compiler.

:- end_tests(bootstrap).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	is_even(X) :- 0 is X mod 2.
	even_decrement(X, Y) :- 0 is X mod 2, Y is X - 1.

:- begin_tests(utils).

	test(sum) :- 
		sum([],0),
		sum([10], 10),
		sum([1,2,3,4], 10).

	test(count_occurences) :-
		count_occurences(foo, bar, 0),
		count_occurences(foo, foo, 1),
		count_occurences(foo, (foo,(foo,foo)), 3).

	test(add_functor_args) :-
		add_functor_args(foo, [], foo),
		add_functor_args(foo(1,2), [3,4], foo(1,2,3,4)),
		add_functor_args(foo, [1,2], foo(1,2)).

	test(list_to_comma_functor) :-
		list_to_comma_functor([1],1),
		list_to_comma_functor([1,2],(1,2)),
		list_to_comma_functor([1,2,3,4,5,6],(1,2,3,4,5,6)).

	%test(filter) :-
	%	filter(even_decrement, [1,2,4,7,20], [1,3,19]),
	%	filter(even_decrement, [], []).

	test(partition) :-
		partition(is_even, [1,2,3,4,5],[2,4],[1,3,5]),
		partition(is_even, [], [], []).

	test(add_qoutes) :-
		add_qoutes(1,1),
		add_qoutes(hello,'\'hello\''),
		add_qoutes('a\\a','\'a\\\\a\'').

:- end_tests(utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(parse).
	
	test(bind_vars_to_name) :-
		Ls = [=('X', X),=('Y', Y), =('Z', Z)],
		bind_vars_to_name(Ls),
		not(not_unified(X, 'X')),
		not(not_unified(Y, 'Y')),
		not(not_unified(Z, 'Z')).

	test(not_unified) :-
		not_unified(_,_),
		A = B,
		not(not_unified(A, B)),
		not_unified(a, b),
		not(not_unified(C, C)).

	get_parse_method(N, C) :-
		interpret_query(new(parser, P, _)),
		::(P, N, C).

	test(is_named_member) :-
		get_parse_method(is_named_member, C),
		call(C, X, [=('X',X)]),
		\+ call(C, X, [=('X',_)]),
		\+ call(C, X, []).

	test(name_dont_care) :-
		get_parse_method(name_dont_care, C),
		Eqs = [=('X',X),=('Y',Y)],
		call(C, Eqs, Y),
		not_unified(X, '_'),
		call(C, Eqs, Z),
		not(not_unified(Z, '_')).

	test(tag_predicate) :-
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

