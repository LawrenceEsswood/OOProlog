time_test_file(File) :- atomic_list_concat([File, '.oopl'], In),
	atomic_list_concat([File, '.out.pl'], Out),
	atomic_list_concat([File, '.result.txt'], ResOut),
	['../../build/generate.pl'],
	p_generic_compile(In, Out, [no_interpret]),
	write('compiled.\n'),
	[Out],
	p_times(N),
	p_entries(Gs),
	open(ResOut, write, Strm),
	time_over_n_ls(Gs, N, Strm).

time_over_n_ls([],_,_) :- write('done.\n').
time_over_n_ls([(G, Nm) |Gs], N, Strm) :- time_over_n(G, N, T),
	atomic_list_concat([N, ' iterations of ', Nm, ' = ', T, 's.\n'], M),
	write(Strm, M),
	time_over_n_ls(Gs, N, Strm).


time_over_n(Goal, N, T) :- statistics(process_cputime, T1),
	goal_n_times(Goal, N),
	statistics(process_cputime, T2),
	T is T2 - T1.

goal_n_times(_, 0) :- !.
goal_n_times(Goal, N) :- call(Goal), Nd is N - 1, !, goal_n_times(Goal, Nd).