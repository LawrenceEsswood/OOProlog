	%%%%%%%%%%%%%%%%%%%%
	% Testing Bin Heap %
	%%%%%%%%%%%%%%%%%%%%

gen_data(0, []) :- !.
gen_data(N, [(R, N)|Rest]) :- 
	N > 0,
	random_between(0,100000000, R),
	N2 is N-1,
	gen_data(N2, Rest).

desc((K1,_),(K2, _)) :- K1 >= K2.
test_descend([_]).
test_descend([X,Y|Z]) :- desc(X, Y), test_descend([Y|Z]).

extract_n_pl(0, _, Ls, Ls).
extract_n_pl(N, Heap, Ls, Res) :- N > 0,
	Nd is N-1,
	extract_min(El, Heap, Heap2),
	extract_n_pl(Nd, Heap2, [El|Ls], Res).

extract_n_oopl(0, _, Ls, Ls).
extract_n_oopl(N, Heap, Ls, Res) :- N > 0,
	Nd is N-1,
	p_extract_min(El, Heap, Heap2),
	extract_n_oopl(Nd, Heap2, [El|Ls], Res).

extract_n_lt(0, _, Ls, Ls).
extract_n_lt(N, Heap, Ls, Res) :- N > 0,
	Nd is N-1,
	extract_min_lt(El, Heap, Heap2),
	extract_n_lt(Nd, Heap2, [El|Ls], Res).

insert_lt(Element, Heap,Heap2) :- insert_element_lt(Element, Heap, Heap2).
time_bin_lt(Data, N, Res) :-
	new_heap_lt(N, Heap),
	foldl(insert_lt, Data, Heap, FullHeap),
	extract_n_lt(N, FullHeap, [], Res).

time_bin_pl(Data, N, Res) :-
	['binheap.pl'],
	new_heap(N, Heap),
	foldl(insert_element, Data, Heap, FullHeap),
	extract_n_pl(N, FullHeap, [], Res).

insert_oopl(Element, Heap, Heap2) :- p_insert_element(Element, Heap, Heap2), !.
time_bin_oopl(Data, N, Res) :-
	p_new_heap(N, Heap),
	foldl(insert_oopl, Data, Heap, FullHeap),
	extract_n_oopl(N, FullHeap, [], Res).

test_heaps :- N = 1 000 000,
	gen_data(N, Data),
	['../../build/generate.pl'],
	p_generic_compile('./binheap.oopl', './binheap.out.pl', [no_interpret]),
	['binheap.out.pl'],
	time_over_n(time_bin_oopl(Data, N, _), 1, T2),
	write(T2), write('\n'),
	time_over_n(time_bin_pl(Data, N, _), 1, T1),
	write(T1), write('\n'),
	%time_over_n(time_bin_lt(Data, N, _), 1, T3),
	%write(T3), write('\n'),
	write('done!').
	%%%%%%%%%%%%%%%%%%%%%%%%
	% Generic Bench Stuffs %
	%%%%%%%%%%%%%%%%%%%%%%%%
compile_and_load(File) :-
	atomic_list_concat([File, '.oopl'], In),
	atomic_list_concat([File, '.out.pl'], Out),
	['../../build/generate.pl'],
	p_generic_compile(In, Out, [no_interpret]),
	[Out],
	write('compiled.\n').

res_file(File, Strm) :-
	atomic_list_concat([File, '.result.txt'], ResOut),
	open(ResOut, write, Strm).

space_test_file(File) :-
	compile_and_load(File),
	res_file(File, Strm),
	p_times(N),
	p_entries(Gs),
	space_over_n_ls(Gs, N, Strm).

space_over_n_ls([],_,_).
space_over_n_ls([(G, Nm)|Gs], N, Strm) :-
	space_over_n(G, N, S),
	atomic_list_concat([N, ' copies of ', Nm, ' = ', S, ' bytes.\n'], M),
	write(Strm, M),
	space_over_n_ls(Gs, N, Strm).

space_over_n(G, N, Space) :-
	garbage_collect,
	statistics(global_stack, [Before,_]),
	call(G, Ls),
	!,
	garbage_collect,
	statistics(global_stack, [After,_]),
	funk(Ls),
	Space is After - Before.

funk(Ls) :- (1 = 1 ; 1 = 2 , write(Ls)).

list_of(_, 0, []).
list_of(G, N, [X|Xs]) :- 
	N > 0, 
	Nd is N - 1,
	call(G, X),
	list_of(G, Nd, Xs).

time_test_file(File) :- 
	compile_and_load(File),
	res_file(File, Strm),
	p_times(N),
	p_entries(Gs),
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