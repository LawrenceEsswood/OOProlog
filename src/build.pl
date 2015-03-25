:- ['./utils/utils.pl'].
:- ['./make_prolog_interface'].

reload(X) :- ensure_loaded(X), link_names, make, link_names.

paths(milestone, '../milestone/').
paths(build, '../build/').
paths(examples, './eval/Examples/').
paths(oopl, './compiler_ooprolog/').
paths(pl, './compiler_prolog/').
paths(test,'./tests/').
paths(tmp,'../tmp/').

oopl_file(File, OOPL_File) :- atomic_list_concat([File, '.oopl'], OOPL_File).
oopl_out_file(File, Out) :- atomic_list_concat([File, '.out.pl'], Out).

file(Dir, Name, Path) :- paths(Dir, DirPath), atom_concat(DirPath, Name, Path).
copy_file_from_dir(Dir1, Dir2, Name) :- file(Dir1, Name, P1), file(Dir2, Name, P2), copy(P1, P2).

test_N_path(N, P) :- paths(test, Dir), atomic_list_concat([Dir, 'test', N, '.oopl'], P).
build_N_path(N, P) :- paths(build, Dir), atomic_list_concat([Dir, 'test', N, '.out.pl'], P).

:- dynamic standard_path/1.
standard_path('./standard/standard.oopl').

standard_out_path(X) :- paths(build, BP), atom_concat(BP, 'standard.out.pl', X).

load_compiler :-  file(build, 'generate.pl', G), reload(G).
load_interpreter :- file(build, 'interpret.pl', I), reload(I).

% Use the prolog based compiler to build the standard include
build_standard :- 
	load_compiler,
	standard_path(In),
	standard_out_path(Out),
	compile(In, Out, '', [no_munge, no_interpret, no_dump]).

swap_standard :- retractall(standard_path(_)), assert(standard_path('./standard/standard2.oopl')).

% Build the nth test code
build_test(N) :- test_N_path(N, P), build_N_path(N, P2), generic_compile(P, P2).

% Build an example
build_example(Name) :-
	oopl_file(Name, InF),
	oopl_out_file(Name, OutF),
	file(examples, InF, InP),
	file(build, OutF, OutP),
	load_compiler,
	generic_compile(InP, OutP).

% Build and run the nth test
run_test(N) :- load_compiler, build_test(N), build_N_path(N, P), [P].

% Copy the prolog based compiler to build to start bootstrapping
copy_from_prolog :- 
	copy_file_from_dir(pl, build, 'parse.pl'),
	copy_file_from_dir(pl, build, 'generate.pl'),
	copy_file_from_dir(pl, build, 'interpret.pl').

copy_from_milestone :-
	copy_file_from_dir(milestone, build, 'generate.pl'),
	copy_file_from_dir(milestone, build, 'standard.out.pl').

build_from_ooprolog :- 
	file(oopl, 'generate.oopl', InPath),
	file(tmp, 'generate.pl', OutPath),
	generic_compile(InPath, OutPath, [no_interpret]).

build_interpret :-
	file(oopl, 'interpret.oopl', InPath),
	file(build, 'interpret.pl', OutPath),
	generic_compile(InPath, OutPath, [no_interpret]).

copy_from_tmp :- copy_file_from_dir(tmp, build, 'generate.pl').

restart_bootstrap :- copy_from_prolog, load_compiler, build_from_ooprolog, copy_from_tmp.
cycle_compiler :- load_compiler, build_from_ooprolog, copy_from_tmp.

% Starting point using prolog compiler
%main :- copy_from_prolog, load_compiler, build_standard.