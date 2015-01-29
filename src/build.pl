:- ['./utils/utils.pl'].

paths(build, '../build/').
paths(oopl, './compiler_ooprolog/').
paths(pl, './compiler_prolog/').
paths(test,'./tests/').
paths(tmp,'../tmp/').

file(Dir, Name, Path) :- paths(Dir, DirPath), atom_concat(DirPath, Name, Path).
copy_file_from_dir(Dir1, Dir2, Name) :- file(Dir1, Name, P1), file(Dir2, Name, P2), copy(P1, P2).

test_N_path(N, P) :- paths(test, Dir), atomic_list_concat([Dir, 'test', N, '.oopl'], P).
build_N_path(N, P) :- paths(build, Dir), atomic_list_concat([Dir, 'test', N, '.out.pl'], P).
standard_path('./standard/standard.oopl').
standard_out_path(X) :- build_dir_path(BP), atom_concat(BP, 'standard.out.pl', X).

load_compiler :-  file(build, 'generate.pl', G), [G].

% Use the prolog based compiler to build the standard include
build_standard :- standard_path(In), standard_out_path(Out), compile(In, Out, '', [no_munge, no_interpret]).

% Build the nth test code
build_test(N) :- test_N_path(N, P), build_N_path(N, P2), generic_compile(P, P2).

% Build and run the nth test
run_test(N) :- load_compiler, build_test(N), build_N_path(N, P), [P].

% Copy the prolog based compiler to build to start bootstrapping
copy_from_prolog :- 
	copy_file_from_dir(pl, build, 'parse.pl'),
	copy_file_from_dir(pl, build, 'generate.pl'),
	copy_file_from_dir(pl, build, 'interpret.pl').

build_from_ooprolog :- 
	file(oopl, 'generate.oopl', InPath),
	file(tmp, 'generate.pl', OutPath),
	generic_compile(InPath, OutPath, [no_interpret]).

copy_from_tmp :- copy_file_from_dir(tmp, build, 'generate.pl').

cycle_compiler :- load_compiler, build_from_ooprolog, copy_from_tmp.

% Starting point using prolog compiler
%main :- copy_from_prolog, load_compiler, build_standard.

%some compatability stuff
compile(In,Out,Extras,Opts):- p__compile(In,Out,Extras,Opts).
quick(X,Extras,Opts):- p__quick(X,Extras,Opts).
quick(X,Opts):- p__quick(X,Opts).
quick(X):- p__quick(X).
generic_compile(In,Out):- p__generic_compile(In,Out).
generic_compile(In,Out,Opts):- p__generic_compile(In,Out,Opts).
quick_generic(In):- p__quick_generic(In).