:- ['./utils/utils.pl'].

build_dir_path('../build/').
oopl_dir_path('./compiler_ooprolog/').
pl_dir_path('./compiler_prolog/').

test_dir_path('./tests/').
test_N_path(N, P) :- test_dir_path(Dir), atomic_list_concat([Dir, 'test', N, '.oopl'], P).
build_N_path(N, P) :- build_dir_path(Dir), atomic_list_concat([Dir, 'test', N, '.out.pl'], P).
standard_path('./standard/standard.oopl').
standard_out_path(X) :- build_dir_path(BP), atom_concat(BP, 'standard.out.pl', X).

parser_prolog(P) :- pl_dir_path(BP), atom_concat(BP, 'parse.pl', P).
interpret_prolog(P) :- pl_dir_path(BP), atom_concat(BP, 'interpret.pl', P).
generate_prolog(P) :- pl_dir_path(BP), atom_concat(BP, 'generate.pl', P).

parser_out_path(P) :- build_dir_path(BP), atom_concat(BP, 'parse.pl', P).
interpret_out_path(P) :- build_dir_path(BP), atom_concat(BP, 'interpret.pl', P).
generate_out_path(P) :- build_dir_path(BP), atom_concat(BP, 'generate.pl', P).

load_compiler :- parser_out_path(P), generate_out_path(G), [P], [G].

% Use the prolog based compiler to build the standard include
build_standard :- standard_path(In), standard_out_path(Out), compile(In, Out, '', [no_munge, no_interpret]).

% Build the nth test code
build_test(N) :- test_N_path(N, P), build_N_path(N, P2), generic_compile(P, P2).

% Build and run the nth test
run_test(N) :- load_compiler, build_test(N), build_N_path(N, P), [P].

% Copy the prolog based compiler to build to start bootstrapping
copy_from_prolog :- parser_out_path(P), interpret_out_path(I), generate_out_path(G),
	parser_prolog(Pin), interpret_prolog(Iin), generate_prolog(Gin),
	copy(Pin, P), copy(Iin, I), copy(Gin, G).

% Starting point using prolog compiler
main :- copy_from_prolog, load_compiler, build_standard.