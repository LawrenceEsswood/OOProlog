:- ['./utils/utils.pl'].

build_dir_path('../build/').
oopl_dir_path('./compiler_ooprolog/').
pl_dir_path('./compiler_prolog/').

test_dir_path('./tests/').
test_N_path(N, P) :- test_dir_path(Dir), atomic_list_concat([Dir, 'test', N], P).

standard_path('./standard/standard.oopl').
standard_out_path(X) :- build_dir_path(BP), atom_concat(BP, 'standard.out.pl', X).

parser_prolog(P) :- pl_dir_path(BP), atom_concat(BP, 'parse.pl', P).
interpret_prolog(P) :- pl_dir_path(BP), atom_concat(BP, 'interpret.pl', P).
generate_prolog(P) :- pl_dir_path(BP), atom_concat(BP, 'generate.pl', P).

parser_out_path(P) :- build_dir_path(BP), atom_concat(BP, 'parse.pl', P).
interpret_out_path(P) :- build_dir_path(BP), atom_concat(BP, 'interpret.pl', P).
generate_out_path(P) :- build_dir_path(BP), atom_concat(BP, 'generate.pl', P).

load_compiler :- parser_out_path(P), generate_out_path(G), [P], [G].

%Use the prolog based compiler to build the standard include
build_standard :- standard_path(In), standard_out_path(Out), compile(In, Out, [], [no_munge, no_interpret]).

%Build the nth test code
copy_for_tests :- standard_out_path(Sin), parser_out_path(Pin), interpret_out_path(Iin), generate_out_path(Gin),
	test_dir_path(TD),
	atom_concat(TD, 'standard.out.pl', S), copy(Sin, S),
	atom_concat(TD, 'parse.pl', P), copy(Pin, P),
	atom_concat(TD, 'interpret.pl', I), copy(Iin, I),
	atom_concat(TD, 'generate.pl', G), copy(Gin, G).
build_test(N) :- test_N_path(N, P), quick_generic(P).

%Copy the prolog based compiler to build to start bootstrapping
copy_from_prolog :- parser_out_path(P), interpret_out_path(I), generate_out_path(G),
	parser_prolog(Pin), interpret_prolog(Iin), generate_prolog(Gin),
	copy(Pin, P), copy(Iin, I), copy(Gin, G).

%main :- copy_from_prolog.
main :- copy_from_prolog, load_compiler, build_standard, copy_for_tests.