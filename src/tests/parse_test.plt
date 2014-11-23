main :- run_tests.

% So these didn't get imported properly FIXME
:- op(100, xfy, ::).
:- op(99, fx, class).
:- op(100, xfx, extends).

:- begin_tests(parse).
:- ['../parse'].

%

% TODO I'll add some more tests later
test(parse1) :- open('./test.oopl', read ,S) , parse_defs((S, user_output, _), Defs, end_of_file), !,
				Expect = [class_def(class_head(foo,[]),
							[class_def(class_head(bar,[foo]),
								[field('X')]
							),
							fact(some_fact(var('X'), var('Y'), var('Z'))),
							rule(foo(var('X'), var('Y')),(atom(1)=atom(1), blah(var('X')::var('Y'), atom(a), atom(b)), atom(2)=atom(2)))
							]
							),
						fact(outer_pred(var('X'),var('X')))],
				write(Expect),
				nl,
				write(Defs),
				nl,
				Defs = Expect.

test(flatten_single) :- flatten_single([(a,[b]),(c,[d])],[b,a,d,c]).

:- end_tests(parse).