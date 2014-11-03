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
								[field('_X')]
							),
							fact(some_fact(var('_X'), var('_Y'), var('_Z'))),
							rule(foo(var('_X'), var('_Y')),(1=1, ::(var('_X'),var('_Y'),Z), blah(Z, a, b), 2=2))
							]
							),
						fact(outer_pred(var('_X'),var('_X')))],
				Defs = Expect.

test(flatten_single) :- flatten_single([(a,[b]),(c,[d])],[b,a,d,c]).

:- end_tests(parse).