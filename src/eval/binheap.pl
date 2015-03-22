	%%%%%%%%%%%%%%%
	% Binary Heap %
	%%%%%%%%%%%%%%%

new_heap(N, heap(H, 1)) :- functor(H, elements, N).

insert_element(Element, heap(H, X), heap(H, Xd)) :- 
	setarg(X, H, Element),
	Xd is X+1, bubble_up(H, X).

extract_min(Element, heap(H, X), heap(H, Xd)) :-
	arg(1, H, Element),
	Xd is X-1,
	sift_down(H, Xd).

bubble_up(_, 1) :- !.
bubble_up(H, X) :- 
	X > 1,
	PX is X div 2,
	less(H, X, PX), !,
	swap(H, X, PX),
	bubble_up(H, PX).
bubble_up(_,_).

sift_down(_,0) :- !.
sift_down(H, X) :- X \= 0, arg(X, H, T), setarg(1, H, T), sift_down(H, X, 1).

sift_down(_, Lim, X) :- TwoX is 2 * X, TwoX >= Lim, !.
sift_down(H, Lim, X) :- LimD is Lim-1, LimD is 2 * X, (not(less(H, LimD, X)) ; swap(H, LimD, X)), !.
sift_down(H, Lim, X) :- 
	C1 is 2 * X, C2 is C1 + 1, 
	min(H, C1, C2, C),
	less(H, C, X),
	!,
	swap(H, C, X),
	sift_down(H, Lim, C).
sift_down(_, _, _).


swap(H, X1, X2) :-
	arg(X1, H, T1),
	arg(X2, H, T2),
	setarg(X1, H, T2),
	setarg(X2, H, T1).

less(H, X1, X2) :- 
	arg(X1, H, (K1, _)),
	arg(X2, H, (K2, _)),
	K1 < K2.

min(H, C1, C2, C1) :- less(H, C1, C2), !.
min(_, _, C2, C2).