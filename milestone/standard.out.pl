hi(Size,Term,HI):-term_hash(Term,Hash),HI is Hash mod Size+1.
hashtable(Size,hashtable(Size,Table)):-functor(Table,'table',Size).
add(hashtable(Size,Table), (K,V)):-atomic(K),'!',hi(Size,K,Index),add_here(hashtable(Size,Table), (K,V),Index).
add_here(hashtable(_,Table),T,Index):-arg(Index,Table,T),'!'.
add_here(hashtable(Size,Table),T,Index):-Index2 is Index mod Size+1,add_here(hashtable(Size,Table),T,Index2).
lookup(hashtable(0,_),_):-'!','fail'.
lookup(hashtable(Size,Table), (K,V)):-atomic(K),'!',hi(Size,K,Index),try_here(hashtable(Size,Table), (K,V),Index).
lookup(hashtable(_,Table), (K,V)):-arg(_,Table, (K,V)),atomic(K).
try_here(hashtable(_,Table),T,Index):-arg(Index,Table,E),not(var(E)),T=E,'!'.
try_here(hashtable(_,Table),_,Index):-arg(Index,Table,Entry),var(Entry),'!','fail'.
try_here(hashtable(Size,Table),T,Index):-Index2 is Index mod Size+1,try_here(hashtable(Size,Table),T,Index2).
add(T,H,H):-add(H,T).
make_fresh_field(Field,Dict,Dict):-add(Dict, (Field,_)).
build_class(Type,C):-build_class(Type,C,[]).
build_class(Type,C,_):-C=class(Type,Members),::(Type,'Name',T0),::(Type,'Fields',T1),table_size(T0,T1,N),hashtable(N,Members),foldl('make_fresh_field',T1,Members,_),'!'.
table_size('classType',_,8).
table_size(_,F,N):-length(F,LF),N is LF*3.
::(O,Nm,Res):-access_field(O,Nm,Res).
::(O,Nm,Res):-access_predicate(O,Nm,Res).
::(class(_,hashtable(_,Table)),Nm,Res,N):-arg(N,Table, (Nm,Res)),'!'.
::(C,Nm,Res,_):- ::(C,Nm,Res).
access_field(class(_,M),Nm,Res):-lookup(M, (Nm,Res)).
access_predicate(O,Nm,Res):-classof(O,C),access_field(C,'Predicates',AllPreds),lookup(AllPreds, (Nm,P)),Res=..[P,O].
object(class(_,_)).
classof(class('self',M),class('self',M)):-'!'.
classof(class(T,_),T).
construct_classType(This,N,F,Pred,Par,Out,Cons):- ::(This,'Name',N,3),::(This,'Fields',F,1),length(Pred,NP),Size is NP*3,hashtable(Size,Table),foldl('add',Pred,Table,_),::(This,'Predicates',Table,4),::(This,'Parent',Par,5),::(This,'Outer',Out,2),::(This,'Constructor',Cons,7).
class_def__classType(T):-nb_getval('classType',T).
class_def_construct_classType(class('self',hashtable(8,table(('Fields',['Name','Fields','Predicates','Parent','Outer','Constructor']), ('Outer','none'), ('Name','classType'), ('Predicates',hashtable(0,'table')), ('Parent','none'), (_,_), ('Constructor','construct_classType'), (_,_))))).
:-class_def_construct_classType(T),nb_setval('classType',T).
add_to_defined_names(NewNames):-current_predicate('defined_names'/1),defined_names(OldNames),'!',union(NewNames,OldNames,Names),retract(defined_names(OldNames)),assert(defined_names(Names)).
add_to_defined_names(NewNames):-assert(defined_names(NewNames)).
