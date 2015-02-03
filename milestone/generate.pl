:- ['./standard.out.pl'].
class_def_io(T):-class_def__classType(X),build_class(X,T),::(X,'Constructor',T0), (T0=none;call(T0,T,io,['In','Out'],[],none,none,p_io_io)).
p_io_io(This,T1,T2):- ::(This,'In',T1),::(This,'Out',T2).
class_def_class_header_token(T):-class_def__classType(X),build_class(X,T),::(X,'Constructor',T3), (T3=none;call(T3,T,class_header_token,['Name','Parents'],[],none,none,p_class_header_token_class_header_token)).
p_class_header_token_class_header_token(This,T4,T5):- ::(This,'Name',T4),::(This,'Parents',T5).
class_def_class_token(T):-class_def__classType(X),build_class(X,T),::(X,'Constructor',T6), (T6=none;call(T6,T,class_token,['Hdr','Bdy'],[],none,none,p_class_token_class_token)).
p_class_token_class_token(This,T7,T8):- ::(This,'Hdr',T7),::(This,'Bdy',T8).
class_def_clause_token(T):-class_def__classType(X),build_class(X,T),::(X,'Constructor',T9), (T9=none;call(T9,T,clause_token,['Left','Name','N','Mods'],[ (split_fnc,p_clause_token_split_fnc)],none,none,none)).
p_clause_token_split_fnc(This):- ::(This,'Left',T10),::(This,'Name',T11),::(This,'N',T12),functor(T10,T11,T12).
class_def_rule_token(T):-class_def__classType(X),build_class(X,T),::(X,'Constructor',T13), (T13=none;call(T13,T,rule_token,['Left','Name','N','Mods','Right'],[ (add_mods,p_rule_token_add_mods), (split_fnc,p_clause_token_split_fnc)],none,none,p_rule_token_rule_token)).
p_rule_token_rule_token(This,T14,T15):- ::(This,'Left',T14),::(This,'Right',T15),p_clause_token_split_fnc(This),::(This,'Mods',T16),T16='[]'.
p_rule_token_add_mods(This,Mds,Tkn):-class_def_rule_token(T18),::(This,'Left',T19),::(This,'Right',T20),build_class(T18,T19),::(T18,'Constructor',T17), (T17=none;call(T17,T19,T20,Tkn)),::(This,'Mods',T21),::(Tkn,'Mods',T22),append(Mds,T21,T22).
class_def_fact_token(T):-class_def__classType(X),build_class(X,T),::(X,'Constructor',T23), (T23=none;call(T23,T,fact_token,['Left','Name','N','Mods'],[ (add_mods,p_fact_token_add_mods), (split_fnc,p_clause_token_split_fnc)],none,none,p_fact_token_fact_token)).
p_fact_token_fact_token(This,T24):- ::(This,'Left',T24),p_clause_token_split_fnc(This),::(This,'Mods',T25),T25='[]'.
p_fact_token_add_mods(This,Mds,Tkn):-class_def_fact_token(T27),::(This,'Left',T28),build_class(T27,T28),::(T27,'Constructor',T26), (T26=none;call(T26,T28,Tkn)),::(This,'Mods',T29),::(Tkn,'Mods',T30),append(Mds,T29,T30).
:-op(100,'yfx','::').
:-op(99,'fx','class').
:-op(100,'xfx','extends').
:-op(1199,'fy','static').
class_def_parser(T):-class_def__classType(X),build_class(X,T),::(X,'Constructor',T31), (T31=none;call(T31,T,parser,['IO'],[ (parse_until_eof,p_parser_parse_until_eof), (parse_defs,p_parser_parse_defs), (parse_def,p_parser_parse_def), (parse_include,p_parser_parse_include), (parse_field,p_parser_parse_field), (parse_class,p_parser_parse_class), (parse_header,p_parser_parse_header), (parse_clause,p_parser_parse_clause), (tag_predicate,p_parser_tag_predicate), (is_named_member,p_parser_is_named_member), (name_dont_care,p_parser_name_dont_care), (tag_atoms,p_parser_tag_atoms)],none,none,p_parser_parser)).
p_parser_parser(This,T32):- ::(This,'IO',T32).
p_parser_parse_until_eof(This,Res):-p_parser_parse_defs(This,Res,'end_of_file').
p_parser_parse_defs(This,DefLs,End):- ::(This,'IO',T34),::(T34,'In',T33),read_term(T33,T,[variable_names(Eqs)|'[]']),p_not_unified(T,End),'!', (p_not_unified(T,'end_of_file');throw('Unexpected end of stream')), (p_parser_parse_def(This,Eqs,T,DefL);throw('Could not parse definition')),p_bind_vars_to_name(Eqs),append(DefL,DefR,DefLs),p_parser_parse_defs(This,DefR,End).
p_parser_parse_defs(This,'[]',_).
p_parser_parse_def(This,_,T,[Def|'[]']):-var(T),'!',p_parser_parse_field(This,T,Def).
p_parser_parse_def(This,_,T,Defs):-p_parser_parse_include(This,T,Defs).
p_parser_parse_def(This,_,T,[Def|'[]']):-p_parser_parse_class(This,T,Def).
p_parser_parse_def(This,Eqs,T,[Def|'[]']):-p_parser_parse_clause(This,Eqs,T,Def).
p_parser_parse_include(This, (:-oopl_include(Path)),Defs):-open(Path,'read',In),class_def_io(T36),::(This,'IO',T38),::(T38,'Out',T37),build_class(T36,NewIO),::(T36,'Constructor',T35), (T35=none;call(T35,NewIO,In,T37)),class_def_parser(T40),build_class(T40,P),::(T40,'Constructor',T39), (T39=none;call(T39,P,NewIO)),::(P,'parse_defs',T41),call(T41,Defs,'end_of_file'),close(In).
p_parser_parse_field(This,X,field(X)):-'!'.
p_parser_parse_class(This,T,class_def(C)):-p_parser_parse_header(This,T,H),p_parser_parse_defs(This,Defs,'endclass'),class_def_class_token(T43),build_class(T43,C),::(T43,'Constructor',T42), (T42=none;call(T42,C,H,Defs)).
p_parser_parse_header(This,class(X)extends Y,H):-atomic(X),atomic(Y),'!',class_def_class_header_token(T45),build_class(T45,H),::(T45,'Constructor',T44), (T44=none;call(T44,H,X,[Y|'[]'])).
p_parser_parse_header(This,class(X),H):-atomic(X),class_def_class_header_token(T47),build_class(T47,H),::(T47,'Constructor',T46), (T46=none;call(T46,H,X,'[]')).
p_parser_parse_clause(This,Eqs, (static A:-B),R):-p_parser_parse_clause(This,Eqs, (static (A:-B)),R).
p_parser_parse_clause(This,Eqs, (static P),R2):-'!',p_parser_parse_clause(This,Eqs,P,R),::(R,'add_mods',T48),call(T48,['static'|'[]'],R2).
p_parser_parse_clause(This,Eqs, (:-Xs),rule(R)):-'!',p_parser_tag_atoms(This,Eqs,Xs,Ys),class_def_rule_token(T50),build_class(T50,R),::(T50,'Constructor',T49), (T49=none;call(T49,R,'',Ys)).
p_parser_parse_clause(This,Eqs, (P:-Xs),rule(R)):-'!',p_parser_tag_predicate(This,Eqs,P,Pd),p_parser_tag_atoms(This,Eqs,Xs,Ys),class_def_rule_token(T52),build_class(T52,R),::(T52,'Constructor',T51), (T51=none;call(T51,R,Pd,Ys)).
p_parser_parse_clause(This,Eqs,P,fact(F)):-p_parser_tag_predicate(This,Eqs,P,Pd),class_def_fact_token(T54),build_class(T54,F),::(T54,'Constructor',T53), (T53=none;call(T53,F,Pd)).
p_parser_tag_predicate(This,_,P,P):-atomic(P).
p_parser_tag_predicate(This,Eqs,P,Pd):-not(atomic(P)),P=..[Nm|Args],::(This,'tag_atoms',T55),add_functor_args(T55,[Eqs|'[]'],F),maplist(F,Args,Args2),Pd=..[Nm|Args2].
p_parser_is_named_member(This,X,[_=Y|_]):-X==Y.
p_parser_is_named_member(This,X,[_|YS]):-p_parser_is_named_member(This,X,YS).
p_parser_name_dont_care(This,Eqs,X):-p_parser_is_named_member(This,X,Eqs).
p_parser_name_dont_care(This,_,'_').
p_parser_tag_atoms(This,Eqs,X,var(X)):-var(X),'!',p_parser_name_dont_care(This,Eqs,X).
p_parser_tag_atoms(This,_,X,atom(X)):-atomic(X),'!'.
p_parser_tag_atoms(This,Eqs,P,compound(P2)):-functor(P,_,_),P=..[Nm|Args],::(This,'tag_atoms',T56),add_functor_args(T56,[Eqs|'[]'],F),maplist(F,Args,Args2),P2=..[Nm|Args2].
p_not_unified(A,B):-not(unifiable(A,B,'[]')).
p_comma_list_append('[]',P,P).
p_comma_list_append([X|Xs],P, (X,Pd)):-p_comma_list_append(Xs,P,Pd).
p_flatten_single('[]','[]').
p_flatten_single([ (V,NstLs)|Rest],Res):-p_flatten_single(Rest,Restd),append(NstLs,[V|Restd],Res).
p_flatten_pair('[]','[]','[]').
p_flatten_pair([ (V,NstLs)|Rest],[V|RestV],Res):-p_flatten_pair(Rest,RestV,RestNstLs),append(NstLs,RestNstLs,Res).
p_bind_vars_to_name([X=X|XS]):-p_bind_vars_to_name(XS).
p_bind_vars_to_name('[]').
:-['../src/utils/utils.pl'|'[]'].
class_def_scope(T):-class_def__classType(X),build_class(X,T),::(X,'Constructor',T57), (T57=none;call(T57,T,scope,['Prefix','Names','Mods','Parent','Hdr','CS','Top'],[ (get_name,p_scope_get_name), (get_defined_names,p_scope_get_defined_names), (new_scope,p_scope_new_scope), (new_class_scope,p_scope_new_class_scope), (is_top,p_scope_is_top), (get_mods,p_scope_get_mods), (dump,p_scope_dump)],none,none,p_scope_scope)).
p_scope_scope(This,T58,T59,T60,T61,T62):- ::(This,'Prefix',T58),::(This,'Names',T59),::(This,'Parent',T60),::(This,'Hdr',T61),::(This,'CS',T62),::(This,'Top',T63),T63='false'.
p_scope_scope(This,Defs):-class_def_class_header_token(T65),::(This,'Hdr',T66),build_class(T65,T66),::(T65,'Constructor',T64), (T64=none;call(T64,T66,'','[]')),::(This,'Parent',T67),T67='top',::(This,'Top',T68),T68='true',p_scope_new_scope(This,Defs,'','').
p_scope_scope(This,Defs,T69,T70):- ::(This,'Hdr',T69),::(This,'Parent',T70),::(This,'Top',T71),T71='false',::(T69,'Name',T72),::(T70,'Prefix',T73),p_scope_new_scope(This,Defs,T72,T73).
p_scope_get_name(This,class_def(Def),class(T74)):- ::(Def,'Hdr',T75),::(T75,'Name',T74).
p_scope_get_name(This,fact(Def),pred(T76,T77)):- ::(Def,'Name',T76),::(Def,'N',T77),T76\=''.
p_scope_get_name(This,rule(Def),pred(T78,T79)):- ::(Def,'Name',T78),::(Def,'N',T79),T78\=''.
p_scope_get_name(This,field(Name),field(Name)).
p_scope_get_defined_names(This,'[]','[]').
p_scope_get_defined_names(This,[D|Ds],[N|Ns]):-p_scope_get_name(This,D,N),'!',p_scope_get_defined_names(This,Ds,Ns).
p_scope_get_defined_names(This,[_|Ds],Ns):-p_scope_get_defined_names(This,Ds,Ns).
p_scope_new_scope(This,Defs,Name,ParentPrefix):- ::(This,'Prefix',T80),atomic_list_concat(['_',Name,ParentPrefix|'[]'],T80),::(This,'Names',T81),p_scope_get_defined_names(This,Defs,T81),::(This,'new_class_scope',T82),::(This,'CS',T83),filter(T82,Defs,T83).
p_scope_new_class_scope(This,class_def(C),cs(T84,ClassScope)):- ::(C,'Hdr',T85),::(T85,'Name',T84),class_def_scope(T87),::(C,'Bdy',T88),build_class(T87,ClassScope),::(T87,'Constructor',T86), (T86=none;call(T86,ClassScope,T88,T85,This)).
p_scope_is_top(This):- ::(This,'Top',T89),T89='true'.
p_scope_get_mods(This,P,M):- ::(This,'Mods',T90),member((P,M),T90).
p_scope_dump(This,IO):- ::(IO,'Out',T91),::(This,'Names',T92),write(T91,defined_names(T92)),write(T91,'.
		:- [\'./interpret.pl\'].
		:- interpret.
	').
p_compile(In,Out,Extras,Opts):-class_def_generator(T94),build_class(T94,G),::(T94,'Constructor',T93), (T93=none;call(T93,G,Opts,In,Out,Extras)),::(G,'g_compile',T95),call(T95).
p_quick(X,Extras,Opts):-atom_concat(X,'.oopl',X1),atom_concat(X,'.out.pl',X2),p_compile(X1,X2,Extras,Opts).
p_quick(X,Opts):-p_quick(X,'',Opts).
p_quick(X):-p_quick(X,'[]').
p_generic_compile(In,Out):-p_generic_compile(In,Out,'[]').
p_generic_compile(In,Out,Opts):-p_compile(In,Out,':- [\'./standard.out.pl\'].
',Opts).
p_quick_generic(In):-atom_concat(In,'.oopl',X1),atom_concat(In,'.out.pl',X2),p_generic_compile(X1,X2).
class_def_generator(T):-class_def__classType(X),build_class(X,T),::(X,'Constructor',T96), (T96=none;call(T96,T,generator,['Opts','TopScope','Extras','InFile','OutFile','IO','TNext'],[ (next_tmp,p_generator_next_tmp), (g_compile,p_generator_g_compile), (get_field_name,p_generator_get_field_name), (is_constructor,p_generator_is_constructor), (is_not_constructor,p_generator_is_not_constructor), (pair_predicate,p_generator_pair_predicate), (get_field_name_qoute,p_generator_get_field_name_qoute), (find_parents_definitions,p_generator_find_parents_definitions), (make_super_call,p_generator_make_super_call), (predicate_inherit,p_generator_predicate_inherit), (find_class_definitions,p_generator_find_class_definitions), (not_same_pred_name,p_generator_not_same_pred_name), (get_unique_predicates,p_generator_get_unique_predicates), (none_if_empty,p_generator_none_if_empty), (make_class,p_generator_make_class), (append_if_not_blank,p_generator_append_if_not_blank), (make_predicate_name,p_generator_make_predicate_name), (make_class_type_functor,p_generator_make_class_type_functor), (make_class_type_args,p_generator_make_class_type_args), (make_class_type_body,p_generator_make_class_type_body), (expand_new,p_generator_expand_new), (make_from_fact,p_generator_make_from_fact), (load_scope,p_generator_load_scope), (lookup,p_generator_lookup), (lookup_here,p_generator_lookup_here), (lookup_nesting,p_generator_lookup_nesting), (lookup_parent,p_generator_lookup_parent), (scope_of,p_generator_scope_of), (generate_defs_top,p_generator_generate_defs_top), (generate_defs,p_generator_generate_defs), (resolve_arg_new,p_generator_resolve_arg_new), (resolve_arg,p_generator_resolve_arg), (resolve_args,p_generator_resolve_args), (goal_funcs,p_generator_goal_funcs), (contin_change,p_generator_contin_change), (resolve_args_comma_functor,p_generator_resolve_args_comma_functor), (add_this,p_generator_add_this), (generate_def,p_generator_generate_def), (compile_exception,p_generator_compile_exception)],none,none,p_generator_generator)).
p_generator_next_tmp(This,T):- ::(This,'TNext',T97),arg(1,T97,N),Ni is N+1,setarg(1,T97,Ni),atom_concat('T',N,T).
p_generator_generator(This,T98,T99,T100,T101):- ::(This,'Opts',T98),::(This,'InFile',T99),::(This,'OutFile',T100),::(This,'Extras',T101),::(This,'TNext',T102),T102=cntr(0).
p_generator_g_compile(This):- ::(This,'InFile',T103),open(T103,'read',InS),class_def_io(T105),build_class(T105,IO1),::(T105,'Constructor',T104), (T104=none;call(T104,IO1,InS,'user_output')),class_def_parser(T107),build_class(T107,P),::(T107,'Constructor',T106), (T106=none;call(T106,P,IO1)),::(P,'parse_until_eof',T108),call(T108,Defs),'!',close(InS),::(This,'OutFile',T109),open(T109,'write',OutS),::(This,'Extras',T110),write_standard(OutS,T110),::(This,'IO',T112),build_class(T105,T112),::(T105,'Constructor',T111), (T111=none;call(T111,T112,'user_input',OutS)),p_generator_generate_defs_top(This,Defs),'!',close(OutS).
p_generator_get_field_name(This,field(Name),Name).
p_generator_is_constructor(This,Scope,pred(Name,N),pred(Name,N)):-Name\='',::(Scope,'Hdr',T114),::(T114,'Name',T113),Name=T113.
p_generator_is_not_constructor(This,Scope,P,P):-not(p_generator_is_constructor(This,Scope,P,_)).
p_generator_pair_predicate(This,Scope,Name, (Name,Resolved)):-p_generator_make_predicate_name(This,Name,Scope,Resolved).
p_generator_get_field_name_qoute(This,X,Y):-p_generator_get_field_name(This,X,Y).
p_generator_find_parents_definitions(This,_,'[]','[]','[]','[]').
p_generator_find_parents_definitions(This,Scope,[Parent|'[]'],Fields,Predicates,SuperPreds):- (p_generator_lookup(This,Scope,class(Parent),PDefScope);throw_atoms(['Could not find a class of name ',Parent,'.'|'[]'])), (p_generator_scope_of(This,PDefScope,Parent,ParentScope);throw_atoms([Parent,' is not a class.'|'[]'])),::(ParentScope,'Hdr',T115),p_generator_find_class_definitions(This,ParentScope,T115,Fields,Predicates,SuperPreds).
p_generator_make_super_call(This,CRes,PRes,N, (L:-R)):-tmp_lst(N,Ts),L=..[CRes,'This'|Ts],R=..[PRes,'This'|Ts].
p_generator_predicate_inherit(This,'[]',TotalPairs,TotalPairs,'[]').
p_generator_predicate_inherit(This,[ (pred(CName,NC),CRes)|Ps],ParentPs,TotalPairs,[Sup|SuperPreds]):-member((pred(CName,NP),PRes),ParentPs),'!',delete(ParentPs, (pred(CName,NP),PRes),ParentPsWO),p_generator_make_super_call(This,CRes,PRes,NP,Sup),p_generator_predicate_inherit(This,[ (pred(CName,NC),CRes)|Ps],ParentPsWO,TotalPairs,SuperPreds).
p_generator_predicate_inherit(This,[ (CName,CRes)|Ps],ParentPs,[ (CName,CRes)|TotalPairs],SuperPreds):-p_generator_predicate_inherit(This,Ps,ParentPs,TotalPairs,SuperPreds).
p_generator_find_class_definitions(This,Scope,Hchild,Fields,Predicates,SuperPreds):- ::(This,'get_field_name_qoute',T116),::(Scope,'Names',T117),filter(T116,T117,ThisFields),::(This,'is_not_constructor',T118),add_functor_args(T118,[Scope|'[]'],F),filter(F,T117,Names2),::(This,'pair_predicate',T119),add_functor_args(T119,[Scope|'[]'],F2),filter(F2,Names2,ThisPredicates),::(Hchild,'Parents',T120),p_generator_find_parents_definitions(This,Scope,T120,ParentFields,ParentPredicates,ParentSuperPreds), (intersection(ThisFields,ParentFields,'[]');throw('Cannot redefine a parents field.')),append(ParentFields,ThisFields,Fields),p_generator_predicate_inherit(This,ThisPredicates,ParentPredicates,Predicates,ThisSuperPreds),append(ParentSuperPreds,ThisSuperPreds,SuperPreds).
p_generator_not_same_pred_name(This,Nm, (pred(Nm2,N),Res), (pred(Nm2,N),Res)):-Nm\=Nm2.
p_generator_get_unique_predicates(This,'[]','[]').
p_generator_get_unique_predicates(This,[ (pred(Nm,_),Res)|Ps],[ (Nm,Res)|OPs]):- ::(This,'not_same_pred_name',T121),add_functor_args(T121,[Nm|'[]'],F),filter(F,Ps,FPs),p_generator_get_unique_predicates(This,FPs,OPs).
p_generator_none_if_empty(This,_,'[]','none').
p_generator_none_if_empty(This,Scope,[X|_],Y):-p_generator_make_predicate_name(This,X,Scope,Y).
p_generator_make_class(This,Def,ScopeAbove,Scope,SuperPreds):- ::(Def,'Hdr',T122),p_generator_find_class_definitions(This,Scope,T122,Fields,Predicates,SuperPreds),p_generator_get_unique_predicates(This,Predicates,UPredicates),::(T122,'Name',T123),p_generator_make_class_type_functor(This,T123,ScopeAbove,Func),p_generator_make_class_type_args(This,ScopeAbove,Args),::(This,'is_constructor',T124),add_functor_args(T124,[Scope|'[]'],F),::(Scope,'Names',T125),filter(F,T125,Constructors),p_generator_none_if_empty(This,Scope,Constructors,Constructor),p_generator_make_class_type_body(This,T123,Fields,UPredicates,Bdy,Constructor),P=..[Func|Args],::(This,'IO',T127),::(T127,'Out',T126),write_standard(T126, (P:-Bdy)),write(T126,'.
').
p_generator_append_if_not_blank(This,Prefix,PredicateName,Result):-Prefix\='',atomic_list_concat(['p',Prefix,PredicateName|'[]'],Result).
p_generator_append_if_not_blank(This,'',PredicateName,PredicateName).
p_generator_make_predicate_name(This,pred('',_),_,'').
p_generator_make_predicate_name(This,pred(PredicateName,_),Scope,Result):-PredicateName\='',::(Scope,'Prefix',T128),p_generator_append_if_not_blank(This,T128,PredicateName,Result).
p_generator_make_class_type_functor(This,ClassName,Scope,Result):- ::(Scope,'Prefix',T129),atomic_list_concat(['class_def',T129,ClassName|'[]'],Result).
p_generator_make_class_type_args(This,Scope,Bdy):-p_generator_add_this(This,['T'|'[]'],Scope,Bdy).
p_generator_make_class_type_body(This,Name,Fields,Predicates,Bdy,Constructor):-maplist('add_qoutes',Fields,QFields),p_generator_next_tmp(This,T),p_generator_expand_new(This,new('X','T',Name,QFields,Predicates,'none','none',Constructor),T,NewComs),list_to_comma_functor([class_def__classType('X')|NewComs],Bdy).
p_generator_expand_new(This,Command,Con,[build_class(Type,Class),::(Type,Constructor,Con), (Con='none';Call)|'[]']):-Command=..['new',Type,Class|Args],add_qoutes('Constructor',Constructor),Call=..['call',Con,Class|Args].
p_generator_make_from_fact(This,Nm,ArgLst,'[]',Res):-Res=..[Nm|ArgLst].
p_generator_make_from_fact(This,Nm,ArgLst,Commands, (P1:-P2)):-Commands\='[]',P1=..[Nm|ArgLst],list_to_comma_functor(Commands,P2).
p_generator_load_scope(This,Names,Scope):-class_def_scope(T131),build_class(T131,Scope),::(T131,'Constructor',T130), (T130=none;call(T130,Scope,'__',Names,'top','top','[]')).
p_generator_lookup(This,Scope,Arg,ScopeRes):-p_generator_lookup_nesting(This,Scope,Arg,ScopeRes),'!'.
p_generator_lookup(This,Scope,Arg,ScopeRes):-p_generator_lookup_parent(This,Scope,Arg,ScopeRes).
p_generator_lookup_here(This,Scope,Arg,Scope):- ::(Scope,'Names',T132),member(Arg,T132).
p_generator_lookup_nesting(This,Scope,Arg,Res):- ::(Scope,'is_top',T133),not(call(T133)),::(Scope,'Parent',T134),p_generator_lookup_nesting(This,T134,Arg,Res).
p_generator_lookup_nesting(This,Scope,Arg,Res):-p_generator_lookup_here(This,Scope,Arg,Res).
p_generator_lookup_parent(This,Scope,Arg,Res):- ::(Scope,'is_top',T135),not(call(T135)),::(Scope,'Hdr',T137),::(T137,'Parents',T136),[P|'[]']=T136,p_generator_lookup(This,Scope,class(P),PDefScope),p_generator_scope_of(This,PDefScope,P,PScope),p_generator_lookup_parent(This,PScope,Arg,Res).
p_generator_lookup_parent(This,Scope,Arg,Res):-p_generator_lookup_here(This,Scope,Arg,Res).
p_generator_scope_of(This,Scope,Pname,Scp):- ::(Scope,'CS',T138),member(cs(Pname,Scp),T138).
p_generator_generate_defs_top(This,Defs):-class_def_scope(T140),::(This,'TopScope',T141),build_class(T140,T141),::(T140,'Constructor',T139), (T139=none;call(T139,T141,Defs)),p_generator_generate_defs(This,Defs,T141),::(This,'Opts',T142),::(T141,'dump',T143),::(This,'IO',T144), (member('no_interpret',T142);call(T143,T144)).
p_generator_generate_defs(This,[Def|Defs],Scope):-p_generator_generate_def(This,Def,Scope),p_generator_generate_defs(This,Defs,Scope).
p_generator_generate_defs(This,'[]',_).
p_generator_resolve_arg_new(This,Scope,Reps,RepsOut,compound(P),AllCommands):-functor(P,'new',_),'!',p_generator_next_tmp(This,Tmp),p_generator_expand_new(This,P,tmp(Tmp),Commands),p_generator_resolve_args(This,Scope,Reps,RepsOut,Commands,Commands1,Commands2,'false'),append(Commands2,Commands1,AllCommands).
p_generator_resolve_arg(This,_,Reps,Reps,T,Td,'[]',_):-member((T,Td),Reps),'!'.
p_generator_resolve_arg(This,Scope,Reps,Reps3,compound(Z),Tmp,Cmds4,_):-functor(Z,'::',2),arg(1,Z,X),arg(2,Z,Y),'!',p_generator_next_tmp(This,Tmp),p_generator_resolve_arg(This,Scope,[ (compound(Z),Tmp)|Reps],Reps2,X,Xd,Cmds1,'false'),p_generator_resolve_arg(This,Scope,Reps2,Reps3,Y,Yd,Cmds2,'false'),append(Cmds1,Cmds2,Cmds3),append(Cmds3,[::(Xd,Yd,Tmp)|'[]'],Cmds4).
p_generator_resolve_arg(This,Scope,Reps,Reps,atom(X),P,'[]','true'):-p_generator_lookup(This,Scope,pred(X,0),PredScope),'!',p_generator_make_predicate_name(This,pred(X,0),PredScope,Nm),p_generator_add_this(This,'[]',PredScope,Args),P=..[Nm|Args].
p_generator_resolve_arg(This,Scope,Reps,[ (atom(X),Tmp)|Reps],atom(X),Tmp,[C|'[]'],_):-p_generator_lookup(This,Scope,class(X),DefScp),'!',p_generator_next_tmp(This,Tmp),p_generator_make_class_type_functor(This,X,DefScp,F),p_generator_add_this(This,[Tmp|'[]'],DefScp,Ls),C=..[F|Ls].
p_generator_resolve_arg(This,_,R,R,atom(X),Y,'[]',_):-'!',add_qoutes(X,Y).
p_generator_resolve_arg(This,Scope,Reps,[ (var(Arg),Tmp)|Reps],var(Arg),Tmp,[::('This',ArgQ,Tmp)|'[]'],_):-p_generator_lookup(This,Scope,field(Arg),_),'!',add_qoutes(Arg,ArgQ),p_generator_next_tmp(This,Tmp).
p_generator_resolve_arg(This,_,R,R,var(Arg),Arg,'[]',_):-'!'.
p_generator_resolve_arg(This,_,R,R,tmp(Arg),Arg,'[]',_):-'!'.
p_generator_resolve_arg(This,Scope,Reps,RepsOut,compound(P),Pd,Commands,'true'):-P=..[Nm|Args],length(Args,N),p_generator_lookup(This,Scope,pred(Nm,N),PredScope),'!', (p_generator_is_not_constructor(This,PredScope,pred(Nm,N),_);throw('Cannot directly call a constructor.')),p_generator_make_predicate_name(This,pred(Nm,N),PredScope,NewName),p_generator_resolve_args(This,Scope,Reps,RepsOut,Args,Args2,Commands,'false'),p_generator_add_this(This,Args2,PredScope,Args3),Pd=..[NewName|Args3].
p_generator_resolve_arg(This,Scope,Reps,RepsOut,compound(P),Pd,Commands,Chge):-P=..[Nm|Args],p_generator_contin_change(This,Nm,Chge,Chge2),p_generator_resolve_args(This,Scope,Reps,RepsOut,Args,Argsd,Commands,Chge2),Pd=..[Nm|Argsd].
p_generator_resolve_arg(This,Scope,Reps,RepsOut,P,Pd,Commands,Chge):-functor(P,Nm,_),Nm\='compound',p_generator_resolve_arg(This,Scope,Reps,RepsOut,compound(P),Pd,Commands,Chge).
p_generator_resolve_args(This,Scope,Reps,RepsOut2,[Arg|Args],[NewArg|NewArgs],AllCommands,Chge):-p_generator_resolve_arg(This,Scope,Reps,RepsOut,Arg,NewArg,Commands,Chge),p_generator_resolve_args(This,Scope,RepsOut,RepsOut2,Args,NewArgs,Commands2,Chge),append(Commands,Commands2,AllCommands).
p_generator_resolve_args(This,_,R,R,'[]','[]','[]',_).
p_generator_goal_funcs(This,[',',';','not'|'[]']).
p_generator_contin_change(This,_,'false','false').
p_generator_contin_change(This,Nm,'true','true'):-p_generator_goal_funcs(This,L),member(Nm,L).
p_generator_contin_change(This,Nm,'true','false'):-p_generator_goal_funcs(This,L),not(member(Nm,L)).
p_generator_resolve_args_comma_functor(This,compound(B),Scope,Reps,RepsOut2,Res):-functor(B,',',_),'!',arg(1,B,Arg),arg(2,B,Bdy), (p_generator_resolve_arg_new(This,Scope,Reps,RepsOut,Arg,CommandsNew),'!',append(CommandsNew,Res1,Res);p_generator_resolve_arg(This,Scope,Reps,RepsOut,Arg,NewArg,Commands1,'true'),append(Commands1,[NewArg|Res1],Res)),p_generator_resolve_args_comma_functor(This,Bdy,Scope,RepsOut,RepsOut2,Res1).
p_generator_resolve_args_comma_functor(This,B,Scope,Reps,RepsOut,Res):-functor(B,Nm,_),Nm\=',', (p_generator_resolve_arg_new(This,Scope,Reps,RepsOut,B,Res),'!';p_generator_resolve_arg(This,Scope,Reps,RepsOut,B,Bd,Commands,'true'),append(Commands,[Bd|'[]'],Res)).
p_generator_add_this(This,Ls,Scope,Ls):- ::(Scope,'is_top',T145),call(T145).
p_generator_add_this(This,Ls,Scope,['This'|Ls]):- ::(Scope,'is_top',T146),not(call(T146)).
p_generator_generate_def(This,fact(FT),Scope):- ::(FT,'Left',T147),T147=..[Nm|ArgLstIn],p_generator_resolve_args(This,Scope,'[]',_,ArgLstIn,ArgLst,Commands,'false'),::(FT,'N',T148),p_generator_make_predicate_name(This,pred(Nm,T148),Scope,NewName),p_generator_add_this(This,ArgLst,Scope,ArgLst2),p_generator_make_from_fact(This,NewName,ArgLst2,Commands,Res),::(This,'IO',T150),::(T150,'Out',T149),write_standard(T149,Res),write(T149,'.
').
p_generator_generate_def(This,rule(RT),Scope):- ::(RT,'Left',T151),T151=..[Nm|ArgLstIn],p_generator_resolve_args(This,Scope,'[]',OutReps,ArgLstIn,ArgLst,Commands,'false'),::(RT,'Right',T152),p_generator_resolve_args_comma_functor(This,T152,Scope,OutReps,_,Res),append(Commands,Res,Body),::(RT,'N',T153),p_generator_make_predicate_name(This,pred(Nm,T153),Scope,NewName),p_generator_add_this(This,ArgLst,Scope,ArgLst2),Pd=..[NewName|ArgLst2],list_to_comma_functor(Body,CBdy),::(This,'IO',T155),::(T155,'Out',T154),write_standard(T154, (Pd:-CBdy)),write(T154,'.
').
p_generator_generate_def(This,class_def(C),Scope):- ::(C,'Hdr',T157),::(T157,'Name',T156),p_generator_scope_of(This,Scope,T156,NewScope),p_generator_make_class(This,C,Scope,NewScope,Supers),::(C,'Bdy',T158),p_generator_generate_defs(This,T158,NewScope),::(This,'IO',T160),::(T160,'Out',T159),write_list(T159,Supers).
p_generator_generate_def(This,field(Nm),Scope):- ::(Scope,'is_top',T161), (not(call(T161));p_generator_compile_exception(This,['Field \'',Nm,'\' defined outside of a class.'|'[]'])).
p_generator_compile_exception(This,Ms):-throw_atoms(Ms).
