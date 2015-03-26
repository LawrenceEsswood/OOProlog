:- ensure_loaded('./standard.out.pl').
class_def_io(T):-nb_getval(io,T).
class_def_construct__io(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T0), (T0\=none*->call(T0,T,io,['In','Out'],[],none,none,p_io_io);true).
:-class_def_construct__io(T),nb_setval(io,T).
p_io_io(This,T1,T2):- ::(This,'In',T1),::(This,'Out',T2).
class_def_class_header_token(T):-nb_getval(class_header_token,T).
class_def_construct__class_header_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T3), (T3\=none*->call(T3,T,class_header_token,['Name','Parents'],[],none,none,p_class_header_token_class_header_token);true).
:-class_def_construct__class_header_token(T),nb_setval(class_header_token,T).
p_class_header_token_class_header_token(This,T4,T5):- ::(This,'Name',T4),::(This,'Parents',T5).
class_def_class_token(T):-nb_getval(class_token,T).
class_def_construct__class_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T6), (T6\=none*->call(T6,T,class_token,['Hdr','Bdy'],[],none,none,p_class_token_class_token);true).
:-class_def_construct__class_token(T),nb_setval(class_token,T).
p_class_token_class_token(This,T7,T8):- ::(This,'Hdr',T7),::(This,'Bdy',T8).
class_def_clause_token(T):-nb_getval(clause_token,T).
class_def_construct__clause_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T9), (T9\=none*->call(T9,T,clause_token,['Left','Name','N','Mods'],[ (split_fnc,p_clause_token_split_fnc), (check_mod_allowed,p_clause_token_check_mod_allowed)],none,none,none);true).
:-class_def_construct__clause_token(T),nb_setval(clause_token,T).
p_clause_token_split_fnc(This):- ::(This,'Left',T10),::(This,'Name',T11),::(This,'N',T12),functor(T10,T11,T12).
p_clause_token_check_mod_allowed(This,Mod):- ::(This,'Mods',T13), (not(member(Mod,T13)), (not(member(Mod,['public','private','protected']));intersection(T13,['public','private','protected'],[]));throw_atoms(['Illegal modifier ',Mod])).
class_def_rule_token(T):-nb_getval(rule_token,T).
class_def_construct__rule_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T14), (T14\=none*->call(T14,T,rule_token,['Left','Name','N','Mods','Right'],[ (add_mod,p_rule_token_add_mod), (split_fnc,p_clause_token_split_fnc), (check_mod_allowed,p_clause_token_check_mod_allowed)],none,none,p_rule_token_rule_token);true).
:-class_def_construct__rule_token(T),nb_setval(rule_token,T).
p_rule_token_rule_token(This,T15,T16):- ::(This,'Left',T15),::(This,'Right',T16),::(This,split_fnc,T17),call(T17),::(This,'Mods',T18),T18=[].
p_rule_token_rule_token(This,T19,T20,T21):- ::(This,'Left',T19),::(This,'Right',T20),::(This,'Mods',T21),::(This,split_fnc,T22),call(T22).
p_rule_token_add_mod(This,Mod,Tkn):- ::(This,check_mod_allowed,T23),call(T23,Mod),class_def_rule_token(T25),::(This,'Left',T26),::(This,'Right',T27),::(This,'Mods',T28),build_class(T25,Tkn,[]),::(T25,'Constructor',T24), (T24\=none*->call(T24,Tkn,T26,T27,[Mod|T28]);true).
class_def_fact_token(T):-nb_getval(fact_token,T).
class_def_construct__fact_token(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T29), (T29\=none*->call(T29,T,fact_token,['Left','Name','N','Mods'],[ (add_mod,p_fact_token_add_mod), (split_fnc,p_clause_token_split_fnc), (check_mod_allowed,p_clause_token_check_mod_allowed)],none,none,p_fact_token_fact_token);true).
:-class_def_construct__fact_token(T),nb_setval(fact_token,T).
p_fact_token_fact_token(This,T30):- ::(This,'Left',T30),::(This,split_fnc,T31),call(T31),::(This,'Mods',T32),T32=[].
p_fact_token_fact_token(This,T33,T34):- ::(This,'Left',T33),::(This,'Mods',T34),::(This,split_fnc,T35),call(T35).
p_fact_token_add_mod(This,Mod,Tkn):- ::(This,check_mod_allowed,T36),call(T36,Mod),class_def_fact_token(T38),::(This,'Left',T39),::(This,'Mods',T40),build_class(T38,Tkn,[]),::(T38,'Constructor',T37), (T37\=none*->call(T37,Tkn,T39,[Mod|T40]);true).
:-op(100,'yfx','::').
:-op(99,'fx','class').
:-op(100,'xfx','extends').
:-op(1199,'fy','static').
:-op(1199,'fy','public').
:-op(1199,'fy','private').
:-op(1199,'fy','protected').
class_def_parser(T):-nb_getval(parser,T).
class_def_construct__parser(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T41), (T41\=none*->call(T41,T,parser,['IO'],[ (parse_until_eof,p_parser_parse_until_eof), (parse_defs,p_parser_parse_defs), (parse_def,p_parser_parse_def), (parse_include,p_parser_parse_include), (parse_field,p_parser_parse_field), (parse_class,p_parser_parse_class), (parse_header,p_parser_parse_header), (all_mods,p_parser_all_mods), (parse_clause,p_parser_parse_clause), (tag_predicate,p_parser_tag_predicate), (is_named_member,p_parser_is_named_member), (name_dont_care,p_parser_name_dont_care), (tag_atoms,p_parser_tag_atoms)],none,none,p_parser_parser);true).
:-class_def_construct__parser(T),nb_setval(parser,T).
p_parser_parser(This,T42):- ::(This,'IO',T42).
p_parser_parse_until_eof(This,Res):- ::(This,parse_defs,T43),call(T43,Res,'end_of_file').
p_parser_parse_defs(This,DefLs,End):- ::(This,'IO',T45),::(T45,'In',T44),read_term(T44,T,[variable_names(Eqs)]),p_not_unified(T,End),'!', (p_not_unified(T,'end_of_file');throw('Unexpected end of stream')),::(This,parse_def,T46), (call(T46,Eqs,T,DefL);throw('Could not parse definition')),p_bind_vars_to_name(Eqs),append(DefL,DefR,DefLs),::(This,parse_defs,T47),call(T47,DefR,End).
p_parser_parse_defs(_,[],_).
p_parser_parse_def(This,_,T,[Def]):-var(T),'!',::(This,parse_field,T48),call(T48,T,Def).
p_parser_parse_def(This,_,T,Defs):- ::(This,parse_include,T49),call(T49,T,Defs).
p_parser_parse_def(This,_,T,[Def]):- ::(This,parse_class,T50),call(T50,T,Def).
p_parser_parse_def(This,Eqs,T,[Def]):- ::(This,parse_clause,T51),call(T51,Eqs,T,Def).
p_parser_parse_include(This, (:-oopl_include(Path)),Defs):-open(Path,'read',In),class_def_io(T53),::(This,'IO',T55),::(T55,'Out',T54),build_class(T53,NewIO,[]),::(T53,'Constructor',T52), (T52\=none*->call(T52,NewIO,In,T54);true),class_def_parser(T57),build_class(T57,P,[]),::(T57,'Constructor',T56), (T56\=none*->call(T56,P,NewIO);true),::(P,'parse_defs',T58),call(T58,Defs,'end_of_file'),close(In).
p_parser_parse_field(_,X,field(X)):-'!'.
p_parser_parse_class(This,T,class_def(C)):- ::(This,parse_header,T59),call(T59,T,H),::(This,parse_defs,T60),call(T60,Defs,'endclass'),class_def_class_token(T62),build_class(T62,C,[]),::(T62,'Constructor',T61), (T61\=none*->call(T61,C,H,Defs);true).
p_parser_parse_header(_,class(X)extends Y,H):-atomic(X),atomic(Y),'!',class_def_class_header_token(T64),build_class(T64,H,[]),::(T64,'Constructor',T63), (T63\=none*->call(T63,H,X,[Y]);true).
p_parser_parse_header(_,class(X),H):-atomic(X),class_def_class_header_token(T66),build_class(T66,H,[]),::(T66,'Constructor',T65), (T65\=none*->call(T65,H,X,[]);true).
p_parser_all_mods(_,['static','public','private','protected']).
p_parser_parse_clause(This,Eqs, (ModA:-B),R):-ModA=..[Mod,A],::(This,all_mods,T67),call(T67,AllMods),member(Mod,AllMods),'!',ModP=..[Mod, (A:-B)],::(This,parse_clause,T68),call(T68,Eqs,ModP,R).
p_parser_parse_clause(This,Eqs,ModP,R2):-ModP=..[Mod,P],::(This,all_mods,T69),call(T69,AllMods),member(Mod,AllMods),'!',::(This,parse_clause,T70),call(T70,Eqs,P,R),R=..[F,Arg],::(Arg,'add_mod',T71),call(T71,Mod,Arg2),R2=..[F,Arg2].
p_parser_parse_clause(This,Eqs, (:-Xs),rule(R)):-'!',::(This,tag_atoms,T72),call(T72,Eqs,Xs,Ys),class_def_rule_token(T74),build_class(T74,R,[]),::(T74,'Constructor',T73), (T73\=none*->call(T73,R,'',Ys);true).
p_parser_parse_clause(This,Eqs, (P:-Xs),rule(R)):-'!',::(This,tag_predicate,T75),call(T75,Eqs,P,Pd),::(This,tag_atoms,T76),call(T76,Eqs,Xs,Ys),class_def_rule_token(T78),build_class(T78,R,[]),::(T78,'Constructor',T77), (T77\=none*->call(T77,R,Pd,Ys);true).
p_parser_parse_clause(This,Eqs,P,fact(F)):- ::(This,tag_predicate,T79),call(T79,Eqs,P,Pd),class_def_fact_token(T81),build_class(T81,F,[]),::(T81,'Constructor',T80), (T80\=none*->call(T80,F,Pd);true).
p_parser_tag_predicate(_,_,P,P):-atomic(P).
p_parser_tag_predicate(This,Eqs,P,Pd):-not(atomic(P)),P=..[Nm|Args],::(This,'tag_atoms',T82),add_functor_args(T82,[Eqs],F),maplist(F,Args,Args2),Pd=..[Nm|Args2].
p_parser_is_named_member(_,X,[_=Y|_]):-X==Y.
p_parser_is_named_member(This,X,[_|YS]):- ::(This,is_named_member,T83),call(T83,X,YS).
p_parser_name_dont_care(This,Eqs,X):- ::(This,is_named_member,T84),call(T84,X,Eqs).
p_parser_name_dont_care(_,_,'_').
p_parser_tag_atoms(This,Eqs,X,var(X)):-var(X),'!',::(This,name_dont_care,T85),call(T85,Eqs,X).
p_parser_tag_atoms(_,_,X,atom(X)):-atomic(X),'!'.
p_parser_tag_atoms(This,Eqs,P,compound(P2)):-functor(P,_,_),P=..[Nm|Args],::(This,'tag_atoms',T86),add_functor_args(T86,[Eqs],F),maplist(F,Args,Args2),P2=..[Nm|Args2].
p_not_unified(A,B):-not(unifiable(A,B,[])).
p_bind_vars_to_name([X=X|XS]):-p_bind_vars_to_name(XS).
p_bind_vars_to_name([]).
:-['../src/utils/utils.pl'].
class_def_scope(T):-nb_getval(scope,T).
class_def_construct__scope(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T87), (T87\=none*->call(T87,T,scope,['Prefix','Parent','Hdr','CS','Top','Defs'],[ (get_name,p_scope_get_name), (get_field_name,p_scope_get_field_name), (get_field_name_qoute,p_scope_get_field_name_qoute), (get_defined_names,p_scope_get_defined_names), (new_scope,p_scope_new_scope), (new_class_scope,p_scope_new_class_scope), (is_top,p_scope_is_top), (dump,p_scope_dump), (lookup,p_scope_lookup), (lookup_here,p_scope_lookup_here), (lookup_nesting,p_scope_lookup_nesting), (lookup_parent,p_scope_lookup_parent), (not_in_self,p_scope_not_in_self), (scope_of,p_scope_scope_of), (get_parent_scope,p_scope_get_parent_scope), (pred_is_constructor,p_scope_pred_is_constructor), (is_constructor,p_scope_is_constructor), (is_not_constructor,p_scope_is_not_constructor), (get_constructors,p_scope_get_constructors), (get_non_constructors,p_scope_get_non_constructors), (get_qouted_fields,p_scope_get_qouted_fields), (get_pred_mods,p_scope_get_pred_mods), (pred_has_mod,p_scope_pred_has_mod), (pred_to_cononical_name,p_scope_pred_to_cononical_name), (pred_cls,p_scope_pred_cls), (get_mod_conflict,p_scope_get_mod_conflict), (any_mod_conflict,p_scope_any_mod_conflict), (error_checks,p_scope_error_checks)],none,none,p_scope_scope);true).
:-class_def_construct__scope(T),nb_setval(scope,T).
p_scope_scope(This,T88):- ::(This,'Defs',T88),class_def_class_header_token(T90),::(This,'Hdr',T91),build_class(T90,T91,[]),::(T90,'Constructor',T89), (T89\=none*->call(T89,T91,'',[]);true),::(This,'Parent',T92),T92='top',::(This,'Top',T93),T93='true',::(This,new_scope,T94),call(T94,'',''),::(This,error_checks,T95),call(T95).
p_scope_scope(This,T96,T97,T98):- ::(This,'Defs',T96),::(This,'Hdr',T97),::(This,'Parent',T98),::(This,'Top',T99),T99='false',::(T97,'Name',T100),::(T98,'Prefix',T101),::(This,new_scope,T102),call(T102,T100,T101),::(This,error_checks,T103),call(T103).
p_scope_get_name(_,class_def(Def),class(T104)):- ::(Def,'Hdr',T105),::(T105,'Name',T104).
p_scope_get_name(_,fact(Def),pred(T106,T107)):- ::(Def,'Name',T106),::(Def,'N',T107),T106\=''.
p_scope_get_name(_,rule(Def),pred(T108,T109)):- ::(Def,'Name',T108),::(Def,'N',T109),T108\=''.
p_scope_get_name(_,field(Name),field(Name)).
p_scope_get_field_name(_,field(Name),Name).
p_scope_get_field_name_qoute(This,X,Y):- ::(This,get_field_name,T110),call(T110,X,Y).
p_scope_get_defined_names(_,[],[]).
p_scope_get_defined_names(This,[D|Ds],[N|Ns]):- ::(This,get_name,T111),call(T111,D,N),'!',::(This,get_defined_names,T112),call(T112,Ds,Ns).
p_scope_get_defined_names(This,[_|Ds],Ns):- ::(This,get_defined_names,T113),call(T113,Ds,Ns).
p_scope_new_scope(This,Name,ParentPrefix):- ::(This,'Prefix',T114),atomic_list_concat(['_',Name,ParentPrefix],T114),::(This,'new_class_scope',T115),::(This,'Defs',T116),::(This,'CS',T117),filter(T115,T116,T117).
p_scope_new_class_scope(This,class_def(C),cs(T118,ClassScope)):- ::(C,'Hdr',T119),::(T119,'Name',T118),class_def_scope(T121),::(C,'Bdy',T122),build_class(T121,ClassScope,[]),::(T121,'Constructor',T120), (T120\=none*->call(T120,ClassScope,T122,T119,This);true).
p_scope_is_top(This):- ::(This,'Top',T123),T123='true'.
p_scope_dump(This,IO):- ::(This,'Defs',T124),::(This,get_defined_names,T125),call(T125,T124,Names),::(IO,'Out',T126),write_standard(T126, (:-add_to_defined_names(Names))),write(T126,'.').
p_scope_lookup(This,Arg,ScopeRes):- ::(This,lookup_parent,T127),call(T127,Arg,ScopeRes),'!'.
p_scope_lookup(This,Arg,ScopeRes):- ::(This,lookup_nesting,T128),call(T128,Arg,ScopeRes).
p_scope_lookup_here(This,Arg,This):- ::(This,'Defs',T129),member(Def,T129),::(This,get_name,T130),call(T130,Def,Arg).
p_scope_lookup_nesting(This,Arg,Res):- ::(This,lookup_here,T131),call(T131,Arg,Res).
p_scope_lookup_nesting(This,Arg,Res):- ::(This,is_top,T132),not(call(T132)),::(This,'Parent',T134),::(T134,'lookup_nesting',T133),call(T133,Arg,Res).
p_scope_lookup_parent(This,Arg,Res):- ::(This,lookup_here,T135),call(T135,Arg,Res).
p_scope_lookup_parent(This,Arg,Res):- ::(This,is_top,T136),not(call(T136)),::(This,'Hdr',T138),::(T138,'Parents',T137),[P]=T137,::(This,not_in_self,T139),call(T139,Arg,P),::(This,get_parent_scope,T140),call(T140,P,PScope),::(PScope,'lookup_parent',T141),call(T141,Arg,Res).
p_scope_not_in_self(_,class(P),P):-'!','fail'.
p_scope_not_in_self(_,_,_).
p_scope_scope_of(This,Pname,Scp):- ::(This,'CS',T142),member(cs(Pname,Scp),T142).
p_scope_get_parent_scope(This,Pname,Scp):- ::(This,lookup,T143), (call(T143,class(Pname),PDefScope),'!';throw_atoms(['Could not find a class of name ',Pname,'.'])),::(PDefScope,'scope_of',T144), (call(T144,Pname,Scp),'!';throw_atoms([Pname,' is not a class.'])).
p_scope_pred_is_constructor(This,pred(T145,_)):- ::(This,'Hdr',T146),::(T146,'Name',T145),T145\=''.
p_scope_is_constructor(This,Def,pred(T147,N)):- ::(This,'Hdr',T148),::(T148,'Name',T147),::(This,get_name,T149),call(T149,Def,pred(T147,N)),T147\=''.
p_scope_is_not_constructor(This,Def,pred(Nm,N)):- ::(This,is_constructor,T150),not(call(T150,Def,_)),::(This,get_name,T151),call(T151,Def,pred(Nm,N)).
p_scope_get_constructors(This,Cons):- ::(This,'is_constructor',T152),::(This,'Defs',T153),filter(T152,T153,Cons).
p_scope_get_non_constructors(This,Cons):- ::(This,'is_not_constructor',T154),::(This,'Defs',T155),filter(T154,T155,Cons).
p_scope_get_qouted_fields(This,Fields):- ::(This,'get_field_name_qoute',T156),::(This,'Defs',T157),filter(T156,T157,Fields).
p_scope_get_pred_mods(This,Arg,Mods):- ::(This,'Defs',T158),member(Def,T158),::(This,get_name,T159),call(T159,Def,Arg),Def=..[_,C],::(C,'Mods',T160),Mods=T160.
p_scope_pred_has_mod(This,Mod,Arg):- ::(This,get_pred_mods,T161),call(T161,Arg,Mods),member(Mod,Mods).
p_scope_pred_to_cononical_name(_,pred(Nm,N),Nm/N).
p_scope_pred_cls(_,rule(RT),RT).
p_scope_pred_cls(_,fact(RT),RT).
p_scope_get_mod_conflict(This,Def,OtherDefs):- ::(This,pred_cls,T162),call(T162,Def,A),::(This,get_name,T163),call(T163,Def,Nm),member(Def2,OtherDefs),::(This,get_name,T164),call(T164,Def2,Nm),::(This,pred_cls,T165),call(T165,Def2,B),::(A,'Mods',T166),::(B,'Mods',T167),T166\=T167.
p_scope_any_mod_conflict(_,[]).
p_scope_any_mod_conflict(This,[X|XS]):- ::(This,get_mod_conflict,T168),not(call(T168,X,XS)),::(This,any_mod_conflict,T169),call(T169,XS).
p_scope_any_mod_conflict(This,[X|_]):- ::(This,get_name,T170),call(T170,X,Nm),::(This,pred_to_cononical_name,T171),call(T171,Nm,CNm),throw_atoms(['Predicate ',CNm,' defined with different access modifiers.']).
p_scope_error_checks(This):- ::(This,'Defs',T172),::(This,any_mod_conflict,T173),call(T173,T172).
p_compile(In,Out,Extras,Opts):-class_def_generator(T175),build_class(T175,G,[]),::(T175,'Constructor',T174), (T174\=none*->call(T174,G,Opts,In,Out,Extras);true),::(G,'g_compile',T176),call(T176),'!'.
p_quick(X,Extras,Opts):-atom_concat(X,'.oopl',X1),atom_concat(X,'.out.pl',X2),p_compile(X1,X2,Extras,Opts).
p_quick(X,Opts):-p_quick(X,'',Opts).
p_quick(X):-p_quick(X,[]).
p_generic_compile(In,Out):-p_generic_compile(In,Out,[]).
p_generic_compile(In,Out,Opts):-p_compile(In,Out,':- ensure_loaded(\'./standard.out.pl\').
',Opts).
p_quick_generic(In):-atom_concat(In,'.oopl',X1),atom_concat(In,'.out.pl',X2),p_generic_compile(X1,X2).
class_def_generator(T):-nb_getval(generator,T).
class_def_construct__generator(T):-class_def__classType(X),build_class(X,T,[]),::(X,'Constructor',T177), (T177\=none*->call(T177,T,generator,['Opts','TopScope','Extras','InFile','OutFile','IO','TNext'],[ (next_tmp,p_generator_next_tmp), (g_compile,p_generator_g_compile), (pair_predicate,p_generator_pair_predicate), (find_parents_definitions,p_generator_find_parents_definitions), (make_super_call,p_generator_make_super_call), (predicate_inherit,p_generator_predicate_inherit), (find_class_definitions,p_generator_find_class_definitions), (not_same_pred_name,p_generator_not_same_pred_name), (get_unique_predicates,p_generator_get_unique_predicates), (none_if_empty,p_generator_none_if_empty), (make_class,p_generator_make_class), (append_if_not_blank,p_generator_append_if_not_blank), (make_predicate_name,p_generator_make_predicate_name), (make_class_type_functor,p_generator_make_class_type_functor), (make_class_type_constructor_functor,p_generator_make_class_type_constructor_functor), (make_class_type_args,p_generator_make_class_type_args), (make_class_type_constructor,p_generator_make_class_type_constructor), (expand_new,p_generator_expand_new), (make_from_fact,p_generator_make_from_fact), (generate_defs_top,p_generator_generate_defs_top), (dump,p_generator_dump), (interp,p_generator_interp), (generate_defs,p_generator_generate_defs), (resolve_arg_new,p_generator_resolve_arg_new), (resolve_arg,p_generator_resolve_arg), (resolve_args,p_generator_resolve_args), (make_dynamic,p_generator_make_dynamic), (goal_funcs,p_generator_goal_funcs), (contin_change,p_generator_contin_change), (resolve_args_comma_functor,p_generator_resolve_args_comma_functor), (add_this,p_generator_add_this), (generate_def,p_generator_generate_def), (g_add_qoutes,p_generator_g_add_qoutes), (compile_exception,p_generator_compile_exception)],none,none,p_generator_generator);true).
:-class_def_construct__generator(T),nb_setval(generator,T).
p_generator_next_tmp(This,T):- ::(This,'TNext',T178),arg(1,T178,N),Ni is N+1,setarg(1,T178,Ni),atom_concat('T',N,T).
p_generator_generator(This,T179,T180,T181,T182):- ::(This,'Opts',T179),::(This,'InFile',T180),::(This,'OutFile',T181),::(This,'Extras',T182),::(This,'TNext',T183),T183=cntr(0).
p_generator_g_compile(This):- ::(This,'InFile',T184),open(T184,'read',InS),class_def_io(T186),build_class(T186,IO1,[]),::(T186,'Constructor',T185), (T185\=none*->call(T185,IO1,InS,'user_output');true),class_def_parser(T188),build_class(T188,P,[]),::(T188,'Constructor',T187), (T187\=none*->call(T187,P,IO1);true),::(P,'parse_until_eof',T189),call(T189,Defs),'!',close(InS),::(This,'OutFile',T190),open(T190,'write',OutS),::(This,'Extras',T191),write_standard(OutS,T191),::(This,'IO',T193),build_class(T186,T193,[]),::(T186,'Constructor',T192), (T192\=none*->call(T192,T193,'user_input',OutS);true),::(This,generate_defs_top,T194),call(T194,Defs),'!',close(OutS).
p_generator_pair_predicate(This,Scope,Name, (Name,Resolved)):- ::(This,make_predicate_name,T195),call(T195,Name,Scope,Resolved).
p_generator_find_parents_definitions(_,_,[],[],[],[]).
p_generator_find_parents_definitions(This,Scope,[Parent],Fields,Predicates,SuperPreds):- ::(Scope,'get_parent_scope',T196),call(T196,Parent,ParentScope),::(ParentScope,'Hdr',T197),::(This,find_class_definitions,T198),call(T198,ParentScope,T197,Fields,Predicates,SuperPreds).
p_generator_make_super_call(_,CRes,PRes,N, (L:-R)):-tmp_lst(N,Ts),L=..[CRes,'This'|Ts],R=..[PRes,'This'|Ts].
p_generator_predicate_inherit(_,[],TotalPairs,TotalPairs,[]).
p_generator_predicate_inherit(This,[ (pred(CName,NC),CRes)|Ps],ParentPs,TotalPairs,[Sup|SuperPreds]):-member((pred(CName,NP),PRes),ParentPs),'!',delete(ParentPs, (pred(CName,NP),PRes),ParentPsWO),::(This,make_super_call,T199),call(T199,CRes,PRes,NP,Sup),::(This,predicate_inherit,T200),call(T200,[ (pred(CName,NC),CRes)|Ps],ParentPsWO,TotalPairs,SuperPreds).
p_generator_predicate_inherit(This,[ (CName,CRes)|Ps],ParentPs,[ (CName,CRes)|TotalPairs],SuperPreds):- ::(This,predicate_inherit,T201),call(T201,Ps,ParentPs,TotalPairs,SuperPreds).
p_generator_find_class_definitions(This,Scope,Hchild,Fields,Predicates,SuperPreds):- ::(Scope,'get_qouted_fields',T202),call(T202,ThisFields),::(Scope,'get_non_constructors',T203),call(T203,AllPreds),::(Scope,'pred_has_mod',T204),add_functor_args(T204,['static'],PHM),partition(PHM,AllPreds,_,NonStaticPreds),::(This,'pair_predicate',T205),add_functor_args(T205,[Scope],F),filter(F,NonStaticPreds,ThisPredicates),::(Hchild,'Parents',T206),::(This,find_parents_definitions,T207),call(T207,Scope,T206,ParentFields,ParentPredicates,_), (intersection(ThisFields,ParentFields,[]);throw('Cannot redefine a parents field.')),append(ParentFields,ThisFields,Fields),::(This,predicate_inherit,T208),call(T208,ThisPredicates,ParentPredicates,Predicates,SuperPreds).
p_generator_not_same_pred_name(_,Nm, (pred(Nm2,N),Res), (pred(Nm2,N),Res)):-Nm\=Nm2.
p_generator_get_unique_predicates(_,[],[]).
p_generator_get_unique_predicates(This,[ (pred(Nm,_),Res)|Ps],[ (Nm,Res)|OPs]):- ::(This,'not_same_pred_name',T209),add_functor_args(T209,[Nm],F),filter(F,Ps,FPs),::(This,get_unique_predicates,T210),call(T210,FPs,OPs).
p_generator_none_if_empty(_,_,[],'none').
p_generator_none_if_empty(This,Scope,[X|_],Y):- ::(This,make_predicate_name,T211),call(T211,X,Scope,Y).
p_generator_make_class(This,Def,ScopeAbove,Scope,SuperPreds):- ::(Def,'Hdr',T212),::(This,find_class_definitions,T213),call(T213,Scope,T212,Fields,PredicatesPaired,SuperPreds),::(This,get_unique_predicates,T214),call(T214,PredicatesPaired,UPredicates),::(Scope,'get_non_constructors',T215),call(T215,NonConstructors),::(Scope,'pred_has_mod',T216),add_functor_args(T216,['static'],PHM),partition(PHM,NonConstructors,Statics,_),::(This,'pair_predicate',T217),add_functor_args(T217,[Scope],F),maplist(F,Statics,StaticsPaired),::(This,get_unique_predicates,T218),call(T218,StaticsPaired,UStaticsPaired),::(T212,'Name',T219),::(This,make_class_type_functor,T220),call(T220,T219,ScopeAbove,Func),::(This,make_class_type_constructor_functor,T221),call(T221,T219,ScopeAbove,CFunc),::(This,make_class_type_args,T222),call(T222,ScopeAbove,Args),::(Scope,'get_constructors',T223),call(T223,Constructors),::(This,none_if_empty,T224),call(T224,Scope,Constructors,Constructor),::(This,make_class_type_constructor,T225),call(T225,T219,Fields,UPredicates,CBdy,Constructor,UStaticsPaired),PT=..[Func|Args],PC=..[CFunc|Args],list_to_comma_functor([PC,nb_setval(T219,'T')],Cache),::(This,'IO',T227),::(T227,'Out',T226),write_standard(T226, (PT:-nb_getval(T219,'T'))),write(T226,'.
'),write_standard(T226, (PC:-CBdy)),write(T226,'.
'),write_standard(T226, (:-Cache)),write(T226,'.
').
p_generator_append_if_not_blank(_,Prefix,PredicateName,Result):-Prefix\='',atomic_list_concat(['p',Prefix,PredicateName],Result).
p_generator_append_if_not_blank(_,'',PredicateName,PredicateName).
p_generator_make_predicate_name(This,pred(Nm,_),_,Nm):- ::(This,'Opts',T228),member('no_munge',T228),'!'.
p_generator_make_predicate_name(_,pred('',_),_,'').
p_generator_make_predicate_name(This,pred(PredicateName,_),Scope,Result):-PredicateName\='',::(Scope,'Prefix',T229),::(This,append_if_not_blank,T230),call(T230,T229,PredicateName,Result).
p_generator_make_class_type_functor(_,ClassName,Scope,Result):- ::(Scope,'Prefix',T231),atomic_list_concat(['class_def',T231,ClassName],Result).
p_generator_make_class_type_constructor_functor(_,ClassName,Scope,Result):- ::(Scope,'Prefix',T232),atomic_list_concat(['class_def_construct_',T232,ClassName],Result).
p_generator_make_class_type_args(This,Scope,Bdy):- ::(This,add_this,T233),call(T233,['T'],Scope,Bdy).
p_generator_make_class_type_constructor(This,Name,Fields,Predicates,Bdy,Constructor,Statics):- ::(This,'g_add_qoutes',T234),maplist(T234,Fields,QFields),::(This,next_tmp,T235),call(T235,T),::(This,expand_new,T236),call(T236,new('X','T',Name,QFields,Predicates,'none','none',Constructor),T,Statics,NewComs),list_to_comma_functor([class_def__classType('X')|NewComs],Bdy).
p_generator_expand_new(This,Command,Con,Result):- ::(This,expand_new,T237),call(T237,Command,Con,[],Result).
p_generator_expand_new(This,Command,Con,Statics,[build_class(Type,Class,Statics),::(Type,Constructor,Con), (Con\='none'*->Call;'true')]):-Command=..['new',Type,Class|Args],::(This,g_add_qoutes,T238),call(T238,'Constructor',Constructor),Call=..['call',Con,Class|Args].
p_generator_make_from_fact(_,Nm,ArgLst,[],Res):-Res=..[Nm|ArgLst].
p_generator_make_from_fact(_,Nm,ArgLst,Commands, (P1:-P2)):-Commands\=[],P1=..[Nm|ArgLst],list_to_comma_functor(Commands,P2).
p_generator_generate_defs_top(This,Defs):-class_def_scope(T240),::(This,'TopScope',T241),build_class(T240,T241,[]),::(T240,'Constructor',T239), (T239\=none*->call(T239,T241,Defs);true),::(This,generate_defs,T242),call(T242,Defs,T241),::(This,dump,T243),call(T243),::(This,interp,T244),call(T244).
p_generator_dump(This):- ::(This,'Opts',T245),not(member('no_dump',T245)),'!',::(This,'TopScope',T247),::(T247,'dump',T246),::(This,'IO',T248),call(T246,T248).
p_generator_dump(_).
p_generator_interp(This):- ::(This,'Opts',T249),not(member('no_interpret',T249)),'!',::(This,'IO',T251),::(T251,'Out',T250),write_standard(T250,'
:- ensure_loaded(\'./interpret.pl\').
:- p_interpret.').
p_generator_interp(_).
p_generator_generate_defs(This,[Def|Defs],Scope):- ::(This,generate_def,T252),call(T252,Def,Scope),::(This,generate_defs,T253),call(T253,Defs,Scope).
p_generator_generate_defs(_,[],_).
p_generator_resolve_arg_new(This,Scope,Reps,RepsOut,compound(P),AllCommands):-functor(P,'new',_),'!',::(This,next_tmp,T254),call(T254,Tmp),::(This,expand_new,T255),call(T255,P,tmp(Tmp),Commands),::(This,resolve_args,T256),call(T256,Scope,Reps,RepsOut,Commands,Commands1,Commands2,'false'),append(Commands2,Commands1,AllCommands).
p_generator_resolve_arg(_,_,Reps,Reps,T,Td,[],_):-member((T,Td),Reps),'!'.
p_generator_resolve_arg(This,Scope,Reps,Reps3,compound(Z),Tmp,Cmds4,_):-functor(Z,'::',2),arg(1,Z,X),arg(2,Z,Y),'!',::(This,next_tmp,T257),call(T257,Tmp),::(This,resolve_arg,T258),call(T258,Scope,[ (compound(Z),Tmp)|Reps],Reps2,X,Xd,Cmds1,'false'),::(This,resolve_arg,T259),call(T259,Scope,Reps2,Reps3,Y,Yd,Cmds2,'false'),append(Cmds1,Cmds2,Cmds3),append(Cmds3,[::(Xd,Yd,Tmp)],Cmds4).
p_generator_resolve_arg(This,Scope,Reps,Reps,atom(X),P,Commands,'true'):- ::(Scope,'lookup',T260),call(T260,pred(X,0),PredScope),'!',::(PredScope,'pred_is_constructor',T261), (not(call(T261,pred(X,0)));throw('Cannot directly call a constructor.')),::(This,make_dynamic,T262),call(T262,PredScope,X,[],P,Commands).
p_generator_resolve_arg(This,Scope,Reps,[ (atom(X),Tmp)|Reps],atom(X),Tmp,[C],_):- ::(Scope,'lookup',T263),call(T263,class(X),DefScp),'!',::(This,next_tmp,T264),call(T264,Tmp),::(This,make_class_type_functor,T265),call(T265,X,DefScp,F),::(This,add_this,T266),call(T266,[Tmp],DefScp,Ls),C=..[F|Ls].
p_generator_resolve_arg(This,_,R,R,atom(X),Y,[],_):-'!',::(This,g_add_qoutes,T267),call(T267,X,Y).
p_generator_resolve_arg(This,Scope,Reps,[ (var(Arg),Tmp)|Reps],var(Arg),Tmp,[::('This',ArgQ,Tmp)],_):- ::(Scope,'lookup',T268),call(T268,field(Arg),_),'!',::(This,g_add_qoutes,T269),call(T269,Arg,ArgQ),::(This,next_tmp,T270),call(T270,Tmp).
p_generator_resolve_arg(_,_,R,R,var(Arg),Arg,[],_):-'!'.
p_generator_resolve_arg(_,_,R,R,tmp(Arg),Arg,[],_):-'!'.
p_generator_resolve_arg(This,Scope,Reps,RepsOut,compound(P),Pd,Commands3,'true'):-P=..[Nm|Args],length(Args,N),::(Scope,'lookup',T271),call(T271,pred(Nm,N),PredScope),'!',::(PredScope,'pred_is_constructor',T272), (not(call(T272,pred(Nm,N)));throw('Cannot directly call a constructor.')),::(This,resolve_args,T273),call(T273,Scope,Reps,RepsOut,Args,Args2,Commands,'false'),::(This,make_dynamic,T274),call(T274,PredScope,Nm,Args2,Pd,Commands2),append(Commands,Commands2,Commands3).
p_generator_resolve_arg(This,Scope,Reps,RepsOut,compound(P),Pd,Commands,Chge):-P=..[Nm|Args],::(This,contin_change,T275),call(T275,Nm,Chge,Chge2),::(This,resolve_args,T276),call(T276,Scope,Reps,RepsOut,Args,Argsd,Commands,Chge2),Pd=..[Nm|Argsd].
p_generator_resolve_arg(This,Scope,Reps,RepsOut,P,Pd,Commands,Chge):-functor(P,Nm,_),Nm\='compound',::(This,resolve_arg,T277),call(T277,Scope,Reps,RepsOut,compound(P),Pd,Commands,Chge).
p_generator_resolve_args(This,Scope,Reps,RepsOut2,[Arg|Args],[NewArg|NewArgs],AllCommands,Chge):- ::(This,resolve_arg,T278),call(T278,Scope,Reps,RepsOut,Arg,NewArg,Commands,Chge),::(This,resolve_args,T279),call(T279,Scope,RepsOut,RepsOut2,Args,NewArgs,Commands2,Chge),append(Commands,Commands2,AllCommands).
p_generator_resolve_args(_,_,R,R,[],[],[],_).
p_generator_make_dynamic(This,Scope,Nm,Args,Res,[]):- ::(Scope,'is_top',T280),call(T280),'!',::(This,make_predicate_name,T281),call(T281,pred(Nm,_),Scope,NewName),Res=..[NewName|Args].
p_generator_make_dynamic(This,_,Nm,Args,Res,[::('This',Nm,Tmp)]):- ::(This,next_tmp,T282),call(T282,Tmp),Res=..['call',Tmp|Args].
p_generator_goal_funcs(_,[',',';','not']).
p_generator_contin_change(_,_,'false','false').
p_generator_contin_change(This,Nm,'true','true'):- ::(This,goal_funcs,T283),call(T283,L),member(Nm,L).
p_generator_contin_change(This,Nm,'true','false'):- ::(This,goal_funcs,T284),call(T284,L),not(member(Nm,L)).
p_generator_resolve_args_comma_functor(This,compound(B),Scope,Reps,RepsOut2,Res):-functor(B,',',_),'!',arg(1,B,Arg),arg(2,B,Bdy),::(This,resolve_arg_new,T285),::(This,resolve_arg,T286), (call(T285,Scope,Reps,RepsOut,Arg,CommandsNew),'!',append(CommandsNew,Res1,Res);call(T286,Scope,Reps,RepsOut,Arg,NewArg,Commands1,'true'),append(Commands1,[NewArg|Res1],Res)),::(This,resolve_args_comma_functor,T287),call(T287,Bdy,Scope,RepsOut,RepsOut2,Res1).
p_generator_resolve_args_comma_functor(This,B,Scope,Reps,RepsOut,Res):-functor(B,Nm,_),Nm\=',',::(This,resolve_arg_new,T288),::(This,resolve_arg,T289), (call(T288,Scope,Reps,RepsOut,B,Res),'!';call(T289,Scope,Reps,RepsOut,B,Bd,Commands,'true'),append(Commands,[Bd],Res)).
p_generator_add_this(This,N,Ls,Scope,Pred,Ls2):-N>=1,::(This,add_this,T290),call(T290,Ls,Scope,Pred,Ls2).
p_generator_add_this(_,0,Ls,Scope,Pred,Ls):- ::(Scope,'is_top',T291),::(Scope,'pred_has_mod',T292), (call(T291);call(T292,'static',Pred)),'!'.
p_generator_add_this(_,0,Ls,_,_,['_'|Ls]).
p_generator_add_this(_,Ls,Scope,Pred,Ls):- ::(Scope,'is_top',T293),::(Scope,'pred_has_mod',T294), (call(T293);call(T294,'static',Pred)),'!'.
p_generator_add_this(_,Ls,_,_,['This'|Ls]).
p_generator_add_this(_,Ls,Scope,Ls):- ::(Scope,'is_top',T295),call(T295),'!'.
p_generator_add_this(_,Ls,_,['This'|Ls]).
p_generator_generate_def(This,fact(FT),Scope):- ::(FT,'Left',T296),T296=..[Nm|ArgLstIn],::(This,resolve_args,T297),call(T297,Scope,[],_,ArgLstIn,ArgLst,Commands,'false'),::(FT,'N',T298),::(This,make_predicate_name,T299),call(T299,pred(Nm,T298),Scope,NewName),count_occurences('This', (Commands,ArgLst),Ns),::(This,add_this,T300),call(T300,Ns,ArgLst,Scope,pred(Nm,T298),ArgLst2),::(This,make_from_fact,T301),call(T301,NewName,ArgLst2,Commands,Res),::(This,'IO',T303),::(T303,'Out',T302),write_standard(T302,Res),write(T302,'.
').
p_generator_generate_def(This,rule(RT),Scope):- ::(RT,'Left',T304),T304=..[Nm|ArgLstIn],::(This,resolve_args,T305),call(T305,Scope,[],OutReps,ArgLstIn,ArgLst,Commands,'false'),::(RT,'Right',T306),::(This,resolve_args_comma_functor,T307),call(T307,T306,Scope,OutReps,_,Res),append(Commands,Res,Body),::(RT,'N',T308),::(This,make_predicate_name,T309),call(T309,pred(Nm,T308),Scope,NewName),count_occurences('This', (Body,ArgLst),Ns),::(This,add_this,T310),call(T310,Ns,ArgLst,Scope,pred(Nm,T308),ArgLst2),Pd=..[NewName|ArgLst2],list_to_comma_functor(Body,CBdy),::(This,'IO',T312),::(T312,'Out',T311),write_standard(T311, (Pd:-CBdy)),write(T311,'.
').
p_generator_generate_def(This,class_def(C),Scope):- ::(Scope,'scope_of',T313),::(C,'Hdr',T315),::(T315,'Name',T314),call(T313,T314,NewScope),::(This,make_class,T316),call(T316,C,Scope,NewScope,Supers),::(C,'Bdy',T317),::(This,generate_defs,T318),call(T318,T317,NewScope),::(This,'IO',T320),::(T320,'Out',T319),write_list(T319,Supers).
p_generator_generate_def(This,field(Nm),Scope):- ::(Scope,'is_top',T321),::(This,compile_exception,T322), (not(call(T321));call(T322,['Field \'',Nm,'\' defined outside of a class.'])).
p_generator_g_add_qoutes(This,X,X):- ::(This,'Opts',T323),member('no_qoutes',T323),'!'.
p_generator_g_add_qoutes(_,X,Y):-add_qoutes(X,Y).
p_generator_compile_exception(_,Ms):-throw_atoms(Ms).
:-add_to_defined_names([class(io),class(class_header_token),class(class_token),class(clause_token),class(rule_token),class(fact_token),class(parser),pred(not_unified,2),pred(bind_vars_to_name,1),pred(bind_vars_to_name,1),class(scope),pred(compile,4),pred(quick,3),pred(quick,2),pred(quick,1),pred(generic_compile,2),pred(generic_compile,3),pred(quick_generic,1),class(generator)]).