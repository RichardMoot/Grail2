% ====================================================================
% create_lexicon_window
% open a window which contains the current lexicon
% ====================================================================

create_lexicon_window :-
       my_tk_interpreter(I),
       tcl_eval(I,'source lex.tcl',_).

% =

lexicon :-
       my_tk_interpreter(I),
       'current lex'(Words),
       lex_listbox(Words,'.lex',I).

delete_lex_entry :-
       my_tk_interpreter(I),
       tcl_eval(I,'set tags [.lex.c gettags lex_bar]
                   set list_ind [lsearch -regexp $tags {^(lex_item)[0-9]+$}]
                   set itemno_temp [lindex $tags $list_ind]
                   set cur_sel [string range $itemno_temp 8 end]',NumS),
       NumS \== [],
       number_chars(Num,NumS),
       delete_lex_entry1(Num).

delete_lex_entry1(Num) :-
       my_tk_interpreter(I),
       'current lex'(Lex0),
       delete_item_num(Num,Lex0,Lex),
       retractall(lex(_,_,_)),
       assert_lex(Lex),
       retractall('current lex'(_)),
       assert('current lex'(Lex)),
       retractall('previous lex'(_)),
       assert('previous lex'(Lex0)),
       tcl_eval(I,'.lex.c delete all',_),
       lex_listbox(Lex,'.lex',I).

new_lex_entry :-
       my_tk_interpreter(I),
       tcl_eval(I,'.lex.c delete lex_bar',_),
       edit_lex_entry.

edit_lex_entry :-
       my_tk_interpreter(I),
       tcl_eval(I,'if {[winfo exists .lexedit]} {
                   wm deiconify .lexedit
                   raise .lexedit
                   } else {
                   prolog create_lexedit_window
                   }',_),
       'current lex'(Lex),
       retractall('current entry'(_)),
       retractall('unmod entry'(_)),
      (tcl_eval(I,'set tags [.lex.c gettags lex_bar]
                   set list_ind [lsearch -regexp $tags {^(lex_item)[0-9]+$}]
                   set itemno_temp [lindex $tags $list_ind]
                   set cur_sel [string range $itemno_temp 8 end]',NumS),
       NumS \== [] ->
       number_chars(Num,NumS),
       get_item_num(Num,Lex,Entry),
       Entry=Pros:Form0-Sem,
       macro_expand(Form0,Form),
       assert('current entry'(Form)),
       assert('unmod entry'(Pros:Form-Sem))
      ;
       Pros='',
       Form=' * ',
       Sem='',
       Entry=Pros:Form-Sem,
       assert('current entry'(Form)),
       assert('unmod entry'(Entry))
      ),
       paint_entry(Entry),
      (select_var(Form,0) -> true ; true).

create_lexedit_window :-
       my_tk_interpreter(I),
       findall(UI,'unary mode'(UI),Us),
       findall(BI,'binary mode'(BI),Bs),
       findall(CL,'current literal'(CL),Lits),
       findall(CM,'current macro'(CM),Macros),
       tcl_eval(I,'source lexedit.tcl',_),
       constr(dl,  I,'.lexedit.f3.dl' ,black,x,16,16,_),
       constr(prod,I,'.lexedit.f3.p'  ,black,x,16,15,_),
       constr(dr,  I,'.lexedit.f3.dr' ,black,x,16,16,_),
       constr(dia, I,'.lexedit.f4.dia',black,x,15,15,_),
       constr(box, I,'.lexedit.f4.box',black,x,15,15,_),
       menu_list(Bs,I,'.lexedit.f3.index.m',select_binary), 
       menu_list(Us,I,'.lexedit.f4.index.m',select_unary),
       menu_list(Lits,I,'.lexedit.f5.atom.m',set_literal),
       menu_list(Macros,I,'.lexedit.f5.macro.m',set_macro).

menu_list([],_,_,_).
menu_list([X|Xs],I,W,UB) :-
       tcl_eval(I,format('~w add command -label ~p -command {prolog {~w(~k)}}',[W,X,UB,X]),_),
       menu_list(Xs,I,W,UB).

select_unary(U) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('set un_index ~w',[U]),_).

select_binary(B) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('set bin_index ~w',[B]),_).

paint_entry1(Form0) :-
       my_tk_interpreter(I),
       tcl_eval(I,'.lexedit.f0.c delete all',_),
       macro_expand(Form0,Form),
       paint_formula(Form,I,0,10,X,32,Y,_),
       tcl_eval(I,format('.lexedit.f0 configure -width [expr ~w+10]',[X]),_),
       tcl_eval(I,format('.lexedit.f0 configure -height [expr ~w+8]',[Y]),_),
       tcl_eval(I,format('.lexedit.f0.c configure -width [expr ~w+10]',[X]),_),
       tcl_eval(I,format('.lexedit.f0.c configure -height [expr ~w+8]',[Y]),_),
       tcl_eval(I,'wm geometry .lexedit {}',_).


paint_entry(Pros:A0-Sem) :-
       my_tk_interpreter(I),
       tcl_eval(I,'.lexedit.f0.c delete all',_),
       macro_expand(A0,A),
       paint_formula(A,I,0,10,X,32,Y,_),
       tcl_eval(I,format('.lexedit.f0 configure -width [expr ~w+10]',[X]),_),
       tcl_eval(I,format('.lexedit.f0 configure -height [expr ~w+8]',[Y]),_),
       tcl_eval(I,format('.lexedit.f0.c configure -width [expr ~w+10]',[X]),_),
       tcl_eval(I,format('.lexedit.f0.c configure -height [expr ~w+8]',[Y]),_),
       tcl_eval(I,'wm geometry .lexedit {}',_),
       tcl_eval(I,format('set itempros {~q}',[Pros]),_),
       tcl_eval(I,format('set itemsem {~q}',[Sem]),_),
       tcl_eval(I,'update',_).

paint_formula(bogus(A),Int,Num,X0,X,Y0,Y,t(MidB,B2)) :-
      (A = ' * ' -> Tag = 'emtpy' ; Tag = ''),
       tcl_eval(Int,format('.lexedit.f0.c create text ~w ~w -tags {bogus node ~w b0 t~w} -anchor w -text "~p" -fill red',[X0,Y0,Tag,Num,A]),Item),
       tcl_eval(Int,format('.lexedit.f0.c bbox ~s',[Item]),BBox),
       quadruple(Int,BBox,B1,B2,X,Y),
       mid(B1,X,MidB).

paint_formula(lit(A),Int,Num,X0,X,Y0,Y,t(MidB,B2)) :-
      (A = ' * ' -> Tag = 'emtpy' ; Tag = ''),
       tcl_eval(Int,format('.lexedit.f0.c create text ~w ~w -tags {node ~w b0 t~w} -anchor w -text "~p"',[X0,Y0,Tag,Num,A]),Item),
       tcl_eval(Int,format('.lexedit.f0.c bbox ~s',[Item]),BBox),
       quadruple(Int,BBox,B1,B2,X,Y),
       mid(B1,X,MidB).

paint_formula(dr(I,A,B),Int,Num0,X0,X,Y0,Y,t(MidX,B2)) :-
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+35,
       paint_formula(A,Int,Num1,X0,X1,Y1,Y2,t(MidA,AY)),
       X2 is X1+15,
       paint_formula(B,Int,Num2,X2,X3,Y1,Y3,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_constr(dr,I,Int,'.lexedit.f0.c',black,Num0,MidX,Y0,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.lexedit.f0.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.lexedit.f0.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_formula(dl(I,A,B),Int,Num0,X0,X,Y0,Y,t(MidX,B2)) :-
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+35,
       paint_formula(A,Int,Num1,X0,X1,Y1,Y2,t(MidA,AY)),
       X2 is X1+15,
       paint_formula(B,Int,Num2,X2,X3,Y1,Y3,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_constr(dl,I,Int,'.lexedit.f0.c',black,Num0,MidX,Y0,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.lexedit.f0.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.lexedit.f0.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_formula(p(I,A,B),Int,Num0,X0,X,Y0,Y,t(MidX,B2)) :-
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+35,
       paint_formula(A,Int,Num1,X0,X1,Y1,Y2,t(MidA,AY)),
       X2 is X1+15,
       paint_formula(B,Int,Num2,X2,X3,Y1,Y3,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_constr(prod,I,Int,'.lexedit.f0.c',black,Num0,MidX,Y0,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.lexedit.f0.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.lexedit.f0.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_formula(dia(I,A),Int,Num0,X0,X,Y0,Y,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+35,
       paint_formula(A,Int,Num1,X0,X1,Y1,Y,t(MidA,AY)),
       paint_constr(dia,I,Int,'.lexedit.f0.c',black,Num0,MidA,Y0,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.lexedit.f0.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_formula(box(I,A),Int,Num0,X0,X,Y0,Y,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+35,
       paint_formula(A,Int,Num1,X0,X1,Y1,Y,t(MidA,AY)),
       paint_constr(box,I,Int,'.lexedit.f0.c',black,Num0,MidA,Y0,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.lexedit.f0.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

store_entry :-
       my_tk_interpreter(I),
       store_entry(I).

store_entry(I) :-
       'current entry'(Syn),
     ( select_var(Syn,0) ->
       tcl_eval(I,'dialog .d {Formula Error} {The edited formula\
       is underspecified} error 0 {Cancel}',_)
     ;
       'current lex'(Lex0),
       tcl_eval(I,'set itempros',Str1),
       tokenize_term(Str1,Pros),
       tcl_eval(I,'set itemsem',Str2),
       tokenize_term(Str2,Sem0),
       melt_sem(Sem0,Sem),
       ( is_formula(Syn) ->
         assert(lex(Pros,Syn,Sem)),
         numbervars(Sem,47,_),
         insert_lex_entry(Lex0,Pros:Syn-Sem,0,LexNo,Lex),
         retractall('current lex'(_)),
         retractall('previous lex'(_)),
         assert('current lex'(Lex)),
         assert('previous lex'(Lex0)),
         tcl_eval(I,'.lex.c delete all',_),
         lex_listbox(Lex,'.lex',I),
         tcl_eval(I,format('positionLexBar ~w',[LexNo]),_)
       ;
         tcl_eval(I,'dialog .d {Formula Error} {The formula you\
         tried to store is not welltyped} error 0 {Cancel}',_)
       )
     ).

insert_lex_entry([],E,N,N,[E]).
insert_lex_entry([L0|Ls0],E,N0,N,Ls) :-
       E @< L0 ->
       Ls = [E,L0|Ls0],
       N is N0
     ;
       Ls= [L0|Ls1],
       N1 is N0+1,
       insert_lex_entry(Ls0,E,N1,N,Ls1).      


paste_constr(Num,P,A,SI) :-
       tokenize_term(SI,I),
       add_index(A,SI),
       construct(P,A,I,Term),
       'current entry'(Form0),
       num_to_list(Num,List),
       paste_form1(List,Form0,Term,Form),
       retractall('current entry'(_)),
       assert('current entry'(Form)),
       paint_entry1(Form),
      (select_var(Term,Num) -> true
      ;select_var(Form,0) -> true
      ;true).

construct(P,A,I,Term) :-
       functor(Term,P,A),
       arg(1,Term,I),
       construct1(2,A,Term).

construct1(N,N,Term) :-
       arg(N,Term,' * ').
construct1(N0,N,Term) :-
       N0<N,
       arg(N0,Term,' * '),
       N1 is N0+1,
       construct1(N1,N,Term).

add_index(2,Idx) :-
       add_u_index(Idx).

add_index(3,Idx) :-
       add_b_index(Idx).

add_b_index(SIdx) :-
       tokenize_term(SIdx,Idx),
       findall(BI,'binary mode'(BI),Bs),
      (member_chk(Idx,Bs) ->
       true
      ;
       assert('binary mode'(Idx)),
       assert(lazy_dl(Idx)),
       assert(lazy_dr(Idx)),
       assert(external(Idx)),
       my_tk_interpreter(Int),
       tcl_eval(Int,format('if {[winfo exists .postedit.f3.index.m]} {
                            .postedit.f3.index.m add command -label {~p} -command {prolog select_binary_p(~w)}}',[Idx,Idx]),_),
       tcl_eval(Int,format('if {[winfo exists .lexedit.f3.index.m]} {
                            .lexedit.f3.index.m add command -label {~p} -command {prolog select_binary(~w)}}',[Idx,Idx]),_),
       tcl_eval(Int,'if {[winfo exists .an]} {
                        prolog update_analysis_window
                        }',_)
      ).

add_u_index(SIdx) :-
       tokenize_term(SIdx,Idx),
       findall(UI,'unary mode'(UI),Us),
      (member_chk(Idx,Us) ->
       true
      ;
       assert('unary mode'(Idx)),
       assert(lazy_unpack(Idx)),
       assert(external_dia(Idx)),
       my_tk_interpreter(Int),
       tcl_eval(Int,format('if {[winfo exists .postedit.f4.index.m]} {
                            .postedit.f4.index.m add command -label {~p} -command {prolog select_unary_p(~w)}}',[Idx,Idx]),_),
       tcl_eval(Int,format('if {[winfo exists .lexedit.f4.index.m]} {
                            .lexedit.f4.index.m add command -label {~p} -command {prolog select_unary(~w)}}',[Idx,Idx]),_),
       tcl_eval(Int,'if {[winfo exists .an]} {
                        prolog update_analysis_window
                        }',_)
      ).

add_literal(LitS) :-
       tokenize_term(LitS,Lit),
       \+ 'current macro'(Lit),
      ('current literal'(Lit) ->
       true
      ;
       my_tk_interpreter(I),
       assert('current literal'(Lit)),
       tcl_eval(I,format('.lexedit.f5.atom.m add command -label ~p -command {prolog set_literal(~w)}',[Lit,Lit]),_)
      ),
      (literal(Lit) ->
       true
      ;
       assert(atomic_formula(Lit))
      ),
       set_literal(Lit).

set_literal(Lit) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('set literal {~p}',[Lit]),_),
       tcl_eval(I,format('
                   set tags [.lexedit.f0.c gettags selectbox]
                   set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
                   set path_temp [lindex $tags $list_ind]
                   set path [string range $path_temp 1 end]
                   if {$path != {}} {
                   set prologcall ""
                   append prologcall "paste_atom1($path," {lit(~k))} 
                   prolog $prologcall
                   }',[Lit]),_).

set_macro(Macro) :-
       my_tk_interpreter(I),
       macro_expand(Macro,Form),
       tcl_eval(I,format('
                   set tags [.lexedit.f0.c gettags selectbox]
                   set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
                   set path_temp [lindex $tags $list_ind]
                   set path [string range $path_temp 1 end]
                   if {$path != {}} {
                   set prologcall ""
                   append prologcall "paste_atom1($path," {~k)} 
                   prolog $prologcall
                   }',[Form]),_).

store_macro(Num,LitS) :-
       my_tk_interpreter(I),
       'current entry'(Form0),
       tokenize_term(LitS,Lit),
       num_to_list(Num,List),
       copy_form(List,Form0,Form),
      (select_var(Form,0) ->
       tcl_eval(I,'dialog .d {Formula Error} {The edited formula\
      is underspecified} error 0 {Cancel}',_)
      ;
      (Lit = '' ->
       tcl_eval(I,'dialog .d {No Name Given} {You must specify a name for the macro.} error 0 Cancel',"666")
      ;
      ('current literal'(Lit) ->
       tcl_eval(I,'dialog .d {Redefining Literal} {You cannot redefine your atomic formulas!} error 0 Cancel',"666")
      ;
      ('current macro'(Lit) ->
       tcl_eval(I,format('dialog .d {Redefining Macro} {Do you wish to redefine the previous definition of ~w?} warning 1 Yes No',[Lit]),"0")
      ;
       true),
       retractall(macro(Lit,_)),
       assert(macro(Lit,Form)),
       assert('current macro'(Lit)),
       tcl_eval(I,format('.lexedit.f5.macro.m add command -label ~p -command {prolog set_macro(~w)}',[Lit,Lit]),_)))).

paste_atom1(Num,Lit) :-
       'current entry'(Form0),
       num_to_list(Num,List),
       paste_form(List,Form0,Lit,Form),
       retractall('current entry'(_)),
       assert('current entry'(Form)),
       paint_entry1(Form),
      (select_var(Form,0) -> true
      ;true).       

paste(Num) :-
       'current entry'(Form0),
       'paste buffer'(P),
       num_to_list(Num,List),
       paste_form(List,Form0,P,Form),
       retractall('current entry'(_)),
       assert('current entry'(Form)),
       paint_entry1(Form),
      (select_var(P,Num) -> true
      ;select_var(Form,0) -> true
      ;true).  

cut(Num) :-
       'current entry'(Form0),
       retractall('current entry'(_)),
       retractall('paste buffer'(_)),
       num_to_list(Num,List),
       delete_form(List,Form0,Form,P),
       assert('current entry'(Form)),
       assert('paste buffer'(P)),
       paint_entry1(Form),
       select_form(Num).       

copy(Num) :-
       'current entry'(Form),
       retractall('paste buffer'(_)),
       num_to_list(Num,List),
       copy_form(List,Form,P),
       assert('paste buffer'(P)).

delete(Num) :-
       'current entry'(Form0),
       retractall('current entry'(_)),
       num_to_list(Num,List),
       delete_form(List,Form0,Form,_),
       assert('current entry'(Form)),
       paint_entry1(Form).       

select_var(' * ',Num) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('.lexedit.f0.c delete selectbox
                   set path t~w
                   set tags $path
                   set list_ind2 [lsearch -regexp $tags {^(b)[0123456789]+$}]
                   set branches [lindex $tags $list_ind2]
                   set box [.lexedit.f0.c bbox $path]
                   set boxl [expr [lindex $box 0] -1]
                   set boxr [expr [lindex $box 2] +1]
                   set boxu [expr [lindex $box 1] -1]
                   set boxd [expr [lindex $box 3] +1]
                   set s_tags {}
                   lappend s_tags selectbox $branches $path
           .lexedit.f0.c create line $boxl $boxu $boxr $boxu -tags $s_tags
           .lexedit.f0.c create line $boxl $boxd $boxr $boxd -tags $s_tags
           .lexedit.f0.c create line $boxl $boxd $boxl $boxu -tags $s_tags
           .lexedit.f0.c create line $boxr $boxu $boxr $boxd -tags $s_tags',[Num]),_).

select_var(box(_,A),Num0) :-
       Num is (Num0*10)+1,
       select_var(A,Num).
       
select_var(dia(_,A),Num0) :-
       Num is (Num0*10)+1,
       select_var(A,Num).
       
select_var(p(_,A,_),Num0) :-
       Num is (Num0*10)+1,
       select_var(A,Num).

select_var(p(_,_,B),Num0) :-
       Num is (Num0*10)+2,
       select_var(B,Num).

select_var(dl(_,A,_),Num0) :-
       Num is (Num0*10)+1,
       select_var(A,Num).

select_var(dl(_,_,B),Num0) :-
       Num is (Num0*10)+2,
       select_var(B,Num).

select_var(dr(_,A,_),Num0) :-
       Num is (Num0*10)+1,
       select_var(A,Num).

select_var(dr(_,_,B),Num0) :-
       Num is (Num0*10)+2,
       select_var(B,Num).

select_form(Num) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('.lexedit.f0.c delete selectbox
                   set path t~w
                   set list_ind2 [lsearch -regexp $tags {^(b)[0123456789]+$}]
                   set branches [lindex $tags $list_ind2]
                   set box [.lexedit.f0.c bbox $path]
                   set boxl [expr [lindex $box 0] -1]
                   set boxr [expr [lindex $box 2] +1]
                   set boxu [expr [lindex $box 1] -1]
                   set boxd [expr [lindex $box 3] +1]
                   set s_tags {}
                   lappend s_tags selectbox $branches $path
           .lexedit.f0.c create line $boxl $boxu $boxr $boxu -tags $s_tags
           .lexedit.f0.c create line $boxl $boxd $boxr $boxd -tags $s_tags
           .lexedit.f0.c create line $boxl $boxd $boxl $boxu -tags $s_tags
           .lexedit.f0.c create line $boxr $boxu $boxr $boxd -tags $s_tags',[Num]),_).


copy_form([],P,P).
copy_form([N|Ns],F,P) :-
       sub_term(F,_,N,A,_),
       copy_form(Ns,A,P).

delete_form([],P ,' * ',P).
delete_form([N|Ns],F0, F ,P  ) :-
       sub_term(F0,F,N,A0,A),
       delete_form(Ns,A0,A,P).

paste_form1([],dia(_,A),dia(I,_),dia(I,A)).
paste_form1([],dia(_,A),box(I,_),box(I,A)).

paste_form1([],box(_,A),box(I,_),box(I,A)).
paste_form1([],box(_,A),dia(I,_),dia(I,A)).

paste_form1([],dr(_,A,B),dl(J,_,_),dl(J,B,A)).
paste_form1([],dr(_,A,B),dr(J,_,_),dr(J,A,B)).
paste_form1([],dr(_,A,B),p(J,_,_),p(J,A,B)).

paste_form1([],dl(_,A,B),dr(J,_,_),dr(J,B,A)).
paste_form1([],dl(_,A,B),p(J,_,_),p(J,A,B)).
paste_form1([],dl(_,A,B),dl(J,_,_),dl(J,A,B)).

paste_form1([],p(_,A,B),p(J,_,_),p(J,A,B)).
paste_form1([],p(_,A,B),dl(J,_,_),dl(J,A,B)).
paste_form1([],p(_,A,B),dr(J,_,_),dr(J,A,B)).
       
paste_form1([],' * ',P,P).
paste_form1([N|Ns],F0,P,F) :-
       sub_term(F0,F,N,A0,A),
       paste_form1(Ns,A0,P,A).

paste_form([],' * ',P,P).
paste_form([N|Ns],F0,P,F) :-
       sub_term(F0,F,N,A0,A),
       paste_form(Ns,A0,P,A).

sub_term(dr(I,X0,Y),dr(I,X,Y),1,X0,X).
sub_term(dr(I,X,Y0),dr(I,X,Y),2,Y0,Y).
sub_term(dl(I,X0,Y),dl(I,X,Y),1,X0,X).
sub_term(dl(I,X,Y0),dl(I,X,Y),2,Y0,Y).
sub_term(p(I,X0,Y),p(I,X,Y),1,X0,X).
sub_term(p(I,X,Y0),p(I,X,Y),2,Y0,Y).
sub_term(dia(I,X0),dia(I,X),1,X0,X).
sub_term(zip(I,X0),zip(I,X),1,X0,X).
sub_term(box(I,X0),box(I,X),1,X0,X).
sub_term(l(I,X0),l(I,X),1,X0,X).
sub_term(r(I,X0),r(I,X),1,X0,X).
sub_term(unzip(I,X0),unzip(I,X),1,X0,X).
sub_term(unpack(I,X0),unpack(I,X),1,X0,X).


% =

lex_listbox(List,W,I) :-
       lex_listbox1(List,0,10,X0,20,Y,W,I),
      (X0 < 120 -> X=120 ; X=X0),
       tcl_eval(I,format('~w.c configure -width [expr ~w]',[W,X]),_),
       tcl_eval(I,format('~w.c configure -scrollregion "6 9 [expr ~w-6] [expr ~w-10]"',[W,X,Y]),_).

lex_listbox1([],_,X,X,Y,Y,_,_).
lex_listbox1([Pros:F0-_Sem|Fs],N0,X0,X,Y0,Y,W,I) :-
       macro_expand(F0,F),
       tcl_eval(I,format('fontWidget ~w.c create text ~w ~w -font -*-times-medium-i-normal--18-* -tags {lex~w} -anchor w -text "~w - "',[W,X0,Y0,N0,Pros]),Item),
       tcl_eval(I,format('~w.c bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,_,_,X1,_),
       name(W,Str),
       append(Str,[46,99],Str2),
       name(WCanv,Str2),
       listbox_item(F,N0,1,X1,X2,Y0,_,WCanv,I),
       N is N0+1,
       Y3 is Y0+22,
       lex_listbox1(Fs,N,X0,X3,Y3,Y,W,I),
       X is max(X2,X3).

listbox_item(lit(F),N,_,X0,X,Y0,Y,W,I) :-
       tcl_eval(I,format('~w create text ~w ~w -tags {lex~w} -anchor w -text "~w"',[W,X0,Y0,N,F]),Item),
       tcl_eval(I,format('~w bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,_B1,_B2,X,Y).

listbox_item(p(J,A,B),N,Br,X0,X,Y0,Y,W,I) :-
       paint_bo(Br,N,X0,X1,Y0,Y1,black,W,I),
       binding(A,p(J,A,B),BrA),
       listbox_item(A,N,BrA,X1,X2,Y0,Y2,W,I),
       X2A is X2+3,
       paint_constr(prod,J,I,W,black,'{lexx}',X2A,Y0,[_,_,_,Y3,X3]),
       binding(B,p(J,A,B),BrB),
       listbox_item(B,N,BrB,X3,X4,Y0,Y4,W,I),
       paint_bc(Br,N,X4,X,Y0,_,black,W,I),
       MY0 is max(Y1,Y2),
       MY1 is max(Y3,Y4),
       Y is max(MY0,MY1).

listbox_item(dl(J,A,B),N,Br,X0,X,Y0,Y,W,I) :-
       paint_bo(Br,N,X0,X1,Y0,Y1,black,W,I),
       binding(A,dl(J,A,B),BrA),
       listbox_item(A,N,BrA,X1,X2,Y0,Y2,W,I),
       X2A is X2+2,
       paint_constr(dl,J,I,W,black,'{lexx}',X2A,Y0,[_,_,_,Y3,X3]),
       binding(B,dl(J,A,B),BrB),
       listbox_item(B,N,BrB,X3,X4,Y0,Y4,W,I),
       paint_bc(Br,N,X4,X,Y0,_,black,W,I),
       MY0 is max(Y1,Y2),
       MY1 is max(Y3,Y4),
       Y is max(MY0,MY1).

listbox_item(dr(J,A,B),N,Br,X0,X,Y0,Y,W,I) :-
       paint_bo(Br,N,X0,X1,Y0,Y1,black,W,I),
       binding(A,dr(J,A,B),BrA),
       listbox_item(A,N,BrA,X1,X2,Y0,Y2,W,I),
       X2A is X2+2,
       paint_constr(dr,J,I,W,black,'{lexx}',X2A,Y0,[_,_,_,Y3,X3]),
       binding(B,dr(J,A,B),BrB),
       listbox_item(B,N,BrB,X3,X4,Y0,Y4,W,I),
       paint_bc(Br,N,X4,X,Y0,_,black,W,I),
       MY0 is max(Y1,Y2),
       MY1 is max(Y3,Y4),
       Y is max(MY0,MY1).

listbox_item(dia(J,A),N,Br,X0,X,Y0,Y,W,I) :-
       X0A is X0+2,
       paint_constr(dia,J,I,W,black,'{lexx}',X0A,Y0,[_,_,_,Y1,X1]),
       X1A is X1+2,
       paint_bo(Br,N,X1A,X2,Y0,Y2,black,W,I),
       binding(A,dia(J,A),BrA),
       listbox_item(A,N,BrA,X2,X3,Y0,Y3,W,I),
       paint_bc(Br,N,X3,X,Y0,_,black,W,I),
       MY0 is max(Y1,Y2),
       Y is max(MY0,Y3).

listbox_item(box(J,A),N,Br,X0,X,Y0,Y,W,I) :-
       X0A is X0+2,
       paint_constr(box,J,I,W,black,'{lexx}',X0A,Y0,[_,_,_,Y1,X1]),
       X1A is X1+3,
       paint_bo(Br,N,X1A,X2,Y0,Y2,black,W,I),
       binding(A,box(J,A),BrA),
       listbox_item(A,N,BrA,X2,X3,Y0,Y3,W,I),
       paint_bc(Br,N,X3,X,Y0,_,black,W,I),
       MY0 is max(Y1,Y2),
       Y is max(MY0,Y3).

% ====================================================================
% test_lex
% Check for typos in the formulas assigned to lexical entries
% ====================================================================

test_lex:-
       findall(lex(V,X,W),(lex(V,X0,W),
                           macro_expand(X0,X)),L),
       map_is_formula(L).

map_is_formula([]).
map_is_formula([lex(V,X0,W)|Xs]) :-
     ( is_formula(X0) ->
       true
     ;
       retractall(lex(V,X0,W)
     ),
       my_tk_interpreter(I),
       tcl_eval(I,format('dialog .d {Lexicon Error} {A lexical entry\
       for "~k" does not have a valid formula assigned to it.} error 0 "Delete" "View"',[V]),Answ),
       ( Answ = "0" ->
         true
       ;
         tcl_eval(I,'if {[winfo exists .lexedit]} {
                     wm deiconify .lexedit
                     raise .lexedit
                     } else {
                     prolog create_lexedit_window
                     }',_),
         make_formula(X0,X),
         retractall('current entry'(_)),
         assert('current entry'(X)),
         tcl_eval(I,format('set itempros {~q}
                            set itemsem {~q}
                            update idletasks
                            update',[V,W]),_),
         paint_entry(V:X-W),
         tcl_eval(I,format('set itempros {~q}
                            set itemsem {~q}
                            update idletasks
                            update',[V,W]),_) 
       )
     ),
       map_is_formula(Xs).

is_formula(lit(_)).
is_formula(dia(_,A)) :-
       is_formula(A).
is_formula(box(_,A)) :-
       is_formula(A).
is_formula(dl(_,A,B)) :-
       is_formula(A),
       is_formula(B).
is_formula(dr(_,A,B)) :-
       is_formula(A),
       is_formula(B).
is_formula(p(_,A,B)) :-
       is_formula(A),
       is_formula(B).

make_formula(lit(X),lit(X)) :- !.
make_formula(dia(I,A),dia(I,B)) :-
       !,
       make_formula(A,B).
make_formula(box(I,A),box(I,B)) :-
       !,
       make_formula(A,B).
make_formula(dl(I,A,B),dl(I,C,D)) :-
       !,
       make_formula(A,C),
       make_formula(B,D).
make_formula(dr(I,A,B),dr(I,C,D)) :-
       !,
       make_formula(A,C),
       make_formula(B,D).
make_formula(p(I,A,B),p(I,C,D)) :-
       !,
       make_formula(A,C),
       make_formula(B,D).
make_formula(X,bogus(X)).
