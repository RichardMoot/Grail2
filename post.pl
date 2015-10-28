% ================================================================
% create_postulate_window
% open a window containing a canvas with tree representations
% of the structural postulates
% ================================================================

create_postulate_window :-
       my_tk_interpreter(I),
       tcl_eval(I,'source post.tcl',_),
       post_canvas(I).

post_listbox(List,W,I) :-
       post_listbox1(List,0,0,MinX,0,MaxX,20,Y,W,I),
       Width is -MinX+MaxX+20,
       tcl_eval(I,format('foreach i [~w.c find withtag pname] {
                  ~w.c itemconfigure $i -anchor e
                  set crd [~w.c coords $i]
                  set ycrd [lindex $crd 1]
                  ~w.c coords $i ~w $ycrd
                  }',[W,W,W,W,MaxX]),_),
       tcl_eval(I,format('~w.c configure -width [expr ~w]',[W,Width]),_),
       tcl_eval(I,format('~w.c configure -scrollregion "[expr ~w-10] 9 [expr ~w+10] [expr ~w-10]"',[W,MinX,MaxX,Y]),_).

post_listbox1([],_,MinX,MinX,MaxX,MaxX,Y,Y,_,_).
post_listbox1([postulate1(A,B,Name)|Ps],N0,MinX0,MinX,X0,MaxX,Y0,Y,W,I) :-
       numbervars_post(A,0,_),
       numbervars_post(B,0,_),
       name(W,Str),
       append(Str,[46,99],Str2),
       name(WCanv,Str2),
       X0R is MinX0-20,
       X0A is X0+24,
       tcl_eval(I,format('~w.c create line [expr ~w-16] [expr ~w+0] [expr ~w+16] [expr ~w+0] -fill $grail_dfg -arrow last -tags {g_dfg arrow dir_lr}',[W,X0,Y0,X0,Y0]),_), 
       p_listbox_item_r(B,N0,1,X0R,MinX1,Y0,_,'$grail_dfg',WCanv,I),
       p_listbox_item(A,N0,1,X0A,X1,Y0,_Y1,'$grail_dfg',WCanv,I),
       N is N0+1,
       X2 is X1+10,
       tcl_eval(I,format('fontWidget ~w.c create text ~w ~w -font -*-times-medium-i-normal--18-* -tags {g_dfg lex~w pname} -anchor w -text "(~w)" -fill $grail_dfg',[W,X2,Y0,N0,Name]),Item),
       tcl_eval(I,format('~w.c bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,_,_,X3,_),
       Y2 is Y0+22,
       post_listbox1(Ps,N,MinX0,MinX2,X0,X4,Y2,Y,W,I),
       MinX is min(MinX1,MinX2),
       MaxX is max(X3,X4).

post_listbox1([postulate(A,B,Name)|Ps],N0,MinX0,MinX,X0,MaxX,Y0,Y,W,I) :-
       numbervars_post(A,0,_),
       numbervars_post(B,0,_),
       name(W,Str),
       append(Str,[46,99],Str2),
       name(WCanv,Str2),
       X0R is MinX0-20,
       X0A is X0+24,
       tcl_eval(I,format('~w.c create line [expr ~w-16] [expr ~w+0] [expr ~w+16] [expr ~w+0] -arrow last -tags {arrow dir_lr}',[W,X0,Y0,X0,Y0]),_), 
       p_listbox_item_r(B,N0,1,X0R,MinX1,Y0,_,black,WCanv,I),
       p_listbox_item(A,N0,1,X0A,X1,Y0,_Y1,black,WCanv,I),
       N is N0+1,
       X2 is X1+10,
       tcl_eval(I,format('fontWidget ~w.c create text ~w ~w -font -*-times-medium-i-normal--18-* -tags {lex~w pname} -anchor w -text "(~w)"',[W,X2,Y0,N0,Name]),Item),
       tcl_eval(I,format('~w.c bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,_,_,X3,_),
       Y2 is Y0+22,
       post_listbox1(Ps,N,MinX0,MinX2,X0,X4,Y2,Y,W,I),
       MinX is min(MinX1,MinX2),
       MaxX is max(X3,X4).

post_canvas(I) :-
       'post list'(Ps),
       post_listbox(Ps,'.post',I).

new_postulate :-
       my_tk_interpreter(I),
       tcl_eval(I,'.post.c delete post_bar',_),
       edit_postulate.

reverse_postulate :-
       'current post'(postulate(A,B,C)),
       retractall('current post'(_)),
       assert('current post'(postulate(B,A,C))),
       paint_post(postulate(B,A,C)).

flip_postulate :-
       my_tk_interpreter(I),
       'post list'(Ps0),
       tcl_eval(I,'set tags [.post.c gettags post_bar]
                   set list_ind [lsearch -regexp $tags {^(post_n)[0-9]+$}]
                   set itemno_temp [lindex $tags $list_ind]
                   set cur_sel [string range $itemno_temp 6 end]',NumS),
       NumS \== [],
       number_chars(Num,NumS),
       flip_item_num(Num,Ps0,Ps,Fill),
       tcl_eval(I,format('set mybbox [.post.c bbox post_bar]
                   set x1 [lindex $mybbox 0]
                   set y1 [lindex $mybbox 1]
                   set x2 [lindex $mybbox 2]
                   set y2 [lindex $mybbox 3]
                   set boxed_items [.post.c find enclosed $x1 $y1 $x2 $y2] 
                   foreach i $boxed_items {
                   set itags [.post.c gettags $i]
                   if {[lsearch $itags post_bar] == -1} {
                   .post.c itemconfigure $i -fill ~w
                   }
                   if {[lsearch $itags oval] != -1} {
                   .post.c itemconfigure $i -outline ~w
                   }}',[Fill,Fill]),_),
       retractall(postulate(_,_,_)),
       retractall(postulate1(_,_,_)),
       assert_melt_list(Ps),
       'opt state'(OS0),
      (Fill  = black  -> OptSt = unknown 
      ;OS0   = manual -> OptSt = manual
      ;OptSt = safe),
       retractall('opt state'(_)),
       assert('opt state'(OptSt)),
       retractall('post list'(_)),
       assert('post list'(Ps)),
       retractall('post undo'(_)),
       assert('post_undo'(Ps0)).

delete_postulate :-
       my_tk_interpreter(I),
       tcl_eval(I,'set tags [.post.c gettags post_bar]
                   set list_ind [lsearch -regexp $tags {^(post_n)[0-9]+$}]
                   set itemno_temp [lindex $tags $list_ind]
                   set cur_sel [string range $itemno_temp 6 end]',NumS),
       NumS \== [],
       tokenize_term(NumS,Num),
       delete_postulate1(Num).

delete_postulate1(Num) :-
       my_tk_interpreter(I),
       'post list'(Ps0),
       delete_item_num(Num,Ps0,Ps),
       retractall(postulate(_,_,_)),
       retractall(postulate1(_,_,_)),
       assert_melt_list(Ps),
       retractall('post list'(_)),
       assert('post list'(Ps)),
       retractall('post undo'(_)),
       assert('post_undo'(Ps0)),
       'opt state'(OS0),
       retractall('opt state'(_)),
      (OS0 = optimal -> OS = safe
      ;OS = OS0),
       assert('opt state'(OS)), 
       tcl_eval(I,'.post.c delete all',_),
       post_canvas(I).

edit_postulate :-
       my_tk_interpreter(I),
       tcl_eval(I,'if {[winfo exists .postedit]} {
                   wm deiconify .postedit
                   raise .postedit
                   } else {
                   prolog create_postedit_window
                   }',_),
       'post list'(Ps),
       retractall('current post'(_)),
       retractall('unmod post'(_)),
      ( tcl_eval(I,'set tags [.post.c gettags post_bar]
                    set list_ind [lsearch -regexp $tags {^(post_n)[0-9]+$}]
                    set itemno_temp [lindex $tags $list_ind]
                    set cur_sel [string range $itemno_temp 6 end]',NumS),
        NumS \== [] ->
        number_chars(Num,NumS),
        get_item_num(Num,Ps,Post0)
      ;
        Post=postulate(' * ',' * ','')
      ),
      ( Post0=postulate1(Ant,Suc,Name) ->
        Post =postulate(Ant,Suc,Name)
      ; Post =Post0
      ),
       assert('current post'(Post)),
       assert('unmod post'(Post)),
       Post=postulate(_,_,Name),      
       tcl_eval(I,format('set postulatename {~w}',[Name]),_),
       paint_post(Post).

paint_post(postulate(Ant,Suc,_Name)) :-
       my_tk_interpreter(I),
       tcl_eval(I,'.postedit.f0.c delete all',_),
       tcl_eval(I,'.postedit.f5.atom.m delete 0 end',_),
       postulate_vars(Ant,Vars0,Vars1),
       postulate_vars(Suc,Vars1,[]),
       sort(Vars0,Vars2),
       add_new_var(Vars2,Vars,0,_),
       menu_list(Vars,I,'.postedit.f5.atom.m',paste_var),
       paint_label(Suc,I,0,suc,10,X0,10,Y0,_),
       X1 is X0+40,
       paint_label(Ant,I,0,ant,X1,X,10,Y1,_),
       Y is max(Y0,Y1),
       mid(10,Y,MidY),
       tcl_eval(I,format('.postedit.f0.c create line [expr ~w+4] ~w [expr ~w+36] ~w -arrow last -tags {arrow dir_lr}',[X0,MidY,X0,MidY]),_),
       tcl_eval(I,format('.postedit.f0 configure -width [expr ~w+10]',[X]),_),
       tcl_eval(I,format('.postedit.f0.c configure -width [expr ~w+10]',[X]),_),
       tcl_eval(I,format('.postedit.f0 configure -height [expr ~w+6]',[Y]),_),
       tcl_eval(I,format('.postedit.f0.c configure -height [expr ~w+6]',[Y]),_),
       tcl_eval(I,'wm geometry .postedit {}',_).

paint_label(p(I,A,B),Int,Num0,Tag,X0,X,Y0,Y,t(MidX,B2)) :-
       !,
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+35,
       paint_label(A,Int,Num1,Tag,X0,X1,Y1,Y2,t(MidA,AY)),
       X2 is X1+15,
       paint_label(B,Int,Num2,Tag,X2,X3,Y1,Y3,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_constr(prod,I,Int,'.postedit.f0.c',black,Num0,Tag,MidX,Y0,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.postedit.f0.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.postedit.f0.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_label(zip(I,A),Int,Num0,Tag,X0,X,Y0,Y,t(MidA,B2)) :-
       !,
       Num1 is (Num0*10)+1,
       Y1 is Y0+35,
       paint_label(A,Int,Num1,Tag,X0,X1,Y1,Y,t(MidA,AY)),
       paint_constr(dia,I,Int,'.postedit.f0.c',black,Num0,Tag,MidA,Y0,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.postedit.f0.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label(bogus(A),Int,Num,Tags,X0,X,Y0,Y,t(MidB,B2)) :-
       !,
       tcl_eval(Int,format('.postedit.f0.c create text ~w ~w -fill red -tags {node ~w b0 bogus~w} -anchor w -text "~p"',[X0,Y0,Tags,Num,A]),Item),
       tcl_eval(Int,format('.postedit.f0.c bbox ~s',[Item]),BBox),
       quadruple(Int,BBox,B1,B2,X,Y),
       mid(B1,X,MidB).

paint_label(A,Int,Num,Tags,X0,X,Y0,Y,t(MidB,B2)) :-
      (A = ' * ' -> Tag = 'emtpy' ; Tag = ''),
       tcl_eval(Int,format('.postedit.f0.c create text ~w ~w -tags {node ~w b0 ~w~w} -anchor w -text "~p"',[X0,Y0,Tag,Tags,Num,A]),Item),
       tcl_eval(Int,format('.postedit.f0.c bbox ~s',[Item]),BBox),
       quadruple(Int,BBox,B1,B2,X,Y),
       mid(B1,X,MidB).

paste_var(Var) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('
                   set tags [.postedit.f0.c gettags postselectbox]
                   set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
                   set path_temp [lindex $tags $list_ind]
                   set path [string range $path_temp 3 end]
                   set as [string range $path_temp 0 2]
                   if {$path != {}} {
                   set prologcall ""
                   append prologcall "paste_var1($as,$path," {~k)} 
                   prolog $prologcall
                   }',[Var]),_).

paste_var1(AS,Num,Var) :-
       'current post'(postulate(Ant0,Suc0,Name)),
       num_to_list(Num,List),
      (AS = ant ->
       paste_post(List,Ant0,Var,Ant),
       Suc = Suc0
      ;
       paste_post(List,Suc0,Var,Suc),
       Ant = Ant0
      ),
       retractall('current post'(_)),
       assert('current post'(postulate(Ant,Suc,Name))),
       paint_post(postulate(Ant,Suc,Name)),
      (AS = ant ->
       (select_post_var(Ant,ant,0) -> true
       ;select_post_var(Suc,suc,0) -> true
       ;true)
      ;
      (select_post_var(Suc,suc,0) -> true
      ;select_post_var(Ant,ant,0) -> true
      ;true)
      ).

create_postedit_window :-
       my_tk_interpreter(I),
       tcl_eval(I,'source postedit.tcl',_),
       findall(UI,'unary mode'(UI),Us),
       findall(BI,'binary mode'(BI),Bs),
       constr(prod,I,'.postedit.f3.p'  ,black,x,16,15,_),
       constr(dia, I,'.postedit.f4.dia',black,x,15,15,_),
       menu_list(Bs,I,'.postedit.f3.index.m',select_binary_p), 
       menu_list(Us,I,'.postedit.f4.index.m',select_unary_p).

paste_constr(AS,Num,P,A,SI) :-
       tokenize_term(SI,I),
       add_index(A,SI),
       construct(P,A,I,Term),
       'current post'(postulate(Ant0,Suc0,Name)),
       num_to_list(Num,List),
      (AS = ant ->
       paste_post(List,Ant0,Term,Ant),
       Suc = Suc0
      ;
       paste_post(List,Suc0,Term,Suc),
       Ant = Ant0),
       retractall('current post'(_)),
       assert('current post'(postulate(Ant,Suc,Name))),
       paint_post(postulate(Ant,Suc,Name)),
      (select_post_var(Term,AS,Num) -> true
      ;select_post_var(Ant,ant,0) -> true
      ;select_post_var(Suc,suc,0) -> true
      ;true).

paste_post(AS,Num) :-
       'current post'(postulate(A0,B0,N)),
       'paste post'(C0),
       melt(C0,C),
       num_to_list(Num,List),
      (AS = ant ->
       postulate_vars(A0,AV0,[]),
       sort(AV0,AV),
       freeze(C,AV,_),
       paste_post(List,A0,C,A),
       B = B0
      ;
       postulate_vars(B0,BV0,[]),
       sort(BV0,BV),
       freeze(C,BV,_),
       paste_post(List,B0,C,B),
       A = A0),
       retractall('current post'(_)),
       assert('current post'(postulate(A,B,N))),
       paint_post(postulate(A,B,N)),
      (select_post_var(C,AS,Num) -> true
      ;select_post_var(B,suc,0) -> true
      ;select_post_var(A,ant,0) -> true
      ;true).  

cut_post(AS,Num) :-
       'current post'(postulate(A0,B0,N)),
       retractall('current post'(_)),
       retractall('paste post'(_)),
       num_to_list(Num,List),
      (AS = ant ->
       delete_post(List,A0,A,C),
       B = B0
      ;
       delete_post(List,B0,B,C),
       A = A0),
       assert('current post'(postulate(A,B,N))),
       assert('paste post'(C)),
       paint_post(postulate(A,B,N)),
       select_post(AS,Num).       

copy_post(AS,Num) :-
       'current post'(postulate(A,B,_)),
       retractall('paste post'(_)),
       num_to_list(Num,List),
      (AS = ant ->
       copy_post(List,A,C)
      ;
       copy_post(List,B,C)),
       assert('paste post'(C)).

select_post_var(' * ',AS,Num) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('.postedit.f0.c delete postselectbox
                   set path ~w~w
                   set tags $path
                   set list_ind2 [lsearch -regexp $tags {^(b)[0123456789]+$}]
                   set branches [lindex $tags $list_ind2]
                   set box [.postedit.f0.c bbox $path]
                   set boxl [expr [lindex $box 0] -1]
                   set boxr [expr [lindex $box 2] +1]
                   set boxu [expr [lindex $box 1] -1]
                   set boxd [expr [lindex $box 3] +1]
                   set s_tags {}
                   lappend s_tags postselectbox $branches $path
           .postedit.f0.c create line $boxl $boxu $boxr $boxu -tags $s_tags
           .postedit.f0.c create line $boxl $boxd $boxr $boxd -tags $s_tags
           .postedit.f0.c create line $boxl $boxd $boxl $boxu -tags $s_tags
           .postedit.f0.c create line $boxr $boxu $boxr $boxd -tags $s_tags',[AS,Num]),_).
       
select_post_var(zip(_,A),AS,Num0) :-
       Num is (Num0*10)+1,
       select_post_var(A,AS,Num).
       
select_post_var(p(_,A,_),AS,Num0) :-
       Num is (Num0*10)+1,
       select_post_var(A,AS,Num).

select_post_var(p(_,_,B),AS,Num0) :-
       Num is (Num0*10)+2,
       select_post_var(B,AS,Num).

select_post(AS,Num) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('.postedit.f0.c delete postselectbox
                   set path ~w~w
                   set list_ind2 [lsearch -regexp $tags {^(b)[0123456789]+$}]
                   set branches [lindex $tags $list_ind2]
                   set box [.postedit.f0.c bbox $path]
                   set boxl [expr [lindex $box 0] -1]
                   set boxr [expr [lindex $box 2] +1]
                   set boxu [expr [lindex $box 1] -1]
                   set boxd [expr [lindex $box 3] +1]
                   set s_tags {}
                   lappend s_tags postselectbox $branches $path
           .postedit.f0.c create line $boxl $boxu $boxr $boxu -tags $s_tags
           .postedit.f0.c create line $boxl $boxd $boxr $boxd -tags $s_tags
           .postedit.f0.c create line $boxl $boxd $boxl $boxu -tags $s_tags
           .postedit.f0.c create line $boxr $boxu $boxr $boxd -tags $s_tags',[AS,Num]),_).


paste_post([],' * ',P,P).
paste_post([N|Ns],F0,P,F) :-
       sub_term(F0,F,N,A0,A),
       paste_post(Ns,A0,P,A).

delete_post([],P ,' * ',P).
delete_post([N|Ns],F0, F ,P  ) :-
       sub_term(F0,F,N,A0,A),
       delete_post(Ns,A0,A,P).

copy_post([],P,P).
copy_post([N|Ns],F,P) :-
       sub_term(F,_,N,A,_),
       copy_post(Ns,A,P).

store_postulate :-
       my_tk_interpreter(I),
       store_postulate(I).

store_postulate(I) :-
       'current post'(postulate(A,B,_)),
       sahlqvist(I,A,B),
       tcl_eval(I,'set postulatename',PS),         
       name(PN,PS),
       tcl_eval(I,'set tags [.postedit.f0.c gettags arrow]
                   set list_ind [lsearch -regexp $tags {^(dir_)[a-z]+$}]
                   set itemno_temp [lindex $tags $list_ind]
                   set cur_dir [string range $itemno_temp 4 end]',CurDir),
       CurDir \== [],
      (CurDir = "lr" -> assert_postulate(I,A,B,PN)
      ;CurDir = "rl" -> assert_postulate(I,B,A,PN)
      ;CurDir = "eq" -> assert_postulate(I,A,B,PN),
                        assert_postulate(I,B,A,PN)
      ),
       tcl_eval(I,'.post.c delete all',_),
       retractall('opt state'(_)),
       assert('opt state'(unknown)),
       post_canvas(I).

assert_postulate(I,A,B,PN) :-
      (normalize1(A,B) ->
       tcl_eval(I,'dialog .d {Postulate Exists} {This postulate is already a consequence of other postulates in this fragment. 
Do you wish to store it anyway?} warning 1 Store Cancel',"0")
     ; true),
       melt(postulate(A,B,PN),Postulate),
       assertz(Postulate),
       'post list'(Post0),
       retractall('post list'(_)),
       retractall('post undo'(_)),
       assert('post undo'(Post0)),
       insert_end(Post0,postulate(A,B,PN),Post),
       assert('post list'(Post)).

sahlqvist(I,A,B) :-
     ( select_post_var(B,suc,0) -> 
       tcl_eval(I,'dialog .d {Underspecified Postulate} {The postulate you have tried to store is underspecified.} error 0 Cancel',"666")
     ; select_post_var(A,ant,0) ->
       tcl_eval(I,'dialog .d {Underspecified Postulate} {The postulate you have tried to store is underspecified.} error 0 Cancel',"666")
     ; repeating(I,A,B),
       more_constrs(I,A,B)
     ).

repeating(I,A,B) :-
       postulate_vars(A,AV,[]),
       postulate_vars(B,BV,[]),
     ( repeating(AV,RA) ->
        tcl_eval(I,format('dialog .d {Nonlinear Postulate} {The postulate you tried to store has more than one occurrence of the label "~p" on the left hand side.} error 0 Cancel',[RA]),"666")
     ; repeating(BV,RB) ->
       tcl_eval(I,format('dialog .d {Nonlinear Postulate} {The postulate you tried to store has more than one occurrence of the label "~p" on the right hand side.} error 0 Cancel',[RB]),"666")
     ; sort(AV,AVS),
       sort(BV,BVS),
       diff_vars(AVS,BVS,Diff),
       ( Diff = [D|_] ->
         tcl_eval(I,format('dialog .d {Nonlinear Postulate} {The variable "~p" does not occur on both sides of the postulate.} error 0 Cancel',[D]),"666")
       ; true
       )
     ).

more_constrs(I,A,B) :-
       count_constrs(A,0,N0),
       count_constrs(B,0,N),
     ( N>N0 -> tcl_eval(I,'dialog .d {Possible Nontermination} {The postulate you tried to store has more label constructors on the left hand side than on the right hand side. 
The theorem prover may fail to terminate if you store this postulate.} warning 1 Store Cancel',"0")
     ;
       true
     ).

count_constrs('$VAR'(_),N,N).
count_constrs(_-_,N,N).
count_constrs(zip(_,A),N0,N) :-
       N1 is N0+1,
       count_constrs(A,N1,N).
count_constrs(p(_,A,B),N0,N) :-
       N1 is N0+1,
       count_constrs(A,N1,N2),
       count_constrs(B,N2,N).

diff_vars([],Bs,Bs) :- !.
diff_vars(As,[],As).
diff_vars([A|As],[B|Bs],Cs) :-
       A = B -> diff_vars(As,Bs,Cs)
     ; A @< B -> Cs=[A|Cs0],diff_vars(As,[B|Bs],Cs0)
     ; Cs=[B|Cs0],diff_vars([A|As],Bs,Cs0).

repeating([X|Xs],Y) :-
       member_chk(X,Xs) ->
       Y=X
     ; repeating(Xs,Y).

postulate_vars(p(_,A,B),Vs0,Vs) :-
       postulate_vars(A,Vs0,Vs1),
       postulate_vars(B,Vs1,Vs).
postulate_vars(zip(_,A),Vs0,Vs) :-
        postulate_vars(A,Vs0,Vs).
postulate_vars('$VAR'(N),['$VAR'(N)|Vs],Vs).
postulate_vars(_-_,Vs,Vs).
postulate_vars(bogus(_),Vs,Vs).
postulate_vars(' * ',Vs,Vs).

select_unary_p(U) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('set post_un_index ~w',[U]),_).

select_binary_p(B) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('set post_bin_index ~w',[B]),_).

add_new_var([],['$VAR'(N)],N,N).
add_new_var(['$VAR'(N0)|Rest0],Rest,N1,N) :-
       N1 < N0 ->
       Rest = ['$VAR'(N1),'$VAR'(N0)|Rest0],
       N is N1
     ;
       N2 is N1 +1,
       Rest = ['$VAR'(N0)|Rest1],
       add_new_var(Rest0,Rest1,N2,N). 

% ====================================================================
% test_post
% Check for typos/errors in the structural postulates 
% needs revision...
% ====================================================================

test_post :-
       findall(postulate(A,B,C),postulate(A,B,C),L),
       map_is_valid_postulate(L),
       findall(postulate(A,B,C),postulate1(A,B,C),L1),
       map_is_valid_postulate(L1).

map_is_valid_postulate([]).
map_is_valid_postulate([postulate(A,B,C)|Ps]) :-
        is_valid_postulate(A,B,C),
        map_is_valid_postulate(Ps).

is_valid_postulate(A,B,C) :-
      ( is_postulate_term(A),
	is_postulate_term(B) ->
	 true
      ;
	 my_tk_interpreter(I),
         retractall(postulate(A,B,C)),
	 retractall(postulate1(A,B,C)),
         tcl_eval(I,'if {[winfo exists .postedit]} {
                     wm deiconify .postedit
                     raise .postedit
                     } else {
                     prolog create_postedit_window
                     }',_),
         make_postulate(A,B,C,X),
	 numbervars(X,0,_),
         retractall('current entry'(_)),
         assert('current entry'(X)),
         paint_post(X),
         tcl_eval(I,format('dialog .d {Postulate Error} {The postulate\
         "~w" is not in a valid format.} error 0 "Delete"',[C]),_)
       ).

is_postulate_term(X) :-
	var(X),
	!.
is_postulate_term(_-_).
is_postulate_term(zip(_,A)) :-
	is_postulate_term(A).
is_postulate_term(p(_,A,B)) :-
	is_postulate_term(A),
	is_postulate_term(B).

make_postulate(A0,B0,C,postulate(A,B,C)) :-
	make_postulate_term(A0,A),
	make_postulate_term(B0,B).

make_postulate_term(A,A) :-
	var(A),
	!.
make_postulate_term(x-C,x-C) :-
	!.
make_postulate_term(zip(I,A0),zip(I,A)) :-
	!,
	make_postulate_term(A0,A).
make_postulate_term(p(I,A0,B0),p(I,A,B)) :-
	!,
	make_postulate_term(A0,A),
	make_postulate_term(B0,B).
make_postulate_term(A,bogus(A)).
