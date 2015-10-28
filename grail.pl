% =====================
% =     Grail 2.0     =
% =====================

% Release Notes:
%

% 24-10-00 Some minor changes to make the interface to the CGN
%          database a bit more convenient.
% 24-10-00 Added "commit" option to manual axiom link. By pressing
%          <Shift-Button-1> instead of <Button-1> any alternative
%	   axiom links are automatically disregarded.
% 24-10-00 The size of the font for the indices is also changeable
%          in the font option menu.
% 10-10-00 Previewer type selectable. Current choices are 'none',
%          'dvi', 'ps' and 'pdf'. Restructured menus to reflect this
%          more naturally.
% 29-06-00 LaTeX error messages are now piped to stdout instead of
%          /dev/null/
%          Added some missing safeguards for unavailable fonts.
% 20-03-00 Added option for more compact lexical entries.
% 31-01-00 Used workaround for strange working_directory/2 behaviour
%          in SICStus 3.8.1
% 17-03-99 Added support for color selection to the options menu.
%          Colors and fonts are saved with the other options.
% 09-03-99 Added support for font selection in the proof net and
%          rewrite window in the options menu. Connectives will
%          resize to match the current font
% 02-03-99 Added option to show linear precedence information to
%          the rewrite window. Modified undo menu item to activate
%          and deactive itself. Changed label output to produce
%          non-filled circles.
% 11-11-98 Added undo function to rewrite window. When rewriting
%          you can undo your manual rewrite steps. 
% 22-09-98 If the file ~/.grail_default_options exists, Grail will
%          assume it contains the user's options. Users can save the
%          current options to this file from the file menu. Options
%          not available from the menu are also saved.
% 21-09-98 Added extra semantics option for choice between functional
%          ((f y) x) and predicate f(x,y) notation.
% 21-09-98 Added preliminary possibility for user-defined preferences.
% 23-06-98 Term tokenization now melts semantic terms before storing
%          them in the lexicon.
% 14-05-98 Updated term and string tokenization. New version also
%          accepts Prolog symbols and tries to correct typos.
% 13-05-98 Made eager label conversions interactive.
% 13-05-98 Label conversion windows now only check if the label
%          is normal/df if the last action was creep or leap.
%          This allows you to continue rewriting after you encounter
%          a normal or df label.
% 12-05-98 Made eager label conversions visible to user. At this
%          point, eager label conversions are not interactive.
% 12-05-98 Improved detection of cycles.
% 08-05-98 Added an indicator of the (estimated) number of links
%          remaining for the current lookup.
% 06-05-98 Removed (too) detailed messages when running in "leap" or
%          "nonstop" mode.
% 27-04-98 Added option of interactive vs. automatic run mode.
% 21-04-98 Moved all string tokenization from Tcl to Prolog.
%          Added special_string/2 declaration to allow user-defined
%          strings to have their own lexical entry.
% 15-04-98 Included proof net and rewrite window. Version is now
%          2.0 beta.
% 03-04-98 Removed Makefile. LaTeX is only called if options were
%          changed or a new parse was performed.
% 01-04-98 When latex_output_format(none) is selected, computation
%          of natural deduction proofs will be suppressed.
% 13-03-98 Added predicate which tests lexical integrity on loading.
%          This will help when you make a typo in the formulas.
% 10-12-97 Made Grail usable without the tcltk or system libraries.
% 17-11-97 Removed mistake in check_continuity/3.
% 13-11-97 Modified the natural deduction routines prevent generation
%          of natural deduction proofs where the final rule is an
%          unnecessary structural rule.
% 13-11-97 Removed most Prolog messages, since for the average user
%          these are not very useful. You can turn these back on by
%          selecting Options/Prolog Messages/Verbose.
% 10-11-97 Added 'Compile Source' option to file menu. This will look
%          in the directory GRAIL_EXTENSIONS_DIR to find the named .ql
%          or .pl file (whichever is newer). 
% 21-10-97 Removed conflict between lazy/transparent declarations.
%          When a lazy constructor occurred in a transparent context,
%          transparency could force eager evalution of this constructor.
% 14-10-97 Added gray colors to options database. Corrected several
%          inconsistent colorings.
% 18-09-97 Mistake with input of unary/binary indexes corrected. More
%          consistent treatment of new indexes.
% 04-09-97 Support for CLP over finite domains added. See
%          ./fragments/constr.pl for an example. Interaction with
%          postulate and analysis window is not fully supported. 
%          Worried about interaction between constraints and closed set.
% 29-08-97 Environment variables GRAIL_FRAGMENTS_DIR and GRAIL_TEXOUT_DIR 
%          can be set to override the default directories for both the
%          fragments (./fragments) and the LaTeX output (.)
% 07-08-97 Beta version released for ESSLLI'97

:- use_module(library(tcltk)).
:- use_module(library(file_systems)).

%:- abolish(user:portray_message/2).
%user:portray_message(error,_).
%user:portray_message(informational,_).
%user:portray_message(warning,_).

%:- prolog_flag(character_escapes,_,off).
%:- prolog_flag(redefine_warnings,_,off).
%:- prolog_flag(unknown,_,fail).
%:- prolog_flag(syntax_errors,_,fail).

:- format('~n==========================~n= Welcome to Grail 2.0.1 =~n==========================~nRelease: 28-10-2015~n~n',[]).

:- use_module(library(system)).

:- ensure_loaded(engine).
:- ensure_loaded(lex).
:- ensure_loaded(post).
:- ensure_loaded(nd).
:- ensure_loaded(latex).
:- ensure_loaded(analysis).
:- ensure_loaded(tokenize).
:- ensure_loaded(pp).
:- ensure_loaded(reduce_sem).

% = load options

load_options :-
   ( environ('HOME',HomeDir) ->
     tcl_new(I),
     tcl_eval(I,format('file tail ~w',[HomeDir]),User),
     tcl_delete(I)
   ;
     HomeDir='.',
     User="unknown"
   ),
     absolute_file_name('~/.grail_default_options.pl',Options),
   ( 
     file_exists(Options,read) ->
     [Options],
    ( environ('USER',moot) -> moot_message
    ; format('Using personal preferences of ~s.~n',[User])
    )

   ;
     [options]
   ).

grail_version(2,0).

grail :-
       /* erase previous tk windows opened by this program */
       delete_tk_interps,
       retractall(tex_out_dir(_)),
       retractall(fragment_dir(_)),
       retractall(extension_dir(_)),
       /* open tk window and save its reference */
       my_tk_new([name('Grail 2.0')],I),
       assert(my_tk_interpreter(I)),
      ( (predicate_property(color_options, _), color_options) -> true ; true),
       tcl_eval(I,'source grail.tcl',_),
       initialize,
       tcl_eval(I,'prolog assert(tex_out_dir(''$texoutdir''))',_),
       tcl_eval(I,'prolog assert(fragment_dir(''$fragmentdir''))',_),
       tcl_eval(I,'prolog assert(extension_dir(''$extensiondir''))',_),
       tcl_eval(I,'set runningstate done\n\
                   set rewritestate done',_),
       example_sentences(I),
       tcl_eval(I,'source options.tcl',_),
       default_options,
       set_pdf_output,
       tcl_eval(I,'set xdvisize "800x600"',_),
       tk_main_loop,
       my_tcl_delete(I),
       halt.


paint_constr(Constr,Index,Int,Win,Fill,Tags,X,Y,[C1,C2,C3,C4,MaxX]) :-
       constr(Constr,Int,Win,Fill,Tags,X,Y,[_A1,A2,A3,A4]),
       branching_degree(Constr,BD),
       tcl_eval(Int,format('fontWidget ~w create text [expr ~w+4] ~w -tags {node b~w t~w} -anchor nw -font -*-helvetica-medium-o-*-*-8* -text "~p" -fill ~w',[Win,X,Y,BD,Tags,Index,Fill]),Item1),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item1]),BBox1),
       quadruple(Int,BBox1,_B1,B2,B3,B4),
       C1 is X-4,
       C3 is X+4,
       C2 is min(A2,B2),
       MaxX is max(A3,B3),
       C4 is max(A4,B4).

paint_constr_r(Constr,Index,Int,Win,Fill,Tags,X,Y,[C1,C2,C3,C4,MinX]) :-
       branching_degree(Constr,BD),
       tcl_eval(Int,format('fontWidget ~w create text [expr ~w-2] ~w -tags {node b~w t~w} -anchor ne -font -*-helvetica-medium-o-*-*-8* -text "~p" -fill ~w',[Win,X,Y,BD,Tags,Index,Fill]),Item1),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item1]),BBox1),
       quadruple(Int,BBox1,B1,B2,_B3,B4),
       X1 is B1-3,
       constr(Constr,Int,Win,Fill,Tags,X1,Y,[MinX,A2,_A3,A4]),
       C1 is MinX,
       C3 is X,
       C2 is min(A2,B2),
       C4 is max(A4,B4).

constr(prod,Int,Win,Fill,Tags,X,Y,[L1,D1,R1,U1]) :-
       L is X-3,
       R is X+3,
       U is Y+3,
       D is Y-3,
       L1 is L-3,
       R1 is R+3,
       U1 is U+3,
       D1 is D-3,       
       tcl_eval(Int,format('~w create oval ~w ~w ~w ~w -tags {node oval b2 t~w} -outline ~w -fill ~w',[Win,L,D,R,U,Tags,Fill,Fill]),_).

constr(dia,Int,Win,Fill,Tags,X,Y,[L1,D1,R1,U1]) :-
       L is X-5,
       R is X+5,
       U is Y+5,
       D is Y-5,
       mid(L,R,MidX),
       mid(U,D,MidY),
       L1 is L-3,
       R1 is R+3,
       U1 is U+3,
       D1 is D-3,
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,MidY,MidX,U,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,MidY,MidX,D,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,R,MidY,MidX,U,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,R,MidY,MidX,D,Fill,Tags]),_).

constr(box,Int,Win,Fill,Tags,X,Y,[L1,D1,R1,U1]) :-
       L is X-4,
       R is X+4,
       U is Y+4,
       D is Y-4,
       L1 is L-3,
       R1 is R+3,
       U1 is U+3,
       D1 is D-3,
       tcl_eval(Int,format('~w create line ~w ~w ~w [expr ~w+7] -tags {node b1 t~w} -fill ~w',[Win,R1,D1,R1,D1,Tags,Fill]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+7] [expr ~w-2] [expr ~w+5] -tags {node b1 t~w} -fill ~w',[Win,R1,D1,R1,D1,Tags,Fill]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+7] [expr ~w+2] [expr ~w+5] -tags {node b1 t~w} -fill ~w',[Win,R1,D1,R1,D1,Tags,Fill]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -tags {node b1 t~w} -fill ~w',[Win,L,U,L,D,Tags,Fill]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -tags {node b1 t~w} -fill ~w',[Win,R,U,R,D,Tags,Fill]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -tags {node b1 t~w} -fill ~w',[Win,L,U,R,U,Tags,Fill]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -tags {node b1 t~w} -fill ~w',[Win,L,D,R,D,Tags,Fill]),_).

constr(dl,Int,Win,Fill,Tags,X,Y,[B1,B2,B3,B4]) :-
       tcl_eval(Int,format('~w create text ~w ~w -text "\\\\" -tags {node b2 t~w} -fill ~w',[Win,X,Y,Tags,Fill]),Item),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item]),BS),
       quadruple(Int,BS,B1,B2,B3,B4).

constr(dr,Int,Win,Fill,Tags,X,Y,[B1,B2,B3,B4]) :-
       tcl_eval(Int,format('~w create text ~w ~w -text "/" -tags {node b2 t~w} -fill ~w',[Win,X,Y,Tags,Fill]),Item),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item]),BS),
       quadruple(Int,BS,B1,B2,B3,B4).

% =

num_to_list(N,L) :-
       num_to_list(N,[],L).

num_to_list(0,X,X).
num_to_list(N0,X0,X) :-
       N0>0,
       Y is N0 mod 10,
       N is N0 // 10,
       num_to_list(N,[Y|X0],X).

mid(X,Y,Z) :-
       Z is integer((X+Y)/2).

% =

p_listbox_item_r(p(J,A,B),N,Br,X0,X,Y0,Y,Fill,W,I) :-
       !,
       paint_bc_r(Br,N,X0,X1,Y0,_,Fill,W,I),
       binding(B,p(J,A,B),BrB),
       X1A is X1,
       p_listbox_item_r(B,N,BrB,X1A,X2,Y0,Y4,Fill,W,I),
       X2A is X2,
       paint_constr_r(prod,J,I,W,Fill,'{post_a}',X2A,Y0,[X3,_,_,Y3,_]),
       binding(A,p(J,A,B),BrA),
       p_listbox_item_r(A,N,BrA,X3,X4,Y0,Y2,Fill,W,I),
       paint_bo_r(Br,N,X4,X5,Y0,Y1,Fill,W,I),
       X is X5-4,
       MY0 is max(Y1,Y2),
       MY1 is max(Y3,Y4),
       Y is max(MY0,MY1).

p_listbox_item_r(zip(J,A),N,_,X0,X,Y0,Y,Fill,W,I) :-
       !,
       X0A is X0,
       binding(A,zip(J,A),BrA),
       p_listbox_item_r(A,N,BrA,X0A,X1,Y0,Y2,Fill,W,I),
       X1A is X1,
       paint_constr_r(dia,J,I,W,Fill,'{post_a}',X1A,Y0,[X,_,_,Y1,_]),
       Y is max(Y1,Y2).

p_listbox_item_r(A,_N,_,X0,X,Y0,Y,Fill,W,I) :-
       tcl_eval(I,format('~w create text ~w ~w -tags {post_a} -anchor e -text "~p" -fill ~w',[W,X0,Y0,A,Fill]),Item),
       tcl_eval(I,format('~w bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,X,_B2,_B3,Y).

p_listbox_item(p(J,A,B),N,Br,X0,X,Y0,Y,Fill,W,I) :-
       !,
       paint_bo(Br,N,X0,X1,Y0,Y1,Fill,W,I),
       binding(A,p(J,A,B),BrA),
       p_listbox_item(A,N,BrA,X1,X2,Y0,Y2,Fill,W,I),
       X2A is X2+3,
       paint_constr(prod,J,I,W,Fill,'{post_p}',X2A,Y0,[_,_,_,Y3,X3]),
       binding(B,p(J,A,B),BrB),
       p_listbox_item(B,N,BrB,X3,X4,Y0,Y4,Fill,W,I),
       paint_bc(Br,N,X4,X,Y0,_,Fill,W,I),
       MY0 is max(Y1,Y2),
       MY1 is max(Y3,Y4),
       Y is max(MY0,MY1).

p_listbox_item(zip(J,A),N,_,X0,X,Y0,Y,Fill,W,I) :-
       !,
       X0A is X0+2,
       paint_constr(dia,J,I,W,Fill,'{post_s}',X0A,Y0,[_,_,_,Y1,X1]),
       X1A is X1+2,
       binding(A,zip(J,A),BrA),
       p_listbox_item(A,N,BrA,X1A,X,Y0,Y2,Fill,W,I),
       Y is max(Y1,Y2).

p_listbox_item(A,_N,_,X0,X,Y0,Y,Fill,W,I) :-
          tcl_eval(I,format('~w create text ~w ~w -tags {post_s} -anchor w -text "~p" -fill ~w',[W,X0,Y0,A,Fill]),Item),
       tcl_eval(I,format('~w bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,_B1,_B2,X,Y).

paint_bo(0,N,X0,X,Y0,Y,Fill,W,I) :-
       X0A is X0-2,
       tcl_eval(I,format('~w create text ~w ~w -tags {lex~w} -anchor w -text "(" -fill ~w',[W,X0A,Y0,N,Fill]),Item),
       tcl_eval(I,format('~w bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,_,_,X,Y).

paint_bo(1,_,X,X,Y,Y,_,_,_).

paint_bc(0,N,X0,X,Y0,Y,Fill,W,I) :-
       tcl_eval(I,format('~w create text ~w ~w -tags {lex~w} -anchor w -text ")" -fill ~w',[W,X0,Y0,N,Fill]),Item),
       tcl_eval(I,format('~w bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,_,_,X,Y).

paint_bc(1,_,X,X,Y,Y,_,_,_).

paint_bo_r(0,N,X0,X,Y0,Y,Fill,W,I) :-
       X0A is X0,
       tcl_eval(I,format('~w create text ~w ~w -tags {lex~w} -anchor e -text "(" -fill ~w',[W,X0A,Y0,N,Fill]),Item),
       tcl_eval(I,format('~w bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,X,Y,_,_).

paint_bo_r(1,_,X,X,Y,Y,_,_,_).

paint_bc_r(0,N,X0,X,Y0,Y,Fill,W,I) :-
       tcl_eval(I,format('~w create text ~w ~w -tags {lex~w} -anchor e -text ")" -fill ~w',[W,X0,Y0,N,Fill]),Item),
       tcl_eval(I,format('~w bbox ~s',[W,Item]),BBox),
       quadruple(I,BBox,X,Y,_,_).

paint_bc_r(1,_,X,X,Y,Y,_,_,_).

% =

paint_constr(Constr,Index,Int,Win,Fill,Num,Tags,X,Y,[C1,C2,C3,C4,MaxX]) :-
       constr(Constr,Int,Win,Fill,Num,Tags,X,Y,[_A1,A2,A3,A4]),
       branching_degree(Constr,BD),
       tcl_eval(Int,format('fontWidget ~w create text [expr ~w+4] ~w -tags {node b~w ~w~w} -anchor nw -font -*-helvetica-medium-o-*-*-8*\
       -text "~p" -fill ~w',[Win,X,Y,BD,Tags,Num,Index,Fill]),Item1),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item1]),BBox1),
       quadruple(Int,BBox1,_B1,B2,B3,B4),
       C1 is X-4,
       C3 is X+4,
       C2 is min(A2,B2),
       MaxX is max(A3,B3),
       C4 is max(A4,B4).

constr(prod,Int,Win,Fill,Num,Tags,X,Y,[L1,D1,R1,U1]) :-
       L is X-3,
       R is X+3,
       U is Y+3,
       D is Y-3,
       L1 is L-3,
       R1 is R+3,
       U1 is U+3,
       D1 is D-3,       
       tcl_eval(Int,format('~w create oval ~w ~w ~w ~w -tags {node oval b2 ~w~w} -outline ~w -fill ~w',[Win,L,D,R,U,Tags,Num,Fill,Fill]),_).

constr(dia,Int,Win,Fill,Num,Tags,X,Y,[L1,D1,R1,U1]) :-
       L is X-5,
       R is X+5,
       U is Y+5,
       D is Y-5,
       mid(L,R,MidX),
       mid(U,D,MidY),
       L1 is L-3,
       R1 is R+3,
       U1 is U+3,
       D1 is D-3,
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 ~w~w}',[Win,L,MidY,MidX,U,Fill,Tags,Num]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 ~w~w}',[Win,L,MidY,MidX,D,Fill,Tags,Num]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 ~w~w}',[Win,R,MidY,MidX,U,Fill,Tags,Num]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 ~w~w}',[Win,R,MidY,MidX,D,Fill,Tags,Num]),_).

% ====================================================================
% create_proofnet_window
% open a window with a graphical representation of the partial
% proof structure
% ====================================================================

create_proofnet_window :-
       my_tk_interpreter(I),
       tcl_eval(I,'if {[winfo exists .stats]} then {\n\
                  wm deiconify .stats\n\
                  raise .stats\n\
                  .stats config -cursor left_ptr\n\
                  } else {\n\
                  source proofnet.tcl\n\
                  }',_).

% = proof nets

paint_word(W,Int,M0,X,Y,Left,Right) :-
       tcl_eval(Int,format('fontWidget .stats.c create text ~w ~w -font $wordfont -tags {l~w} -fill $wordcolor -text "~p"',[X,Y,M0,W]),Item),
       tcl_eval(Int,format('.stats.c bbox ~s',[Item]),BBox),
       quadruple(Int,BBox,Left,_,Right,_).

paint_formula1(lit(A),Int,Tag,Num0,Num,X0,X,Y0,Y,_,_,t(MidB,B2)) :-
       Num is Num0+1,
       tcl_eval(Int,format('fontWidget .stats.c create text ~w ~w -font $pnfont -fill black -tags {node n~w l~w} -anchor w -text "~p"',[X0,Y0,Num0,Tag,A]),Item),
       tcl_eval(Int,format('.stats.c bbox ~s',[Item]),BBox),
       quadruple(Int,BBox,B1,Y,X,B2),
       tcl_eval(Int,format('set height(~w) ~w',[Num0,Y]),_),
       mid(B1,X,MidB).

paint_formula1(dr(I,A,B),Int,Tag,Num0,Num,X0,X,Y0,Y,XOff,YOff,t(MidX,B4)) :-
       Y1 is Y0+YOff,
       paint_formula1(A,Int,Tag,Num0,Num1,X0,X1,Y1,Y2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_formula0(B,Int,Tag,Num1,Num,X2,X3,Y1,Y3,XOff,YOff,t(MidB,BY)),
       Y is min(Y2,Y3),
       CY is max(AY,BY),
       mid(MidA,MidB,MidX),
       paint_connective(dr,I,Int,Tag,'.stats.c',black,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w}',[MidA,CY,B1,B2,Tag]),_),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w}',[MidB,CY,B3,B2,Tag]),_).

paint_formula1(dl(I,A,B),Int,Tag,Num0,Num,X0,X,Y0,Y,XOff,YOff,t(MidX,B4)) :-
       Y1 is Y0+YOff,
       paint_formula0(A,Int,Tag,Num0,Num1,X0,X1,Y1,Y2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_formula1(B,Int,Tag,Num1,Num,X2,X3,Y1,Y3,XOff,YOff,t(MidB,BY)),
       Y is min(Y2,Y3),
       CY is max(AY,BY),
       mid(MidA,MidB,MidX),
       paint_connective(dl,I,Int,Tag,'.stats.c',black,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w}',[MidA,CY,B1,B2,Tag]),_),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w}',[MidB,CY,B3,B2,Tag]),_).

paint_formula1(p(I,A,B),Int,Tag,Num0,Num,X0,X,Y0,Y,XOff,YOff,t(MidX,B4)) :-
       Y1 is Y0+YOff,
       paint_formula1(A,Int,Tag,Num0,Num1,X0,X1,Y1,Y2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_formula1(B,Int,Tag,Num1,Num,X2,X3,Y1,Y3,XOff,YOff,t(MidB,BY)),
       Y is min(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_connective(prod,I,Int,Tag,'.stats.c',black,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w} -stipple @lines50.xbm',[MidA,AY,B1,B2,Tag]),_),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w} -stipple @lines50.xbm',[MidB,BY,B3,B2,Tag]),_).

paint_formula1(dia(I,A),Int,Tag,Num0,Num1,X0,X,Y0,Y,XOff,YOff,t(MidA,B4)) :-
       Y1 is Y0+YOff,
       paint_formula1(A,Int,Tag,Num0,Num1,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_connective(dia,I,Int,Tag,'.stats.c',black,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w} -stipple gray50',[MidA,AY,MidA,B2,Tag]),_).

paint_formula1(box(I,A),Int,Tag,Num0,Num1,X0,X,Y0,Y,XOff,YOff,t(MidA,B4)) :-
       Y1 is Y0+YOff,
       paint_formula1(A,Int,Tag,Num0,Num1,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_connective(box,I,Int,Tag,'.stats.c',black,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w}',[MidA,AY,MidA,B2,Tag]),_).

paint_formula0(lit(A),Int,Tag,Num0,Num,X0,X,Y0,Y,_,_,t(MidB,B2)) :-
       Num is Num0+1,
       tcl_eval(Int,format('fontWidget .stats.c create text [expr ~w-.2] [expr ~w-.2] -font $pnfont -fill black -tags {node l~w} -anchor w -text "~p"',[X0,Y0,Tag,A]),_),
       tcl_eval(Int,format('fontWidget .stats.c create text [expr ~w+.2] [expr ~w-.2] -font $pnfont -fill black -tags {node l~w} -anchor w -text "~p"',[X0,Y0,Tag,A]),_),
       tcl_eval(Int,format('fontWidget .stats.c create text [expr ~w-.2] [expr ~w+.2] -font $pnfont -fill black -tags {node l~w} -anchor w -text "~p"',[X0,Y0,Tag,A]),_),
       tcl_eval(Int,format('fontWidget .stats.c create text [expr ~w+.2] [expr ~w+.2] -font $pnfont -fill black -tags {node l~w} -anchor w -text "~p"',[X0,Y0,Tag,A]),_),
       tcl_eval(Int,format('fontWidget .stats.c create text ~w ~w -font $pnfont -fill white -tags {node n~w l~w} -anchor w -text "~p"',[X0,Y0,Num0,Tag,A]),Item),
       tcl_eval(Int,format('.stats.c bbox ~s',[Item]),BBox),
       quadruple(Int,BBox,B1,Y,X,B2),
       tcl_eval(Int,format('set height(~w) ~w',[Num0,Y]),_),
       mid(B1,X,MidB).

paint_formula0(dr(I,A,B),Int,Tag,Num0,Num,X0,X,Y0,Y,XOff,YOff,t(MidX,B4)) :-
       Y1 is Y0+YOff,
       paint_formula1(B,Int,Tag,Num0,Num1,X0,X1,Y1,Y2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_formula0(A,Int,Tag,Num1,Num,X2,X3,Y1,Y3,XOff,YOff,t(MidB,BY)),
       Y is min(Y2,Y3),
       CY is max(AY,BY),
       mid(MidA,MidB,MidX),
       paint_connective(dr,I,Int,Tag,'.stats.c',black,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w} -stipple @lines50.xbm',[MidA,CY,B1,B2,Tag]),_),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w} -stipple @lines50.xbm',[MidB,CY,B3,B2,Tag]),_),
       C2 is B2-5,
       mid(B1,B3,C1),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w ~w ~w -smooth true -joinstyle round -tags {l~w}',[B1,B2,C1,C2,B3,B2,Tag]),_).

paint_formula0(dl(I,A,B),Int,Tag,Num0,Num,X0,X,Y0,Y,XOff,YOff,t(MidX,B4)) :-
       Y1 is Y0+YOff,
       paint_formula0(B,Int,Tag,Num0,Num1,X0,X1,Y1,Y2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_formula1(A,Int,Tag,Num1,Num,X2,X3,Y1,Y3,XOff,YOff,t(MidB,BY)),
       Y is min(Y2,Y3),
       CY is max(AY,BY),
       mid(MidA,MidB,MidX),
       paint_connective(dl,I,Int,Tag,'.stats.c',black,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w} -stipple @lines50.xbm',[MidA,CY,B1,B2,Tag]),_),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w} -stipple @lines50.xbm',[MidB,CY,B3,B2,Tag]),_),
       C2 is B2-5,
       mid(B1,B3,C1),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w ~w ~w -smooth true -joinstyle round -tags {l~w}',[B1,B2,C1,C2,B3,B2,Tag]),_).

paint_formula0(p(I,A,B),Int,Tag,Num0,Num,X0,X,Y0,Y,XOff,YOff,t(MidX,B4)) :-
       Y1 is Y0+YOff,
       paint_formula0(B,Int,Tag,Num0,Num1,X0,X1,Y1,Y2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_formula0(A,Int,Tag,Num1,Num,X2,X3,Y1,Y3,XOff,YOff,t(MidB,BY)),
       Y is min(Y2,Y3),
       mid(MidA,MidB,MidX),
       CY is max(AY,BY),
       paint_connective(prod,I,Int,Tag,'.stats.c',black,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w}',[MidA,CY,B1,B2,Tag]),_),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w}',[MidB,CY,B3,B2,Tag]),_).

paint_formula0(dia(I,A),Int,Tag,Num0,Num,X0,X,Y0,Y,XOff,YOff,t(MidA,B4)) :-
       Y1 is Y0+YOff,
       paint_formula0(A,Int,Tag,Num0,Num,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_connective(dia,I,Int,Tag,'.stats.c',black,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w}',[MidA,AY,MidA,B2,Tag]),_).

paint_formula0(box(I,A),Int,Tag,Num0,Num,X0,X,Y0,Y,XOff,YOff,t(MidA,B4)) :-
       Y1 is Y0+YOff,
       paint_formula0(A,Int,Tag,Num0,Num,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_connective(box,I,Int,Tag,'.stats.c',black,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.stats.c create line ~w ~w ~w ~w -tags {l~w} -stipple gray50',[MidA,AY,MidA,B2,Tag]),_).

paint_connective(LC,Index,Int,Tag,Win,Fill,X,Y,XOff,[C1,C2,C3,C4,MaxX]) :-
       connective(LC,Int,Tag,Win,Fill,X,Y,XOff,[_A1,A2,A3,A4]),
       Skip2 is XOff/4,
       tcl_eval(Int,format('fontWidget ~w create text [expr ~w+~w] [expr ~w+(~w/2)] -anchor nw -font $indexfont -text "~p" -fill ~w -tags {l~w}',[Win,X,Skip2,Y,Skip2,Index,Fill,Tag]),Item1),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item1]),BBox1),
       quadruple(Int,BBox1,_B1,B2,B3,B4),
       Skip is XOff/3,
       C1 is X-Skip,
       C3 is X+Skip,
       C2 is min(A2,B2),
       MaxX is max(A3,B3),
       C4 is max(A4,B4).

connective(prod,Int,Tag,Win,Fill,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip is XOff/6,
       L is X-Skip,
       R is X+Skip,
       U is Y+Skip,
       D is Y-Skip,
       L1 is L-Skip,
       R1 is R+Skip,
       U1 is U+Skip,
       D1 is D-Skip,       
       tcl_eval(Int,format('~w create oval ~w ~w ~w ~w -outline ~w -fill ~w -tags {l~w}',[Win,L,D,R,U,Fill,Fill,Tag]),_).

connective(dia,Int,Tag,Win,Fill,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip1 is XOff/4,
       Skip2 is XOff/3,
       L is X-Skip2,
       R is X+Skip2,
       U is Y+Skip2,
       D is Y-Skip2,
       mid(L,R,MidX),
       mid(U,D,MidY),
       L1 is L-Skip1,
       R1 is R+Skip1,
       U1 is U+Skip1,
       D1 is D-Skip1,
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -tags {l~w} -fill ~w',[Win,L,MidY,MidX,U,Tag,Fill]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -tags {l~w} -fill ~w',[Win,L,MidY,MidX,D,Tag,Fill]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -tags {l~w} -fill ~w',[Win,R,MidY,MidX,U,Tag,Fill]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -tags {l~w} -fill ~w',[Win,R,MidY,MidX,D,Tag,Fill]),_).

connective(box,Int,Tag,Win,Fill,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip1 is XOff/4,
       Skip2 is XOff/4,
       Skip3 is XOff/2.5,
       Skip4 is XOff/9,
       Skip5 is XOff/4,
       L is X-Skip1,
       R is X+Skip1,
       U is Y+Skip1,
       D is Y-Skip1,
       L1 is L-Skip2,
       R1 is R+Skip2,
       U1 is U+Skip2,
       D1 is D-Skip2,
       tcl_eval(Int,format('~w create line ~w ~w ~w [expr ~w+~w] -tags {l~w} -fill ~w',[Win,R1,D1,R1,D1,Skip3,Tag,Fill]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+~w] [expr ~w-~w] [expr ~w+~w] -fill ~w -tags {l~w}',[Win,R1,D1,Skip3,R1,Skip4,D1,Skip5,Fill,Tag]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+~w] [expr ~w+~w] [expr ~w+~w] -fill ~w -tags {l~w}',[Win,R1,D1,Skip3,R1,Skip4,D1,Skip5,Fill,Tag]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {l~w}',[Win,L,U,L,D,Fill,Tag]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {l~w}',[Win,R,U,R,D,Fill,Tag]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {l~w}',[Win,L,U,R,U,Fill,Tag]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {l~w}',[Win,L,D,R,D,Fill,Tag]),_).

connective(dl,Int,Tag,Win,Fill,X,Y,_,[B1,B2,B3,B4]) :-
       tcl_eval(Int,format('fontWidget ~w create text ~w ~w -font $pnfont -text "\\\\" -tags {l~w} -fill ~w',[Win,X,Y,Tag,Fill]),Item),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item]),BS),
       quadruple(Int,BS,B1,B2,B3,B4).

connective(dr,Int,Tag,Win,Fill,X,Y,_,[B1,B2,B3,B4]) :-
       tcl_eval(Int,format('fontWidget ~w create text ~w ~w -text "/" -font $pnfont -tags {l~w} -fill ~w',[Win,X,Y,Tag,Fill]),Item),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item]),BS),
       quadruple(Int,BS,B1,B2,B3,B4).

% ====================================================================
% create_rewrite_window
% open a window with an interactive label rewrite system
% ====================================================================

create_rewrite_window :-
       my_tk_interpreter(I),
       tcl_eval(I,'if {[winfo exists .rewrite]} {\n\
                   wm deiconify .rewrite\n\
                   raise .rewrite\n\
                   .rewrite config -cursor left_ptr\n\
                   } else {\n\
                   if {$interactive != "auto"} {\n\
                   source rewrite.tcl\n\
                   }\n\
                   }',_).

% = display_label(+Label,+Flag)

display_label(Label,Flag) :-
       my_tk_interpreter(Interp),
     ( tcl_eval(Interp,'winfo exists .rewrite',"1") ->
       tcl_eval(Interp,'set rewritestate',Rewrite),
       ( ( 
           Rewrite="nonstop"
         ;
           Rewrite="continue"
         ),
           Flag=1 -> true
       ; 
         retractall('current label'(_)),
         assert('current label'(Label)),
         tcl_eval(Interp,'fontWidget .rewrite.c create text 0 0 -font $pnfont -text "M"',MItem),
         tcl_eval(Interp,format('.rewrite.c bbox ~s',[MItem]),BBox),
         quadruple(Interp,BBox,Left,Bot,Right,Top),
         tcl_eval(Interp,'fontWidget .rewrite.c create text 0 0 -font $indexfont -text "M"',MItem2),
         tcl_eval(Interp,format('.rewrite.c bbox ~s',[MItem2]),BBox2),
         quadruple(Interp,BBox2,Left2,Bot2,Right2,Top2),
         XOff is max(Right-Left,Right2-Left2),
         YOff is 2*max(Top-Bot,Top2-Bot2),
         tcl_eval(Interp,'.rewrite.c delete all',_),
         paint_label(Label,Interp,0,10,X,32,Y,XOff,YOff,_),
	 update_rewrite_size(Interp,X,Y)
       )
     ; true
     ).

redraw_label :-
          my_tk_interpreter(Interp),
	( 'current label'(Label) ->
	  tcl_eval(Interp,'fontWidget .rewrite.c create text 0 0 -font $pnfont -text "M"',MItem),
          tcl_eval(Interp,format('.rewrite.c bbox ~s',[MItem]),BBox),
          quadruple(Interp,BBox,Left,Bot,Right,Top),
          tcl_eval(Interp,'fontWidget .rewrite.c create text 0 0 -font $indexfont -text "M"',MItem2),
          tcl_eval(Interp,format('.rewrite.c bbox ~s',[MItem2]),BBox2),
          quadruple(Interp,BBox2,Left2,Bot2,Right2,Top2),
          XOff is max(Right-Left,Right2-Left2),
          YOff is 2*max(Top-Bot,Top2-Bot2),
          tcl_eval(Interp,'.rewrite.c delete all',_),
          paint_label(Label,Interp,0,10,X,32,Y,XOff,YOff,_),
	  update_rewrite_size(Interp,X,Y)
       ;
	  true
	).

display_rd_label(Label,Flag) :-
       my_tk_interpreter(Interp),
     ( tcl_eval(Interp,'winfo exists .rewrite',"1") ->
       tcl_eval(Interp,'set rewritestate',Rewrite),
       ( (
           Rewrite="nonstop"
         ;
           Rewrite="continue"
         ),
           Flag=1 -> true
       ; 
         retractall('current label'(_)),
         assert('current label'(Label)),
         tcl_eval(Interp,'fontWidget .rewrite.c create text 0 0 -font $pnfont -text "M"',MItem),
         tcl_eval(Interp,format('.rewrite.c bbox ~s',[MItem]),BBox),
         quadruple(Interp,BBox,Left,Bot,Right,Top),
         tcl_eval(Interp,'fontWidget .rewrite.c create text 0 0 -font $indexfont -text "M"',MItem2),
         tcl_eval(Interp,format('.rewrite.c bbox ~s',[MItem2]),BBox2),
         quadruple(Interp,BBox2,Left2,Bot2,Right2,Top2),
         XOff is max(Right-Left,Right2-Left2),
         YOff is 2*max(Top-Bot,Top2-Bot2),
	 tcl_eval(Interp,'.rewrite.c delete all',_),
         paint_label_rd(Label,Interp,0,10,X,32,Y,x,_,XOff,YOff,_),
	 update_rewrite_size(Interp,X,Y)
       )
     ; true
     ).

update_rewrite_size(Interp,X,Y) :-
         tcl_eval(Interp,format('.rewrite configure -width [expr ~w+10]',[X]),_),
         tcl_eval(Interp,format('.rewrite configure -height [expr ~w+8]',[Y]),_),
         tcl_eval(Interp,format('.rewrite.c configure -width [expr ~w+10]',[X]),_),
         tcl_eval(Interp,format('.rewrite.c configure -height [expr ~w+8]',[Y]),_),
         tcl_eval(Interp,format('.rewrite.c lower [.rewrite.c create rectangle 0 0 [lindex [.rewrite.c configure -width] end] [lindex [.rewrite.c configure -height] end] -tags {g_bg} -fill $grail_bg -outline $grail_bg]',[]),_),
        tcl_eval(Interp,'wm geometry .rewrite {}',_),
         tcl_eval(Interp,'update idletasks',_).

rewrite_menu(Num) :-
       'current label'(Label0),
       num_to_list(Num,Pos),
       select_sublabel(Pos,Label0,Label1),
       findall(t('Res',Pos,Label1,Label2),reduce(Label1,Label2),Rs),
       findall(t(Name,Pos,Label1,Label3),safe_call(postulate(Label1,Label3,Name)),Ps),
       create_popup_menu(Rs,Ps,Num).

rewrite_menus :-
       'current label'(Label0),
       findall(Pos,Label1^select_sublabel(Pos,Label0,Label1),List),
       rewrite_menus(List).

rewrite_menus([]).
rewrite_menus([Pos|Rest]) :-
       'current label'(Label0),
       list_to_num(Pos,0,Num),
       select_sublabel(Pos,Label0,Label1),
       findall(t('Res',Pos,Label1,Label2),reduce(Label1,Label2),Rs),
       findall(t(Name,Pos,Label1,Label3),safe_call(postulate(Label1,Label3,Name)),Ps),
       length(Rs,LR),
       length(Ps,LP),
       Tot is LR+LP,
     ( Tot = 0 ->
       rewrite_menus(Rest)
     ;
       create_popup_menu(Rs,Ps,Num),
       my_tk_interpreter(Interp),
       tcl_eval(Interp,'set waitmenu "waiting"\n\
                        tkwait variable waitmenu',_),
       rewrite_menus(Rest)
     ).

list_to_num([],N,N).
list_to_num([X|Xs],N0,N) :-
       N1 is (N0*10)+X,
       list_to_num(Xs,N1,N).

create_popup_menu(Rs,Ps,Path) :-
       my_tk_interpreter(Interp),
       tcl_eval(Interp,'if {[winfo exists .popup]} {\n\
                        destroy .popup\n\
                         }',_),
       tcl_eval(Interp,'menu .popup -tearoff 0',_),
       create_rs_list(Rs,Interp),
       create_ps_list(Ps,Interp),
       tcl_eval(Interp,format('set list [.rewrite.c coords t~w]\n\
               if {[llength $list]==4} {\n\
               set x [expr round([winfo rootx .rewrite.c] + ([lindex $list 2] + [lindex $list 0])/2)]\n\
               set y [expr round([winfo rooty .rewrite.c] + ([lindex $list 3] + [lindex $list 1])/2)]\n\
               } else {\n\
               set x [expr round([winfo rootx .rewrite.c] + [lindex $list 0])]\n\
               set y [expr round([winfo rooty .rewrite.c] + [lindex $list 1])]\n\
               }\n\
               tk_popup .popup $x $y',[Path]),_).

create_rs_list([],_).
create_rs_list([t(Name,Pos,Label1,Label2)|Rs],Interp) :-
       tcl_eval(Interp,format('.popup add command -label "~w" -command {prolog {replace_label(~k,~k,~k)}\n\
               set rewritestate "stepped"\n\
               destroy .popup}',[Name,Pos,Label1,Label2]),_),
       create_rs_list(Rs,Interp).

create_ps_list([],_).
create_ps_list([t(Name,Pos,Label1,Label2)|Ps],Interp) :-
       tcl_eval(Interp,format('.popup add command -label "~w" -command {prolog {replace_label(~k,~k,~k)}\n\
               set rewritestate "stepped"\n\
               destroy .popup}',[Name,Pos,Label1,Label2]),_),
       create_ps_list(Ps,Interp).

replace_label(Ns,Label1,Label2) :-
       'current label'(Label0),
       replace_label1(Ns,Label0,Label1,Label2,Label),
       retractall('current label'(_)),
       assert('current label'(Label)).

replace_label1([],Label0,Label0,Label,Label).
replace_label1([N|Ns],Label0,Label1,Label2,Label) :-
       sub_term(Label0,Label,N,Label3,Label4),
       replace_label1(Ns,Label3,Label1,Label2,Label4).

select_sublabel([],Label,Label).
select_sublabel([N|Ns],Label0,Label) :-
       sub_term(Label0,_,N,Label1,_),
       select_sublabel(Ns,Label1,Label).

% =

paint_label(Pos-A,Int,Num,X0,X,Y0,Y,XOff,_,t(MidB,B2)) :-
	(A='$VAR'(Num0) ->
		 Tag = numvar,
		 pros_var_label(Num0, Label) ; Tag = word, Label = A),
       tcl_eval(Int,format('fontWidget .rewrite.c create text ~w ~w -tags {node ~w b0 t~w} -anchor w -font $pnfont -text "~p"',[X0,Y0,Tag,Num,Label]),Item),
       tcl_eval(Int,format('.rewrite.c bbox ~s',[Item]),BBox),
       quadruple(Int,BBox,B1,B2,X1,Y),
      ( show_lp_numbers(yes),
	number(Pos) ->
       tcl_eval(Int,format('fontWidget .rewrite.c create text ~w [expr ~w+(~w/8)] -tags {node ~w b0 t~w} -anchor nw -text "~p" -font -*-helvetica-medium-o-*-*-8*',[X1,Y0,XOff,Tag,Num,Pos]),Item2),
       tcl_eval(Int,format('.rewrite.c bbox ~s',[Item2]),BBox2),
       quadruple(Int,BBox2,_,_,X,_)
      ;
	X=X1),
       mid(B1,X,MidB).

paint_label(dr(I,A,B),Int,Num0,X0,X,Y0,Y,XOff,YOff,t(MidX,B2)) :-
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+YOff,
       paint_label(A,Int,Num1,X0,X1,Y1,Y2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_label(B,Int,Num2,X2,X3,Y1,Y3,XOff,YOff,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_label_constr(dr,I,Int,'.rewrite.c',norm,Num0,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_label(dl(I,A,B),Int,Num0,X0,X,Y0,Y,XOff,YOff,t(MidX,B2)) :-
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+YOff,
       paint_label(A,Int,Num1,X0,X1,Y1,Y2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_label(B,Int,Num2,X2,X3,Y1,Y3,XOff,YOff,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_label_constr(dl,I,Int,'.rewrite.c',norm,Num0,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_label(p(I,A,B),Int,Num0,X0,X,Y0,Y,XOff,YOff,t(MidX,B2)) :-
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+YOff,
       paint_label(A,Int,Num1,X0,X1,Y1,Y2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_label(B,Int,Num2,X2,X3,Y1,Y3,XOff,YOff,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_label_constr(prod,I,Int,'.rewrite.c',norm,Num0,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_label(zip(I,A),Int,Num0,X0,X,Y0,Y,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label(A,Int,Num1,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_label_constr(dia,I,Int,'.rewrite.c',norm,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label(box(I,A),Int,Num0,X0,X,Y0,Y,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label(A,Int,Num1,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_label_constr(box,I,Int,'.rewrite.c',norm,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label(l(I,A),Int,Num0,X0,X,Y0,Y,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label(A,Int,Num1,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_label_constr(l,I,Int,'.rewrite.c',norm,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label(r(I,A),Int,Num0,X0,X,Y0,Y,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label(A,Int,Num1,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_label_constr(r,I,Int,'.rewrite.c',norm,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label(unzip(I,A),Int,Num0,X0,X,Y0,Y,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label(A,Int,Num1,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_label_constr(unzip,I,Int,'.rewrite.c',norm,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label(unpack(I,A),Int,Num0,X0,X,Y0,Y,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label(A,Int,Num1,X0,X1,Y1,Y,XOff,YOff,t(MidA,AY)),
       paint_label_constr(unpack,I,Int,'.rewrite.c',norm,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

% =

paint_label_rd(Pos-A,Int,Num,X0,X,Y0,Y,P0,P,XOff,_,t(MidB,B2)) :-
	( Pos = x -> P=P0, Fill = black
	; precedes(P0,Pos) -> P = Pos, Fill = black
	; P = P0,	Fill = '$grail_dfg'
	),
	(A='$VAR'(Num0) ->
		 Tag = numvar,
		 pros_var_label(Num0, Label) ; Tag = word, Label = A),
       tcl_eval(Int,format('fontWidget .rewrite.c create text ~w ~w -font $pnfont -fill ~w -tags {node ~w b0 t~w} -anchor w -text "~p"',[X0,Y0,Fill,Tag,Num,Label]),Item),
       tcl_eval(Int,format('.rewrite.c bbox ~s',[Item]),BBox),
       quadruple(Int,BBox,B1,B2,X1,Y),
     ( show_lp_numbers(yes),
	number(Pos) ->
	tcl_eval(Int,format('fontWidget .rewrite.c create text ~w [expr ~w+(~w/8)] -tags {node ~w b0 t~w} -anchor nw -text "~p" -font -*-helvetica-medium-o-*-*-8*',[X1,Y0,XOff,Tag,Num,Pos]),Item2),
	tcl_eval(Int,format('.rewrite.c bbox ~s',[Item2]),BBox2),
	quadruple(Int,BBox2,_,_,X,_)
      ;
	X=X1),
       mid(B1,X,MidB).

paint_label_rd(dr(I,A,B),Int,Num0,X0,X,Y0,Y,_,_,XOff,YOff,t(MidX,B2)) :-
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+YOff,
       paint_label_rd(A,Int,Num1,X0,X1,Y1,Y2,x,_,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_label_rd(B,Int,Num2,X2,X3,Y1,Y3,x,_,XOff,YOff,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_label_constr(dr,I,Int,'.rewrite.c',rd,Num0,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_label_rd(dl(I,A,B),Int,Num0,X0,X,Y0,Y,_,_,XOff,YOff,t(MidX,B2)) :-
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+YOff,
       paint_label_rd(A,Int,Num1,X0,X1,Y1,Y2,x,_,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_label_rd(B,Int,Num2,X2,X3,Y1,Y3,x,_,XOff,YOff,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_label_constr(dl,I,Int,'.rewrite.c',rd,Num0,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_label_rd(p(I,A,B),Int,Num0,X0,X,Y0,Y,P0,P,XOff,YOff,t(MidX,B2)) :-
      (transparent(I) -> P1=P0,P2=P3,P4=P
      ;P1=x,P3=x),
       Num1 is (Num0*10)+1,
       Num2 is Num1+1,
       Y1 is Y0+YOff,
       paint_label_rd(A,Int,Num1,X0,X1,Y1,Y2,P1,P2,XOff,YOff,t(MidA,AY)),
       X2 is X1+XOff,
       paint_label_rd(B,Int,Num2,X2,X3,Y1,Y3,P3,P4,XOff,YOff,t(MidB,BY)),
       Y is max(Y2,Y3),
       mid(MidA,MidB,MidX),
       paint_label_constr(prod,I,Int,'.rewrite.c',rd,Num0,MidX,Y0,XOff,[B1,B2,B3,B4,X4]),
       X is max(X3,X4),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,B1,B4]),_),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidB,BY,B3,B4]),_).

paint_label_rd(zip(I,A),Int,Num0,X0,X,Y0,Y,P0,P,XOff,YOff,t(MidA,B2)) :-
      (transparent_dia(I) -> P1=P0,P2=P
      ;P1=x),
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label_rd(A,Int,Num1,X0,X1,Y1,Y,P1,P2,XOff,YOff,t(MidA,AY)),
       paint_label_constr(dia,I,Int,'.rewrite.c',rd,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label_rd(box(I,A),Int,Num0,X0,X,Y0,Y,_,_,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label_rd(A,Int,Num1,X0,X1,Y1,Y,x,_,XOff,YOff,t(MidA,AY)),
       paint_label_constr(box,I,Int,'.rewrite.c',rd,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label_rd(l(I,A),Int,Num0,X0,X,Y0,Y,_,_,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label_rd(A,Int,Num1,X0,X1,Y1,Y,x,_,XOff,YOff,t(MidA,AY)),
       paint_label_constr(l,I,Int,'.rewrite.c',rd,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label_rd(r(I,A),Int,Num0,X0,X,Y0,Y,_,_,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label_rd(A,Int,Num1,X0,X1,Y1,Y,x,_,XOff,YOff,t(MidA,AY)),
       paint_label_constr(r,I,Int,'.rewrite.c',rd,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label_rd(unzip(I,A),Int,Num0,X0,X,Y0,Y,_,_,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label_rd(A,Int,Num1,X0,X1,Y1,Y,x,_,XOff,YOff,t(MidA,AY)),
       paint_label_constr(unzip,I,Int,'.rewrite.c',rd,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label_rd(unpack(I,A),Int,Num0,X0,X,Y0,Y,_,_,XOff,YOff,t(MidA,B2)) :-
       Num1 is (Num0*10)+1,
       Y1 is Y0+YOff,
       paint_label_rd(A,Int,Num1,X0,X1,Y1,Y,x,_,XOff,YOff,t(MidA,AY)),
       paint_label_constr(unpack,I,Int,'.rewrite.c',rd,Num0,MidA,Y0,XOff,[_B1,B2,_B3,B4,X2]),
       X is max(X1,X2),
       tcl_eval(Int,format('.rewrite.c create line ~w ~w ~w ~w',[MidA,AY,MidA,B4]),_).

paint_label_constr(LC,Index,Int,Win,NormRD,Tags,X,Y,XOff,[C1,C2,C3,C4,MaxX]) :-
       label_constr_fill(NormRD,LC,Index,Fill),
       label_constr(LC,Int,Win,Fill,Tags,X,Y,XOff,[_A1,A2,A3,A4]),
       branching_degree(LC,BD),
       tcl_eval(Int,format('fontWidget ~w create text [expr ~w+(~w/4)] [expr ~w+(~w/8)] -tags {node b~w t~w} -anchor nw -font -*-helvetica-medium-o-*-*-8* -text "~p" -fill ~w',[Win,X,XOff,Y,XOff,BD,Tags,Index,Fill]),Item1),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item1]),BBox1),
       quadruple(Int,BBox1,_B1,B2,B3,B4),
       C1 is X-(XOff/3),
       C3 is X+(XOff/3),
       C2 is min(A2,B2),
       MaxX is max(A3,B3),
       C4 is max(A4,B4).

label_constr_fill(rd,LC,Index,Fill) :-
       label_constr_fill_rd(LC,Index,Fill).
label_constr_fill(norm,LC,Index,Fill) :-
       label_constr_fill_norm(LC,Index,Fill).

label_constr_fill_rd(dl,I,Fill) :-
     ( lazy_dl(I) ->
       Fill = black
     ;
       Fill = '$grail_dfg'
     ).
label_constr_fill_rd(dr,I,Fill) :-
     ( lazy_dr(I) ->
       Fill = black
     ;
       Fill = '$grail_dfg'
     ).
label_constr_fill_rd(unpack,I,Fill) :-
     ( lazy_unpack(I) ->
       Fill = black
     ;
       Fill = '$grail_dfg'
     ).
label_constr_fill_rd(prod,_,black).
label_constr_fill_rd(dia,_,black).
label_constr_fill_rd(l,_,black).
label_constr_fill_rd(r,_,black).
label_constr_fill_rd(unzip,_,black).

label_constr_fill_norm(dl,_,'$grail_dfg').
label_constr_fill_norm(dr,_,'$grail_dfg').
label_constr_fill_norm(l,_,'$grail_dfg').
label_constr_fill_norm(r,_,'$grail_dfg').
label_constr_fill_norm(unzip,_,'$grail_dfg').
label_constr_fill_norm(unpack,_,'$grail_dfg').
label_constr_fill_norm(prod,I,Fill) :-
     ( external(I) ->
       Fill = black
     ;
       Fill = '$grail_dfg'
     ).
label_constr_fill_norm(dia,I,Fill) :-
     ( external_dia(I) ->
       Fill = black
     ;
       Fill = '$grail_dfg'
     ).

label_constr(prod,Int,Win,Fill,Tags,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip is XOff/6,
       L is X-Skip,
       R is X+Skip,
       U is Y+Skip,
       D is Y-Skip,
       L1 is L-Skip,
       R1 is R+Skip,
       U1 is U+Skip,
       D1 is D-Skip,       
       tcl_eval(Int,format('~w create oval ~w ~w ~w ~w -tags {node oval b2 t~w} -outline ~w -fill ~w',[Win,L,D,R,U,Tags,Fill,'$grail_bg']),_).

label_constr(l,Int,Win,Fill,Tags,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip is XOff/5,
       U is Y+Skip,
       D is Y-Skip,
       L is X-Skip,
       R is X+Skip,
       L1 is L-Skip,
       R1 is R+Skip,
       U1 is U+Skip,
       D1 is D-Skip,       
       tcl_eval(Int,format('~w create polygon ~w ~w ~w ~w ~w ~w \
       -tags {node oval b1 t~w} -outline ~w -fill ~w',[Win,R,D,R,U,L,Y,Tags,Fill,Fill]),_).

label_constr(r,Int,Win,Fill,Tags,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip is XOff/5,
       U is Y+Skip,
       D is Y-Skip,
       L is X-Skip,
       R is X+Skip,
       L1 is L-Skip,
       R1 is R+Skip,
       U1 is U+Skip,
       D1 is D-Skip,       
       tcl_eval(Int,format('~w create polygon ~w ~w ~w ~w ~w ~w \
       -tags {node oval b1 t~w} -outline ~w -fill ~w',[Win,L,D,L,U,R,Y,Tags,Fill,Fill]),_). 

label_constr(dia,Int,Win,Fill,Tags,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip1 is XOff/3,
       Skip2 is XOff/5,
       L is X-Skip1,
       R is X+Skip1,
       U is Y+Skip1,
       D is Y-Skip1,
       mid(L,R,MidX),
       mid(U,D,MidY),
       L1 is L-Skip2,
       R1 is R+Skip2,
       U1 is U+Skip2,
       D1 is D-Skip2,
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,MidY,MidX,U,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,MidY,MidX,D,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,R,MidY,MidX,U,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,R,MidY,MidX,D,Fill,Tags]),_).

label_constr(box,Int,Win,Fill,Tags,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip1 is XOff/4,
       Skip2 is XOff/4,
       Skip3 is XOff/2.5,
       Skip4 is XOff/9,
       Skip5 is XOff/4,
       L is X-Skip1,
       R is X+Skip1,
       U is Y+Skip1,
       D is Y-Skip1,
       L1 is L-Skip2,
       R1 is R+Skip2,
       U1 is U+Skip2,
       D1 is D-Skip2,
       tcl_eval(Int,format('~w create line ~w ~w ~w [expr ~w+~w] -tags {node b1 t~w} -fill ~w',[Win,R1,D1,R1,D1,Skip3,Tags,Fill]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+~w] [expr ~w-~w] [expr ~w+~w] -fill ~w -tags  {node b1 t~w}',[Win,R1,D1,Skip3,R1,Skip4,D1,Skip5,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+~w] [expr ~w+~w] [expr ~w+~w] -fill ~w -tags {node b1 t~w}',[Win,R1,D1,Skip3,R1,Skip4,D1,Skip5,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,U,L,D,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,R,U,R,D,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,U,R,U,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,D,R,D,Fill,Tags]),_).

label_constr(unzip,Int,Win,Fill,Tags,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip1 is XOff/4,
       Skip2 is XOff/4,
       Skip3 is XOff/2.5,
       Skip4 is XOff/9,
       Skip5 is XOff/4,
       L is X-Skip1,
       R is X+Skip1,
       U is Y+Skip1,
       D is Y-Skip1,
       L1 is L-Skip2,
       R1 is R+Skip2,
       U1 is U+Skip2,
       D1 is D-Skip2,
       tcl_eval(Int,format('~w create line ~w ~w ~w [expr ~w+~w] -tags {node b1 t~w} -fill ~w',[Win,R1,D1,R1,D1,Skip3,Tags,Fill]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+~w] [expr ~w-~w] [expr ~w+~w] -fill ~w -tags  {node b1 t~w}',[Win,R1,D1,Skip3,R1,Skip4,D1,Skip5,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+~w] [expr ~w+~w] [expr ~w+~w] -fill ~w -tags {node b1 t~w}',[Win,R1,D1,Skip3,R1,Skip4,D1,Skip5,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,U,L,D,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,R,U,R,D,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,D,R,D,Fill,Tags]),_).

label_constr(unpack,Int,Win,Fill,Tags,X,Y,XOff,[L1,D1,R1,U1]) :-
       Skip1 is XOff/4,
       Skip2 is XOff/4,
       Skip3 is XOff/2.5,
       Skip4 is XOff/9,
       Skip5 is XOff/4,
       L is X-Skip1,
       R is X+Skip1,
       U is Y+Skip1,
       D is Y-Skip1,
       L1 is L-Skip2,
       R1 is R+Skip2,
       U1 is U+Skip2,
       D1 is D-Skip2,
       tcl_eval(Int,format('~w create line ~w ~w ~w [expr ~w+~w] -tags {node b1 t~w} -fill ~w',[Win,R1,D1,R1,D1,Skip3,Tags,Fill]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+~w] [expr ~w-~w] [expr ~w+~w] -fill ~w -tags  {node b1 t~w}',[Win,R1,D1,Skip3,R1,Skip4,D1,Skip5,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w [expr ~w+~w] [expr ~w+~w] [expr ~w+~w] -fill ~w -tags {node b1 t~w}',[Win,R1,D1,Skip3,R1,Skip4,D1,Skip5,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,U,L,D,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,R,U,R,D,Fill,Tags]),_),
       tcl_eval(Int,format('~w create line ~w ~w ~w ~w -fill ~w -tags {node b1 t~w}',[Win,L,U,R,U,Fill,Tags]),_).

label_constr(dl,Int,Win,Fill,Tags,X,Y,_,[B1,B2,B3,B4]) :-
       tcl_eval(Int,format('fontWidget ~w create text ~w ~w -font $pnfont -text "\\\\" -tags {node b2 t~w} -fill ~w',[Win,X,Y,Tags,Fill]),Item),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item]),BS),
       quadruple(Int,BS,B1,B2,B3,B4).

label_constr(dr,Int,Win,Fill,Tags,X,Y,_,[B1,B2,B3,B4]) :-
       tcl_eval(Int,format('fontWidget ~w create text ~w ~w -font $pnfont -text "/" -tags {node b2 t~w} -fill ~w',[Win,X,Y,Tags,Fill]),Item),
       tcl_eval(Int,format('~w bbox ~s',[Win,Item]),BS),
       quadruple(Int,BS,B1,B2,B3,B4).

% =

branching_degree(prod,2).
branching_degree(dr,2).
branching_degree(dl,2).
branching_degree(dia,1).
branching_degree(zip,1).
branching_degree(box,1).
branching_degree(unzip,1).
branching_degree(unpack,1).
branching_degree(l,1).
branching_degree(r,1).

% = indicator

create_estimate_window(Interp,Est,Max) :-
       create_proofnet_window,
       compute_estimate(Est,Max),
       format1('~nMax # links: ~w~n===~n',[Max]),
       tcl_eval(Interp,'.stats.ind create rect 0 6 204 16 -fill black',_),
       portray_estimate(Interp,Est,Max),
       Incr0 is round(exp(10,round(log(10,Max)))),
     ( Incr0 >= 10 -> Incr is Incr0//10 ; Incr = Incr0),
       create_bars(Interp,0,Incr,Max).

create_bars(Interp,B0,Incr,M) :-
       H0 is B0 mod (Incr*10),
       H1 is B0 mod (Incr*5),
     ( H0 = 0 -> H = 0 
     ; H1 = 0 -> H = 3
     ; H = 4
     ),
       Bar is (B0*204)//M,
       tcl_eval(Interp,format('.stats.ind create line ~w ~w ~w 6',[Bar,H,Bar]),_),
       B1 is B0+Incr,
     ( B1 > M -> true
     ; create_bars(Interp,B1,Incr,M)
     ).

portray_estimate(Interp,Est,Max) :-
       compute_estimate(Est,E),
       Bar is (E*204)//Max,
       tcl_eval(Interp,format('.stats.ind delete bar\n\
                               .stats.ind create rect 0 6 ~w 16 -outline white -fill white -tag bar',[Bar]),_).

est_to_nf(Xs,Zs) :-
     ( select(F-N,Xs,Ys) ->
       compute_base(Ys,1,Base),
       est_to_nf([f(F,Base,N)|Ys],Zs)
     ;
       Zs = Xs
     ).

compute_estimate(L0,E) :-
       est_to_nf(L0,L),
       compute_estimate(L,1,E).

compute_estimate([],E,E).
compute_estimate([f(_,Base,N0)|R],E0,E) :-
       E1 is E0+(Base*(N0-1)),
       compute_estimate(R,E1,E).

estimate([],L,L).
estimate([vertex(_,As,_)|Vs],L0,L) :-
       estimate_atom(As,L0,L1),
       estimate(Vs,L1,L).

estimate_atom([],L,L).
estimate_atom([A|As],L0,L) :-
       increase_estimate(A,L0,L1),
       estimate_atom(As,L1,L).

increase_estimate(neg(A,_,_,_,_,_),L0,L) :-
       /* allows use of compounds as atomic formulas */ 
       functor(A,F,_),
       increase_estimate(L0,F,1,L).
increase_estimate(pos(_,_,_,_,_,_),L,L).

increase_estimate([B-N0|As0],A,Incr,As) :-
     ( A=B ->
       N is N0+Incr,
       As=[A-N,B-N0|As0]
     ;
       As=[B-N0|As1],
       increase_estimate(As0,A,Incr,As1)
     ).

increase_estimate([],A,Incr,[A-Incr]).

get_estimate(Atom,f(F,Base,N),EstL0,EstL) :-
       atom_formula(Atom,A),
       functor(A,F,_),
       select(F-N,EstL0,EstL),
       compute_base(EstL,1,Base),
       !.

compute_base([],B,B).
compute_base([X|Xs],B0,B) :-
     ( X=_-N ->
       B1 is B0*N
     ;
       B1 = B0
     ),
       compute_base(Xs,B1,B).

% ======================================================================
% Sentence Operations
% ======================================================================

delete_sentence(N) :-
       my_tk_interpreter(I),
       'ex sentences'(Exs0),
       delete_item_num(N,Exs0,Exs),
       retractall('ex sentences'(_)),
       retractall(example(_,_,_,_)),
       assert('ex sentences'(Exs)),
       assert_list(Exs),
       tcl_eval(I,format('.input.sent delete ~w',[N]),_).

example_sentences :-
       my_tk_interpreter(I),
       example_sentences(I).

example_sentences(I) :-
       'ex sentences'(Exs),
       tcl_eval(I,'.input.sent delete 0 end',_),
       example_sentences1(Exs,0,_N,I).

example_sentences1([],N,N,_).
example_sentences1([Sent|Sentences],N0,N,I) :-
	example_sentences2(Sent, I),
	N1 is N0+1,
	example_sentences1(Sentences,N1,N,I).

example_sentences2(example(_,_,Str,_), I) :-
	tcl_eval(I,format('.input.sent insert end {~s}',[Str]),_).
example_sentences2(example(Str,_), I) :-
	tcl_eval(I,format('.input.sent insert end {~s}',[Str]),_).


get_memo(Memo) :-
       my_tk_interpreter(I),
       'ex sentences'(Exs),
       get_item_num(Memo,Exs,example(Str,Y)),
       tcl_eval(I,format('set inputtxt "~s"',[Str]),_),
       tcl_eval(I,format('set inputform {~w}',[Y]),_).

tex_memo(Memo) :-
       my_tk_interpreter(I),
       'ex sentences'(Exs),
       get_item_num(Memo,Exs,example(File,Num,Str,Y)),
       tcl_eval(I,format('set inputtxt "~s"',[Str]),_),
       tcl_eval(I,format('set inputform {~w}',[Y]),_),
       tcl_eval(I,format('prolog parse_new("$inputtxt","$inputform",~w,~w)',[File,Num]),_).

all_examples :-
       retract(latex_output_format(LOF)),
       retractall(latex_output_format(_)),
       assert(latex_output_format(none)),
       'ex sentences'(Exs),
       statistics(runtime,[T0|_]),
       format('~nFull test of all examples. Patience may be required...~2n',[]),
       all_examples(Exs,0),
       retractall(latex_output_format(_)),
       assert(latex_output_format(LOF)),
       statistics(runtime,[T|_]),
       Time is T-T0,
       format('~n~nDone testing all examples.~nTotal CPU Time used: ~3D~n',[Time]).

all_examples([],_).
all_examples([example(Str,F)|Rest],N0) :-
       my_tk_interpreter(I),
       tcl_eval(I,'.input.sent selection clear 0 end',_),
       tcl_eval(I,format('.input.sent selection set ~w',[N0]),_),
       tcl_eval(I,format('.input.sent see ~w',[N0]),_),
       N is N0+1,
       parse(Str,F),  
       all_examples(Rest,N).

% ===================================================================
% Options
% ===================================================================

set_latexout(Y) :-
       options_changed,
       retractall(latex_output_format(_)),
       assert(latex_output_format(Y)).

set_sr(implicit) :-
       options_changed,
       retractall(output_sr(_)),
       retractall(collapse_sr(_,_)),
       assert(output_sr(no)).
set_sr(collapsed) :-
       options_changed,
       retractall(output_sr(_)),
       retractall(collapse_sr(_,_)),
       assert(output_sr(yes)),
       assert(collapse_sr(X,X)).
set_sr(explicit) :-
       options_changed,
       retractall(output_sr(_)),
       retractall(collapse_sr(_,_)),
       assert(output_sr(yes)),
       assert(collapse_sr([],_)).

set_unary_sem('0') :-
       options_changed,
       retractall(unary_semantics(_)),
       assert(unary_semantics(inactive)).
set_unary_sem('1') :-
       options_changed,
       retractall(unary_semantics(_)),
       assert(unary_semantics(active)).
set_unary_sem(0) :-
       options_changed,
       retractall(unary_semantics(_)),
       assert(unary_semantics(inactive)).
set_unary_sem(1) :-
       options_changed,
       retractall(unary_semantics(_)),
       assert(unary_semantics(active)).

set_messages(verbose) :-
       my_tk_interpreter(I),
       tcl_eval(I,'set prologmessages verbose',_),
       retractall(display_mode(_)).

set_messages(quiet) :-
       my_tk_interpreter(I),
       tcl_eval(I,'set prologmessages quiet',_),
       retractall(display_mode(_)),
       assert(display_mode(quiet)).

save_options :-
       my_tk_interpreter(I),
       absolute_file_name('~/.grail_default_options.pl',DFile),
       tell(DFile),
       write(':- dynamic unary_semantics/1,latex_output_format/1,eta_long_proofs/1.\n\
:- dynamic hypo_scope/1,ignore_brackets/1,macro_reduce/1.\n\
:- dynamic output_expl_brackets/1,output_labels/1,output_semantics/1.\n\
:- dynamic output_subst_lex_sem/1,output_reduced_sem/1,output_tex_sem/1.\n\
:- dynamic output_sr/1,collapse_sr/2,show_lp_numbers/1,compact_lex/1.\n\
\n\
:- abolish(portray_message/2).'),nl,
       listing(portray_message/2),nl,
       write('% ==========================================================\n\
% Menu Options\n\
% =========================================================='),nl,nl,
       write('default_options :-\n\
       my_tk_interpreter(I),\n\
       /* menu variables */'),
       tcl_eval(I,'set unarysem',UnarySem),
       tcl_eval(I,'set bracketsem',BracketSem),
       tcl_eval(I,'set outputlabels',OutputLabels),
       tcl_eval(I,'set outputsem',OutputSem),
       tcl_eval(I,'set etalongproofs',EtaLongProofs),
       tcl_eval(I,'set hyposcope',HypoScope),
       tcl_eval(I,'set substlexsem',SubstLexSem),
       tcl_eval(I,'set macroreduce',MacroReduce),
       tcl_eval(I,'set reducesem',ReduceSem),
       tcl_eval(I,'set latexout',LatexOut),
       tcl_eval(I,'set sr',SR),
       tcl_eval(I,'set prologmessages',PrologMessages),
       tcl_eval(I,'set compactlex',CompactLex),
       format('       tcl_eval(I,\'set unarysem       ~s\',_),', [UnarySem]),
       format('       tcl_eval(I,\'set bracketsem     ~s\',_),', [BracketSem]),
       format('       tcl_eval(I,\'set outputlabels   ~s\',_),', [OutputLabels]),
       format('       tcl_eval(I,\'set outputsem      ~s\',_),', [OutputSem]),
       format('       tcl_eval(I,\'set etalongproofs  ~s\',_),', [EtaLongProofs]),
       format('       tcl_eval(I,\'set hyposcope      ~s\',_),', [HypoScope]),
       format('       tcl_eval(I,\'set substlexsem    ~s\',_),', [SubstLexSem]),
       format('       tcl_eval(I,\'set macroreduce    ~s\',_),', [MacroReduce]),
       format('       tcl_eval(I,\'set reducesem      ~s\',_),', [ReduceSem]),
       format('       tcl_eval(I,\'set latexout       ~s\',_),', [LatexOut]),
       format('       tcl_eval(I,\'set sr             ~s\',_),', [SR]),
       format('       tcl_eval(I,\'set prologmessages ~s\',_),', [PrologMessages]),
       format('       tcl_eval(I,\'set compactlex     ~s\',_)', [CompactLex]),
       format('~n       /* interactive debugger */~n', []),
       tcl_eval(I,'set defaultrun',DefaultRun),
       tcl_eval(I,'set defaultrewrite',DefaultRewrite),
       tcl_eval(I,'set interactive',Interactive),
       tcl_eval(I,'set eagerl',EagerL),
       tcl_eval(I,'set showlplabels',ShowLP),
       format('       tcl_eval(I,\'set interactive    ~s\',_),', [Interactive]),
       format('       tcl_eval(I,\'set defaultrun     ~s\',_),', [DefaultRun]),
       format('       tcl_eval(I,\'set defaultrewrite ~s\',_),', [DefaultRewrite]),
       format('       tcl_eval(I,\'set eagerl         ~s\',_),', [EagerL]),
       format('       tcl_eval(I,\'set showlplabels   ~s\',_),', [ShowLP]),
       format('~n       /* font selections */~n', []),
       tcl_eval(I,'set pnfontfamily',PnFontFamily),
       tcl_eval(I,'set pnfontweight',PnFontWeight),
       tcl_eval(I,'set pnfontslant',PnFontSlant),
       tcl_eval(I,'set pnfontsize',PnFontSize),
       tcl_eval(I,'set wordfontsize',WordFontSize),
       format('       tcl_eval(I,\'set pnfontfamily   "~s"\',_),~n', [PnFontFamily]),
       format('       tcl_eval(I,\'set pnfontweight   "~s"\',_),~n', [PnFontWeight]),
       format('       tcl_eval(I,\'set pnfontslant    "~s"\',_),~n', [PnFontSlant]),
       format('       tcl_eval(I,\'set pnfontsize     "~s"\',_),~n', [PnFontSize]),
       format('       tcl_eval(I,\'set wordfontsize   "~s"\',_).~2n', [WordFontSize]),
       write('color_options :-\n\
       my_tk_interpreter(I),\n\
       /* color selections */\n'),
       tcl_eval(I,'set wordcolor',WordColor),
       tcl_eval(I,'set grail_bg',GrailBg),
       tcl_eval(I,'set grail_fg',GrailFg),
       tcl_eval(I,'set grail_abg',GrailAbg),
       tcl_eval(I,'set grail_dfg',GrailDfg),
       tcl_eval(I,'set grail_tc',GrailTc),
       tcl_eval(I,'set grail_sc',GrailSc),
       format('       tcl_eval(I,\'set wordcolor      ~s\',_),~n', [WordColor]),
       format('       tcl_eval(I,\'set grail_bg       ~s\',_),~n', [GrailBg]),
       format('       tcl_eval(I,\'set grail_fg       ~s\',_),~n', [GrailFg]),
       format('       tcl_eval(I,\'set grail_abg      ~s\',_),~n', [GrailAbg]),
       format('       tcl_eval(I,\'set grail_dfg       ~s\',_),~n', [GrailDfg]),
       format('       tcl_eval(I,\'set grail_tc       ~s\',_),~n', [GrailTc]),
       format('       tcl_eval(I,\'set grail_sc       ~s\',_).~2n', [GrailSc]),	     
       write('% ==========================================================\n\
% Options\n\
% ==========================================================\n\
\n\
% ignore_brackets(?Mode)\n\
% Often, too many brackets will make the output unreadable. When\n\
% you are not interested in certain brackets (because they are\n\
% associative, for example, and more readable in list-like notation)\n\
% use this declaration. This will not remove associativity inferences \n\
% from the derivation.'),nl,
       listing(ignore_brackets/1),nl,
       write('% output_expl_brackets(?Flag)\n\
% Setting this flag to ''no'' will not output brackets when the\n\
% precedence of the operators allows this.\n\
% For example, this will produce A*B/C instead of (A*B)/C.'),nl,
       listing(output_expl_brackets/1),nl,
       write('% ='),nl,
       listing(output_text_sem/1),nl,
       write('% boring_rule(?RuleName)\n\
% sequences of rules declared as boring (by rule name) are\n\
% abbreviated by writing a series of dots as premiss of the \n\
% last ''interesting'' rule. Does not make much sense when\n\
% latex_output_format is set to ''fitch''.'),nl,
       listing(boring_rule/1),nl,
       write('% logical_rule(?RuleName)\n\
% identifies the names of the logical rules.'),nl,
       listing(logical_rule/1),nl,
       told,
       [DFile].

% ===================================================================
% confirm_exit
% open a dialog box for exit confirmation
% ===================================================================

confirm_exit :-
       my_tk_interpreter(I),
       tcl_eval(I,'dialog .d {Exit Comfirmation} {Are you sure you want to quit?} questhead 1 Yes No',Answer),
      (Answer = "0" ->
       make_clean,
       tcl_eval(I,'set runningstate "exiting"\n\
                   set rewritestate "exiting"\n\
                   destroy .',_)
      ;
       true).

% ====================================================================
% no_help
% open dialog box with help message
% ====================================================================

no_help :-
       my_tk_interpreter(I),
       tcl_eval(I,'dialog .d {No Help Avaiable} {Sorry, the help function is still under construction.} info 0 Ok',_).

% ====================================================================
% save_fragment
% save the grammar fragment currently in memory
% ====================================================================

save_fragment :-
        my_tk_interpreter(I),
        tcl_eval(I,'set filename [tk_getSaveFile -initialdir $fragmentdir -defaultextension .pl -title "Save as..." -filetypes {{"Prolog Source" {.pl .pro}} {"All Files" *} }]\n\
                    if {$filename != {}} {\n\
                       set fragmentdir [file dirname $filename]\n\
                       prolog retractall(fragment_dir(_))\n\
                       prolog assert(fragment_dir(''$fragmentdir''))\n\
                       saveFile $filename\n\
                    }',_).

save_fragment_as(String) :-
       my_tk_interpreter(I),
       tcl_eval(I,format('file tail ~s',[String]),TrimmedS),       
       name(FilePath,String),
       grail_version(V,R),
       tell(FilePath),
       /* title */
       format('% ~60c~n% ~s~n% ~60c~n% !grail v~w.~w ~n',[61,TrimmedS,61,V,R]),
       /* init */
       format(':- abolish(lazy_unpack/1).~n', []),
       format(':- abolish(lazy_dr/1).~n', []),
       format(':- abolish(lazy_dl/1).~n', []),
       format(':- abolish(transparent_dia/1).~n', []),
       format(':- abolish(transparent/1).~n', []),
       format(':- abolish(continuous_dia/1).~n', []),
       format(':- abolish(continuous/1).~n', []),
       format(':- abolish(external_dia/1).~n', []),
       format(':- abolish(external/1).~n', []),
       format(':- abolish(postulate/3).~n', []),
       format(':- abolish(postulate1/3).~n', []),
       format(':- abolish(macro/2).~n', []),
       format(':- abolish(lex/3).~n', []),
       format(':- abolish(example/2).~2n', []),
       format(':- dynamic lazy_unpack/1,lazy_dr/1,lazy_dl/1.~n', []),
       format(':- dynamic transparent_dia/1,transparent/1.~n', []),
       format(':- dynamic continuous_dia/1,continuous/1.~n', []),
       format(':- dynamic external_dia/1,external/1,atomic_formula/1.~n', []),
       format(':- dynamic postulate/3,postulate1/3,special_string/2.~n', []),
       format(':- dynamic macro/2,lex/3,example/2.~2n', []),
       format('/* postulates */~2n', []),
       format('% ~60c~n% Postulates~n% ~60c~2n',[61,61]),

       format('% = structural postulates~n',[]),
       listing(postulate/3),
       listing(postulate1/3),
       
       format('~n% = lazy evaluation~n',[]),
       listing(lazy_dl/1),
       listing(lazy_dr/1),
       listing(lazy_unpack/1),

       format('~n% = transparency~n',[]),
       listing(transparent/1),
       listing(transparent_dia/1),
	
       format('~n% = continuity~n',[]),
       listing(continuous/1),
       listing(continuous_dia/1),

       format('~n% = non internal modes~n',[]),
       listing(external/1),
       listing(external_dia/1),

        /* macros */

       format('~n% ~60c~n% Macros~n% ~60c~2n',[61,61]),
       format('% = macro(Form,Replacement)~n',[]),
       listing(macro/2),

        /* lexicon */

       format('~n% ~60c~n% Lexicon~n% ~60c~2n',[61,61]),
       format('% = lex(Pros,Formula,Sem)~n',[]),
       listing(lex/3),

        format('~n% = atomic_formula(Formula)~n',[]),
        listing(atomic_formula/1),
	listing(special_string/2),

        /* examples */

        format('~n% ~60c~n% Examples~n% ~60c~2n',[61,61]),
        format('% = example(String,Formula)~n',[]),
        findall(S-F,safe_call(example(S,F)),List),
        portray_examples(List),
        listing(query_db),
        told,
        tcl_eval(I,format('set fl_nm_t [file rootname [file tail ~s]]',[String]),_),
        tcl_eval(I,'if {$fl_nm_t == {}} {\n\
               set fl_nm_t "grail 2.0"\n\
           }\n\
           set fl_nm1 [string toupper [string range $fl_nm_t 0 0]]\n\
           set fl_nm2 [string tolower [string range $fl_nm_t 1 end]]\n\
           wm title . "$fl_nm1$fl_nm2"',_).

% ====================================================================
% compile_file
% opens a window which prompts the user for a filename, and compiles it
% =====================================================================

compile_source :-
        my_tk_interpreter(I),
        tcl_eval(I,'set filename [tk_getOpenFile -initialdir $extensiondir -defaultextension .pl -title "Compile Source" -filetypes {{"Prolog Source" {.pl .pro}}\n\
                                  {"Compiled Prolog" {.ql}}\n\
                                  {"All Files" * }}]\n\
                    if {$filename != {}} {\n\
                       set extensiondir [file dirname $filename]\n\
                       prolog retractall(extension_dir(_))\n\
                       prolog assert(extension_dir(''$extensiondir''))\n\
                       compileSource $filename\n\
                    }',_).

compile_source(String) :-
       name(File,String),
       compile(File).

% ====================================================================
% load_new_fragment(+FileNameString)
% consult file in FileNameString, and give error message if it
% does not exist
% ====================================================================

compile_file :-
        my_tk_interpreter(I),
        tcl_eval(I,'set filename [tk_getOpenFile -initialdir $fragmentdir -defaultextension .pl -title "Load Fragment" -filetypes {{"Prolog Source" {.pl .pro}}\n\
                                  {"Compiled Prolog" {.ql}}\n\
                                  {"All Files" * }}]\n\
                    if {$filename != {}} {\n\
                       set fragmentdir [file dirname $filename]\n\
                       prolog retractall(fragment_dir(_))\n\
                       prolog assert(fragment_dir(''$fragmentdir''))\n\
                       compileFile $filename\n\
                    }',_),
       test_lex,
       test_post,
       update_windows(I),
       retractall('opt state'(_)),
       assert('opt state'(manual)),
       example_sentences(I).

load_new_fragment(String) :-
       my_tk_interpreter(I),
       name(File,String),
       check_file_id(I,File),
       retractall(atomic_formula(_)),
       retractall(special_string(_,_)),
       retractall(query_db(_,_)),
       retractall(query_db(_,_,_)),
       retractall(query_db(_,_,_,_)),
       retractall(example(_,_,_,_)),
       [File],
       tcl_eval(I,format('set fl_nm_t [file rootname [file tail ~s]]',[String]),_),
       tcl_eval(I,'if {$fl_nm_t == {}} {\n\
               set fl_nm_t "grail 2.0"\n\
           }\n\
           set fl_nm1 [string toupper [string range $fl_nm_t 0 0]]\n\
           set fl_nm2 [string tolower [string range $fl_nm_t 1 end]]\n\
           wm title . "$fl_nm1$fl_nm2"',_).

% ====================================================================
% check_file_id
% Check if specified file is a Grail grammar fragment, and generate
% warnings if not.
% ====================================================================

check_file_id(I,File) :-
       see(File),
       skip_line,
       skip_line,
       skip_line,
       get14(List),
  (
	/* Grail 0/Grail 1 file */       
       List = [37,32,33,108,97,98,101,108,115,32,118,CVer0,46,CRel0]
  ;
       /* Grail 2 file */
       List = [37,32,33,103,114,97,105,108,32,118,CVer0,46,CRel0,_]
  ;
       /* Grail 3 file */
       List = [37,32,33,103,114,97,105,108,32,CVer0,46,CRel0,_,_]
  ),
       seen,
       !,
       Ver0 is CVer0-48,
       Rel0 is CRel0-48,
  (
       Ver0 < 1
   ->
       tcl_eval(I,format('dialog .d {Old File Format} {File "~w" was created by an old version of Grail.} warning 1 {Continue} {Cancel}',[File]),"0")
     ;
       Rel0 < 0
   ->
       tcl_eval(I,format('dialog .d {Old File Format} {File "~w" was created by an old release of Grail.} warning 1 {Continue} {Cancel}',[File]),"0")
     ;
       true
   ),
       check_predicates(I,File).

check_file_id(I,File) :-
       seen,
       tcl_eval(I,format('dialog .d {Unknown File Format} {File "~w" is in an unknown file format.} warning 1 {Continue} {Cancel}',[File]),"0"),
       check_predicates(I,File).

check_predicates(I,File) :-
       see(File),
       check_predicates(I,File,[],[]),
       !,
       seen.

check_predicates(_,_) :-
       seen.

check_predicates(I,File,Cs,Os) :-
       Cmds = [lazy_unpack-1,lazy_dr-1,lazy_dl-1,
               transparent_dia-1,transparent-1,special_string-2,
               continuous_dia-1,continuous-1,ensure_loaded-1,
               external_dia-1,external-1,postulate1-3,query_db-_,
               postulate-3,macro-2,lex-3,lex-6,example-4,
               example-2,atomic_formula-1],
       read(X),
     ( X=end_of_file -> print_oc_msgs(I,File,Cs,Os)
     ; X=(:-_)       -> check_predicates(I,File,Cs,Os)
     ; X=(H:-T)      -> functor(H,F,A),
                       ( member_check(F-A,Cmds) ->
                         ( check_tail(T) -> Cs1=Cs 
                         ;
                           Cs1=[F-A|Cs]
                         ),
                         check_predicates(I,File,Cs1,Os)
                       ;
                        check_predicates(I,File,Cs,[F-A|Os])
                       )
     ; functor(X,F,A),
       ( member_check(F-A,Cmds) -> check_predicates(I,File,Cs,Os)
       ; check_predicates(I,File,Cs,[F-A|Os])
       )
     ).

check_tail('is a var') :-
       !,
       fail.

check_tail((T1,T2)) :-
       !,
       check_tail(T1),
       check_tail(T2).

check_tail(T) :-
       functor(T,F,A),
       member_check(F-A,[findall-3,sort-2,member-2,in-2,(#=)-2,(#\=)-2,(#<)-2,(#=<)-2,(#>)-2,(#>=)-2]).

print_oc_msgs(I,File,Cs,Os) :-
       print_o_msgs(Os,I,File),
       print_c_msgs(Cs,I,File).

print_o_msgs([],_,_).
print_o_msgs([O|Os],I,File) :-
       sort([O|Os],Os1),
       list_to_tcl(Os1,Str),
       tcl_eval(I,format('dialog .d {Unknown Predicates} {File "~w" contains definitions for auxiliary predicates: ~s.\n\
\n\
Though this is probably harmless, these predicates will not be saved with the rest of the fragment.} warning 1 {Continue} {Cancel}',[File,Str]),"0").

print_c_msgs([],_,_).
print_c_msgs([C|Cs],I,File) :-
       sort([C|Cs],Cs1),
       list_to_tcl(Cs1,Str),
       tcl_eval(I,format('dialog .d {Conditional Predicates} {File "~w" contains predicates with side conditions: ~s.\n\
\n\
Some clauses for the predicates used in the program are not stated as Prolog facts. It is usually a good idea to rewrite these clauses. Proceed with caution.} warning 1 {Continue} {Cancel}',[File,Str]),"0").

new_fragment :-
       my_tk_interpreter(I),
       new_fragment(I).

new_fragment(I) :-
       abolish(lazy_unpack/1),
       abolish(lazy_dr/1),
       abolish(lazy_dl/1),
       abolish(transparent_dia/1),
       abolish(transparent/1),
       abolish(continuous_dia/1),
       abolish(continuous/1),
       abolish(external_dia/1),
       abolish(external/1),
       abolish(postulate/3),
       abolish(postulate1/3),
       abolish(macro/2),
       abolish(lex/3),
       abolish(example/2),
       retractall(atomic_formula(_)),
       retractall(special_string(_,_)),
       retractall('opt state'(_)),
       assert('opt state'(safe)),
       tcl_eval(I,'.input.sent delete 0 end',_),
       update_windows(I).

update_windows(I) :-
       initialize,
       tcl_eval(I,'.input.entry delete 0 end',_),
       tcl_eval(I,'.input.form delete 0 end',_),
       tcl_eval(I,'winfo exists .post',[Flag]),
      (Flag = 48 ->
       true
      ;
       tcl_eval(I,'.post.c delete all',_),
       tcl_eval(I,'wm geometry .post {}',_),
       post_canvas(I)),
       tcl_eval(I,'winfo exists .lex',[Flag1]),
      (Flag1 = 48 ->
       true
      ;
       tcl_eval(I,'.lex.c delete all',_),
       tcl_eval(I,'wm geometry .lex {}',_),
       lexicon),
      (tcl_eval(I,'winfo exists .an',"0") ->
       true
      ;
       update_analysis_window),
       tcl_eval(I,'if {[winfo exists .lexedit]} {\n\
                   destroy .lexedit\n\
                   }\n\
                   if {[winfo exists .postedit]} {\n\
                   destroy .postedit\n\
                   }',_).

% ====================================================================
% Auxiliaries
% ====================================================================

options_changed :-
       retractall('options changed'),
       assert('options changed').

list_to_tcl([],"").
list_to_tcl([F-A],S) :-
       !,
       name(F,S0),
       name(A,S1),
       append(S0,"/",S2),
       append(S2,S1,S).
list_to_tcl([F-A|Ls],S) :-
       name(F,S0),
       name(A,S1),
       list_to_tcl(Ls,S2),
       append(S0,"/",S3),
       append(S3,S1,S4),
       append(S4,",",S5),
       append(S5,S2,S).

portray_examples([]) :- nl,nl.

portray_examples([S-F|R]) :-
       format('~nexample("~s",~w).',[S,F]),
       portray_examples(R).

get14(L) :-
       get(14,L,[]).

get(0 ,L, L).
get(N0,[C|L0],L) :-
       N0>0,
       N is N0-1,
       get_code(C),
       get(N,L0,L).

delete_tk_interps :-
       retractall(my_tk_interpreter(_)).

set_prolog_var(prologmessages,Val) :-
       set_messages(Val).
set_prolog_var(sr            ,Val) :-
       set_sr(Val).
set_prolog_var(latexout      ,Val) :-
       set_latexout(Val).
set_prolog_var(unarysem      ,Val) :-
       set_unary_sem(Val).
set_prolog_var(etalongproofs ,Val0) :-
       bool(Val0,Val),
       retractall(eta_long_proofs(_)),
       assert(eta_long_proofs(Val)).
set_prolog_var(compactlex    ,Val0) :-
       bool(Val0,Val),
       retractall(compact_lex(_)),
       assert(compact_lex(Val)).
set_prolog_var(hyposcope     ,Val0) :-
       bool(Val0,Val),
       retractall(hypo_scope(_)),
       assert(hypo_scope(Val)).
set_prolog_var(outputlabels  ,Val0) :-
       bool(Val0,Val),
       retractall(output_labels(_)),
       assert(output_labels(Val)).
set_prolog_var(macroreduce   ,Val0) :-
       bool(Val0,Val),
       retractall(macro_reduce(_)),
       assert(macro_reduce(Val)).
set_prolog_var(outputsem     ,Val0) :-
       bool(Val0,Val),
       retractall(output_semantics(_)),
       assert(output_semantics(Val)).
set_prolog_var(bracketsem    ,Val0) :-
       bool(Val0,Val),
       retractall(sem_brackets(_)),
       assert(sem_brackets(Val)).
set_prolog_var(reducesem     ,Val0) :-
       bool(Val0,Val),
       retractall(output_reduced_sem(_)),
       assert(output_reduced_sem(Val)).
set_prolog_var(substlexsem   ,Val0) :-
       bool(Val0,Val),
       retractall(output_subst_lex_sem(_)),
       assert(output_subst_lex_sem(Val)).
set_prolog_var(showlplabels  ,Val0) :-
       bool(Val0,Val),
       retractall(show_lp_numbers(_)),
       assert(show_lp_numbers(Val)).
set_prolog_var(xdvisize      ,Name) :-
       name(Name,String),
       retractall('xdvi initialsize'(_)),
       assert('xdvi initialsize'(String)).

reverse_as(ant,suc).
reverse_as(suc,ant).

bool('0',no).
bool('1',yes).

parse_new(String,FS,File,Num) :-
       tokenize_term(FS,F),
       parse(String,F,File,Num).

parse(String,F,File,Num) :-
       my_tk_interpreter(I),
       tokenize_string(String,X),
       /* check for goal formula */
     ( F = [] ->
       tcl_eval(I,'dialog .d {No Goal Formula} {You have not specified a goal formula.} error 0 Cancel',_)
     ; 
       /* check for optimization */
       ( 'opt state'(unknown) ->
           findall(U,'unary mode'(U),Us),
           findall(B,'binary mode'(B),Bs),
           format1('1. === Retracting Mode Declarations~n',[]),
           retract_all_decls,
           format1('2. === Disabling Early Failure~n',[]), 
           assert(lazy_dl(_)),
           assert(lazy_dr(_)),
           assert(lazy_unpack(_)),
	   ( tcl_eval(I,'winfo exists .an',"1") ->
               set_safe_buttons(Us,Bs,I)
	   ;
	       true
	   ),
           retractall('opt state'(_)),
           assert('opt state'(safe))
       ;
	   true
       ),
         tcl_eval(I,'set runningstate $defaultrun\n\
                     set rewritestate $defaultrewrite\n\
                     if {[winfo exists .stats]} {\n\
                     wm deiconify .stats\n\
                     raise .stats\n\
                     } else {\n\
                     prolog create_proofnet_window\n\
                     tkwait visibility .stats\n\
                     }\n\
                     .stats.txt configure -text "Initializing..."\n\
                     .stats.exit configure -state normal\n\
                     update idletasks\n\
                     set oldFocus [focus]\n\
                     grab set .stats\n\
                     focus .stats',_),
         on_exception(EXC,tex(X,F,File,Num,N),
                              (print(EXC),
                               write_solutions(N0),
                               write_statistics,
                               tell_texout('eg.tex'),nl,
                               told,
                              (N0 = 0 -> N = -1 ; N = N0),
                               tcl_eval(I,'grab release .stats\n\
                               if {[winfo exists .rewrite]} {\n\
                               grab release .rewrite\n\
                               .rewrite.exit configure -state disabled\n\
                               }\n\
                               set runningstate "done"\n\
                               set rewritestate "done"\n\
                               set_cursor left_ptr left_ptr\n\
                               .stats.c dtag node\n\
                               .stats.c delete ax\n\
                               .stats.exit configure -state disabled\n\
                               .stats.txt configure -text "Aborted"\n\
                               focus $oldFocus',_)
                              )
                     ),
         trim_string(String,String0),
         ( N = -1 -> String1=[63|String0]
         ; N = 0  -> String1=[42|String0]
         ; N > 0  -> String1=[32|String0]
         ),
         tcl_eval(I,format('set inputtxt "~s"',[String1]),_),
         'ex sentences'(Exs0),
         insert_ex(Exs0,I,example(0,0,String1,F),0,XN,Exs),
         retractall('ex sentences'(_)),
         assert('ex sentences'(Exs)),
         retractall(example(_,_,_,_)),
         assert_list(Exs),
         tcl_eval(I,'set yv [lindex [.input.sent yview] 0]',_),
         example_sentences(I),
         tcl_eval(I,format('.input.sent yview moveto $yv\n\
                            .input.sent see ~w\n\
                            .input.sent selection clear 0 end\n\
                            .input.sent selection set ~w',[XN,XN]),_)
       ).

insert_ex([],_,example(A,B,S,F),N,N,[example(A,B,S,F)]).
insert_ex([example(_,_,S1,F1)|Rest0],I,example(_,_,S2,F2),N0,N,Rest) :-
     ( F1=F2,
       tokenize_string(S1,WList),
       tokenize_string(S2,WList) ->
       N = N0,
       Rest=[example(S2,F2)|Rest0]
     ; N1 is N0+1,
       Rest=[example(S1,F1)|Rest1],
       insert_ex(Rest0,I,example(S2,F2),N1,N,Rest1)
     ).

trim_string([],[]).
trim_string([C|Cs],Ds) :-
       trim_string(C,Cs,Ds).

trim_string(63,Cs,Ds) :-
       !,
       trim_string(Cs,Ds).
trim_string(32,Cs,Ds) :-
       !,
       trim_string(Cs,Ds).
trim_string(42,Cs,Ds) :-
       !,
       trim_string(Cs,Ds).
trim_string(C,Cs,[C|Cs]).

% = string conversion

quadruple(I,List,A,B,C,D) :-
       tcl_eval(I,format('lindex {~s} 0',[List]),A0),
       tcl_eval(I,format('lindex {~s} 1',[List]),B0),
       tcl_eval(I,format('lindex {~s} 2',[List]),C0),
       tcl_eval(I,format('lindex {~s} 3',[List]),D0),
       number_codes(A,A0),
       number_codes(B,B0),
       number_codes(C,C0),
       number_codes(D,D0).

member_chk(X,[X|_]) :- 
       !.

member_chk(X,[_|Ys]) :-
       member_chk(X,Ys).

lookup(Key,dict(Key,X,_,_),Value) :-
       !,
       X= Value.
lookup(Key,dict(Key1,_,Left,_),Value) :-
       Key<Key1,
       lookup(Key,Left,Value).
lookup(Key,dict(Key1,_,_,Right),Value) :-
       Key>Key1,
       lookup(Key,Right,Value).

freeze(X,Vars0,Vars) :-
       var(X),
       !,
       add_new_var(Vars0,Vars,0,N),
       X = '$VAR'(N).

freeze(' * ',Vars,Vars).

freeze('$VAR'(_),Vars,Vars).

freeze(zip(_,X),Vars0,Vars) :-
       freeze(X,Vars0,Vars).

freeze(p(_,X,Y),Vars0,Vars) :-
       freeze(X,Vars0,Vars1),
       freeze(Y,Vars1,Vars).      

melt_sem(X,X) :-
       var(X),
       !.
melt_sem(lambda(V,Y0),lambda(X,Y)) :-
       !,
       replace_sem(Y0,V,X,Y1),
       melt_sem(Y1,Y).
melt_sem(quant(Q,V,T0),quant(Q,X,T)) :-
       !,
       replace_sem(T0,V,X,T1),
       melt_sem(T1,T).
melt_sem(U,V) :-
       functor(U,F,N),
       functor(V,F,N),
       melt_sem(N,U,V).

melt_sem(0,_,_) :- !.
melt_sem(N0,U,V) :-
       N0>0,
       N is N0-1,
       arg(N0,U,A),
       melt_sem(A,B),
       arg(N0,V,B),
       melt_sem(N,U,V).

melt(A,B) :-
       melt(A,B,_),
       !.

melt('$VAR'(N),X,Dict) :-
       lookup(N,Dict,X).
melt(X,X,_) :-
       atomic(X).
melt(X,Y,Dict) :-
       compound(X),
       functor(X,F,N),
       functor(Y,F,N),
       melt(N,X,Y,Dict).
melt(N,X,Y,Dict) :-
       N>0,
       arg(N,X,ArgX),
       melt(ArgX,ArgY,Dict),
       arg(N,Y,ArgY),
       N1 is N-1,
       melt(N1,X,Y,Dict).
melt(0,_,_,_).

flip_item_num(0, [P0|Ps], [P|Ps],Fill) :-
       flip(P0,P,Fill).
flip_item_num(N0,[P|Ps0], [P|Ps],Fill) :-
       N0>0,
       N is N0-1,
       flip_item_num(N,Ps0,Ps,Fill).

flip(postulate(A,B,C),postulate1(A,B,C),'$grail_dfg').
flip(postulate1(A,B,C),postulate(A,B,C),black).

insert_end([],Y,[Y]).
insert_end([X|Xs],Y,[X|Zs]) :-
       insert_end(Xs,Y,Zs).

delete_item_num(0 ,[_|Rs],Rs).
delete_item_num(N0,[R|Rs0],[R|Rs]) :-
       N0>0,
       N is N0-1,
       delete_item_num(N,Rs0,Rs).

get_item_num(0 ,[P|_],P).
get_item_num(N0,[_|Ps],Q) :-
       N0>0,
       N is N0-1,
       get_item_num(N,Ps,Q).

initialize :-
       retractall('post list'(_)),
       retractall('post undo'(_)),
       retractall('ex sentences'(_)),
       retractall('unary mode'(_)),
       retractall('binary mode'(_)),
       retractall('current literal'(_)),
       retractall('current macro'(_)),
       retractall('current lex'(_)),
       retractall('options changed'),
       findall(postulate(X,Y,N),(safe_call(postulate(X,Y,N)),
                                 numbervars_post(X,0,_),
                                 numbervars_post(Y,0,_)),EPs),
       findall(postulate1(X,Y,N),(safe_call(postulate1(X,Y,N)),
                                  numbervars_post(X,0,_),
                                  numbervars_post(Y,0,_)),DPs),
       findall(X:Y-Z,(safe_call(lex(X,Y,Z)),numbervars(Z,47,_)),Words0),
       findall(example(FN,WN,X,Y),safe_examples(FN,WN,X,Y),Exs),
       append(EPs,DPs,Ps),
       sort(Words0,Words),
       assert('current lex'(Words)),
       assert('ex sentences'(Exs)),
       assert('post list'(Ps)),
       assert('post undo'(Ps)),
       get_modes(Us,Bs),
       assert_all(Us,'unary mode'),
       assert_all(Bs,'binary mode'),
       literals(Ls),
       assert_all(Ls,'current literal'),
       macros(Ms),
       assert_all(Ms,'current macro'),
       filter_decls(Us,Bs).

safe_examples(0, 0, X, Y) :-
	predicate_property(example(_,_), _),
	example(X, Y).
safe_examples(N, W, X, Y) :-
	predicate_property(example(_,_,_,_), _),
	example(N, W, X, Y).


filter_decls(Us,Bs) :-
       map_filter(Us,lazy_unpack,LUs),
       retractall(lazy_unpack(_)),
       assert_all(LUs,lazy_unpack),
       map_filter(Bs,lazy_dl,LLBs),
       retractall(lazy_dl(_)),
       assert_all(LLBs,lazy_dl),
       map_filter(Bs,lazy_dr,LRBs),
       retractall(lazy_dr(_)),
       assert_all(LRBs,lazy_dr),
       map_filter(Us,external_dia,EUs),
       retractall(external_dia(_)),
       assert_all(EUs,external_dia),
       map_filter(Bs,external,EBs),
       retractall(external(_)),
       assert_all(EBs,external),
       map_filter(Us,transparent_dia,TUs),
       retractall(transparent_dia(_)),
       assert_all(TUs,transparent_dia),
       map_filter(Bs,transparent,TBs),
       retractall(transparent(_)),
       assert_all(TBs,transparent),
       map_filter(Us,continuous_dia,CUs),
       retractall(continuous_dia(_)),
       assert_all(CUs,continuous_dia),
       map_filter(Bs,continuous,CBs),
       retractall(continuous(_)),
       assert_all(CBs,continuous).

map_filter([],_,[]).
map_filter([X|Xs],F,Ys) :-
       functor(T,F,1),
       arg(1,T,X),
     ( call(T) ->
       Ys=[X|Ys0],
       map_filter(Xs,F,Ys0)
     ;
       map_filter(Xs,F,Ys)
     ).

assert_all([],_).
assert_all([X|Xs],F) :-
       functor(T,F,1),
       arg(1,T,X),
       assert(T),
       assert_all(Xs,F).

assert_lex([]).
assert_lex([X:Y-Z|Rest]) :-
       assert(lex(X,Y,Z)),
       assert_lex(Rest).

assert_list([]).
assert_list([X|Xs]) :-
       assert(X),
       assert_list(Xs).

assert_melt_list([]).
assert_melt_list([X0|Xs]) :-
       melt(X0,X),
       assert(X),
       assert_melt_list(Xs).

literals(Ls) :-
       findall(L,(safe_call(lex(_,T0,_)),
                  macro_expand(T0,T),
                  literals1(T,L)),Ls0),
       sort(Ls0,Ls).

literals1(dr(_,A,B),C) :-
       literals1(A,C)
     ;
       literals1(B,C).
literals1(dl(_,A,B),C) :-
       literals1(A,C)
     ;
       literals1(B,C).
literals1(p(_,A,B),C) :-
       literals1(A,C)
     ;
       literals1(B,C).
literals1(dia(_,A),B) :-
       literals1(A,B).
literals1(box(_,A),B) :-
       literals1(A,B).
literals1(lit(A),A).

macros(Ms) :-
       findall(M,(safe_call(macro(M,_)),
                  literal(M)),Ms0),
       sort(Ms0,Ms).

file_dir_writable(Dir0,File0,Dir,File) :-
	my_tk_interpreter(I),
       ( \+ directory_exists(Dir0) ->
	    tcl_eval(I,'dialog .d {Directory Does Not Exists} {Please specify a directory where you have write permission.} error 0 OK',_),
 	    change_directory(Dir0,File0,Dir1,File1),
	    file_dir_writable(Dir1,File1,Dir,File)
       ;
	 \+ directory_exists(Dir0,write) ->
	    tcl_eval(I,'dialog .d {No Write Permission} {Please specify a directory where you have write permission.} error 0 OK',_),
 	    change_directory(Dir0,File0,Dir1,File1),
	    file_dir_writable(Dir1,File1,Dir,File)
       ;
	 \+ file_exists(File0) ->
	    Dir = Dir0,
	    File = File0
       ;
	 file_exists(File0,write) ->
	   Dir = Dir0,
           File = File0
       ;
	   tcl_eval(I,'dialog .d {No Write Permission} {Please specify a file which you are allowed to write.} error 0 OK',_),
	   change_directory(Dir0,File0,Dir1,File1),
	   file_dir_writable(Dir1,File1,Dir,File)
       ).
       
change_directory(Dir0,File0,Dir,File) :-
	my_tk_interpreter(I),
	tcl_eval(I,format('tk_getSaveFile -title "Select Directory" -initialdir {~w} -initialfile [file tail ~w]',[Dir0,File0]),FileS),
        (  FileS = "" ->
	   Dir = Dir0,
	   File = File0
	;  tcl_eval(I,format('file dirname ~s',[FileS]),DirS),
	   name(Dir,DirS),
	   name(File,FileS)
	).
	
tell_texout(X0) :-
       tex_out_dir(Dir0),
       generate_file_name(Dir0,X0,File0),
       file_dir_writable(Dir0,File0,Dir,File),
       retractall(tex_out_dir(_)),
       assert(tex_out_dir(Dir)),
       format(user,'~n{Telling LaTeX output file: ~w}~n',[File]),
       tell(File).

tell_fragment(X0) :-
       fragment_dir(Dir0),
       generate_file_name(Dir0,X0,File0),
       file_dir_writable(Dir0,File0,Dir,File),
       retractall(fragment_dir(_)),
       assert(fragment_dir(Dir)),
       format(user,'~n{Telling fragment file: ~w}~n',[File]),
       tell(File).

tell_extension(X0) :-
       extension_dir(Dir0),
       file_dir_writable(Dir0,X0,Dir,X),
       retractall(extension_dir(_)),
       assert(extension_dir(Dir)),
       generate_file_name(Dir,X,File),
       format(user,'~n{Telling extension file: ~w}~n',[File]),
       tell(File).

generate_file_name(Dir,File,DirFile) :-
	name(Dir,DirS),
	name(File,FileS),
	append(DirS,[47|FileS],DirFileS),
	name(DirFile0,DirFileS),
	absolute_file_name(DirFile0,DirFile).

numbervars_post('$VAR'(N0),N0,N) :-
       !,
       N is N0+1.

numbervars_post(zip(_,A),N0,N) :-
       !,
       numbervars_post(A,N0,N).

numbervars_post(p(_,A,B),N0,N) :-
       !,
       numbervars_post(A,N0,N1),
       numbervars_post(B,N1,N).

numbervars_post(_,N,N).

% = safe_call(:Pred)
%
% as call/1, but fails when Pred is undefined (instead of throwing an exception).

safe_call(Pred) :-
	functor(Pred,F,A),
	/* check whether predicate F/A is defined */ 
	functor(Aux,F,A),
	predicate_property(Aux, _),
	!,
	call(Pred).


% = wait for user interaction unlesss the current mode
%   is nonstop or continue.

wait_running(Interp,Result) :-
       tcl_eval(Interp,'update\n\
                        set runningstate',Running),
     ( ( Running = "nonstop"
       ; Running = "continue"
       ) -> Result = Running
     ;
       Running = "cancel" ->
       tcl_eval(Interp,'.stats.exit configure -state disabled',_),
       raise_exception('Cancel')
     ;
       tcl_eval(Interp,'activate .stats\n\
                        if {[winfo exists .rewrite]} {\n\
                        .rewrite.exit configure -state disabled\n\
                        }\n\
                        set runningstate "waiting"\n\
                        tkwait variable runningstate\n\
                        deactivate .stats\n\
                        set runningstate',Result),
       ( Result = "cancel" ->
         tcl_eval(Interp,'.stats.exit configure -state disabled',_),
         raise_exception('Cancel')
       ;
         ( Result = "fail" ->
           tcl_eval(Interp,'.stats.c delete selectbox',_),
           tcl_eval(Interp,'set runningstate "creep"',_),
           fail
         ;
           true
         )
       )
     ).

% = wait for user interaction unless the current mode
%   is nonstop. Called after lookup and after all axiom
%   links have been performed.

wait_running1(Interp,Result) :-
       tcl_eval(Interp,'update\n\
                        set runningstate',Running),
     ( Running = "nonstop" -> Result = Running
     ;
       Running = "cancel" ->
       tcl_eval(Interp,'.stats.exit configure -state disabled',_),
       raise_exception('Cancel')
     ;
       tcl_eval(Interp,'activate .stats\n\
                        if {[winfo exists .rewrite]} {\n\
                        .rewrite.exit configure -state disabled\n\
                        }\n\
                        set runningstate "waiting"\n\
                        tkwait variable runningstate\n\
                        deactivate .stats\n\
                        set runningstate',Result),
       ( Result = "cancel" ->
         tcl_eval(Interp,'.stats.exit configure -state disabled',_),
         raise_exception('Cancel')
       ;
         ( Result = "fail" ->
           tcl_eval(Interp,'.stats.c delete selectbox',_),
           tcl_eval(Interp,'set runningstate "creep"',_),
           fail
         ;
           true
         )
       )
     ).

wait_rewrite(Interp,Result) :-
       tcl_eval(Interp,'update\n\
                        set rewritestate',Rewrite),
     ( Rewrite = "cancel" ->
       Result = "cancel",
       tcl_eval(Interp,'if {[winfo exists .rewrite]} {.rewrite.exit configure -state disabled}',_),
       raise_exception('Cancel')
     ;
       ( Rewrite = "nonstop"
       ; Rewrite = "continue"
       ) -> Result = Rewrite
     ;
       tcl_eval(Interp,'activate .rewrite\n\
                        if {[winfo exists .stats]} {\n\
                        .stats.exit configure -state disabled\n\
                        }\n\
                        set rewritestate "waiting"\n\
                        tkwait variable rewritestate\n\
                        deactivate .rewrite\n\
                        .rewrite.c dtag node', _),
       tcl_eval(Interp,'set rewritestate', Result),
       ( Result = "cancel" ->
         tcl_eval(Interp,'.rewrite.exit configure -state disabled',_),
         raise_exception('Cancel')
       ;
         ( Result = "fail" ->
           tcl_eval(Interp,'set rewritestate "creep"',_),
           fail
         ;
           true
         )
       )
     ).

% = Predicates for Tk-less Grail
	   
load_fragment(File) :-
       retractall(atomic_formula(_)),
       retractall(special_string(_,_)),
       fragment_dir(Dir),
       generate_file_name(Dir,File,FFile),
       [FFile],
       initialize.

% = produce appropriate messages when used without tcltk library

my_tcl_delete('is a variable') :-
       !,
       fail.
my_tcl_delete(I) :-
       tcl_delete(I).

my_tk_new(X,Y) :-
       tcl_new(I),
     ( var(I) ->
       true
     ;
       tk_new(X,Y)
     ).

user:unknown_predicate_handler(tk_new(_,_),_,user:setup_no_interface).

user:unknown_predicate_handler(tcl_new(_),_,user:setup_no_interface).

user:unknown_predicate_handler(tk_main_loop,_,true).

user:unknown_predicate_handler(tcl_delete(_),_,true).

user:unknown_predicate_handler(tcl_eval(_,_,R),_,(var(R) -> R="0";true)).

user:unknown_predicate_handler(my_tk_interpreter(I),_,(var(I) -> I='$dummy';true)).

user:unknown_predicate_handler(shell(_),_,format('~n{Warning: shell call failed!}~n',[])).

user:unknown_predicate_handler(datime(X),_,(ground(X) -> true ; X=datime(1997,8,7,12,0,0))).

user:unknown_predicate_handler(environ('HOME',X),_,(var(X) -> X='.';true)).

user:unknown_predicate_handler(environ('USER',X),_,(var(X) -> X=unknown;true)).

user:unknown_predicate_handler(environ('GRAIL_TEXOUT_DIR',X),_,(var(X) -> X='~/tex';true)).

user:unknown_predicate_handler(environ('GRAIL_FRAGMENTS_DIR',X),_,(var(X) -> X='~/fragments';true)).

user:unknown_predicate_handler(environ('GRAIL_EXTENSIONS_DIR',X),_,(var(X) -> X='~';true)).

catch_environ(Var,Default,Value) :-
     ( environ(Var,Value) ->
       true
     ;
       Value=Default
     ).

setup_no_interface :-
       abolish(user:portray_message/2),
       format('{Warning: something appears to be wrong with the TclTk library!}~n', []),
       format('{You can still use Grail, but you will have limited functionality}~n', []),
       format('Type ''help'' for an overview of the available commands.~n',[]),
       catch_environ('GRAIL_TEXOUT_DIR','~/tex',GTD0),
       catch_environ('GRAIL_FRAGMENTS_DIR','~/fragments',GFD0),
       catch_environ('GRAIL_EXTENSIONS_DIR','~',GED0),
       absolute_file_name(GTD0,GTD),
       absolute_file_name(GFD0,GFD),
       absolute_file_name(GED0,GED),
       assert(tex_out_dir(GTD)),
       assert(fragment_dir(GFD)),
       assert(extension_dir(GED)),
       set_prolog_var(bracketsem,'0'),
       set_prolog_var(outputlabels,'1'),
       set_prolog_var(outputsem,'0'),
       set_prolog_var(etalongproofs,'0'),
       set_prolog_var(hyposcope,'1'),
       set_prolog_var(substlexsem,'1'),
       set_prolog_var(macroreduce,'0'),
       set_prolog_var(reducesem,'1'),
       set_prolog_var(compactlex,'0'),
       set_prolog_var(latexout,nd),
       set_prolog_var(sr,explicit),
       retractall(display_mode(_)).

user_help :-
       format('~Ngrail                      Try to start User Interface again.~n', []),
       format('load_fragment(FileName)    Consult a grammar fragment.~n', []),
       format('parse(String,Formula)      Parse string as expression of type formula.~n', []),
       format('tex(ListOfWords,Formula)   Parse words as expression of type formula.~n', []),
       format('portray_examples           Show example sentences of current fragment.~n', []),
       format('portray_lexicon            Show words in current lexicon.~n,', []),
       format('xdvi                       Launch Xdvi.~n,', []),
       format('halt                       Exit Prolog.~n',[]).

tcl(X) :-
       my_tk_interpreter(I),
       tcl_eval(I,X,S),
       format('~n~s~n',[S]).

% = start

start :-
	load_options,
	grail.

:- start.
