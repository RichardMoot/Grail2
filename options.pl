:- dynamic unary_semantics/1,latex_output_format/1,eta_long_proofs/1.
:- dynamic hypo_scope/1,ignore_brackets/1,macro_reduce/1.
:- dynamic output_expl_brackets/1,output_labels/1,output_semantics/1.
:- dynamic output_subst_lex_sem/1,output_reduced_sem/1,output_tex_sem/1.
:- dynamic output_sr/1,collapse_sr/2,show_lp_numbers/1, compact_lex/1.

:- abolish(portray_message/2).

% ==========================================================
% Menu Options
% ==========================================================

default_options :-
       my_tk_interpreter(I),
       /* menu variables */
       tcl_eval(I,'set unarysem 1', _),
       tcl_eval(I,'set bracketsem 1', _),
       tcl_eval(I,'set outputlabels 1', _),
       tcl_eval(I,'set outputsem 0', _),
       tcl_eval(I,'set etalongproofs 0', _),
       tcl_eval(I,'set hyposcope 1', _),
       tcl_eval(I,'set substlexsem 0', _),
       tcl_eval(I,'set macroreduce 0', _),
       tcl_eval(I,'set reducesem 0', _),
       tcl_eval(I,'set latexout nd', _),
       tcl_eval(I,'set sr explicit', _),
       tcl_eval(I,'set compactlex 0', _),
       tcl_eval(I,'set prologmessages quiet',_),
       /* interactive debugger */
       tcl_eval(I,'set interactive    interactive', _),
       tcl_eval(I,'set defaultrun     creep', _),
       tcl_eval(I,'set defaultrewrite creep', _),
       tcl_eval(I,'set eagerl         none', _),
       tcl_eval(I,'set showlplabels   0',_).

% ==========================================================
% Options
% ==========================================================

% ignore_brackets(?Mode)
% Often, too many brackets will make the output unreadable. When
% you are not interested in certain brackets (because they are
% associative, for example, and more readable in list-like notation) 
% use this declaration. This will not remove associativity inferences 
% from the derivation.

% output_expl_brackets(?Flag)
% Setting this flag to 'no' will not output brackets when the
% precedence of the operators allows this.
% For example, this will produce A*B/C instead of (A*B)/C.

output_expl_brackets(no).

% =

output_text_sem(yes).

% boring_rule(?RuleName)
% sequences of rules declared as boring (by rule name) are
% abbreviated by writing a series of dots as premiss of the 
% last 'interesting' rule. Does not make much sense when
% latex_output_format is set to 'fitch'.

boring_rule(ax).
boring_rule(lex).

% logical_rule(?RuleName)
% identifies the names of the logical rules.

logical_rule(lex).
logical_rule(hyp(_)).
logical_rule(uhyp).
logical_rule(dri(_,_)).
logical_rule(dre(_)).
logical_rule(dli(_,_)).
logical_rule(dle(_)).
logical_rule(pi(_)).
logical_rule(pe(_,_,_)).
logical_rule(boxi(_)).
logical_rule(boxe(_)).
logical_rule(diai(_)).
logical_rule(diae(_,_)).

