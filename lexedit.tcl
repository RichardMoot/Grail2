toplevel .lexedit
wm title .lexedit "Edit Lexical Entry"

# menu bar

frame .lexedit.mbar -relief raise -bd 2

menubutton .lexedit.mbar.window -text Window -underline 0 -menu .lexedit.mbar.window.menu
menubutton .lexedit.mbar.edit -text Edit -underline 0 -menu .lexedit.mbar.edit.menu
menubutton .lexedit.mbar.macro -text Macro -underline 0 -menu .lexedit.mbar.macro.menu
menubutton .lexedit.mbar.help -text Help -underline 0 -menu .lexedit.mbar.help.menu

pack .lexedit.mbar.window .lexedit.mbar.edit .lexedit.mbar.macro -side left
pack .lexedit.mbar.help -side right
tk_menuBar .lexedit.mbar .lexedit.mbar.window .lexedit.mbar.help

# window menu

menu .lexedit.mbar.window.menu -tearoff 0 -postcommand {propagate_options .lexedit.mbar.window.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.lexedit.mbar.window.menu add command -underline 0 -label Close -command {if {[winfo exists .lexedit]} {destroy .lexedit}}

# edit menu

menu .lexedit.mbar.edit.menu -tearoff 0 -postcommand {propagate_options .lexedit.mbar.edit.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.lexedit.mbar.edit.menu add command -underline 1 -label "Clear Entry"\
   -command {set itempros "" ; set itemsem ""
             prolog new_lex_entry}
.lexedit.mbar.edit.menu add command -underline 0 -label "Store Entry" -command {prolog store_entry}

.lexedit.mbar.edit.menu add separator

.lexedit.mbar.edit.menu add command -accelerator "(C-k)" -underline 1 -label "Cut" -command {
             set tags [.lexedit.f0.c gettags selectbox]
             set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
             set path_temp [lindex $tags $list_ind]
             set path [string range $path_temp 1 end]
             prolog cut($path)
             }
.lexedit.mbar.edit.menu add command -accelerator "(C-c)" -underline 0 -label "Copy" -command {
             set tags [.lexedit.f0.c gettags selectbox]
             set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
             set path_temp [lindex $tags $list_ind]
             set path [string range $path_temp 1 end]
             prolog copy($path)
             }
.lexedit.mbar.edit.menu add command -accelerator "(C-y)" -underline 0 -label "Paste" -command {
             set tags [.lexedit.f0.c gettags selectbox]
             set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
             set path_temp [lindex $tags $list_ind]
             set path [string range $path_temp 1 end]
             prolog paste($path)
             }

# macro menu

menu .lexedit.mbar.macro.menu -tearoff 0 -postcommand {propagate_options .lexedit.mbar.macro.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.lexedit.mbar.macro.menu add command -underline 6 -label "Store Entry As Macro" -command {prolog store_macro(0,"$literal")}
.lexedit.mbar.macro.menu add command -underline 6 -label "Store Selection As Macro" -command {
             set tags [.lexedit.f0.c gettags selectbox]
             set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
             set path_temp [lindex $tags $list_ind]
             set path [string range $path_temp 1 end]
             prolog store_macro($path,"$literal")
             }
# help menu

menu .lexedit.mbar.help.menu -tearoff 0 -postcommand {propagate_options .lexedit.mbar.help.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.lexedit.mbar.help.menu add command -label "On This Window" -underline 8 -command "prolog no_help"

pack .lexedit.mbar -side top -fill x

frame  .lexedit.f0 -bd 2 -relief sunken
frame  .lexedit.f1 -bd 2
frame  .lexedit.f2 -bd 2
frame  .lexedit.f3 -bd 2
frame  .lexedit.f4 -bd 2
frame  .lexedit.f5 -bd 2

canvas .lexedit.f0.c -closeenough 5 -highlightthickness 0

label  .lexedit.f1.l -text "Pros"
entry  .lexedit.f1.e -textvariable itempros

label  .lexedit.f2.l -text "Sem"
entry  .lexedit.f2.e -textvariable itemsem

canvas .lexedit.f3.dl -width 20 -height 20 -bd 4 -relief groove
canvas .lexedit.f3.p -width 20 -height 20 -bd 4 -relief groove
canvas .lexedit.f3.dr -width 20 -height 20 -bd 4 -relief groove
menubutton .lexedit.f3.index -text "Ind" -bd 2 -relief raised -menu .lexedit.f3.index.m
entry .lexedit.f3.e -width 3 -textvariable bin_index

canvas .lexedit.f4.dummy -width 20 -height 20 -bd 4 -relief flat
canvas .lexedit.f4.dia -width 20 -height 20 -bd 4 -relief groove
canvas .lexedit.f4.box -width 20 -height 20 -bd 4 -relief groove
menubutton .lexedit.f4.index -text "Ind" -bd 2 -relief raised -menu .lexedit.f4.index.m
entry .lexedit.f4.e -width 3 -textvariable un_index

entry .lexedit.f5.l -width 6 -textvariable literal
menubutton .lexedit.f5.atom -text "Atom" -bd 2 -relief raised -menu .lexedit.f5.atom.m
menubutton .lexedit.f5.macro -text "Macro" -bd 2 -relief raised -menu .lexedit.f5.macro.m
menu .lexedit.f5.atom.m -tearoff 0 -postcommand {propagate_options .lexedit.f5.atom.m $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}
menu .lexedit.f5.macro.m -tearoff 0 -postcommand {propagate_options .lexedit.f5.macro.m $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

pack .lexedit.f1.l -side left -padx 2
pack .lexedit.f1.e -side left -expand 1 -fill x -padx 2
pack .lexedit.f2.l -side left -padx 2
pack .lexedit.f2.e -side left -expand 1 -fill x -padx 2
pack .lexedit.f2 -side bottom -fill x
pack .lexedit.f1 -side bottom -fill x
pack .lexedit.f0.c -side left -expand 1
pack .lexedit.f0 -side left -expand 1 -fill both -padx 5 -pady 5
pack .lexedit.f3.dr .lexedit.f3.p .lexedit.f3.dl -side left
pack .lexedit.f3.e -padx 2 -side right
pack .lexedit.f3.index -padx 2 -side right
pack .lexedit.f4.box .lexedit.f4.dia .lexedit.f4.dummy -side left
pack .lexedit.f4.e -padx 2 -side right
pack .lexedit.f4.index -padx 2 -side right
pack .lexedit.f5.atom -padx 2 -side left
pack .lexedit.f5.macro -padx 2 -side left
pack .lexedit.f5.l -padx 2 -side left
pack .lexedit.f3 -pady 2 -side bottom -fill x
pack .lexedit.f4 -pady 2 -side bottom -fill x
pack .lexedit.f5 -pady 4 -side bottom -fill x

# paste

bind .lexedit.f0.c <Control-y> {
     set tags [.lexedit.f0.c gettags selectbox]
     set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 1 end]
     prolog paste($path)
}

# copy
      
bind .lexedit.f0.c <Control-c> {
     set tags [.lexedit.f0.c gettags selectbox]
     set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 1 end]
     prolog copy($path)
}

# cut

bind .lexedit.f0.c <Control-k> {
     set tags [.lexedit.f0.c gettags selectbox]
     set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 1 end]
     prolog cut($path)
}

# select
      
.lexedit.f0.c bind node <Button-1> {
     focus .lexedit.f0.c
     .lexedit.f0.c delete selectbox
     set tags [.lexedit.f0.c gettags current]
     set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
     set path [lindex $tags $list_ind]
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
     .lexedit.f0.c create line $boxr $boxu $boxr $boxd -tags $s_tags
}

menu .lexedit.f4.index.m -tearoff 0 -postcommand {propagate_options .lexedit.f4.index.m $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}        
menu .lexedit.f3.index.m -tearoff 0  -postcommand {propagate_options .lexedit.f3.index.m $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

simulateButton {.lexedit.f3.dl .lexedit.f3.p .lexedit.f3.dr 
                .lexedit.f4.dia .lexedit.f4.box}

# dl button

bind .lexedit.f3.dl <ButtonRelease-1> {
     .lexedit.f3.dl configure -relief groove
     set tags [.lexedit.f0.c gettags selectbox]
     set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 1 end]
     if {$path != {}} {
     prolog paste_constr($path,dl,3,"$bin_index")
     }
}

# p button

bind .lexedit.f3.p <ButtonRelease-1> {
     .lexedit.f3.p configure -relief groove
     set tags [.lexedit.f0.c gettags selectbox]
     set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 1 end]
     if {$path != {}} {
     prolog paste_constr($path,p,3,"$bin_index")
     }
}

# dr button

bind .lexedit.f3.dr <ButtonRelease-1> {
     .lexedit.f3.dr configure -relief groove
     set tags [.lexedit.f0.c gettags selectbox]
     set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
     if {$list_ind != -1} {
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 1 end]
     prolog paste_constr($path,dr,3,"$bin_index")
     }
}

# dia button

bind .lexedit.f4.dia <ButtonRelease-1> {
     .lexedit.f4.dia configure -relief groove
     set tags [.lexedit.f0.c gettags selectbox]
     set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 1 end]
     if {$path != {}} {
     prolog paste_constr($path,dia,2,"$un_index")
     }
}

# box button

bind .lexedit.f4.box <ButtonRelease-1> {
     .lexedit.f4.box configure -relief groove
     set tags [.lexedit.f0.c gettags selectbox]
     set list_ind [lsearch -regexp $tags {^(t)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 1 end]
     if {$path != {}} {
     prolog paste_constr($path,box,2,"$un_index")
     }
}

bind .lexedit.f3.e <Return> {
     prolog add_b_index("$bin_index")
}

bind .lexedit.f4.e <Return> {
     prolog add_u_index("$un_index")
}

bind .lexedit.f5.l <Return> {
     prolog add_literal("$literal")
}

bind .lexedit <Control-s> {
     prolog store_entry
}
