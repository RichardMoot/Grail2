toplevel .postedit
wm title .postedit "Edit Postulate"

frame .postedit.mbar -relief raise -bd 2
menubutton .postedit.mbar.window -text Window -underline 0 -menu .postedit.mbar.window.menu
menubutton .postedit.mbar.edit -text Edit -underline 0 -menu .postedit.mbar.edit.menu
menubutton .postedit.mbar.help -text Help -underline 0 -menu .postedit.mbar.help.menu

pack .postedit.mbar.window .postedit.mbar.edit -side left
pack .postedit.mbar.help -side right
tk_menuBar .postedit.mbar .postedit.mbar.window .postedit.mbar.help

menu .postedit.mbar.window.menu -tearoff 0 -postcommand {propagate_options .postedit.mbar.window.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.postedit.mbar.window.menu add command -underline 0 -label Close -command {if {[winfo exists .postedit]} {destroy .postedit}}

menu .postedit.mbar.edit.menu -tearoff 0 -postcommand {propagate_options .postedit.mbar.edit.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.postedit.mbar.edit.menu add command -underline 1 -label "Clear Postulate" -command {prolog new_postulate}
.postedit.mbar.edit.menu add command -underline 0 -label "Store Postulate" -command {prolog store_postulate}
.postedit.mbar.edit.menu add command -underline 0 -label "Reverse Postulate" -command {prolog reverse_postulate}

.postedit.mbar.edit.menu add separator

.postedit.mbar.edit.menu add command -accelerator "(C-k)" -underline 1 -label "Cut" -command {
     set tags [.postedit.f0.c gettags postselectbox]
     set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
     if {$list_ind != -1} {
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 3 end]
     set as [string range $path_temp 0 2]
     prolog cut_post($as,$path)
     }
 }
.postedit.mbar.edit.menu add command -accelerator "(C-c)" -underline 0 -label "Copy" -command {
     set tags [.postedit.f0.c gettags postselectbox]
     set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
     if {$list_ind != -1} {
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 3 end]
     set as [string range $path_temp 0 2]
     prolog copy_post($as,$path)
     }
 }
.postedit.mbar.edit.menu add command -accelerator "(C-y)" -underline 0 -label "Paste" -command {
     set tags [.postedit.f0.c gettags postselectbox]
     set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
     if {$list_ind != -1} {
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 3 end]
     set as [string range $path_temp 0 2]
     prolog paste_post($as,$path)
     }
 }

menu .postedit.mbar.help.menu -tearoff 0 -postcommand {propagate_options .postedit.mbar.help.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.postedit.mbar.help.menu add command -label "On This Window" -underline 8 -command "prolog no_help"

pack .postedit.mbar -side top -fill x

frame  .postedit.f0 -bd 2 -relief sunken
frame  .postedit.f1 -bd 2
frame  .postedit.f3 -bd 2
frame  .postedit.f4 -bd 2
frame  .postedit.f5 -bd 2

canvas .postedit.f0.c -closeenough 5 -highlightthickness 0
label  .postedit.f1.l -text "Name"
entry  .postedit.f1.e -textvariable postulatename
canvas .postedit.f3.p -width 20 -height 20 -bd 4 -relief groove
canvas .postedit.f4.dia -width 20 -height 20 -bd 4 -relief groove
menubutton .postedit.f3.index -text "Ind" -bd 2 -relief raised -menu .postedit.f3.index.m
menubutton .postedit.f4.index -text "Ind" -bd 2 -relief raised -menu .postedit.f4.index.m
entry .postedit.f3.e -width 3 -textvariable post_bin_index
entry .postedit.f4.e -width 3 -textvariable post_un_index
menubutton .postedit.f5.atom -text "Variable" -bd 2 -relief raised -menu .postedit.f5.atom.m
menu .postedit.f5.atom.m -tearoff 0 -postcommand {propagate_options .postedit.f5.atom.m $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

pack .postedit.f1.l -side left -padx 2
pack .postedit.f1.e -side left -expand 1 -fill x -padx 2
pack .postedit.f1 -side bottom -fill x
pack .postedit.f0.c -side left -expand 1
pack .postedit.f0 -side left -expand 1 -fill both -padx 5 -pady 5
pack .postedit.f3.p -side left
pack .postedit.f3.e -padx 2 -side right
pack .postedit.f3.index -padx 2 -side right
pack .postedit.f4.dia  -side left
pack .postedit.f4.e -padx 2 -side right
pack .postedit.f4.index -padx 2 -side right
pack .postedit.f5.atom -padx 2 -side left
pack .postedit.f3 -pady 2 -side bottom -fill x
pack .postedit.f4 -pady 2 -side bottom -fill x
pack .postedit.f5 -pady 4 -side bottom -fill x

menu .postedit.f4.index.m -tearoff 0 -postcommand {propagate_options .postedit.f4.index.m $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}        
menu .postedit.f3.index.m -tearoff 0  -postcommand {propagate_options .postedit.f3.index.m $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

simulateButton {.postedit.f3.p .postedit.f4.dia}

# paste

bind .postedit.f0.c <Control-y> {
     set tags [.postedit.f0.c gettags postselectbox]
     set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 3 end]
     set as [string range $path_temp 0 2]
     prolog paste_post($as,$path)
}

# copy

bind .postedit.f0.c <Control-c> {
     set tags [.postedit.f0.c gettags postselectbox]
     set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 3 end]
     set as [string range $path_temp 0 2]
     prolog copy_post($as,$path)
}

# cut

bind .postedit.f0.c <Control-k> {
     set tags [.postedit.f0.c gettags postselectbox]
     set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 3 end]
     set as [string range $path_temp 0 2]
     prolog cut_post($as,$path)
}

# select

.postedit.f0.c bind node <Button-1> {
     focus .postedit.f0.c
     .postedit.f0.c delete postselectbox
     set tags [.postedit.f0.c gettags current]
     set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
     set path [lindex $tags $list_ind]
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
     .postedit.f0.c create line $boxr $boxu $boxr $boxd -tags $s_tags
}

bind .postedit.f3.p <ButtonRelease-1> {
     .postedit.f3.p configure -relief groove
     set tags [.postedit.f0.c gettags postselectbox]
     set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 3 end]
     set as [string range $path_temp 0 2]
     if {$path != {}} {
     prolog paste_constr($as,$path,p,3,"$post_bin_index")
     }
}

bind .postedit.f4.dia <ButtonRelease-1> {
     .postedit.f4.dia configure -relief groove
     set tags [.postedit.f0.c gettags postselectbox]
     set list_ind [lsearch -regexp $tags {^(ant|suc)[012]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 3 end]
     set as [string range $path_temp 0 2]
     if {$path != {}} {
     prolog paste_constr($as,$path,zip,2,"$post_un_index")
     }
}

.postedit.f0.c bind arrow <Button-1> {
     set tags [.postedit.f0.c gettags arrow]
     set list_ind [lsearch -regexp $tags {^(dir_)(eq|lr|rl)}]
     set dir_temp [lindex $tags $list_ind]
     .postedit.f0.c dtag arrow $dir_temp
     set direction [string range $dir_temp 4 end]
     switch $direction {
     
	 eq {.postedit.f0.c itemconfigure arrow -arrow first
             .postedit.f0.c addtag dir_rl withtag arrow}

         lr {.postedit.f0.c itemconfigure arrow -arrow both
             .postedit.f0.c addtag dir_eq withtag arrow}

         rl {.postedit.f0.c itemconfigure arrow -arrow last
             .postedit.f0.c addtag dir_lr withtag arrow}
     }
 }
