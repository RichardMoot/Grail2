toplevel .post
wm title .post "Postulates"
scrollbar .post.sc -command ".post.c yview"
canvas .post.c -highlightthickness 0 -confine true -bd 2 -relief sunken -yscrollcommand ".post.sc set"

# menu bar

frame .post.mbar -relief raise -bd 2
menubutton .post.mbar.window -text Window -underline 0 -menu .post.mbar.window.menu
menubutton .post.mbar.edit -text Edit -underline 0 -menu .post.mbar.edit.menu
menubutton .post.mbar.help -text Help -underline 0 -menu .post.mbar.help.menu

pack .post.mbar.window .post.mbar.edit -side left
pack .post.mbar.help -side right
tk_menuBar .post.mbar .post.mbar.window .post.mbar.help

# window menu

menu .post.mbar.window.menu -tearoff 0 -postcommand {propagate_options .post.mbar.window.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.post.mbar.window.menu add command -underline 0 -label Close -command {if {[winfo exists .post]} {destroy .post}
                 if {[winfo exists .postedit]} {destroy .postedit}}

# edit menu

menu .post.mbar.edit.menu -tearoff 0 -postcommand {propagate_options .post.mbar.edit.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.post.mbar.edit.menu add command -underline 0 -label "New Postulate..." -command "prolog new_postulate"
.post.mbar.edit.menu add command -underline 0 -label "Edit Postulate..." -command "prolog edit_postulate"
.post.mbar.edit.menu add command -underline 0 -label "Delete Postulate" -command "prolog delete_postulate"
.post.mbar.edit.menu add command -underline 2 -label "Disable/Enable Postulate" -command "prolog flip_postulate"

# help menu

menu .post.mbar.help.menu -tearoff 0 -postcommand {propagate_options .post.mbar.help.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.post.mbar.help.menu add command -label "On This Window" -underline 8 -command "prolog no_help"

pack .post.mbar -side top -fill x
pack .post.sc -side right -fill y
                   pack .post.c -padx 2 -pady 2 -expand 1 -fill both

bind .post.c <Configure> {
     set tags [.post.c gettags post_bar]
     if {$tags != {}} {
     set list_ind [lsearch -regexp $tags {^(post_n)[0-9]+$}]
     set itemno_temp [lindex $tags $list_ind]
     set itemno [string range $itemno_temp 6 end]
     set col [expr $itemno*22]
     set miny [expr $col+9]
     set maxy [expr $col+31]
     set scr_r [lindex [.post.c configure -scrollregion] 4]
     set minx [expr [lindex $scr_r 0]-0]
     set geom [wm geometry .post]
     set xpos [expr [string first x $geom]-1]
     set maxx [expr [string range $geom 0 $xpos] + $minx - 32]
     .post.c delete post_bar
     .post.c create rectangle $minx $miny $maxx $maxy -outline "" -fill #c3c3c3 -tags "post_bar post_n$itemno"
     .post.c create line $minx $maxy $maxx $maxy $maxx $miny -fill #606060 -tags "post_bar post_n$itemno"
     .post.c create line $minx $maxy $minx $miny $maxx $miny -fill #f6f6f6 -tags "post_bar post_n$itemno"
     .post.c lower post_bar
     }
 }

bind .post.c <Button-1> {
     set col [expr int([.post.c canvasy %y])]
     set li [expr ($col-10) / 22]
     set col [expr $li*22]
     set miny [expr $col+9]
     set maxy [expr $col+31]
     set scr_r [lindex [.post.c configure -scrollregion] 4]
     set minx [expr [lindex $scr_r 0]-0]
     set geom [wm geometry .post]
     set xpos [expr [string first x $geom]-1]
     set maxx [expr [string range $geom 0 $xpos] + $minx - 32]
     set scr_y [lindex $scr_r 3]
     .post.c delete post_bar
     if {$miny > 0 && $maxy<=$scr_y} {
     .post.c create rectangle $minx $miny $maxx $maxy -outline "" -fill #c3c3c3 -tags "post_bar post_n$li"    
     .post.c create line $minx $maxy $maxx $maxy $maxx $miny -fill #606060 -tags "post_bar post_n$li"
     .post.c create line $minx $maxy $minx $miny $maxx $miny -fill #f6f6f6 -tags "post_bar post_n$li"
     .post.c lower post_bar
     }}

bind .post.c <Control-Button-1> {
     set col [expr int([.post.c canvasy %y])]
     set li [expr ($col-10) / 22]
     set col [expr $li*22]
     set miny [expr $col+9]
     set maxy [expr $col+31]
     set scr_r [lindex [.post.c configure -scrollregion] 4]
     set scr_y [lindex $scr_r 3]
     if {$miny > 0 && $maxy<=$scr_y} {
     prolog delete_postulate1($li)
     }
}

bind .post.c <Double-Button-1> {
     prolog edit_postulate
}

bind .post.c <Button-2> {
     set col [expr int([.post.c canvasy %y])]
     set li [expr ($col-10) / 22]
     set col [expr $li*22]
     set miny [expr $col+9]
     set maxy [expr $col+31]
     set scr_r [lindex [.post.c configure -scrollregion] 4]
     set minx [expr [lindex $scr_r 0]-0]
     set geom [wm geometry .post]
     set xpos [expr [string first x $geom]-1]
     set maxx [expr [string range $geom 0 $xpos] + $minx - 32]
     set scr_y [lindex $scr_r 3]
     .post.c delete post_bar
     if {$miny > 0 && $maxy<=$scr_y} {
     .post.c create rectangle $minx $miny $maxx $maxy -outline "" -fill #c3c3c3 -tags "post_bar post_n$li"
     .post.c create line $minx $maxy $maxx $maxy $maxx $miny -fill #606060 -tags "post_bar post_n$li"
     .post.c create line $minx $maxy $minx $miny $maxx $miny -fill #f6f6f6 -tags "post_bar post_n$li"
     .post.c lower post_bar
     prolog flip_postulate
     }
}

bind .post.c <Double-Button-1> {
     prolog edit_postulate
     }

bind .post <Control-Button-3> {
     prolog xdvi_post
}
