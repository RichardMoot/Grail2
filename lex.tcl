
# lexicon window

toplevel .lex
wm title .lex "Lexicon"

# menu bar 

frame .lex.mbar -relief raise -bd 2

menubutton .lex.mbar.window -text Window -underline 0 -menu .lex.mbar.window.menu
menubutton .lex.mbar.edit -text Edit -underline 0 -menu .lex.mbar.edit.menu
menubutton .lex.mbar.help -text Help -underline 0 -menu .lex.mbar.help.menu

pack .lex.mbar.window .lex.mbar.edit -side left
pack .lex.mbar.help -side right

tk_menuBar .lex.mbar .lex.mbar.window .lex.mbar.help

# window menu

menu .lex.mbar.window.menu -tearoff 0 -postcommand {propagate_options .lex.mbar.window.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.lex.mbar.window.menu add command -underline 0 -label Close -command {if {[winfo exists .lex]} {destroy .lex} 
if {[winfo exists .lexedit]} {destroy .lexedit}}

# edit menu

menu .lex.mbar.edit.menu -tearoff 0 -postcommand {propagate_options .lex.mbar.edit.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.lex.mbar.edit.menu add command -underline 0 -label "New Entry..." -command "prolog new_lex_entry"
.lex.mbar.edit.menu add command -underline 0 -label "Edit Entry..." -command "prolog edit_lex_entry"
.lex.mbar.edit.menu add command -underline 0 -label "Delete Entry" -command "prolog delete_lex_entry"

# help menu

menu .lex.mbar.help.menu -tearoff 0 -postcommand {propagate_options .lex.mbar.help.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc} 

.lex.mbar.help.menu add command -label "On This Window" -underline 8 -command "prolog no_help"

scrollbar .lex.sc -command ".lex.c yview"
canvas .lex.c -highlightthickness 0 -closeenough 5 -relief sunken -bd 2 -yscrollcommand ".lex.sc set"

pack .lex.mbar -side top -fill x
pack .lex.c -side left -padx 2 -pady 2 -expand 1 -fill both
pack .lex.sc -side right -fill y -expand 0

prolog lexicon

bind .lex <Configure> {
     set tags [.lex.c gettags lex_bar]
     if {$tags != {}} {
     set list_ind [lsearch -regexp $tags {^(lex_item)[0-9]+$}]
     set itemno_temp [lindex $tags $list_ind]
     set itemno [string range $itemno_temp 8 end]
     set col [expr $itemno*22]
     set miny [expr $col+9]
     set maxy [expr $col+31]
     set geom [wm geometry .lex]
     set xpos [expr [string first x $geom]-1]
     set maxx [expr [string range $geom 0 $xpos]-26]
     .lex.c delete lex_bar
     .lex.c create rectangle 6 $miny $maxx $maxy -outline "" -fill #c3c3c3 -tags "lex_bar lex_item$itemno"
     .lex.c create line 6 $maxy $maxx $maxy $maxx $miny -fill #606060 -tags "lex_bar lex_item$itemno"
     .lex.c create line 6 $maxy 6 $miny $maxx $miny -fill #f6f6f6 -tags "lex_bar lex_item$itemno"
     .lex.c lower lex_bar
     }
 }

bind .lex.c <Control-Button-1> {
     set col [expr int([.lex.c canvasy %y])]
     set li [expr ($col-10) / 22]
     set col [expr $li*22]
     set miny [expr $col+9]
     set maxy [expr $col+31]
     set scr_r [lindex [lindex [.lex.c configure -scrollregion] 4] 3]
     if {$miny > 0 && $maxy<=$scr_r} {
     prolog delete_lex_entry1($li)
     }
}

bind .lex.c <Double-Button-1> {prolog edit_lex_entry}  

bind .lex.c <Button-1> {
     set col [expr int([.lex.c canvasy %y])]
     set li [expr ($col-10) / 22]
     set col [expr $li*22]
     set miny [expr $col+9]
     set maxy [expr $col+31]
     set geom [wm geometry .lex]
     set xpos [expr [string first x $geom]-1]
     set maxx [expr [string range $geom 0 $xpos]-26]
     set scr_r [lindex [lindex [.lex.c configure -scrollregion] 4] 3]
     .lex.c delete lex_bar
     if {$miny > 0 && $maxy<=$scr_r} {
     .lex.c create rectangle 6 $miny $maxx $maxy -outline "" -fill #c3c3c3 -tags "lex_bar lex_item$li"    
     .lex.c create line 6 $maxy $maxx $maxy $maxx $miny -fill #606060 -tags "lex_bar lex_item$li"
     .lex.c create line 6 $maxy 6 $miny $maxx $miny -fill #f6f6f6 -tags "lex_bar lex_item$li"
     .lex.c lower lex_bar
                }
                }

proc positionLexBar {li} {
     set col [expr $li*22]
     set miny [expr $col+9]
     set maxy [expr $col+31]
     set geom [wm geometry .lex]
     set xpos [expr [string first x $geom]-1]
     set maxx [expr [string range $geom 0 $xpos]-26]
     set scr_r [lindex [lindex [.lex.c configure -scrollregion] 4] 3]
     .lex.c delete lex_bar
     if {$miny > 0 && $maxy<=$scr_r} {
     .lex.c create rectangle 6 $miny $maxx $maxy -outline "" -fill #c3c3c3 -tags "lex_bar lex_item$li"    
     .lex.c create line 6 $maxy $maxx $maxy $maxx $miny -fill #606060 -tags "lex_bar lex_item$li"
     .lex.c create line 6 $maxy 6 $miny $maxx $miny -fill #f6f6f6 -tags "lex_bar lex_item$li"
     .lex.c lower lex_bar
     }
}

bind .lex <Control-Button-3> {
     prolog xdvi_lex
}

