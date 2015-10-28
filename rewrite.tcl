
# rewrite window

toplevel .rewrite
wm title .rewrite "Rewrite"

# menu bar 

frame .rewrite.mbar -relief raise -bd 2

menubutton .rewrite.mbar.window -text Window -underline 0 -menu .rewrite.mbar.window.menu
menubutton .rewrite.mbar.label -text Labels -underline 0 -menu .rewrite.mbar.label.menu
menubutton .rewrite.mbar.link -text Run -underline 0 -menu .rewrite.mbar.link.menu
menubutton .rewrite.mbar.help -text Help -underline 0 -menu .rewrite.mbar.help.menu

pack .rewrite.mbar.window .rewrite.mbar.label .rewrite.mbar.link -side left
pack .rewrite.mbar.help -side right

tk_menuBar .rewrite.mbar .rewrite.mbar.window .rewrite.mbar.help

# window menu

menu .rewrite.mbar.window.menu -tearoff 0 -postcommand {propagate_options .rewrite.mbar.window.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.rewrite.mbar.window.menu add command -underline 0 -label "Postulate Window" -command {if {[winfo exists .post]} {
                    wm deiconify .post
                    raise .post
                  } else {
                    prolog create_postulate_window
	      }   }
.rewrite.mbar.window.menu add command -underline 0 -label "Save Postscript" -command {write_ps .rewrite.c $texoutdir $grail_dfg $grail_bg}
.rewrite.mbar.window.menu add command -underline 0 -label Close \
      -command {set rewritestate "cancel"
                if {[winfo exists .rewrite]} {
                    destroy .rewrite}
               }

# label menu

menu .rewrite.mbar.label.menu -tearoff 0 -postcommand {propagate_options .rewrite.mbar.label.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.rewrite.mbar.label.menu add radiobutton -underline 0 -label "No Eager Evaluation" -variable eagerl -value "none"
.rewrite.mbar.label.menu add radiobutton -underline 0 -label "Automatic Eager Evaluation" -variable eagerl -value "auto"
.rewrite.mbar.label.menu add radiobutton -underline 0 -label "Manual Eager Evaluation" -variable eagerl -value "manual"

.rewrite.mbar.label.menu add separator

.rewrite.mbar.label.menu add checkbutton -underline 0 -command {prolog redraw_label} -variable showlplabels -label "Show Linear Precedence Information"

# edit menu

menu .rewrite.mbar.link.menu -tearoff 0 -postcommand {propagate_options .rewrite.mbar.link.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.rewrite.mbar.link.menu add command -label "Undo!" -underline 0 -command {set rewritestate "undo"}
.rewrite.mbar.link.menu add separator
.rewrite.mbar.link.menu add radiobutton -underline 0 -label "Trace" -variable defaultrewrite -value "creep"
.rewrite.mbar.link.menu add radiobutton -underline 0 -label "Nonstop" -variable defaultrewrite -value "nonstop"

# help menu

menu .rewrite.mbar.help.menu -tearoff 0 -postcommand {propagate_options .rewrite.mbar.help.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.rewrite.mbar.help.menu add command -label "On This Window" -underline 8 -command "prolog no_help"

canvas .rewrite.c -highlightthickness 0 -closeenough 5 -relief sunken -bd 2

frame .rewrite.ft
frame .rewrite.fb

button .rewrite.creep -state disabled -width 5 -relief groove -bd 4 -text "Creep" -command {set rewritestate "creep"}
button .rewrite.cont -state disabled -width 5 -relief groove -bd 4 -text "Leap" -command {set rewritestate "continue"}
button .rewrite.fail -state disabled -width 5 -relief groove -bd 4 -text "Fail" -command {set rewritestate "fail"}
button .rewrite.nonstop -state disabled -width 5 -relief groove -bd 4\
-text "Nonstop" -command {set runningstate "nonstop"
                          set rewritestate "nonstop"}
button .rewrite.exit -state disabled -width 5 -relief groove -bd 4 -text "Abort" -command {set rewritestate "cancel"}
label .rewrite.text -width 28 -anchor w -text ""

pack .rewrite.mbar -side top -fill x
pack .rewrite.c -side top -padx 2 -pady 2 -expand 1 -fill both
pack .rewrite.ft -side top
pack .rewrite.fb -side top
pack .rewrite.creep -side left -in .rewrite.fb
pack .rewrite.cont -side left -in .rewrite.fb
pack .rewrite.nonstop -side left -in .rewrite.fb
pack .rewrite.fail -side left -in .rewrite.fb
pack .rewrite.exit -side right -anchor e -in .rewrite.ft
pack .rewrite.text -side right -anchor w -in .rewrite.ft\
                     -padx 2 -pady 2

# select
      
.rewrite.c bind node <Button-1> {
     set tags [.rewrite.c gettags current]
     set list_ind [lsearch -regexp $tags {^(t)[0-2]+$}]
     set path_temp [lindex $tags $list_ind]
     set path [string range $path_temp 1 end]
     prolog rewrite_menu($path)
}

.rewrite.c bind node <Button-2> {
    puts [.rewrite.c gettags current]
}

bind .rewrite.c <Button-3> {
    if {$waitmenu == "waiting"} {
    set waitmenu done
    } else {
    prolog rewrite_menus
    }
}

bind .rewrite <Return> {
    set rewritestate "creep"
    update
}

bind .rewrite <f> {
    set rewritestate "fail"
    update
}

bind .rewrite <Control-f> {
    set rewritestate "up"
}

bind .rewrite <l> {
    set rewritestate "continue"
    update
}

bind .rewrite <n> {
    set runningstate "nonstop"
    set rewritestate "nonstop"
    update
}

bind .rewrite <u> {
    set rewritestate "undo"
    update
}

bind .rewrite <Control-c> {
    set runningstate "cancel"
    set rewritestate "cancel"
    update
}

bind .rewrite <Control-p> {
    if {[winfo exists .post]} {
       raise .post
       wm deiconify .post
    } else {
	prolog create_postulate_window
    }
}  

