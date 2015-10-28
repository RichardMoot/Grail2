
# proof net window

toplevel .stats
wm title .stats "Proof Net"
wm iconname .stats "Proof Net"

# menu bar 

frame .stats.mbar -relief raise -bd 2

menubutton .stats.mbar.window -text Window -underline 0 -menu .stats.mbar.window.menu
menubutton .stats.mbar.run -text Debug -underline 0 -menu .stats.mbar.run.menu
menubutton .stats.mbar.link -text Run -underline 0 -menu .stats.mbar.link.menu
menubutton .stats.mbar.help -text Help -underline 0 -menu .stats.mbar.help.menu

pack .stats.mbar.window .stats.mbar.run .stats.mbar.link -side left
pack .stats.mbar.help -side right

tk_menuBar .stats.mbar .stats.mbar.window .stats.mbar.run .stats.mbar.link .stats.mbar.help

# window menu

menu .stats.mbar.window.menu -tearoff 0 -postcommand {propagate_options .stats.mbar.window.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.stats.mbar.window.menu add command -state normal -underline 0 -label "Save Postscript" -command {write_ps .stats.c $texoutdir $grail_dfg $grail_bg}
.stats.mbar.window.menu add command -underline 0 -label Close \
       -command {set runningstate "cancel"
                 set rewritestate "cancel"
                 if {[winfo exists .stats]} { 
                     wm iconify .stats
                 }
                 update
                 }

# run menu

menu .stats.mbar.run.menu -tearoff 0 -postcommand {propagate_options .stats.mbar.run.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.stats.mbar.run.menu add radiobutton -label Automatic -variable interactive -value "auto" -command {
       set defaultrun nonstop
       set defaultrewrite nonstop
       set runningstate nonstop
       set rewritestate nonstop
    if {[winfo exists .stats]} {
       pack forget .stats.c
       pack forget .stats.fb
       .stats.mbar.window.menu entryconfigure 0 -state disabled
       .stats.mbar.link.menu entryconfigure 0 -state disabled
       .stats.mbar.link.menu entryconfigure 1 -state disabled
       .mbar.window.menu entryconfigure 0 -label "Status Window"
       .mbar.window.menu entryconfigure 0 -accelerator "(C-s)"
       .mbar.window.menu entryconfigure 1 -state disabled
       wm title .stats "Status"
       wm iconname .stats "Status"
       .stats.txt configure -relief sunken
    }
    if {[winfo exists .rewrite]} {
       destroy .rewrite
    }   
}

.stats.mbar.run.menu add radiobutton -label Interactive -variable interactive -value "interactive" -command {
       set defaultrun creep
       set defaultrewrite creep
       set runningstate creep
       set rewritestate creep
    if {[winfo exists .stats]} {
           wm title .stats "Proof Net"
           wm iconname .stats "Proof Net"
           .stats.mbar.window.menu entryconfigure 0 -state normal
           .stats.mbar.link.menu entryconfigure 0 -state normal
           .stats.mbar.link.menu entryconfigure 1 -state normal
           .mbar.window.menu entryconfigure 0 -label "Proof Net Window"
           .mbar.window.menu entryconfigure 0 -accelerator "(C-n)"
           .mbar.window.menu entryconfigure 1 -state normal
           pack forget .stats.c
           pack forget .stats.ft
           pack forget .stats.fb
           pack forget .stats.ind
           pack .stats.c -side top -padx 2 -pady 2 -expand 1 -fill both
           pack .stats.ind -side top
           pack .stats.ft -side top
           pack .stats.fb -side top
           .stats.txt configure -relief flat
    }
        if {[winfo exists .rewrite]} {
           raise .rewrite
           wm deiconify .rewrite
	   prolog redraw_label
        } else {
           source rewrite.tcl
        }

}

# link menu

menu .stats.mbar.link.menu -tearoff 0 -postcommand {propagate_options .stats.mbar.link.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.stats.mbar.link.menu add radiobutton -state disabled -underline 0 -label "Trace" -variable defaultrun -value "creep"
.stats.mbar.link.menu add radiobutton -state disabled -underline 0 -label "Nonstop" -variable defaultrun -value "nonstop"

# help menu

menu .stats.mbar.help.menu -tearoff 0 -postcommand {propagate_options .stats.mbar.help.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.stats.mbar.help.menu add command -label "On This Window" -underline 8 -command "prolog no_help"

canvas .stats.c -highlightthickness 0 -relief sunken -bd 2
canvas .stats.ind -width 204 -height 15

frame  .stats.ft
frame  .stats.fb
button .stats.creep -state disabled -width 5 -relief groove -bd 4 -text "Creep" -command {set runningstate "creep"}
button .stats.cont -state disabled -width 5 -relief groove -bd 4 -text "Leap" -command {set runningstate "continue"}
button .stats.fail -state disabled -width 5 -relief groove -bd 4 -text "Fail" -command {set runningstate "fail"}
button .stats.exit -state disabled -width 5 -relief groove -bd 4 -text "Abort"\
 -command {set runningstate "cancel"}
button .stats.nonstop -state disabled -width 5 -relief groove -bd 4\
-text "Nonstop" -command {set runningstate "nonstop"
                          set rewritestate "nonstop"}
label  .stats.txt -height 2 -width 28 -anchor w -text ""

pack .stats.mbar -side top -fill x
pack .stats.c -side top -padx 2 -pady 2 -expand 1 -fill both
pack .stats.ind -side top
pack .stats.ft -side top
pack .stats.fb -side top
pack .stats.creep -side left -in .stats.fb
pack .stats.cont -side left -in .stats.fb
pack .stats.nonstop -side left -in .stats.fb
pack .stats.fail -side left -in .stats.fb
pack .stats.txt -side left -in .stats.ft -anchor w -padx 2 -pady 2
pack .stats.exit -side right -in .stats.ft -anchor e

# select
      
.stats.c bind node <Button-1> {
     focus .stats.c
    if {[.stats.c gettags selectbox] == {}} {
     set tags [.stats.c gettags current]
     set list_ind [lsearch -regexp $tags {^(n)[0-9]+$}]
     set path [lindex $tags $list_ind]
     set box [.stats.c bbox $path]
     set boxl [expr [lindex $box 0] -1]
     set boxr [expr [lindex $box 2] +1]
     set boxu [expr [lindex $box 1] -1]
     set boxd [expr [lindex $box 3] +1]
     set s_tags {}
     lappend s_tags selectbox $path
     .stats.c create line $boxl $boxu $boxr $boxu -tags $s_tags
     .stats.c create line $boxl $boxd $boxr $boxd -tags $s_tags
     .stats.c create line $boxl $boxd $boxl $boxu -tags $s_tags
     .stats.c create line $boxr $boxu $boxr $boxd -tags $s_tags
     set atomid1 [string range $path 1 end]
     set runningstate "selected"
     } else {
     set tags1 [.stats.c gettags current]
     set list_ind1 [lsearch -regexp $tags1 {^(n)[0-9]+$}]
     set num1 [lindex $tags1 $list_ind1]
     set tags2 [.stats.c gettags selectbox]
     set list_ind2 [lsearch -regexp $tags2 {^(n)[0-9]+$}]
     set num2 [lindex $tags2 $list_ind2]
     set box1 [.stats.c bbox $num1]
     set box2 [.stats.c bbox $num2]     
     set box1x [expr ([lindex $box1 0]+[lindex $box1 2])/2]
     set box2x [expr ([lindex $box2 0]+[lindex $box2 2])/2]
     set box1y [expr [lindex $box1 1]+2]
     set box2y [expr [lindex $box2 1]+2]
     set atomid2 [string range $num1 1 end]
     set atomid1 [string range $num2 1 end]
     set runningstate "linked"
    }
}

.stats.c bind node <Shift-Button-1> {
     focus .stats.c
    if {[.stats.c gettags selectbox] == {}} {
     set tags [.stats.c gettags current]
     set list_ind [lsearch -regexp $tags {^(n)[0-9]+$}]
     set path [lindex $tags $list_ind]
     set box [.stats.c bbox $path]
     set boxl [expr [lindex $box 0] -1]
     set boxr [expr [lindex $box 2] +1]
     set boxu [expr [lindex $box 1] -1]
     set boxd [expr [lindex $box 3] +1]
     set s_tags {}
     lappend s_tags selectbox $path
     .stats.c create line $boxl $boxu $boxr $boxu -tags $s_tags
     .stats.c create line $boxl $boxd $boxr $boxd -tags $s_tags
     .stats.c create line $boxl $boxd $boxl $boxu -tags $s_tags
     .stats.c create line $boxr $boxu $boxr $boxd -tags $s_tags
     set atomid1 [string range $path 1 end]
     set runningstate "selected"
     } else {
     set tags1 [.stats.c gettags current]
     set list_ind1 [lsearch -regexp $tags1 {^(n)[0-9]+$}]
     set num1 [lindex $tags1 $list_ind1]
     set tags2 [.stats.c gettags selectbox]
     set list_ind2 [lsearch -regexp $tags2 {^(n)[0-9]+$}]
     set num2 [lindex $tags2 $list_ind2]
     set box1 [.stats.c bbox $num1]
     set box2 [.stats.c bbox $num2]     
     set box1x [expr ([lindex $box1 0]+[lindex $box1 2])/2]
     set box2x [expr ([lindex $box2 0]+[lindex $box2 2])/2]
     set box1y [expr [lindex $box1 1]+2]
     set box2y [expr [lindex $box2 1]+2]
     set atomid2 [string range $num1 1 end]
     set atomid1 [string range $num2 1 end]
     set runningstate "commit"
    }
}

.stats.c bind ax <Button-1> {
     focus .stats.c
     .stats.c itemconfigure current_ax -fill black
     .stats.c dtag current_ax
     set tags [.stats.c gettags current]
     set list_ind [lsearch -regexp $tags {^(n)[0-9]+(n)[0-9]+$}]
     set nums [lindex $tags $list_ind]
     .stats.c addtag current_ax withtag $nums
     .stats.c itemconfigure current_ax -fill white
     .stats.c raise current_ax
}

bind .stats <Return> {
    set runningstate "creep"
    update
}

bind .stats <f> {
    set runningstate "fail"
    update
}

bind .stats <l> {
    set runningstate "continue"
    update
}

bind .stats <n> {
    set runningstate "nonstop"
    set rewritestate "nonstop"
    update
}

bind .stats <Control-c> {
    set runningstate "cancel"
    set rewritestate "cancel"
    update
}

if {$interactive !="interactive"} {
       wm title .stats "Status"
       wm iconname .stats "Status"
       .stats.txt configure -relief sunken
       pack forget .stats.c
       pack forget .stats.fb
}

proc detailedtext {run txt} {
      if {$run != "nonstop" && $run != "continue"} {
           .stats.txt configure -text $txt
	  }
}
