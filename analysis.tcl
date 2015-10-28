toplevel .an
wm title .an Analysis
bind .an <Control-w> "destroy .an"

# menu bar
frame .an.mbar -relief raise -bd 2
menubutton .an.mbar.window -text Window -underline 0 -menu .an.mbar.window.menu
menubutton .an.mbar.options -text Options -underline 0 -menu .an.mbar.options.menu
menubutton .an.mbar.help -text Help -underline 0 -menu .an.mbar.help.menu


pack .an.mbar.window .an.mbar.options -side left
pack .an.mbar.help -side right
tk_menuBar .an.mbar .an.mbar.window .an.mbar.help

# window menu

menu .an.mbar.window.menu -tearoff 0

.an.mbar.window.menu add command -underline 0 -label Close -command {if {[winfo exists .an]} {destroy .an}}

# options menu

menu .an.mbar.options.menu -tearoff 0 -postcommand {propagate_options .an.mbar.options.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.an.mbar.options.menu add command -underline 1 -label "Show Status" -command {prolog display_opt}
.an.mbar.options.menu add command -underline 0 -label "Analyse Postulates" -command {prolog analysis}
.an.mbar.options.menu add command -underline 8 -label "Analyse Convergence" -command {prolog check_convergence}
.an.mbar.options.menu add command -underline 8 -label "Analyse Transparency" -command {prolog check_transparency}
.an.mbar.options.menu add command -underline 9 -label "Analyse Continuity" -command {prolog check_continuity}
.an.mbar.options.menu add command -underline 0 -label "Safe Settings" -command {prolog safe}

# help menu

menu .an.mbar.help.menu -tearoff 0 -postcommand {propagate_options .an.mbar.help.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.an.mbar.help.menu add command -label "On This Window" -underline 8 -command "prolog no_help"

pack .an.mbar -side top -fill x
 
frame .an.ft
frame .an.fb
frame .an.bot
label .an.bot.status -text "" -width 20 -anchor w -relief sunken -bd 2
button .an.bot.analyse -text "Analyse" -relief groove -bd 4 -command {prolog analysis}
button .an.bot.cancel -text "Abort" -relief groove -bd 4 -state disabled -command {set analysis_state "cancel"}
frame .an.ft.l -bd 2 -relief sunken
frame .an.ft.r -bd 2 -relief sunken
frame .an.fb.l -bd 2 -relief sunken
frame .an.fb.r -bd 2 -relief sunken

pack .an.bot -side bottom -fill x
pack .an.bot.status -side left -fill x -expand 1 -ipadx 4 -ipady 3 -padx 2
pack .an.bot.analyse .an.bot.cancel -side left
pack .an.ft .an.fb -pady 2 -side left -fill both -expand 1

pack .an.ft.l .an.ft.r -padx 2 -side left -fill both -expand 1
pack .an.fb.l .an.fb.r -padx 2 -side left -fill both -expand 1

bind .an <h> {prolog display_opt}
bind .an <s> {prolog safe}
bind .an <a> {prolog analysis}
