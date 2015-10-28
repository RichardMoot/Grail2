# set the colors

if [catch {set wordcolor}] {set wordcolor  #636363}
if [catch {set grail_bg}]  {set grail_bg   #d9d9d9}
if [catch {set grail_fg}]  {set grail_fg   black}
if [catch {set grail_abg}] {set grail_abg  #ececec}
if [catch {set grail_dfg}] {set grail_dfg  #a3a3a3}
if [catch {set grail_tc}]  {set grail_tc   #c3c3c3}
if [catch {set grail_sc}]  {set grail_sc   #b03060}
if [catch {set wordcolor}] {set wordcolor  #636363}

# set options

option add *background          $grail_bg
option add *foreground          $grail_fg
option add *activeBackground    $grail_abg
option add *activeForeground    $grail_fg
option add *disabledForeground  $grail_dfg
option add *highlightColor      $grail_fg
option add *highlightBackground $grail_bg
option add *insertBackground    $grail_fg
option add *troughColor         $grail_tc
option add *selectColor         $grail_sc
option add *selectBackground    $grail_tc
option add *selectForeground    $grail_fg

# fonts for the proof net and rewrite windows

if [catch {set pnfontfamily}] {set pnfontfamily "itc souvenir"}
if [catch {set pnfontweight}] {set pnfontweight  light}
if [catch {set pnfontslant}]  {set pnfontslant  r}
if [catch {set pnfontwidth}]  {set pnfontwidth  normal}
if [catch {set pnfontsize}]   {set pnfontsize   20}
if [catch {set wordfontsize}] {set wordfontsize 14}
if [catch {set indexsize}] {set indexsize 8}
set pnfont "*-$pnfontfamily-$pnfontweight-$pnfontslant-$pnfontwidth--$pnfontsize-*"
set wordfont "*-$pnfontfamily-$pnfontweight-$pnfontslant-$pnfontwidth--$wordfontsize-*"
set indexfont "-*-helvetica-medium-o-*-*-$indexsize-*"

proc update_options {grail_bg grail_fg grail_abg grail_dfg grail_tc grail_sc} {
     option clear
     option add *background          $grail_bg
     option add *foreground          $grail_fg
     option add *activeBackground    $grail_abg
     option add *activeForeground    $grail_fg
     option add *disabledForeground  $grail_dfg
     option add *highlightColor      $grail_fg
     option add *highlightBackground $grail_bg
     option add *insertBackground    $grail_fg
     option add *troughColor         $grail_tc
     option add *selectColor         $grail_sc
     option add *selectBackground    $grail_tc
     option add *selectForeground    $grail_fg
     propagate_options . $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
     foreach toplevel {. .stats .lex .lexedit .post .postedit .an .color .font .rewrite} {
	 if {[winfo exists $toplevel]} {
     propagate_options $toplevel $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
     }   }
     if {[winfo exists .color]} {
     upvar #0 wordcolor wc
     .color.l1 configure -fg $grail_fg -bg $grail_fg
     .color.l2 configure -fg $grail_bg -bg $grail_bg
     .color.l3 configure -fg $grail_abg -bg $grail_abg
     .color.l4 configure -fg $grail_dfg -bg $grail_dfg
     .color.l5 configure -fg $grail_tc -bg $grail_tc
     .color.l6 configure -fg $grail_sc -bg $grail_sc
     .color.l7 configure -fg $wc -bg $wc
     }
}

proc propagate_options {w grail_bg grail_fg grail_abg grail_dfg grail_tc grail_sc} {
    catch {$w configure -bg $grail_bg}
    catch {$w configure -fg $grail_fg}
    catch {$w configure -activebackground $grail_abg}
    catch {$w configure -activeforeground $grail_fg}
    catch {$w configure -disableforeground $grail_dfg}
    catch {$w configure -highlightbackground $grail_bg}
    catch {$w configure -highlightcolor $grail_fg}
    catch {$w configure -insertbackground $grail_fg}
    catch {$w configure -troughcolor $grail_tc}
    catch {$w configure -selectcolor $grail_sc}
    catch {$w configure -selectbackground $grail_tc}
    catch {$w configure -selectforeground $grail_fg}
    set wlist [pack slaves $w]
    foreach sub $wlist {
    propagate_options $sub $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
}   }

proc setprologvar {var el op} {
     upvar #0 $var x
     prolog set_prolog_var($var,'$x')
}

proc interactive_mode {var el op} {
     upvar #0 $var x
     if {$x == "auto"} {
        set defaultrun nonstop
        set defaultrewrite nonstop
        set runningstate nonstop
        set rewritestate nonstop
        .mbar.window.menu entryconfigure 0 -label "Status Window"
        .mbar.window.menu entryconfigure 0 -accelerator "(C-s)"
	.mbar.window.menu entryconfigure 0 -underline 0
        .mbar.window.menu entryconfigure 1 -state disabled
        if {[winfo exists .stats]} {
           pack forget .stats.c
           pack forget .stats.fb
           .stats.mbar.window.menu entryconfigure 0 -state disabled
           .stats.mbar.link.menu entryconfigure 0 -state disabled
           .stats.mbar.link.menu entryconfigure 1 -state disabled
           wm title .stats "Status"
           wm iconname .stats "Status"
           .stats.txt configure -relief sunken
        }
        if {[winfo exists .rewrite]} {
           destroy .rewrite
        }   
    } else {
        set defaultrun creep
        set defaultrewrite creep
        set runningstate creep
        set rewritestate creep
        .mbar.window.menu entryconfigure 0 -label "Proof Net Window"
        .mbar.window.menu entryconfigure 0 -accelerator "(C-n)"
	.mbar.window.menu entryconfigure 0 -underline 6
        .mbar.window.menu entryconfigure 1 -state normal
        if {[winfo exists .stats]} {
           wm title .stats "Proof Net"
           wm iconname .stats "Proof Net"
           .stats.mbar.window.menu entryconfigure 0 -state normal
           .stats.mbar.link.menu entryconfigure 0 -state normal
           .stats.mbar.link.menu entryconfigure 1 -state normal
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
}

# dialog boxes

proc dialog {w title text bitmap default args} {
     global button
     if {[winfo exists $w]} {
     destroy $w            
     }
     toplevel $w -class Dialog
     wm title $w $title
     wm iconname $w Dialog
     frame $w.top -relief raised -bd 1
     pack $w.top -side top -fill both
     frame $w.bot -relief raised -bd 1
     pack $w.bot -side bottom -fill both

     message $w.top.msg -width 3i -text $text 
     pack $w.top.msg -side right -expand 1 -fill both\
          -padx 3m -pady 3m
     if {$bitmap != ""} {
         label $w.top.bitmap -bitmap $bitmap
         pack $w.top.bitmap -side left -padx 3m -pady 3m
     }

     set i 0
   foreach but $args {
         button $w.bot.button$i -text $but -command\
                 "set button $i" -relief groove -bd 4
         if {$i == $default} {
           frame $w.bot.default -relief raised -bd 2
           raise $w.bot.button$i
           pack $w.bot.default -side left -expand 1\
                 -padx 1m -pady 1m
           pack $w.bot.button$i -in $w.bot.default\
                 -side left -padx 0m -pady 0m\
                 -ipadx 2m -ipady 1m
         } else {
            pack $w.bot.button$i -side left -expand 1\
                -padx 3m -pady 3m -ipadx 2m -ipady 1m
         }
         incr i
     }

     if {$default >= 0} {
         bind $w <Return> "$w.bot.button$default flash; \
              set button $default"
     }
     set oldFocus [focus]
     tkwait visibility $w
     grab set $w
     focus $w

     tkwait variable button
     destroy $w
     focus $oldFocus
     return $button
}

# compile file

proc compileSource {file} {
        if {$file != {}} { 
        return [prolog compile_source("$file")]
        } else {
        return 0
        }
}

proc compileFile {file} {
        if {$file != {}} { 
        return [prolog load_new_fragment("$file")]
        } else {
        return 0
        }
}

# save file

proc saveFile {file} {
        if {$file != {}} {
        return [prolog save_fragment_as("$file")]
        } else {
        return 0
        }
}

# make cavas behave like button

proc simulateButton {list} {
     foreach b $list {
         bind $b <Enter> {
           %W configure -background $grail_abg
           }
         bind $b <Leave> {
           %W configure -background $grail_bg
           }
         bind $b <Button-1> {
           %W configure -relief sunken
           }
         bind $b <ButtonRelease-1> {
           %W configure -relief groove
           }
     }
}

# create widget. fall back to fixed font if specified font is bogus

proc fontWidget { args } {
    if [catch $args w] {
        set ix [lsearch $args -font]
	if {$ix >= 0} {
           set args [lreplace $args $ix [expr $ix+1]]
        }
        set w [eval $args {-font fixed}]
    }
    return $w
}

# activate control buttons in window w

proc activate {w} {
    grab set $w
    wm deiconify $w
    raise $w
    focus $w
    $w.cont configure -state normal
    $w.creep configure -state normal
    $w.fail configure -state normal
    $w.exit configure -state normal
    $w.nonstop configure -state normal
}

# deactivate control buttons in window w

proc deactivate {w} {
    grab release $w
    $w.cont configure -state disabled
    $w.creep configure -state disabled
    $w.fail configure -state disabled
    $w.nonstop configure -state disabled
}

# set cursor for top level windows

proc set_cursor {cursor1 cursor2} {
    . config -cursor $cursor1
    if {[winfo exists .lex]} {
    .lex config -cursor $cursor1
    }
    if {[winfo exists .lexedit]} {
    .lexedit config -cursor $cursor1
    }
    if {[winfo exists .post]} {
    .post config -cursor $cursor1
    }
    if {[winfo exists .postedit]} {
    .postedit config -cursor $cursor1
    }
    if {[winfo exists .stats] && 
        [lindex [.stats config -cursor] 4] !=$cursor2} {
    .stats config -cursor $cursor2
    }
    if {[winfo exists .rewrite] &&
        [lindex [.rewrite config -cursor] 4] !=$cursor2} {
    .rewrite config -cursor $cursor2
    }
    if {[winfo exists .an]} {
    .an config -cursor $cursor1
    }
}

proc write_ps {canvas dir dfg bg} {
    if {! [info exists texoutdir]} {
       if [info exists env(GRAIL_TEXOUT_DIR)] {
          set texoutdir $env(GRAIL_TEXOUT_DIR)
       } else {
          set texoutdir [pwd]
       }
   }
   set filename [tk_getSaveFile -initialdir $dir \
                      -defaultextension .ps -title "Save as..."\
                      -filetypes {{"Postscript" {.ps}}
                                  {"All Files" *} }]
                    if {$filename != {}} {		
                        set colorMap($dfg) {0.64 0.64 0.64 setrgbcolor}
                        set colorMap($bg) {0.85 0.85 0.85 setrgbcolor}
                    $canvas postscript -rotate 0 -colormap colorMap\
			-colormode gray -file $filename
                    }
		} 

# --------------

# reset counters

set latexfmt dvi
set solutionsfound 0
set labelcount 0
set totallinks 0
set planarlinks 0
set acclinks 0
set labellinks 0
set lookups 0
set fragmentdir [pwd]
if [info exists env(GRAIL_FRAGMENTS_DIR)] {
    set fragmentdir $env(GRAIL_FRAGMENTS_DIR)
    } else {
    if {[file exists fragments]} {
        if {[file isdirectory fragments]} {
        set fragmentdir [pwd]/fragments
	}
    }
}

if [info exists env(GRAIL_TEXOUT_DIR)] {
    set texoutdir $env(GRAIL_TEXOUT_DIR)
    } else {
    set texoutdir [pwd]
    }

if [info exists env(GRAIL_EXTENSIONS_DIR)] {
   set extensiondir $env(GRAIL_EXTENSIONS_DIR)
   } else {
   set extensiondir [pwd]
   }

# ---------------

# Global Bindings 

bind all <Control-q> {prolog make_clean
                      set runningstate "exiting"
                      set rewritestate "exiting"
                      destroy .}
bind . <Control-v> {prolog save_fragment}
bind . <Control-c> {prolog compile_file}
bind . <Control-w> {wm iconify .}
bind . <Control-f> {prolog compile_file}
bind . <Control-o> {prolog compile_source}
bind . <Control-a> {if {[winfo exists .an]} {
                       raise .an 
                       wm deiconify .an
                       } else {
                       prolog create_analysis_window}
		   }  
bind . <Control-l> {if {[winfo exists .lex]} {
                       raise .lex
                       wm deiconify .lex
                       } else {
                       prolog create_lexicon_window}
		   }  
bind . <Control-p> {if {[winfo exists .post]} {
                       raise .post
                       wm deiconify .post
                       } else {
                       prolog create_postulate_window}
		   }  
bind . <Control-n> {if {[winfo exists .stats]} {
                       raise .stats
                       wm deiconify .stats
                       } else {
                       prolog create_proofnet_window}
		   }
bind . <Control-s> {if {[winfo exists .stats]} {
                       raise .stats
                       wm deiconify .stats
                       } else {
                       prolog create_proofnet_window}
		   }  
bind . <Control-r> {if {[winfo exists .rewrite]} {
                       raise .rewrite
                       wm deiconify .rewrite
                       } else {
                       prolog create_rewrite_window}
		   }  

# Next line deleted because of conflict with back space
# bind all <Control-h> {prolog no_help}

# -------------

frame .bot

# menu bar 

frame .mbar -relief raise -bd 2

menubutton .mbar.file -text File -underline 0 -menu .mbar.file.menu
menubutton .mbar.sent -text Sentences -underline 0 -menu .mbar.sent.menu
menubutton .mbar.options -text Options -underline 0 -menu .mbar.options.menu
menubutton .mbar.window -text Window -underline 0 -menu .mbar.window.menu
menubutton .mbar.help -text Help -underline 0 -menu .mbar.help.menu

pack .mbar.file .mbar.sent .mbar.options .mbar.window -side left
pack .mbar.help -side right

# menu bar file
    
menu .mbar.file.menu -tearoff 0 -postcommand {propagate_options .mbar.file.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.file.menu add command -label "About..." -underline 0 -command {
    if {[winfo exists .about]} {
    wm deiconify .about
    raise .about
    } else {
    toplevel .about
    wm title .about "About Grail"
    canvas .about.c
    set img [image create photo -file bearer.gif -palette 6/6/6 -height 132 -width 106]
    .about.c create image 0 0 -anchor nw -image $img 
    .about.c configure -height 132
    .about.c configure -width 106
    label .about.txt -text "Grail 2.0"
    fontWidget label .about.txt2 -wraplength 5.5c -justify left \
       -font -*-times-medium-r-*--12* \
       -text "Release: 24-10-2000\n(c) 1997-2000 Richard Moot\n\nDistributed under the GNU General Public License.\n\nEmail bug reports, comments or suggestions to:"
    fontWidget label .about.elm -font -*-courier-medium-r-*--12-*-*-*-m-*\
       -text "Richard.Moot@let.uu.nl"
    button .about.exit -relief groove -bd 4 -text "Cancel" -command {destroy .about} 
    pack .about.c .about.txt .about.txt2 .about.elm .about.exit -padx 2 -side top
    bind .about <Any-KeyPress> {destroy .about}
    }   
}

.mbar.file.menu add separator

.mbar.file.menu add command -label "New Fragment" -underline 0 \
    -command {
       wm title . "Grail 2.0" 
       prolog new_fragment}
.mbar.file.menu add command -label "Consult Fragment..." -command {prolog compile_file} -underline 0 -accelerator "(C-c)"
.mbar.file.menu add command -label "Save Fragment As..." -command {prolog save_fragment} -underline 2 -accelerator "(C-v)"

.mbar.file.menu add separator

.mbar.file.menu add command -label "Compile Prolog Source..." -command {prolog compile_source} -underline 1 -accelerator "(C-o)"

.mbar.file.menu add separator

.mbar.file.menu add command -label "Close" -command "wm iconify ." -underline 1 -accelerator "(C-w)"
.mbar.file.menu add command -label "Quit..." -command "prolog confirm_exit" -underline 0 -accelerator "(C-q)"

# menu sentences

menu .mbar.sent.menu -tearoff 0 -postcommand {propagate_options .mbar.sent.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.sent.menu add command -label "Clear Entry" -underline 0 \
	-command {set inputtxt ""
                  set inputform ""}
.mbar.sent.menu add command -label "Parse" -underline 0 -command {
             foreach i [.input.sent curselection] {
             prolog tex_memo($i)
	     }
	 }

.mbar.sent.menu add command -label "Delete" -underline 0 -command {
             foreach i [.input.sent curselection] {
             prolog delete_sentence($i)
	     }
	 }
    
# menu bar options
    
menu .mbar.options.menu -tearoff 0 -postcommand {propagate_options .mbar.options.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.options.menu add cascade -underline 7 -label "Prolog Messages" -menu .mbar.options.menu.pm

.mbar.options.menu add separator

.mbar.options.menu add cascade -underline 3 -label "View Format" -menu .mbar.options.menu.vf 
.mbar.options.menu add cascade -underline 7 -label "Viewer Geometry" -menu .mbar.options.menu.xs

.mbar.options.menu add separator

.mbar.options.menu add cascade -underline 0 -label "Natural Deduction Style" -menu .mbar.options.menu.lo

.mbar.options.menu add cascade -underline 0 -label Proofs \
                -menu .mbar.options.menu.proofs
.mbar.options.menu add cascade -underline 0 -label Labels \
                -menu .mbar.options.menu.lab
.mbar.options.menu add cascade -underline 2 -label Formulas \
                -menu .mbar.options.menu.for
.mbar.options.menu add cascade -underline 0 -label Semantics \
                -menu .mbar.options.menu.sem

.mbar.options.menu add separator

.mbar.options.menu add command -underline 0 -label "Colors..." -command {
    if {[winfo exists .color]} {
    wm deiconify .color
    raise .color
    } else {
    toplevel .color
    wm title .color "Color Selection"
    frame .color.mbar -relief raise -bd 2
    menubutton .color.mbar.window -text Window -underline 0 -menu .color.mbar.window.menu
    pack .color.mbar.window -side left
    tk_menuBar .color.mbar .color.mbar.window

# window menu

    menu .color.mbar.window.menu -tearoff 0

	.color.mbar.window.menu add command -underline 0 -label Close -command {if {[winfo exists .color]} {destroy .color}}
    frame .color.f1
    frame .color.f2
    frame .color.f3
    frame .color.f4
    frame .color.f5
    frame .color.f6
    frame .color.f7
    button .color.b1 -width 10 -anchor w -text "Foreground" -command {
	set temp [tk_chooseColor -initialcolor $grail_fg -title "Choose Color"]
        if {$temp != ""} {
           set grail_fg $temp 
           update_options $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
           .color.l1 configure -fg $grail_fg -bg $grail_fg
	}
}
    button .color.b2 -width 10 -anchor w -text "Background" -command {
	set temp [tk_chooseColor -initialcolor $grail_bg -title "Choose Color"]
        if {$temp != ""} {
           set grail_bg $temp
           update_options $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
           .color.l2 configure -fg $grail_bg -bg $grail_bg
	}
}
    button .color.b3 -width 10 -anchor w -text "Active" -command {
	set temp [tk_chooseColor -initialcolor $grail_abg -title "Choose Color"]
        if {$temp != ""} {
           set grail_abg $temp
           update_options $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
           .color.l3 configure -fg $grail_abg -bg $grail_abg
	}
}
    button .color.b4 -width 10 -anchor w -text "Disabled" -command {
	set temp [tk_chooseColor -initialcolor $grail_dfg -title "Choose Color"]
        if {$temp != ""} {
           set grail_dfg $temp
           update_options $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
           .color.l4 configure -fg $grail_dfg -bg $grail_dfg
	}
}
    button .color.b5 -width 10 -anchor w -text "Trough" -command {
	set temp [tk_chooseColor -initialcolor $grail_tc -title "Choose Color"]
        if {$temp != ""} {
           set grail_tc $temp
           update_options $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
           .color.l5 configure -fg $grail_tc -bg $grail_tc
	}
}
    button .color.b6 -width 10 -anchor w -text "Select" -command {
	set temp [tk_chooseColor -initialcolor $grail_sc -title "Choose Color"]
        if {$temp != ""} {
           set grail_sc $temp
           update_options $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
           .color.l6 configure -fg $grail_sc -bg $grail_sc
	}
}
    button .color.b7 -width 10 -anchor w -text "Words" -command {
	set temp [tk_chooseColor -initialcolor $wordcolor -title "Choose Color"]
        if {$temp != ""} {
           set wordcolor $temp
           .color.l7 configure -fg $wordcolor -bg $wordcolor
	}
}
    label .color.l1 -width 3 -height 1 -bd 2 -relief groove -bg $grail_fg -fg $grail_fg
    label .color.l2 -width 3 -height 1 -bd 2 -relief groove -bg $grail_bg -fg $grail_bg
    label .color.l3 -width 3 -height 1 -bd 2 -relief groove -bg $grail_abg -fg $grail_abg
    label .color.l4 -width 3 -height 1 -bd 2 -relief groove -bg $grail_dfg -fg $grail_dfg
    label .color.l5 -width 3 -height 1 -bd 2 -relief groove -bg $grail_tc -fg $grail_tc
    label .color.l6 -width 3 -height 1 -bd 2 -relief groove -bg $grail_sc -fg $grail_sc
    label .color.l7 -width 3 -height 1 -bd 2 -relief groove -bg $wordcolor -fg $wordcolor
    pack .color.mbar -fill x -side top
    pack .color.f1 .color.f2 .color.f3 .color.f4 .color.f5 .color.f6 .color.f7 -side top
    pack .color.b1 -in .color.f1 -side left
    pack .color.l1 -in .color.f1 -side left -padx 1m -pady 1m
    pack .color.b2 -in .color.f2 -side left
    pack .color.l2 -in .color.f2 -side left -padx 1m -pady 1m
    pack .color.b3 -in .color.f3 -side left
    pack .color.l3 -in .color.f3 -side left -padx 1m -pady 1m
    pack .color.b4 -in .color.f4 -side left
    pack .color.l4 -in .color.f4 -side left -padx 1m -pady 1m
    pack .color.b5 -in .color.f5 -side left
    pack .color.l5 -in .color.f5 -side left -padx 1m -pady 1m
    pack .color.b6 -in .color.f6 -side left
    pack .color.l6 -in .color.f6 -side left -padx 1m -pady 1m
    pack .color.b7 -in .color.f7 -side left
    pack .color.l7 -in .color.f7 -side left -padx 1m -pady 1m
    }
}

.mbar.options.menu add command -underline 0 -label "Fonts..." -command {
    if {[winfo exists .font]} {
    wm deiconify .font
    raise .font
    } else {
    toplevel .font
    wm title .font "Font Selection"
    frame .font.mbar -relief raise -bd 2
    menubutton .font.mbar.window -text Window -underline 0 -menu .font.mbar.window.menu
    pack .font.mbar.window -side left
    tk_menuBar .font.mbar .font.mbar.window

# window menu

    menu .font.mbar.window.menu -tearoff 0 -postcommand {propagate_options .font.mbar.window.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

	.font.mbar.window.menu add command -underline 0 -label Close -command {if {[winfo exists .font]} {destroy .font}}
    frame .font.frame1
    frame .font.frame2
    frame .font.frame3
    frame .font.frame4
    frame .font.frame5
    frame .font.frame6
    frame .font.frame7
    button .font.button -relief groove -bd 4 -text "Apply" -command {
        set pnfont "*-$pnfontfamily-$pnfontweight-$pnfontslant-$pnfontwidth-*-$pnfontsize-*"
        set wordfont "*-$pnfontfamily-$pnfontweight-$pnfontslant-$pnfontwidth-*-$wordfontsize-*"
        set indexfont "-*-helvetica-medium-o-*-*-$indexsize-*"
        prolog redraw_label
        prolog redraw_db
}
    label .font.l1 -width 5 -anchor w -text "Family"
    label .font.l2 -width 5 -anchor w -text "Weight"
    label .font.l3 -width 5 -anchor w -text "Slant"
    label .font.l4 -width 5 -anchor w -text "Width"
    label .font.l5 -width 5 -anchor w -text "Size0"
    label .font.l6 -width 5 -anchor w -text "Size1"
    label .font.l7 -width 5 -anchor w -text "Size2"
    entry .font.e1 -width 12 -textvariable pnfontfamily
    entry .font.e2 -width 12 -textvariable pnfontweight
    entry .font.e3 -width 12 -textvariable pnfontslant
    entry .font.e4 -width 12 -textvariable pnfontwidth
    entry .font.e5 -width 12 -textvariable indexsize
    entry .font.e6 -width 12 -textvariable wordfontsize
    entry .font.e7 -width 12 -textvariable pnfontsize
    pack .font.mbar -side top -fill x
    pack .font.frame1 .font.frame2 .font.frame3 .font.frame4 .font.frame5 .font.frame6 .font.frame7 .font.button -side top
    pack .font.l1 -in .font.frame1 -anchor w -side left -padx 1m -pady 2m
    pack .font.e1 -in .font.frame1 -side left -fill x -expand 1 -padx 1m -pady 2m
    pack .font.l2 -in .font.frame2 -anchor w -side left -padx 1m -pady 2m
    pack .font.e2 -in .font.frame2 -side left -fill x -expand 1 -padx 1m -pady 2m
    pack .font.l3 -in .font.frame3 -anchor w -side left -padx 1m -pady 2m
    pack .font.e3 -in .font.frame3 -side left -fill x -expand 1 -padx 1m -pady 2m
    pack .font.l4 -in .font.frame4 -anchor w -side left -padx 1m -pady 2m
    pack .font.e4 -in .font.frame4 -side left -fill x -expand 1 -padx 1m -pady 2m
    pack .font.l5 -in .font.frame5 -anchor w -side left -padx 1m -pady 2m
    pack .font.e5 -in .font.frame5 -side left -fill x -expand 1 -padx 1m -pady 2m
    pack .font.l6 -in .font.frame6 -anchor w -side left -padx 1m -pady 2m
    pack .font.e6 -in .font.frame6 -side left -fill x -expand 1 -padx 1m -pady 2m
    pack .font.l7 -in .font.frame7 -anchor w -side left -padx 1m -pady 2m
    pack .font.e7 -in .font.frame7 -side left -fill x -expand 1 -padx 1m -pady 2m
    }
}

.mbar.options.menu add separator

.mbar.options.menu add command -underline 2 -label "Save Current Options" \
	-command {prolog save_options}

.mbar.options.menu add separator

.mbar.options.menu add command -underline 8 -label "Restore Default Options" \
	-command {prolog default_options
                  if {[prolog color_options]} {
                      update_options $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc
	          }
	      }
# menu bar options/Prolog Messages

menu .mbar.options.menu.pm -tearoff 0 -postcommand {propagate_options .mbar.options.menu.pm $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.options.menu.pm add radiobutton -label Quiet -variable prologmessages -value "quiet"
.mbar.options.menu.pm add radiobutton -label Verbose -variable prologmessages -value "verbose"

# menu bar options/LaTeX output
    
menu .mbar.options.menu.lo -tearoff 0 -postcommand {propagate_options .mbar.options.menu.lo $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.options.menu.lo add radiobutton -label Prawitz \
      -variable latexout -value nd
.mbar.options.menu.lo add radiobutton -label Fitch \
      -variable latexout -value fitch

# menu bar options/View Format
    
menu .mbar.options.menu.vf -tearoff 0 -postcommand {propagate_options .mbar.options.menu.lo $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.options.menu.vf add radiobutton -label None \
	-variable latexfmt -value none -command {
             .mbar.options.menu entryconfigure 3 -state disabled
             .mbar.options.menu entryconfigure 5 -state disabled
             .mbar.options.menu entryconfigure 6 -state disabled
             .mbar.options.menu entryconfigure 7 -state disabled
             .mbar.options.menu entryconfigure 8 -state disabled
             .mbar.options.menu entryconfigure 9 -state disabled
             .input.latex configure -state disabled
             .input.xdvi configure -state disabled
             set ndformat $latexout
             set latexout none}
.mbar.options.menu.vf add radiobutton -label Dvi \
	-variable latexfmt -value dvi -command {
             .mbar.options.menu entryconfigure 3 -state normal
             .mbar.options.menu entryconfigure 5 -state normal
             .mbar.options.menu entryconfigure 6 -state normal
             .mbar.options.menu entryconfigure 7 -state normal
             .mbar.options.menu entryconfigure 8 -state normal
             .mbar.options.menu entryconfigure 9 -state normal
             .input.latex configure -state normal
             .input.xdvi configure -state normal
             if {$latexout == "none"} {
               if [catch set $ndformat] {set latexout nd
	     } else {set latexout $ndformat}}
             prolog set_dvi_output}
.mbar.options.menu.vf add radiobutton -label Postscript \
	-variable latexfmt -value ps -command {
             .mbar.options.menu entryconfigure 3 -state disabled
             .mbar.options.menu entryconfigure 5 -state normal
             .mbar.options.menu entryconfigure 6 -state normal
             .mbar.options.menu entryconfigure 7 -state normal
             .mbar.options.menu entryconfigure 8 -state normal
             .mbar.options.menu entryconfigure 9 -state normal
             .input.latex configure -state normal
             .input.xdvi configure -state normal
             if {$latexout == "none"} {
               if [catch set $ndformat] {set latexout nd
	     } else {set latexout $ndformat}}
             prolog set_ps_output}
.mbar.options.menu.vf add radiobutton -label Pdf \
	-variable latexfmt -value pdf -command {
             .mbar.options.menu entryconfigure 3 -state normal
             .mbar.options.menu entryconfigure 5 -state normal
             .mbar.options.menu entryconfigure 6 -state normal
             .mbar.options.menu entryconfigure 7 -state normal
             .mbar.options.menu entryconfigure 8 -state normal
             .mbar.options.menu entryconfigure 9 -state normal
             .input.latex configure -state normal
             .input.xdvi configure -state normal
             if {$latexout == "none"} {
               if [catch set $ndformat] {set latexout nd
	     } else {set latexout $ndformat}}
             prolog set_pdf_output}

# menu bar options/Xdvi size

menu .mbar.options.menu.xs -tearoff 0 -postcommand {propagate_options .mbar.options.menu.lo $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.options.menu.xs add radiobutton -label "320x200" \
      -variable xdvisize -value "320x200"
.mbar.options.menu.xs add radiobutton -label "640x400" \
      -variable xdvisize -value "640x400"
.mbar.options.menu.xs add radiobutton -label "800x600" \
      -variable xdvisize -value "800x600"
.mbar.options.menu.xs add radiobutton -label "1024x800" \
      -variable xdvisize -value "1024x800"
.mbar.options.menu.xs add radiobutton -label "1280x1024" \
      -variable xdvisize -value "1280x1024"

# menu bar options/Proofs

menu .mbar.options.menu.proofs -tearoff 0 -postcommand {propagate_options .mbar.options.menu.proofs $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.options.menu.proofs add checkbutton -label "Eta Long Proofs" -variable etalongproofs
.mbar.options.menu.proofs add checkbutton -label "Hypothesis Scope" -variable hyposcope
.mbar.options.menu.proofs add checkbutton -label "Compact Lexical Entries" -variable compactlex

# menu bar options/Labels
    
menu .mbar.options.menu.lab -tearoff 0 -postcommand {propagate_options .mbar.options.menu.lab $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}
    
.mbar.options.menu.lab add checkbutton -label "Output Labels" -variable outputlabels
.mbar.options.menu.lab add separator
.mbar.options.menu.lab add radiobutton -label "Implicit Structural Rules" -variable sr -value implicit
.mbar.options.menu.lab add radiobutton -label "Collapsed Structural Rules" -variable sr -value collapsed
.mbar.options.menu.lab add radiobutton -label "Explicit Structural Rules" -variable sr -value explicit

# menu bar options/Formulas
    
menu .mbar.options.menu.for -tearoff 0 -postcommand {propagate_options .mbar.options.menu.for $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.options.menu.for add checkbutton -label "Reduce Macros" -variable macroreduce

# menu bar options/Semantics

menu .mbar.options.menu.sem -tearoff 0 -postcommand {propagate_options .mbar.options.menu.sem $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.options.menu.sem add checkbutton -label "Output Semantics" -variable outputsem
.mbar.options.menu.sem add separator
.mbar.options.menu.sem add radiobutton -label "Functional Notation: ((f y) x)" -variable bracketsem -value 1
.mbar.options.menu.sem add radiobutton -label "Predicate Notation : f(x,y)" -variable bracketsem -value 0
.mbar.options.menu.sem add separator
.mbar.options.menu.sem add checkbutton -label "Reduce Semantics" -variable reducesem
.mbar.options.menu.sem add checkbutton -label "Substitute Lexical Semantics" -variable substlexsem

.mbar.options.menu.sem add separator
.mbar.options.menu.sem add checkbutton -label "Semantics For Unary Connectives" -variable unarysem

# menu bar window
    
menu .mbar.window.menu -tearoff 0 -postcommand {propagate_options .mbar.window.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.window.menu add command -label "Status Window"\
       -underline 0 -accelerator "(C-s)" -command {if {[winfo exists .stats]} {
                    raise .stats
                    wm deiconify .stats
                    } else {
                    prolog create_proofnet_window
                    }}

.mbar.window.menu add command -label "Rewrite Window"\
       -state disabled -underline 0 -accelerator "(C-r)" -command {if {[winfo exists .rewrite]} {
                    raise .rewrite
                    wm deiconify .rewrite
                    } else {
                    prolog create_rewrite_window
                    }}

.mbar.window.menu add command -label "Lexicon Window"\
       -underline 0 -accelerator "(C-l)" -command {if {[winfo exists .lex]} {
                    raise .lex
                    wm deiconify .lex
                    } else {
                    prolog create_lexicon_window
                    }}
.mbar.window.menu add command -label "Postulate Window"\
       -underline 0 -accelerator "(C-p)" -command {if {[winfo exists .post]} {
                    raise .post
                    wm deiconify .post
                    } else {
                    prolog create_postulate_window
                    }}    
.mbar.window.menu add command -label "Analysis Window"\
       -underline 0 -accelerator "(C-a)" -command {if {[winfo exists .an]} {
                    raise .an
                    wm deiconify .an
                    } else {
                    prolog create_analysis_window
                }   } 

# menu bar help
    
menu .mbar.help.menu -tearoff 0 -postcommand {propagate_options .mbar.help.menu $grail_bg $grail_fg $grail_abg $grail_dfg $grail_tc $grail_sc}

.mbar.help.menu add command -label "On This Window" -underline 8 -command {if {[winfo exists .help]} {raise .help} else {prolog no_help}}

frame .input
frame .input.top
frame .input.mid
frame .input.bot

listbox .input.sent -width 40 -relief sunken -bd 2 -yscrollcommand ".input.scroll set"
scrollbar .input.scroll -command ".input.sent yview"

button .input.parse -text "Parse" -relief groove -borderwidth 4 -command {prolog parse_new("$inputtxt","$inputform")}
button .input.latex -text "LaTeX" -relief groove -borderwidth 4 -command "prolog retex"
button .input.xdvi -text "View" -relief groove -borderwidth 4 -command "prolog xdvi"
button .input.exit -text "Exit" -relief groove \
    -borderwidth 4 -command "prolog confirm_exit"

label .input.label -text "Words:"
entry .input.entry -width 30 -relief sunken -bd 2 -textvariable inputtxt
label .input.label2 -text "Formula:"
entry .input.form -width 3 -relief sunken -bd 2 -textvariable inputform

pack .input.label -side left -padx 1m -pady 2m -in .input.mid
pack .input.entry -side left -padx 1m \
    -pady 2m -fill x -expand 1 -in .input.mid
pack .input.label2 -side left -padx 1m -pady 2m -in .input.mid
pack .input.form -side left -padx 1m \
    -pady 2m -fill x -expand 1 -in .input.mid
pack .input.scroll -side right -fill y -in .input.top
pack .input.sent -expand 1 -fill both -in .input.top  
pack .input.parse .input.latex .input.xdvi .input.exit -side left -expand 1 -fill x -in .input.bot

# -----------

bind .input.entry <Return> {prolog parse_new("$inputtxt","$inputform")}
bind .input.form <Return> {prolog parse_new("$inputtxt","$inputform")}
bind .input.sent <Double-Button-1> {
       if {[winfo exists %W]} {
       set index [%W index @%x,%y]
       ::tk::ListboxBeginSelect %W $index
       prolog tex_memo($index)
       }   }
bind .input.sent <Button-1> { 
       if {[winfo exists %W]} {
       set index [%W index @%x,%y]
       ::tk::ListboxBeginSelect %W $index
       prolog get_memo($index)
       }   }
bind .input.sent <Control-Button-1> {
       if {[winfo exists %W]} {
       set index [%W index @%x,%y]
	   prolog delete_sentence($index)}}
bind .input.sent <Control-Button-3> {
     prolog xdvi_sent
}
bind .input.sent <Control-Button-2> {
     prolog all_examples
}

pack  .input.top -expand 1 -fill both
pack  .input.mid -fill x
pack  .input.bot

pack .mbar -side top -fill x
pack .input -expand 1 -fill both
pack .bot

focus .input.entry

. config -cursor left_ptr
