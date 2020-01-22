
# The Grail 2 Theorem Prover

The Grail system is a tool for the development and prototyping of grammar fragments for categorial logics. Grail is an automated theorem prover based on proof nets, a graph-based representation of proofs, and labeled deduction.

The [Grail 2](https://github.com/RichardMoot/Grail2) theorem prover is implemented in SICStus Prolog, the user interface in TclTk. Though I would like to continue to provide support for Grail 2, I do not have a SICStus license. Please contact me when you encounter any problems.

[Grail 0](https://github.com/RichardMoot/Grail0), a reduced version of the Grail 2 parser, without the user interface, can be found here. Grail 0 is recommended for users primarily interested in the natural deduction proofs generated by Grail.

For all other users, a next generation Grail theorem prover, [Grail 3](https://github.com/RichardMoot/Grail), has replaced Grail 2 as the current, stable and supported version of Grail. Grail 3 has a legacy mode which allows you to used your old Grail 2 grammars without any changes. Grail 3 does not support natural deduction output.

# Requirements

The current distribution was last modified at 28 October 2015 and
has been verified to work with SICStus Prolog 4.3.2 and TclTk 8.5. A version of
pdflatex (or other LaTeX version) is required for the LaTeX output. This is a rather
extensive update of the source code developed for my PhD thesis, though most changes are compatibility issues (the previous release is over fifteen years old and ran on SICStus 3.5 and TclTk 7.6/4.2). There are probably still many robustness issues.

# Getting started

You can start Grail when you have downloaded the repository by entering the repository directory and typing

```
sicstus -l grail
```

this starts the TclTk user interface.

Read the [manual](http://www.labri.fr/perso/moot/man.html) for further information.

The subdirectory `fragments` contains a number of example grammars to play around with.
