# Experimental formalization of Oak + information flow theorems

The goal of this work is to rigorously ensure that the design of the Oak runtime
securely enforces information flow as intended. Noninterference is a standard
way of formalizing secure information flow. It says that there is no way for
secret values to influence publicly observable values, even indirectly. Dually,
it ensures that untrusted values cannot influence trusted state. An advantage of
security specifications based on noninterference is that security is defined in
terms of observable behaviors rather than intimate design details: as a result,
noninterference specifications may rule out potential information leaks that
system designers would not have anticipated.

This work is experimental and ongoing. At present, the main security definition
is in vfiles/Possibilistic_NI.v and the the top-level file for the runtime model
is in vfiles/RuntimeModel.v. Both are evolving.

Instructions to re-generate makefile: (see also:
[the instructions for CoqProject](https://coq.inria.fr/refman/practical-tools/utilities.html))
coq_makefile -f \_CoqProject -o Makefile