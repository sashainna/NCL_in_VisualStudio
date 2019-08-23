#HEADER#
/TITLE/ NCLIPV Undo Stack Modals
/POSITION/ 0,0
/SIZE/ 195,70

#CHECKBOX#      
/LABEL/ Enable Undo Stack
/POSITION/ 10,8
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Stack Size:
/POSITION/ 105,8
/SIZE/ 100,14
/TYPE/ UD_DASINT
/LEN/ 7
/PREC/ 0
/RANGE/ 0,1000000

#CHECKBOX#      
/LABEL/ Allow Undo Of Fixture Cuts
/POSITION/ 10,25
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#HELP#
========================
NCLIPV Undo Stack Modals
========================
This enables/disables the ability to interactively Undo and Redo cuts in an
NCLIPV session.

Enable Undo Stack
-----------------
Checking this box enables the Undo abiltity of NCLIPV.  The material removed
during cuts will be saved along with the tool definition and position so that
it can be added back to the stock when stepping backwards through the
simulation.  Enabling the Undo Stack will cause the NCLIPV simulation to run
about 10-15 percent slower than when it is disabled and will also use more
memory (the amount of memory used is dependent on the complexity of the cuts
and the number of cuts stored).

Stack Size:
-----------
Defines the maximum number of cuts that will be stored in the Undo stack.  You
will only be able to undo this number of cuts.  A value of 0 defines an
unlimited Undo stack.  Limiting the size of the Undo stack helps to ensure that
you will not run out of memory.

Allow Undo Of Fixture Cuts
--------------------------
Check this box if you want the cuts made to fixtures to be stored on the Undo
stack.  Allowing the Undo of fixture cuts will slow the simulation process even
more, especially if there are a lot of defined fixtures, even if none of the
fixtures are cut.  Storing fixture cuts will also use up more memory.
