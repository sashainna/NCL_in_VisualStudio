#HELP#
============
Tool Display
============
This form is used to define the cutter, shank, and holder display parameters.
It can be activated either from the Cutter Definition form or from the NCLIPV
Edit Tool List form or ctrl/F10.

========
Selected
========
Displays the currently selected Profile or Symbol.

Apply:
------
Applies the currently Selected Profile or Symbol.

Cancel:
-------
Cancels the current Profile or Symbol operation.

========
Profiles
========
The Profiles List contains a list of all profiles contained in the NCL_TOOL_DESC
library that pass the Class selection.  Selecting one of these profiles will
automatically update the Selected field with the profile name.

Class:
------
The Class field allows you to diplay only the profiles that belong to the same
class as selected in this field.  Selecting 'All' will display all profiles
defined in the library. Other available Classes are CUTTER, Cutters, Drills,
Holders, MILL_HOLDER, SHANK and Shanks.

=======
Symbols
=======
The Symbols List contains a list of all defined CADD Symbols.  Selecting one of
these symbols will automatically update the Selected field with the symbol name.
Only Symbols with a single Surface of Revolution can be used for tool display.
All Symbols will have the Path C:\NCCS\NCL102\symbols\Holders_S at the beginning
All previously Selected Symbols will be marked with an Asterisk.
