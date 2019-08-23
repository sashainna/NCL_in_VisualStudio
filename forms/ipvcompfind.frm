#HEADER#
/TITLE/ NCLIPV Gouge Finder
/POSITION/ 50,50
/SIZE/260,50

#PUSHBUTTON#
/LABEL/ Fit
/POSITION/ 10,8
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ <<
/POSITION/ 60,8
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ <
/POSITION/ 85,8
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 115,8
/SIZE/ 30,14
/TYPE/ UD_DASINT
/PREC/ 6
/LEN/ 6

#PUSHBUTTON#
/LABEL/ >
/POSITION/ 155,8
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ >>
/POSITION/ 180,8
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 205,8
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#HELP#
===================
NCLIPV Gouge Finder
===================
After performing an Undercuts or Overcuts style comparison, this form can be
used to bring each undercut/overcut into view.

Fit
---
Fits all stocks and fixtures into the NCLIPV window.

<<
--
Fits the first undercut/overcut into the NCLIPV window.

<
-
Fits the previous undercut/overcut into NCLIPV window.

Text Field
----------
References the undercut/overcut currently displayed in the NCLIPV window.  This
may not always be true though, if any viewing has been done since the last
undercut/overcut was displayed or when this form is first displayed.

>
-
Fits the next undercut/overcut into the NCLIPV window.

>>
--
Fits the last undercut/overcut into the NCLIPV window.

View
----
Enters Dynamic Viewing mode.
