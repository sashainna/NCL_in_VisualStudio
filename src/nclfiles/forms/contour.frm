#HEADER#
/TITLE/ Flank Contouring
/POSITION/ 50,50
/SIZE/290,140

#PUSHBUTTON#
/LABEL/ Part Surface
/POSITION/ 10,8
/SIZE/70,15
/TYPE/ UD_DASSTRING

#CHOICEBOX#      
/LABEL/
/POSITION/ 90,8
/SIZE/40,140
/TYPE/ UD_DASSTRING
/CHOICES/ "Tlofps", "Tlonps"

#COLOR#      
/LABEL/
/POSITION/ 135,8
/SIZE/50,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 200,8
/SIZE/65,15
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Drive Surface
/POSITION/ 10,25
/SIZE/70,15
/TYPE/ UD_DASSTRING

#CHOICEBOX#      
/LABEL/
/POSITION/ 90,25
/SIZE/40,140
/TYPE/ UD_DASSTRING
/CHOICES/ "Tllft","Tlon","Tlrgt"

#COLOR#      
/LABEL/
/POSITION/ 135,25
/SIZE/50,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 200,25
/SIZE/65,15
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Check Surface
/POSITION/ 10,42
/SIZE/70,15
/TYPE/ UD_DASSTRING

#CHOICEBOX#      
/LABEL/
/POSITION/ 90,42
/SIZE/40,140
/TYPE/ UD_DASSTRING
/CHOICES/ "To", "On", "Past", "Tanto", "Pstan",

#COLOR#      
/LABEL/
/POSITION/ 135,42
/SIZE/50,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 200,42
/SIZE/65,15
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Use Initial DS as Final DS
/POSITION/ 10,59
/SIZE/100,15
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ Extensions:
/POSITION/ 10,76
/SIZE/90,140
/TYPE/UD_DASSTRING
/CHOICES/ "Ignore","Respect"

#CHOICEBOX#
/LABEL/ Look Ahead:
/POSITION/ 110,76
/SIZE/75,140
/TYPE/UD_DASSTRING
/CHOICES/ "1","2","3","4","5"

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 10,93
/SIZE/40,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 60,93
/SIZE/40,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Erase Motion
/POSITION/ 110,93
/SIZE/60,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Display Cutter
/POSITION/ 180,93
/SIZE/60,15
/TYPE/UD_DASSTRING

#HELP#
================
Flank Contouring
================

The Flank Contouring form is used to flank contour multiple drive surfaces.  It
allows you to select the drive surfaces, part surface(s), and check surface(s)
for machining.

Part Surface
------------
Press this button to select the part surface.  Multiple surfaces can be
selected for the part surface.  The tool can either be positioned to the part
surface (Tlofps) or with the tool center always on the Part Surface (Tlonps).

The color choice field specifies which color to use to highlight the part
surface(s).  The Deselect All button will deselect all currently selected
part surfaces.

Drive Surface
-------------
Press this button to select the surfaces to machine.  These surfaces must be
selected in the order in which you would like them machined.  You can use the
Surface Chain method for selecting the surfaces.  The tool can be positioned
to the left of the drive surfaces (Tllft), on the drive surfaces (Tlon), or
to the right in reference to the current forward direction of the tool.

The color choice field specifies which color to use to highlight the drive
surfaces.  The Deselect All button will deselect all currently selected
drive surfaces.

Check Surface
-------------
Press this button to select the final check surface.  Up to five check surfaces 
can be selected.  The tool position for the final check surface can also be
chosen.  The tool conditions for intermediate drive/check surfaces are
automatically calculated by the Contour routine.

The color choice field specifies which color to use to highlight the check
surface(s).  The Deselect All button will deselect all currently selected
check surfaces.

Use Initial DS as Final DS
--------------------------
It is sometimes required that the initial drive surface be used as the last
drive surface, for example when machining completely around a part and starting
in the middle of a surface.

Since NCL does not allow the same geometry to be selected twice during a
Selection operation and the nature of this form is to allow a geometry that is
selected a second time (during a separate selection process) to be deselected,
enabling this field will automatically use the first selected drive surface as
the last drive surface.  If the initial drive surface is to be used as the
final check surface, then do not check this box, but rather select this surface
as the check surface also.

Extensions
----------
The drive surface extensions that are checked to by the tool can either be
Ignored or Respected.  It is typical to ignore the surface extensions when
driving connected drive surfaces.

Look Ahead
----------
You can tell the tool to look ahead up to five surfaces when driving each
surface.  This feature is useful when the tool may be too large to fit in some
areas, so it will attempt to check to multiple surfaces, checking to the surface
that it reaches first.

==============
Action Buttons
==============

View
----
Enters dynamic viewing mode.

Preview
-------
Previews the Contour motion.  To save the generated motion and command do not
make any changes to the settings prior to pressing the OK button.  Pressing
the Preview or OK button after a change will overwrite the current motion
and command.  The generated command can be saved after the preview even if
there is an error.  To save the command, make no changes to the settings
and press the OK button.  The command will then be available for editing in
the command line.

Erase Motion
------------
Erases all motion.

Display Cutter
--------------
Displays the current cutter position.
