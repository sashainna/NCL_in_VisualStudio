#HEADER#
/TITLE/ Create Composite Solid
/POSITION/ 0,0
/SIZE/ 280,90

#CHOICEBOX#
/LABEL/ 
/POSITION/ 10,8,10,8
/SIZE/ 66,48
/TYPE/UD_DASSTRING
/CHOICES/ "Surfaces","Solids","All Surfaces"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 85,8
/SIZE/ 44,14
/TYPE/UD_DASSTRING

#COLOR#
/LABEL/ 
/POSITION/ 145,8
/SIZE/ 65,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 220,8
/SIZE/ 50,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Mark Solid as Closed
/POSITION/ 9,30
/SIZE/ 90,13
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ Input Entities:
/POSITION/ 122,30
/SIZE/ 118,38
/TYPE/UD_DASSTRING
/CHOICES/ "Invisible","Keep","Remove"

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 110,47
/SIZE/ 50,14
/TYPE/UD_DASSTRING

#HELP#
======================
Create Composite Solid
======================
This form is used to create a composite solid from a set of surfaces or visual
solids.

Geometry Type
-------------
You can opt to select surfaces, solids, or all defined surfaces as the
components of the composite solid.  Selecting 'Surface' requires you to select
the surfaces from the screen.  'Solid' allows you to select visual solids,
and 'All Surfaces' automatically selects all defined surfaces.

Select
------
Used to select the geometry to create the composite solid with.  You may select
either surfaces or solids depending on the setting of the Geometry Type field.

Color
-----
Chooses a color to highlight the selected solids/surfaces.

Deselect All
------------
Deselects all of the currently selected solids/surfaces.

Mark Solid as Closed
--------------------
Check this box if the surfaces selected to build the composite solid will 
create a closed solid.  This flag is typically only used when exporting
Geometry using the STEP protocol.


Input Entities:
---------------
This field handles how the input surfaces/solids are handled after they are
used to build the composite solid.  They can be made invisible (Invisible),
left alone with no changes (Keep), or removed from the Unibase (Remove).

Apply
-----
Creates the composite solid without taking down the form so that other
composite solids can be defined.
