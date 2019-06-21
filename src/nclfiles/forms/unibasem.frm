#HEADER#
/TITLE/ Unibase Modals
/POSITION/ 50,50
/SIZE/155,140

#CHOICEBOX#
/LABEL/ Save Display List:
/POSITION/ 10,8,105,8
/SIZE/135,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#      
/LABEL/ Save Tessellation List:
/POSITION/ 10,25,105,25
/SIZE/135,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#
/LABEL/ Restore Light Settings:
/POSITION/ 10,42,105,42
/SIZE/135,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#
/LABEL/ Restore Material Settings:
/POSITION/ 10,59,105,59
/SIZE/135,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#
/LABEL/ Restore Colors:
/POSITION/ 10,76,105,76
/SIZE/135,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes", "Merge"

#CHOICEBOX#
/LABEL/ Restore Units:
/POSITION/ 10,93,105,93
/SIZE/135,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#HELP#
===================
Unibase Modals form
===================
This form enables or disables the option for saving the surface variable lists
for displaying wireframe or shaded surfaces. The form also enables and disables
the option to restore the original light and material settings after loading 
external settings.

Save Display List:
------------------
Enabling this option stores the list of points and segments used to display 
the wireframe model of a surface in a saved Unibase. By enabling this option
the Unibase becomes larger in size, however loading it is faster. By disabling
this option the Unibase becomes smaller in size, however loading it is slower.

Save Tessellation List:
-----------------------
Enabling this option stores the list of vertices and normals for shaded
surfaces in a saved Unibase. By enabling this option the Unibase becomes larger 
in size, however loading it is faster. By disabling this option the Unibase
becomes smaller in size, however loading it is is slower. 

Restore Light Settings:
-----------------------
If this option is enabled the original light settings are not overwritten by 
the Unibase light settings when it is loaded, rather the light settings in
effect prior to loading the Unibase will be retained.

Restore Material Settings:
--------------------------
If this option is enabled the original material settings are not overwritten by
the Unibase material settings when it is loaded, rather the material settings
in effect prior to loading the Unibase will be retained.

Restore Colors:
---------------
If this option is set to No, then the colors defined in the Unibase are used.
The colors defined in the ncl_color.mod file are ignored.  If any changes are
made to the Color Dialog form, then the new color map will be saved to the
ncl_color.mod file, overwriting the previous default color definitions.

If this option is set to Yes, the colors defined in the ncl_color.mod file are
used.  The colors defined in the Unibase are ignored.  This means that the
color representation of the Unibase geometry may differ from their original
color settings.

If this option is set to Merge, the colors defined in the Unibase are first
stored and then the colors defined in the ncl_color.mod file are loaded.
Matching colors (RGB values) will be merged into a single color using the
Unibase color name and any unique colors from the ncl_color.mod file will be
stored until the maximum number of colors is reached.  If the user makes
changes to the Color Dialog form, then the new color map will be saved to the
ncl_color.mod file, overwriting the previous default color definitions.

Restore Units:
--------------
Enable this option if you would like NCL to maintain the Units setting (Inches/
MM) that was in effect prior to loading a Unibase using the interface.  If you
want NCL to take on the Units of the Unibase being loaded, then enter NO at this
prompt.
