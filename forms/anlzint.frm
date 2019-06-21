#HEADER#
/TITLE/ Interpolation Analyzation
/POSITION/ 0,0
/SIZE/ 180,160

#COLOR#      
/LABEL/ Geometry:  
/POSITION/ 10,25,50,25
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/
/POSITION/ 110,25
/SIZE/ 60,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Solid", "Small Dash", "Dots", "Center Line", "Phantom"
/CHOICES/ "Large Dash", "Dash Dot", "Dash Space"

#COLOR#      
/LABEL/ Linear:    
/POSITION/ 10,42,50,42
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/
/POSITION/ 110,42
/SIZE/ 60,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Solid", "Small Dash", "Dots", "Center Line", "Phantom"
/CHOICES/ "Large Dash", "Dash Dot", "Dash Space"

#COLOR#      
/LABEL/ Circular:  
/POSITION/ 10,59,50,59
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/
/POSITION/ 110,59
/SIZE/ 60,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Solid", "Small Dash", "Dots", "Center Line", "Phantom"
/CHOICES/ "Large Dash", "Dash Dot", "Dash Space"

#COLOR#      
/LABEL/ Rapid:     
/POSITION/ 10,76,50,76
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/
/POSITION/ 110,76
/SIZE/ 60,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Solid", "Small Dash", "Dots", "Center Line", "Phantom"
/CHOICES/ "Large Dash", "Dash Dot", "Dash Space"

#COLOR#      
/LABEL/ Cycle:     
/POSITION/ 10,93,50,93
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/
/POSITION/ 110,93
/SIZE/ 60,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Solid", "Small Dash", "Dots", "Center Line", "Phantom"
/CHOICES/ "Large Dash", "Dash Dot", "Dash Space"

#COLOR#      
/LABEL/ Tlaxis Chg:
/POSITION/ 10,110,50,110
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default 

#CHOICEBOX#
/LABEL/
/POSITION/ 110,110
/SIZE/ 60,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Solid", "Small Dash", "Dots", "Center Line", "Phantom"
/CHOICES/ "Large Dash", "Dash Dot", "Dash Space"

#LABEL#
/LABEL/ Color
/POSITION/ 60,8
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ Style
/POSITION/ 130,8
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#HELP#
==============================
Interpolation Analyzation Form
==============================
The Interpolation Analyzation form sets the colors and line types to display
various motion types in when playing back motion in NCL with Analyzation set
to Interpolation. 

NCLIPV will also use the color scheme defined in this form for displaying cuts
when Analyzation is set to Interpolation during NCLIPV motion playback.  The
geometry attributes and line styles are ignored in NCLIPV.

The 'Default' settings will use the default geometry/motion colors/line-styles
for the geometry or specified type of motion.  'Black' will display the
motion as invisible.

Geometry:  
---------
Defines the color and line style attributes to display the model with during
motion playback.  All geometry in the model will be displayed using these
attributes during motion playback.  The original attributes for each geometry
will be restored when motion playback is exited.

Selecting any other color besides Default for geometry will display all
geometry in that color.  Shaded geometry will be transparent, in that the
motion displayed behind the geometry will show through the geometry along with
any other geometry that is defined.

Linear:    
-------
Defines the color and line style attributes to display fixed tool axis moves
during motion playback.

Circular:  
---------
Defines the color and line style attributes to display circular moves during
motion playback.

Rapid:     
------
Defines the color and line style attributes to display rapid moves during
motion playback.

Cycle:     
------
Defines the color and line style attributes to display canned cycle moves during
motion playback.

Tlaxis Chg:
-----------
Defines the color and line style attributes to display changing tool axis moves
during motion playback.
