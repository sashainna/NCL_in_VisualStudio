#HEADER#
/TITLE/ Material Properties
/POSITION/ 0,0
/SIZE/ 240,120

#CHOICEBOX#      
/LABEL/ Material:
/POSITION/ 10,8,65,8
/SIZE/ 97,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Type 1", "Type 2", "Type 3", "Type 4", "Type 5", "Type 6"
/CHOICES/ "Type 7", "Type 8", "Type 9", "Type 10", "Type 11", "Type 12"
/CHOICES/ "Type 13", "Type 14", "Type 15", "Type 16"

#EDIT#
/LABEL/ Name:
/POSITION/ 115,8,165,8
/SIZE/ 45,14
/TYPE/ UD_DASSTRING
/PREC/ 12
/LEN/ 12

#EDIT#
/LABEL/ Ambient Factor:
/POSITION/ 10,25,65,25
/SIZE/ 50,14
/TYPE/ UD_DASUNITLESS
/RANGE/0.,1.
/PREC/ 3
/LEN/ 6

#EDIT#
/LABEL/ Diffuse Factor:
/POSITION/ 115,25,165,25
/SIZE/ 50,14
/TYPE/ UD_DASUNITLESS
/RANGE/0.,1.
/PREC/ 3
/LEN/ 6

#EDIT#
/LABEL/ Specular Factor:
/POSITION/ 10,42,65,42
/SIZE/ 50,14
/TYPE/ UD_DASUNITLESS
/RANGE/0.,1.
/PREC/ 3
/LEN/ 6

#EDIT#
/LABEL/ Exponent:
/POSITION/ 115,42,165,42
/SIZE/ 50,14
/TYPE/ UD_DASINT
/RANGE/0,128
/PREC/ 3
/LEN/ 3


#EDIT#
/LABEL/ Specular Color:R
/POSITION/ 10,59,65,59
/SIZE/ 50,14
/TYPE/ UD_DASUNITLESS
/RANGE/0.,1.
/PREC/ 3
/LEN/ 6

#EDIT#
/LABEL/ G 
/POSITION/ 115,59
/SIZE/ 40,14
/TYPE/ UD_DASUNITLESS
/RANGE/0.,1.
/PREC/ 3
/LEN/ 6

#EDIT#
/LABEL/ B 
/POSITION/ 165,59
/SIZE/ 40,14
/TYPE/ UD_DASUNITLESS
/RANGE/0.,1.
/PREC/ 3
/LEN/ 6

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 90,76
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#HELP#
========================
Material Properties Form
========================
This form is used to define the material properties that are assigned to
surfaces for lighting purposes.  Each surface is assigned one of the
sixteen available materials.  The material properties defined in this form
determine how the surface looks when it is shaded.  Accepting this form will
save the material settings in the modals file 'ncl_material.mod', so that the
next time NCL is started, these settings will be used as the defaults.

Material
--------
This toggle field allows for up to 16 material types to be defined by the user.

Name
----
Contains the name of the material currently being defined.  The name of the
material is defined by the user.

Ambient Factor
--------------
Determines the ambience or glow of the material.  This is the light that emits
from the surface even if the physical light does not shine exactly on it.  This
value can be from 0 to 1 and is a percentage of the surface color, so that the
ambience color is always a shade of the surface color.

Diffuse Factor
--------------
Determines the diffuse property of the material, which is the light that is
reflected off of the material by the active lights.  Setting a high diffuse
factor and a low ambient factor will result in heavily shaded surfaces similar
to the real world, but not always appropriate for the screen.  This value can
be from 0 to 1 and is a percentage of the surface color, so that the diffuse
color is always a shade of the surface color.

Specular Factor
---------------
Defines the specular property of the material, which produces highlights on the
surface when directly hit by a light source.  This value can be from 0 to 1 and
is a percentage of the specular color.

Exponent
--------
The Exponent value determines the size and brightness of the highlight created
by the specular property.  The higher the exponent, the smaller and brighter
the highlights.  The exponent can be in the range of 0 to 128.

Specular Color
--------------
Defines the highlight color for the specular property.

Select
------
The Select button allows you to select surfaces that will be changed to the
material currently being defined.  This is an Action button, the surfaces
selected will inherit this material even if the form is cancelled.
