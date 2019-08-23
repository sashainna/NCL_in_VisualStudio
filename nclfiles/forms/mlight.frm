#HEADER#
/TITLE/ Light Sources
/POSITION/ 0,0
/SIZE/ 280,125

#CHOICEBOX#
/LABEL/ Light:
/POSITION/ 10,8
/SIZE/ 68,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Light #1", "Light #2", "Light #3", "Light #4", "Light #5"

#CHOICEBOX#
/LABEL/ Active:
/POSITION/ 90,8
/SIZE/ 60,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#
/LABEL/ Type:
/POSITION/ 175,8
/SIZE/ 75,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Directional", "Point", "Spot"

#CHOICEBOX#
/LABEL/ Space:
/POSITION/ 10,25
/SIZE/ 68,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Model", "Screen"

#COLOR#      
/LABEL/ Color:
/POSITION/ 90,25
/SIZE/ 70,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Intensity:
/POSITION/ 175,25
/SIZE/ 45,14
/TYPE/ UD_DASINT
/RANGE/ 0,100
/PREC/ 3
/LEN/ 4

#PUSHBUTTON#
/LABEL/ Position:
/POSITION/ 10,42
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 55,42
/SIZE/ 95,14
/TYPE/ UD_DASCART
/PREC/ 3
/LEN/ 25

#EDIT#
/LABEL/
/POSITION/ 55,42
/SIZE/ 95,14
/TYPE/ UD_DASVEC
/PREC/ 3
/LEN/ 25

#EDIT#
/LABEL/ Cone Angle:
/POSITION/ 175,42
/SIZE/ 50,14
/TYPE/ UD_DASVAL
/RANGE/0.,90.
/PREC/ 3
/LEN/ 6

#EDIT#
/LABEL/ Direction:
/POSITION/ 10,59
/SIZE/ 150,14
/TYPE/ UD_DASVEC
/PREC/ 3
/LEN/ 25
/INPUT/ FORM_PICK

#EDIT#
/LABEL/ Ambient:
/POSITION/ 175,59
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,1.
/PREC/ 3
/LEN/ 6

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 100,80
/SIZE/ 60,14
/TYPE/ UD_DASSTRING


#HELP#
==================
Light Sources Form
==================
This form is used to define the active lights and their properties.  The active
lights and their properties in addition to the material propertites determine
how surfaces are shaded.  Accepting this form will save the light settings in
the modals file 'ncl_lights.mod', so that the next time NCL is started, these
settings will be used as the defaults.

Light
-----
This toggle field determines which light is currently being defined.  Up to 5
lights can be defined and active at any given time.

Active
------
Determines if this light is active or not.

Type
----
Defines the type of light being defined.  Directional lights are infinitely far
away from the scene and will light up the entire scene.  An example of an
infinite light is the sun.  Point lights can be positioned by the user, they
display light in all directions, similar to a light bulb.  Spot lights can also
be positioned by the user, they only display light in a user defined direction.

Space
-----
Point and Spot lights can be positioned in reference to Model Space or Screen
Space.  When they are positioned in Model Space, they are moved with the model
as it is rotated/translated.  In Screen Space, the lights do not move with the
model, but rather stay fixed to the screen viewport.  Screen Space coordinates
are specified as a percentage of the viewport size, therefore they always
remain in the viewport.

Color
-----
Defines the color of the light.  This color affects the Diffuse and Specular
attributes of the surface material.

Intensity
---------
Defines the intensity or brightness of the light.  It can be in the range of
0 to 100 percent.

Position
--------
Selects the position of the light for Point and Spot lights.  If this button is
pressed and Screen Space is active, then the light will be dynamically dragged
with the cursor.

Cone Angle
----------
Sets the angle between the axis of the cone and the outside of the cone for
Spot lights.  Only the surface areas within this cone will be affected by this
light.

Direction
---------
Defines the vector direction of Directional and Spot lights.

Ambient
-------
The Ambient value determines the amount of ambience this light emits.  This
value affects the ambient property of the surface material, so that a lower
value causes less of the ambient light of the material to be generated.  The
Ambient value can be between 0 (no ambient light) and 1.

Apply
-----
Applies the form light settings so that the results can be viewed without
exiting the form.  Using the Apply button and then the Cancel button will also
change the lights without saving the newly defined lights to the modals file.
