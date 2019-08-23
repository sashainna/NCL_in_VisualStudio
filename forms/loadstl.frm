#HEADER#
/TITLE/ Load STL File
/POSITION/ 50,50
/SIZE/220,90

#PUSHBUTTON#
/LABEL/ STL File:
/POSITION/ 10,8
/SIZE/ 35,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 50,8
/SIZE/ 105,14
/TYPE/ UD_DASSTRING
/PREC/ 35
/LEN/ 35

#EDIT#
/LABEL/ Solid Label:
/POSITION/ 10,28
/SIZE/ 65,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#CHOICEBOX#
/LABEL/ Units:
/POSITION/ 110,28
/SIZE/ 65,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Inch", "MM"

#CHECKBOX#
/LABEL/ Optimize Edge Display
/POSITION/ 10,45
/SIZE/ 90,13
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 110,45
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#HELP#
=============
Load STL File
=============
This form creates a Visual Solid by loading it from an external STL file.  The
STL file can be generated from any CAD system.

STL File:
---------
Enter the external STL file name in this field either by typing it in or by
pressing the prompt button and selecting it from the file browser.

Solid Label:
------------
Contains an optional label for the defined STL solid.

Units:
------
STL files are not stored with the units that they were created in so you
must select the units that the STL file was saved in, either Inch or MM.

Optimize Edge Display
---------------------
STL models are stored as a series of triangles that are used to describe the
faces of the STL solid.  Many triangles may be necessary to define a planar face
of the solid depending on the complexity of the boundary of the planar face.
When this box is checked, NCL will remove the inner triangle edges of a planar
face leaving only the boundary edge of the face.

Optimizing the edge display of an STL solid can be time consuming depending
on the complexity of the solid.  Disabling this feature will leave the edge
display as it is defined in the STL file and will load complex solids much
quicker, but the edge or wireframe display will be quite busy.

Apply
-----
Loads the STL file without taking down the form so that other STL files can be
loaded.
