#HEADER#
/TITLE/ Modals Files Output
/POSITION/ 50,50
/SIZE/150,70

#CHOICEBOX#      
/LABEL/ Store Updated Colors:
/POSITION/ 10,8,95,8
/SIZE/ 130,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#      
/LABEL/ Save Form Size/Position:
/POSITION/ 10,25,95,25
/SIZE/ 130,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#HELP#
This form controls the automatic creation of modals/form positioning files
when modals settings change in various forms or when a form is resized or
repositioned.

Store Updated Colors:
---------------------
Enter Yes at this prompt if you want the 'ncl_color.mod' file created when a
new custom color is defined.  This modals file will be stored in the user's
local modals directory.

The problem that can arise with saving the updated colors is when a Unibase is
loaded with its own custom colors (for example, when imported from another
system).  This will overwrite the custom colors that are normally used within
NCL.  You can of course simply delete the local 'ncl_color.mod' file to restore
the NCL custom colors.

Save Form Size/Position:
------------------------
Enter Yes at this prompt if you want the size and position of the active form
saved to the local forms directory as a 'form.pos' file when the form is
accepted.  This feature allows the form to be displayed at this same position
and size the next time it is activated, even in a separate NCL session.

Enter No if you want the forms to always be displayed in the default position
with the default size.
