/*********************************************************************
**
**    NAME         :  iunihep.msg
**
**       CONTAINS:
**         	Concatenation of help, error and prompt messages for all
**          the subsystems.
**
**	 WARNING:  Length of error message from first non-blank non-tab
**	 character to EOL must be <= 80
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       iunihep.msg , 26.2
**    DATE AND TIME OF LAST MODIFICATION
**       04/10/18 , 15:04:19
**
*********************************************************************/
/*-------------------------------------------------------------------------

                        Assigned Subsystem Numbers

#define UD_DASHEP		1
#define SELECTERR		2
#define UC_CALCERROR	3
#define DASINERROR		4
#define MENUERROR		5
#define UU_FEATERROR	6
#define UM_MODEL		7
#define UU_UBASE		8
#define VIEWERROR		9
#define UX_UDOS			10
#define UB_SYMBOL   	11
#define UJ_SUPPORT		12
#define UA_DRAFTING		13
#define UU_SIGNON		14
#define UU_CLASS		15
#define UM_APPGEO		16
#define UA_NCL			17



**-------------------------------------------------------------------------
      NAME         :  idashep.mg
      CONTAINS: MPE DAS help, error, and prompt messages
-------------------------------------------------------------------------*/

# Subsystem = UD_DASHEP = 1
  # error section
    # number = 1, severity = 0
   Valuator input not allowed; use locate, pick or text string. 
.
    # number = 2, severity = 0
Attempted pick was unsuccessful.  Retry.  [More info in HELP.]
  You attempted to pick an entity and either

  - the cursor was not touching a pickable entity, or
  - the entity you tried to pick was previously picked (multiple 
    pick operations only), or 
  - if you are using a filter, no filtered entity was under the
    cursor.

  The cursor must be touching the entity you wish to pick.  You 
  can retry the pick operation until it is successful, or select 
  Reject Operation.
.
    # number = 3, severity = 0
   Default selected when no default in effect.  [More info in HELP.]
 Default can be selected only when the prompt contains a default
 value enclosed in brackets.  Example: [<0,0,0>].
.
    # number = 4, severity = 0
 Menu in use.  Complete current operation (DONE).  [More info in HELP.]
 You have attempted to select an option from a sub-menu  while a 
 option previously selected from the menu is still in progress.  

 Before selecting another option from this menu you must either:

   - complete the operation currently in progress.
   - cancel the operation by selecting the Reject Operation icon.
   - re-invoke this subsystem to obtain a 'new copy' of the sub-menu.
     You can then select another option from the sub-menu, complete it,
     and then exit the subsystem to return to this subsystem.  You
     can then complete this operation.
.
    # number = 5, severity = 0
 Scalar variable not allowed; enter coordinate.  [More info in HELP.]. 
 The calculator variable entered is of the type "scalar," but the system
 is prompting you for a point.
.
    # number = 6, severity = 1
   DAS#6; unrecognizable input from text parser. [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 7, severity = 1
   DAS#7; invalid input event. [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 8, severity = 1
   Invalid input; use choice. [More info in HELP.]
Change the cursor to Choice mode (arrow cursor) with the middle mouse
button or white puck button and try again.
.
    # number = 9, severity = 1
   DAS#9; invalid DAS request. [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 10, severity = 0
   Increment limit exceeded.
.
    # number = 11, severity = 1
   DAS#11; unsupported symbol table data type. [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 12, severity = 0
   DAS#12; undefined control action encountered. [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 13, severity = 0
 Last entity already deleted; no entity to delete. [More info in HELP.]
 Delete Last can only be used once.  Use Delete in Edit/Move 
 Subsystem to delete entities other than the last one entered.
.
    # number = 14, severity = 0
   %s - Undefined subsystem invoked.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 15, severity = 0
   Zero or negative increments not allowed.
.
    # number = 16, severity = 0
   Incremental form not allowed; scalar input only.
.
    # number = 17, severity = 0
   Coordinate form not allowed; scalar input only.
.
    # number = 20, severity = 0
 Menu in use.  Complete current operation (DONE).  [More info in HELP.]
 You have attempted to select an option from a sub-menu  while a 
 option previously selected from the menu is still in progress.  

 Before selecting another option from this menu you must either:

   - complete the operation currently in progress.
   - cancel the operation by selecting the Reject Operation icon.
   - re-invoke this subsystem to obtain a 'new copy' of the sub-menu.
     You can then select another option from the sub-menu, complete it,
     and then exit the subsystem to return to this subsystem.  You
     can then complete this operation.
.
    # number = 22, severity = 0
Attempted pick was unsuccessful.  Retry.  [More info in HELP.]
You attempted to pick an entity and either

- the cursor was not touching a pickable entity, or
- the entity you tried to pick was previously picked (multiple 
  pick operations only), or 
- if you are using a filter, no filtered entity was under the
  cursor.

The cursor must be touching the entity you wish to pick.  You 
can retry the pick operation until it is successful, or select 
Reject Operation.
.
    # number = 23, severity = 0
 DAS#23; invalid event type in select DAS.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 25, severity = 0
   Invalid input; use location pick.?
.
    # number = 26, severity = 0
   DAS#26; all increment counts are zero; re-enter.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 28, severity = 3
   DAS#28; invalid event played from record/playback file=%s.  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 29, severity = 3
   DAS#29; invalid event being recorded.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 31, severity = 4
   DAS#31; d_select returned from d_jmpmark.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 32, severity = 0
Cursor not on an icon.  Try again.  [More info in HELP.]
The system did not detect that the tip of the choice cursor (arrow) was 
positioned on an icon when the mouse or puck button was pressed.   

If the cursor appears visually to be properly positioned within an icon
when this error occurs, a system problem is indicated.  Call NCL
Support.
.
    # number = 34, severity = 2
   DAS#34; invalid feature picked; data error.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 35, severity = 2
   DAS#35; unsupported symbol table data type.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 36, severity = 0
   Text entered is longer that allowed; re-enter.  [More info in HELP.]
     You entered a text string longer than that allowed for the 
requested information. ;ref 
.
    # number = 38, severity = 0
   Illegal input; enter string. ;?
.
    # number = 42, severity = 0
   Incremental form not allowed; real number input only.  [More info in HELP.]
 You are being prompted for a real number.  You must respond by
	- typing in a real number, or 
	- picking a real number (such as a feature) from the graphics
	  area of the screen, or
	- typing in the name of a calculator variable of the type "real."
	  Calculator variables are entered by typing "^variablename".
		
	 Real numbers are entered in one of the following forms:
 			23
 			23.456
 			789
 			.23e4
.
    # number = 43, severity = 0
Ref. coordinate not allowed; real number input only.  [More info in HELP.]
 You are being prompted for a real number.  You must respond by
	- typing in a real number, or 
	- picking a real number (such as a feature) from the graphics
	  area of the screen, or
	- typing in the name of a calculator variable of the type "real."
	  Calculator variables are entered by typing "^variablename".
		
	 Real numbers are entered in one of the following forms:
 			23
 			23.456
 			789
 			.23e4
.
    # number = 44, severity = 0
   Coordinate form not allowed; real number input only.  [More info in HELP.]
 You are being prompted for a real number.  You must respond by
	- typing in a real number, or 
	- picking a real number (such as a feature) from the graphics
	  area of the screen, or
	- typing in the name of a calculator variable of the type "real."
	  Calculator variables are entered by typing "^variablename".
		
	 Real numbers are entered in one of the following forms:
 			23
 			23.456
 			789
 			.23e4
.
    # number = 45, severity = 0
   Unrecognizable input; enter real number. [More info in HELP.]
 You are being prompted for a real number.  You must respond by
	- typing in a real number, or 
	- picking a real number (such as a feature) from the graphics
	  area of the screen, or
	- typing in the name of a calculator variable of the type "real."
	  Calculator variables are entered by typing "^variablename".
		
	 Real numbers are entered in one of the following forms:
 			23
 			23.456
 			789
 			.23e4
.
    # number = 47, severity = 1
   Choice not allowed for real number input.  [More info in HELP.]
 You are being prompted for a real number.  You must respond by
	- typing in a real number, or 
	- picking a real number (such as a feature) from the graphics
	  area of the screen, or
	- typing in the name of a calculator variable of the type "real."
	  Calculator variables are entered by typing "^variablename".
		
 Real numbers are entered in one of the following forms:
				123
				123.456
				.789
				1.23e4
.
    # number = 49, severity = 0
Location not allowed for vector input.  [More info in HELP.]
 You are being prompted for a vector.  You must respond by
	- typing in a vector, or 
	- picking a vector from the graphics area of the screen, or
        - typing in the name of a calculator variable of the type "vector."
	  Calculator variables are entered by typing "^variablename".

 Vectors are entered in one of the following forms:
				x,y
				x,y,z
				<x,y>
				<x,y,z>
 where x, y and z are integers.

 A vector can be picked from the graphics area of the screen by
 changing the input mode to "pick" and either
	- picking a vector feature (the feature that looks like an arrow).
	- using the keyboard character 'v' to trigger a pick of a curve.
	  Each end of a curve has implicit inward-pointing vectors giving
	  the direction of the curve at that endpoint.
.
    # number = 50, severity = 0
   The keyboard 'v' key must be used to pick a vector.  [More info in HELP.]
.
    # number = 53, severity = 0
   Choice not allowed; enter vector. 
 You are being prompted for a vector.  You must respond by
	- typing in a vector, or 
	- picking a vector from the graphics area of the screen, or
	- typing in the name of a calculator variable of the type "vector."
	  Calculator variables are entered by typing "^variablename".

 Vectors are entered in one of the following forms:
				x,y
				x,y,z
				<x,y>
				<x,y,z>
 where x, y and z are integers.

 A vector can be picked from the graphics area of the screen by
 changing the input mode to "pick" and either
	- picking a vector feature (the feature that looks like an arrow).
	- using the keyboard character 'v' to trigger a pick of a curve.
	  Each end of a curve has implicit inward-pointing vectors giving
	  the direction of the curve at that endpoint.
.
    # number = 54, severity = 0
   Incremental form not allowed; vector input only.
 You are being prompted for a vector.  You must respond by
	- typing in a vector, or 
	- picking a vector from the graphics area of the screen, or
	- typing in the name of a calculator variable of the type "vector."
	  Calculator variables are entered by typing "^variablename".

 Vectors are entered in one of the following forms:
				x,y
				x,y,z
				<x,y>
				<x,y,z>
 where x, y and z are integers.

 A vector can be picked from the graphics area of the screen by
 changing the input mode to "pick" and either
	- picking a vector feature (the feature that looks like an arrow).
	- using the keyboard character 'v' to trigger a pick of a curve.
	- typing in a vector, or 
	- picking a vector from the graphics area of the screen, or
	- typing in the name of a calculator variable of the type "vector."
	  Calculator variables are entered by typing "^variablename".
		
 Vectors are entered in one of the following forms:
				x,y
				x,y,z
				<x,y>
				<x,y,z>
 where x, y and z are integers.

 A vector can be picked from the graphics area of the screen by
 changing the input mode to "pick" and either
	- picking a vector feature (the feature that looks like an arrow).
	- using the keyboard character 'v' to trigger a pick of a curve.
	  Each end of a curve has implicit inward-pointing vectors giving
	  the direction of the curve at that endpoint.
.
    # number = 55, severity = 0
   Reference coordinate definition not allowed; vector input only.
 You are being prompted for a vector.  You must respond by
	- typing in a vector, or 
	- picking a vector from the graphics area of the screen, or
	- typing in the name of a calculator variable of the type "vector."
	  Calculator variables are entered by typing "^variablename".

 Vectors are entered in one of the following forms:
				x,y
				x,y,z
				<x,y>
				<x,y,z>
 where x, y and z are integers.

 A vector can be picked from the graphics area of the screen by
 changing the input mode to "pick" and either
	- picking a vector feature (the feature that looks like an arrow).
	- using the keyboard character 'v' to trigger a pick of a curve.
	  Each end of a curve has implicit inward-pointing vectors giving
	  the direction of the curve at that endpoint.
.
    # number = 56, severity = 0
   Scalar input not allowed; vector input only.
 You are being prompted for a vector.  You must respond by
	- typing in a vector, or 
	- picking a vector from the graphics area of the screen, or
	- typing in the name of a calculator variable of the type "vector."
	  Calculator variables are entered by typing "^variablename".
		
 Vectors are entered in one of the following forms:
				x,y
				x,y,z
				<x,y>
				<x,y,z>
 where x, y and z are integers.

 A vector can be picked from the graphics area of the screen by
 changing the input mode to "pick" and either
	- picking a vector feature (the feature that looks like an arrow).
	- using the keyboard character 'v' to trigger a pick of a curve.
	  Each end of a curve has implicit inward-pointing vectors giving
	  the direction of the curve at that endpoint.
.
    # number = 57, severity = 0
   Unrecognizable input; vector input only.
	You are being prompted for a vector.  You must respond by
	- typing in a vector, or 
	- picking a vector from the graphics area of the screen, or
	- typing in the name of a calculator variable of the type "vector."
	  Calculator variables are entered by typing "^variablename".
	
 Vectors are entered in one of the following forms:
				x,y
				x,y,z
				<x,y>
				<x,y,z>
 where x, y and z are integers.

 A vector can be picked from the graphics area of the screen by
 changing the input mode to "pick" and either
 picking a vector feature (the feature that looks like an arrow).
	- using the keyboard character 'v' to trigger a pick of a curve.
	  Each end of a curve has implicit inward-pointing vectors giving
	  the direction of the curve at that endpoint.
.
    # number = 58, severity = 2
   DAS#58; invalid event in vector input.  [More info in HELP.]
 Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 59, severity = 1
 Vector <0,0,0> invalid.  Re-enter.  [More info in HELP]
 A vector expresses a direction and distance.  For NCL system 
 input, the distance must always be greater than zero, therefore
 at least one of the components of the vector must not be equal to 
 zero, and the vector 0,0,0 is not valid.
.
	 #number = 60, severity = 1
Unable to find the playback file %s.
.
    # number = 61, severity = 0
   DAS#61; cannot open file.  [More info in HELP.]
 Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 62, severity = 0
   Cannot start recording; currently recording or paused.  [More info in HELP.]
 You have previously started recording the session.  You cannot 
 turn record on if it is already on.  To record in a different
 file, select Record Off then Record On with a different file name.
 You can tell when record is on by observing the Record= field in 
 the screen status area, which will display ON.
.
    # number = 63, severity = 0
   Cannot playback; currently recording or paused.  [More info in HELP.]
 You have previously started recording the session.  You cannot 
 turn playback on while record is on. 
.
    # number = 64, severity = 0
   Playback limit exceeded.
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 66, severity = 0
   Cannot playback; playback already started.  [More info in HELP.]
 You have previously started playback.  You cannot 
 turn playback on while playback alread in progress. 
.
    # number = 67, severity = 0
   Illegal keyboard character used to pick entity.  [More info in HELP.]
 A normal pick operation is triggered by pressing the left mouse 
 button or the Return key on the keyboard.  You can also use the 
 keyboard l, p, a, f, or v keys (lowercase only) to trigger a pick 
 if to wish to restrict picking to one of the following entity types:
	l - lines  p - points  a - arcs  f - displayed features  
	v - vectors 
.
    # number = 68, severity = 0
Too many subsystems selected.  Forcing jump to root.  [More HELP.]
 You have selected a subsystem icon from within another (or the same)
 subsystem more than fifteen times.  In order to prevent degradation
 of system performance due to excessive stack lengths, the system
 is forcing a jump to the system root menu (the Sign-On menu).  You
 can continue normal operations after this jump takes place.

 To prevent this situation from occuring, you should "back up" to 
previously invoked subsystems by exiting the current subsystem when
 finished, rather than re-invoking the other subsystems.
.
    # number = 69, severity = 0
   Input constrained to %s, re-enter.;?
.
    # number = 70, severity = 0
   Cannot pick this entity now
.
    # number = 71, severity = 0
   Number of interactions are constrained between %d and %d.;?
.
    # number = 72, severity = 0
   You must enter a value between %g and %g.
.
    # number = 73, severity = 0
   Input not allowed; input stroke with stroke device
.
    # number = 74, severity = 1
   Invalid input; use stroke. 
.
    # number = 75, severity = 1
   DAS#75; invalid input event. [More info in HELP.]
 Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 76, severity = 1
   Restricted entity picked; retry. [More info in HELP.]
 Picking is limited to geometric, drafting, and feature entities.
 You cannot pick axes, viewport information fields, etc.  

 You can retry the operation until successful, or select Command Reject.
.
    # number = 77, severity = 1
   Global select buffer is busy, complete previous operation
.
    # number = 78, severity = 1
   The two points must be in the same viewport.
.
    # number = 79, severity = 1
   Illegal icon rectangle.
.
    # number = 80, severity = 5
   Can't open icon file %s
.
    # number = 81, severity = 5
   Can't create icon file %s (file already exists)
.
    # number = 82, severity = 5
   Can't create icon file %s
.
    # number = 83, severity = 5
   Corrupted icon file %s
.
    # number = 84, severity = 1
   Cursor not in graphics area - reenter
.
    # number = 85, severity = 4
   Zero length icon file %s
.
    # number = 86, severity = 4
   Environment stack overflow - flushing system
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 87, severity = 0
Warning - go to MAIN soon; too many subsystem calls.  [More info in HELP.]
 You have selected a subsystem icon from within another (or the same)
 subsystem nearly fifteen times.  In order to prevent degradation
 of system performance due to excessive stack lengths, the system
 will force a jump to the system root menu (the Sign-On menu) when
 the level of recursion reaches 15. To avoid this, you should select
 the Go To Main icon as soon as possible.  

 To prevent this situation from occuring, you should "back up" to 
 previously invoked subsystems by exiting the current subsystem when
 finished, rather than re-invoking the other subsystems.
.
    # number = 88, severity = 0
   3D vector not allowed while in drawing management.  [More info in HELP.]
	You have entered a vector which has a z component which is not equal to
	zero.  The Drawing Management subsystem of the NCL System is restricted
	to 2D operations, therefore the vector you entered is not valid.  You 
	should enter vectors in the form x,y or <x,y>
.
    # number = 89, severity = 0
   3D coordinate not allowed while in drawing management.  [More info in HELP.]
 You have entered a coordinate which has a z component which is
 not equal to zero.  The Drawing Management subsystem of the NCL
 System is restricted to 2D operations, therefore the coordinate you
 entered is not valid.  You should enter coordinates in the form x,y
 or <x,y>
.
    # number = 90, severity = 0
   Construction plane is orthogonal to view plane.  [More info in HELP.]
 You cannot input points with the locate (+) cursor in a viewport 
 in which the construction plane is orthogonal to the view plane.
 The construction plane is orthogonal when the x and y axes of the
 construction plane axes display appear to be aligned with one-another.

 Use another viewport which has a different view of the model
 displayed, or change the view in this viewport, to avoid this problem.
.
    # number = 91, severity = 0
   Form window is currently in use.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 92, severity = 0
No entities passed select filter.
.
	# number = 93, severity = 0
Only a 'y', 'Y', 'n' or 'N' response is valid
.
    # number = 94, severity = 0
	No help area defined in this layout. Help and tutorials unavailable.
.
    # number = 95, severity = 0
   Window creation error has occured.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 96, severity = 0
	You may not chain select on lines that have line thickness applied.
.
    # number = 97, severity = 4
	MARK stack out of balance !!!
	your application has invoked an unbalanced number of MARK
	and UNMARK functions.  if you invoke a jump to root or command reject
	function at this point you will most certainly crash the system.

	SAVE your part and exit the system.
.
    # number = 98, severity = 0
	Internal Select Buffer is Full
.
    # number = 99, severity = 0
	Coordinates must be between 0.0 and 1.0

	The application is asking for a NDC point.  The coordiantes must be
	normalized between 0.0 and 1.0.
.
    # number = 100, severity = 0
	You have exeeded the maximum number of open windows

	Your application has recursively invoked procedures that have open
	scrolling text windows.  The number of windows eceeds the number that 
	your application is configured for.  Unwind the execution stack to 
	continue your work.
.
    # number = 101, severity = 0
	This entity may not be used for plane definition.
.
    # number = 102, severity = 0
	Plane not yet defined.
.
    # number = 103, severity = 0
	3-D space defined: Restart plane specification.
.
    # number = 104, severity = 0
	Auto-test tables are not linked into this application.
.
    # number = 105, severity = 0
	Subsystems are disabled, complete current input sequence
.
    # number = 106, severity = 0
	Menu Design is already active
.


  # prompt section
    # number = 1
   Enter Z depth:
.
    # number = 2
   Enter delta vector:
   Specify a vector offset from the picked entity.  Invoke DAS help
   for information on entering a vector.
.
    # number = 3
   Enter prompt line: 
.
    # number = 4
   Enter message
.
    # number = 6
   Enter lower left corner of display window.
.
    # number = 7
   Enter record file name: 
.
    # number = 8
   File exists - overwrite?
.
    # number = 9
   Append to existing file?
.
    # number = 10
   Enter playback file name: 
.
    # number = 11
   by choice
.
    # number = 12
   by value 
.
    # number = 13
   by location 
.
    # number = 14
   by pick 
.
    # number = 15
   by text 
.
    # number = 16
   Press command reject key to terminate; any other key to continue.
.
    # number = 17
   END OF MESSAGE; press any key. 
.
    # number = 18
   /* cartesian coordinate input request */
               INPUT A COORDINATE

The legal inputs are:

   LOCATOR:  If a locator is input then the trigger 
             character is examined for validity.  A "Z" 
             signifies that the Z depth is to be input as
             a distance next.  Depth is input in construction
             coordinates.  A trigger with the space bar causes
             the active Z depth to be used.

   PICK:     If an entity is picked, the trigger character is
             examined for a momentary action.  "D" signifies
             that a delta point is required.  Other characters
             are used in conjunction with existing geometry.  If
             a point is picked, then it is used to derive the
             coordinate. The end closest to any other entity
             pick will be derived to satisfy coordinate input.

   CONTROL:  A control input signifies that the user wishes to:

               a. end the session
               b. reject the last input
               c. reject all the last inputs and restart the
                  interaction sequence.
               d. reject this command and return to the menu
                  navigator.
               e. get help

   STRING:   Coordinates can be entered by text string in one
             of the three supported coordinate systems.

    # number = 19
   /* select subsystem input request */
            PICK ENTITIES

   The legal inputs are:

      PICK:     A pick is the normal mode of input.  Graphical
                entities--text or geometry--are picked by centering
                the cursor over the entity and triggering the pick
                by pressing the spacebar or momentary action key.

                The operator can also trigger higher order pick
                by pressing "A" for alternate action.  The alternate
                action menu of options includes:

                  - single select
                  - chain select
                  - select inside a region
                  - select outside a region
                  - all displayed
                  - entire drawing

    CONTROL:    A control input signifies that the user wishes to:

                  a. end the session
                  b. reject the last input
                  c. reject all the last inputs and restart the
                     interaction sequence.
                  d. reject this command and return to the menu
                     navigator.
                  e. get help

.
    # number = 20
   /*  value input request */
                  INPUT A VALUE

   The legal inputs are as follows:

      VALUATOR: If a valuator is input the value is returned
                as it comes in.  No units are allowed.

      STRING:   Enter a value with or without units.

      CONTROL:  A control input signifies that the user wishes to:

                  a. end the session
                  b. reject the last input
                  c. reject all the last inputs and restart the
                     interaction sequence.
                  d. reject this command and return to the menu
                     navigator.
                  e. get help

.
    # number = 21
   /*choice input request */
.
    # number = 22
   /* integer input request */
                     INPUT AN INTEGER 

      VALUATOR: If a valuator is input the value is returned
                as it comes in.  No units are allowed.

      STRING:   Enter a value with or without units.

      CONTROL:  A control input signifies that the user wishes to:

                  a. end the session
                  b. reject the last input
                  c. reject all the last inputs and restart the
                     interaction sequence.
                  d. reject this command and return to the menu
                     navigator.
                  e. get help

.
    # number = 23
   /* vector input request */
                     INPUT A VECTOR

   The legal inputs are as follows:

      PICK:     The following items may be picked to define a vector.

                  line  - a vector from the endpoint farthest from
                          the end picked to the picked end will be
                          generated.

                  feature - a vector feature can be picked

      STRING:   Enter a vector in one of the three supported
                coordinate systems.

      CONTROL:  A control input signifies that the user wishes to:

                  a. end the session
                  b. reject the last input
                  c. reject all the last inputs and restart the
                     interaction sequence.
                  d. reject this command and return to the menu
                     navigator.
                  e. get help
.
    # number = 24
   /* pick input request */
            INPUT A PICK ID (PICK SOME ENTITIES)

.
    # number = 25
   /* location pick input request */
      PICK AN ENTITY AND RETURN THE COORDINATE USED TO PICK IT 

   The legal inputs are as follows:

      PICK:     A pick is  the only valid means of inputting
                this type of action.  The pick event must
                immediately followed by a locator event.

      CONTROL:  A control input signifies that the user wishes to:

                  a. end the session
                  b. reject the last input
                  c. reject all the last inputs and restart the
                     interaction sequence.
                  d. reject this command and return to the menu
                     navigator.
                  e. get help

.
    # number = 26
   /* string input request */
               INPUT A CHARACTER STRING 

   The legal inputs are as follows:

      STRING:   Enter a coordinate, name of file, number, etc. 

      CONTROL:  A control input signifies that the user wishes to:

                  a. end the session
                  b. reject the last input
                  c. reject all the last inputs and restart the
                     interaction sequence.
                  d. reject this command and return to the menu
                     navigator.
                  e. get help
.
    # number = 27
   /* choice input request */
                     INPUT A CHOICE

The legal inputs are as follows:

   CHOICE:   The operator may choose a valid option from a list of
             choices or a menu.

   CONTROL:  A control input signifies that the user wishes to:

               a. end the session
               b. reject the last input
               c. reject all the last inputs and restart the
                  interaction sequence.
               d. reject this command and return to the menu
                  navigator.
               e. get help
.
    # number = 28
   /* cartesian ndc coordinate input request */
.
    # number = 29
   /* angle input */
.
    # number = 30
   /* unitless real number input */
.
    # number = 31
   iteration count exceeds 1024 coordinates, is this what you want?
.
    # number = 32

.
    # number = 33

.
    # number = 34

.
    # number = 35

.
    # number = 36
   (Vector)
.
    # number = 37

.
    # number = 38

.
    # number = 39

.
    # number = 40

.
    # number = 41

.
    # number = 42

.
    # number = 43

.
    # number = 44
   by stroke:
.
	# number = 45
	enter new screen number
.
	# number = 46
	enter number of screen to copy
.
	# number = 47
	enter layout filename
.
	# number = 48
	enter layout filename
.
	# number = 49
	enter new color index
.
	# number = 50
	enter number of rows in this icon menu
.
	# number = 51
	enter number of columns in this icon menu
.
	# number = 52
	enter number of icons in this menu:
.
	# number = 53
	choose an icon position 
.
	# number = 54
	pick the area
.
	# number = 55
	enter new icon menu filename
.
	# number = 56
	enter new icon filename
.
	# number = 57
	Pick start item of chain
.
	# number = 58
	Enter first corner of region
.
	# number = 59
	Enter second corner of region
.
	# number = 60
	Enter save screen filename
.
	# number = 61
	Pick entities for icon
.
	# number = 62
	Pick lower left corner of icon
.
	# number = 63
	Pick upper right corner of icon
.
	# number = 64
	PRESS ANY KEY TO CONTINUE.
.
	# number = 65
	Indicate the entity to select
.
	# number = 66
	Is this the desired entity?
.
	# number = 67
	Reverse direction of vector?
.
	# number = 68
	Enter the origin of the plane
.
	# number = 69
	Enter the normal vector to the plane
.
/*
.....Added by NCCS, Improved picking.  Roberta Zorzynski.
*/
	# number = 70
	Enter new pick aperture
.

/*-------------------------------------------------------------------------
      NAME         :  icalchep.mg
      CONTAINS: MPE CALC help, error, and prompt messages
-------------------------------------------------------------------------*/
# Subsystem = CALC = 3
  # error section
    # number = 1, severity = 0
   Undefined Calculator variable name.  [More info in HELP.]
   If you are in the Calculator when this error occurs, you should type
   'list' to display calculator variable names and types.  

   If you are not in the Calculator, this message can only occur when 
   enter the carat ('^') in response to a prompt.  The carat signifies
   that you wish to answer the prompt with a calculator expression.  If
   you do so and this message occurs, you have used a variable name 
   that was not previously defined.  For example, 

	       ^p=p1+<10,10,0>

   would cause this message to occur if p1 had not previously been 
   defined.
.
    # number = 2, severity = 0
   Syntax error; re-enter.
.
    # number = 3, severity =0 
   Calculator calls exceed limit of 4; action denied.  [More info in HELP.]
 You cannot select the Calculator subsystem from within the 
 Calculator more than four times.  This restriction also applies to
 selecting the Calculator from within a subsystem invoked while in the 
 Calculator.
.
    # number = 4, severity = 0
   Invalid input; illegal expression. 
.
    # number = 5, severity = 0
   CALC#5; error reading lexicon file.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 6, severity = 0
   Expression contains too many operators.  [More info in HELP.] 
 The expression just entered contains more operators than can
 be held in temporary storage at one time.  Simplify the expression
 by entering seperate statements to assign portions of the expression
 to calculator variables.
.
    # number = 7, severity = 0
   Invalid input; undefined data type.;?
.
    # number = 8, severity = 0
   CALC#8; data type not supported in symbol table.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 9, severity = 0
Illegal input; letter 'f' reserved for function names.  [More info in HELP.]
     You have attempted to create a calculator variable with a name 
beginning with the letter 'f', which is used only for naming
functions.
.
    # number = 10, severity = 0
   Illegal use of reserved word.;?
.
    # number = 11, severity = 0
   Dot not allowed in variable name. 
.
    # number = 12, severity = 0
   No input; re-enter. 
.
    # number = 13, severity = 0
   CALC#13; variable not supported in symbol table.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 14, severity = 0
   Invalid input; enter coordinate variable.
.
    # number = 15, severity = 0
   Invalid input; enter scalar variable.
.
    # number = 16, severity = 0
   Function name undefined.  [More info in HELP.]
	    You have entered a calculator statement containing name beginning
with 'f', but that name is not a defined function.  
.
    # number = 17, severity = 0
   Parameters exceed limit of 4. ;?
.
    # number = 18, severity = 0
   Nested functions exceed limit of 20.
.
    # number = 19, severity = 0
   CALC#19; parameter type not supported.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 21, severity = 0
   Number of parameters must match previous definition.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 22, severity = 0
   CALC#22; empty table.
.
    # number = 23, severity = 0
   Invalid input; enter 0 or positive number only.
.
    # number = 24, severity = 0
   Invalid input; number is larger than nine.
.
    # number = 25, severity = 0
   Warning - notation unchanged.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 26, severity = 0
   CALC#26; operation not supported.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 27, severity = 0
   Illegal input; improper operand. 
.
    # number = 28, severity = 0
   Coordinate cannot multiply a coordinate.  [More info in HELP.]
	    Multiplication of coordinates is not defined.  You can add or 
subtract coordinates, or add, subtract, multiply or divide a
coordinate by a scalar.
.  
    # number = 29, severity = 0
   Coordinate cannot divide a coordinate.  [More info in HELP.]
	    Division of coordinates is not defined.  You can add or 
subtract coordinates, or add, subtract, multiply or divide a
coordinate by a scalar.
.
    # number = 30, severity = 0
   Cannot divide by zero.
.
    # number = 31, severity = 0
   Coordinate cannot divide a scalar.
.
    # number = 32, severity = 0
   Illegal exponential expression. 
.
    # number = 33, severity = 0
   Invalid input; use scalar operands.
.
    # number = 34, severity = 0
   Invalid input; use coordinate operands. 
.
    # number = 36, severity = 0
   Value exceeds limit of 10 ^ 15.
.
    # number = 37, severity = 0
   Invalid input; square root of negative number not allowed.
.
    # number = 38, severity = 0
   CALC#38; operator type not supported.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 39, severity = 0
   CALC#39; vector and angle operation not supported.;?
.
    # number = 40, severity = 0
   CALC#40; coordinate component not supported.;?
.
    # number = 41, severity = 0
   CALC#41; coordinate type not supported.;?
.
    # number = 42, severity = 0
   Variable must be a coordinate. 
.
    # number = 43, severity = 0
   Stroke input not implemented.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 44, severity = 0
   CALC#44; choice unsupported.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 46, severity = 0
   Tangent of 90 degrees or 270 degrees not allowed.
.
   # number = 47, severity = 0
   Name must start with a non-numeric character other than 'f'.
.
   # number = 48, severity = 0
   'Help' not supported as a command.  Use the help icon.
.
   # number = 49, severity = 0
   Cannot open file 'calc.grm.  [More info in HELP.]
The calculator requires a file named 'calc.grm' which contains
the grammer used for parsing input statments.  This file was not
found during initialization of the calculator.  Refer to the
Installation section of the reference manual to be sure that the
system is properly installed and that the run script you are using
contains the necessary information.
.
   # number = 50, severity = 0
	Invalid variable name;?
.
   # number = 51, severity = 0
	Cannot open lexicon file %s
The calculator requires a file which was not found during 
initialization of the calculator.  Refer to the Installation 
section of the reference manual to be sure that the system is 
properly installed and that the run script you are using
contains the necessary information.
.
   # number = 52, severity = 0
 	Cannot read lexicon file (%s) length
.
   # number = 53, severity = 0
	Cannot read lexicon file (%s) data
.
   # number = 54, severity = 0
   Invalid input; logarithm of zero or negative number not allowed.
.
   # number = 55, severity = 0
   Input string is longer than the buffer size
.
   # number = 56, severity = 0
   Reserved word cannot be used as a CALC symbol.
.
   # number = 57, severity = 0
   Syntax error - illegal function type.
.




   #  system information
             AVAILABLE COMMANDS AND CAPABILITIES IN CALCULATOR
             --------------------------------------------------
                           April 16, 1984


**NOTE -  Upper and lower case characters are the same in the CALCULATOR.


A. COMMANDS (either upper or lower case)
-------------------------------------------------------

   1. "QUIT"   - Quit the calculator.

   2. "RESET"  - Clear all values and variable names from the symbol table.

   3. "LIST"   - List values and variable names in the symbol table.

   4. "FLT #"  - Convert from scientific to floating decimal notation.  Default
                 is floating decimal with six digits of precision.  "#" is an
                 optional integer specifying precision; default is six.

   5. "SCI"    - Convert from floating decimal to scientific notation.

   6. "DEGREE"    - Set calculator default for angles to degrees.

   7. "RADIAN"    - Set calculator default for angles to radians.


B. CAPABILITIES
---------------

   1. NORMAL CALCULATIONS
   ----------------------

      - add, subtract, multiply, divide, modulus (+, -, *, /, mod)

   2. SCIENTIFIC CALCULATIONS
   --------------------------

      - trigonometric functions (i.e. sin X, cos X, tan X).

      - inverse trigonometric functions (i.e. arcsin X, arccos X, arctan X).

      - hyperbolic trigonometric functions (i.e. sinh X, cosh X, tanh X).

      - power function (i.e. X^Y).

      - square root (i.e. sqrt X).

      - logarithmic functions (i.e. log X , ln X).

      - exponential functions (i.e expten X == 10 ^ X, exp X == e ^ X).

      - absolute value (i.e. abs X).

   3. GEOMETRIC CALCULATIONS
   -------------------------

      - geometric entities are entered in the following form

      COORDINATES:
        CARTESIAN   - "< NUM , NUM , NUM >"
        CYLINDRICAL - "<R NUM, T NUM, Z NUM>"
                      where  R represents radius
                             T represents azimuthal angle
                             Z represents z value
        SPHERICAL   - "<A NUM, B NUM, C NUM>"
                      where  A represents radius
                             B represents azimuthal angle
                             C represents elevation angle
        ( NUM can be a scalar expression )

      VECTORS:
         'V' COORDINATE

      - component (i.e. for CARTESIAN - (C.X | C.Y | C.Z)
                        for CYLINDRICAL - (C.R | C.T | C.Z)
                        for SPHERICAL - (C.A | C.B | C.C)
                   where "C" is a variable name          )
        each component can be reassigned a new value
        (i.e C.X = <expression>)

      - addition ( Operands must be coordinates, but mixed coordinate
      
        types are allowed. i.e. CARTESIAN + SPHERICAL. operator= "+" )

      - subtraction (same as addition. operator = "-").

      - multiplication (One operand must be SCALAR. operator= "*").

      - division ( same as multiplication. operator = "/" )

      - cross ( same as addition. operator = "CROSS").

      - dot  (same as cross. operator = "DOT")

      - distance (Operands must both be coordinates, but mixed
        coordinate types 
                  are allowed. i.e. <coordinate> "DIST" <coordinate>)

      - angle (same as distance. operator = "ANG")

      - unit vector of a coordinate ( "UNITVC" <coordinate>)

      - magnitude of a coordinate ( "MAG" <coordinate>)

      - change degree to radian (i.e. "RAD" ANGLE)

      - change radian to degree (i.e. "DEG" ANGLE)

      - an angle's complement (i.e. "COMP" ANGLE)

      - an angle's supplement (i.e. "SUPM" ANGLE)

        ( ANGLE can be a scalar expression )

   4. FUNCTIONS
   ------------

      - functions are defined by the following form

        FUNCTION NAME "(" PARAMETERS ")" "=" FUNCTION DEFINITION

        where
            FUNCTION NAME- A user-defined name of eight characters or less, and
                           the initial character "F". 

            PARAMETERS   - only four parameters are allowed.

            FUNCTION DEFINITION - any operations allowed in the calculator.

      - FUNCTION CALL

         - parameters can be any legal expression in the calculator.
           (including another function call)
         - an operand in an expression can be a function call
         - function value can be assigned to another non-function name.


   5. MEMORY CALCULATION
   ----------------------

      - Variable names are user-defined, eight characters or less.
        Initial character "F" is reserved for function names; the
        period (.) is reserved for the component of the coordinate.
        (i.e. XYZ = calculation expression. )

      - Variable type is defined by associated value entered by the user.
        Variable type can be SCALAR, CARTESIAN, CYLINDRICAL, SPHERICAL,
        FUNCTION. 

      - Variable names and associated value are retrieved by mentioning
        the name in the expression.

      - Variable value and type are redefined automatically by assigning another
        expression.

      - Variable names and current associated attributes are listed by the
        "LIST" command.


C. PRECEDENCE OF OPERATORS ( in decreasing order)
-----------------------------------------------------

      - parenthesis.

      - unary minus.

      - trigonometric, logarithmic, and exponential functions, square root, 
        absolute value, unit vector, magnitude, degree to radian,
        radian to degree, complement, supplement.

      - ^ (power).

      - *, /, mod, cross, dot, dist.

      - +, -.
.

/*-------------------------------------------------------------------------
      NAME         :  idinhep.mg
      CONTAINS: MPE DASIN help, error, and prompt messages
-------------------------------------------------------------------------*/
#Subsystem = DASIN = 4 
  # error section
    # number = 1, severity = 0
Text input not in format required by prompt.  [More info in HELP.]
The format required depends on the type of data requested by the 
system.  Valid formats are:

POINTS   10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
	      d 10,20,30
	      d a10,b45,c90     
	      d r10,t45,z10

	      Appropriate units (see below) can be used for any value
	      in a cordinate.  Examples:

	      10 cm, 136 mm
	      a10ft,b45,c1.3rad

DISTANCES     10               (uses default set in Environment subsyst.)
	      10 mm            millimeters
	      10 cm            centimeters
	      etc.             m, km, in, ft, mi

ANGLES        90               (uses default angular unit set in Env.)
	      90 deg           (Degrees)
	      1.5 rad          (Radians)

VECTORS       1,2              (x magnitude, y magnitude)
	      1,3,1            (x,y,z)
	      a1,b90,c45       (Spherical)
	      r1,t90,z10       (Cyl.)
.
    # number = 2, severity = 0
   DASIN#2; stack overflow.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 3, severity = 0
   DASIN#3; stack is underflow.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 4, severity = 0
   Incremental number must be positive.
.
    # number = 5, severity = 0
   DASIN#5; improper translation code.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 6, severity = 0
   Invalid input; check units. ;?
.
    # number = 7, severity = 0
   Enter angle or number. ;?
.
    # number = 8, severity = 0
   Invalid input; units required.;?
.
    # number = 9, severity = 0
   Invalid unit input.  Valid units are: in ft mi mm cm m km.;?
.
    # number = 10, severity = 0
   DASIN#10; type not supported.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 11, severity = 0
   DASIN#11; input value type not supported.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.

/*-------------------------------------------------------------------------
      NAME         :  imenuhep.mg
      CONTAINS: MPE MENU help, error, and prompt messages
-------------------------------------------------------------------------*/
# Subsystem = MENUERROR = 5 
  # error section
    # number = 1, severity = 0
Text input not valid.  Use cursor to select an icon.  [More info in HELP.]
During normal operation, text can only be input from the keyboard in
response to a prompt for which there are valid forms of text input 
(not, for example, in response to a request to 'pick').  

Certain system-level debugging and control commands are available
for use by NCL support personnel. If this error occurs when you are
attempting to enter such a command, the command was not valid.  Contact
NCL Support.
.
    # number = 2, severity = 0
This icon or function key not valid at this time.  [More info in HELP.]
You selected an icon with the choice cursor (arrow cursor) or pressed a
function which invokes a 'control' action such as accepting a default
value.  The action invoked is not valid at this time.  Refer to the
Reference Manual for a description of the icon or function key.
.
    # number = 3, severity = 0
   %s
.
    # number = 4, severity = 0
   MENU#4, menu stack overflo.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 5, severity = 0
   MENU#5, menu subroutine stack overflo.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.

/*-------------------------------------------------------------------------
      NAME         :  ifeahep.mg
      CONTAINS: MPE FEATURE help, error, and prompt messages
-------------------------------------------------------------------------*/
# Subsystem = FEATERROR = 6 
   # error section
     # number = 1, severity =1 
    Feature order out of bounds.  [More info in HELP.]
 The 'order' of the feature that will be displayed is determined by
 the number of times that you pick the entity when you are selecting
 it.  one pick = 1st order, two = 2nd order, etc.  You have picked an 
 entity more times than there are orders for that type of entity.

 Refer to the Reference manual for an explaination of feature orders.
.
     # number = 2, severity =1 
    error - selected arc has zero angl.  [More info in HELP.]
	 Error should not occur during normal operation.  Contact NCL Support.
.
     # number = 3, severity = 0
No features display for entity selected.  [More info in HELP.]
 Features can only be displayed for the following geometric 
 entities:
 Line
 Circle and circular arc
 Ellipse and elliptical arc
 Spline
.
     # number = 4, severity =1 
    error in retrieve geometr.  [More info in HELP.]
	 Error should not occur during normal operation.  Contact NCL Support.
.
     # number = 5, severity =1 
    error - pick id out of range for %.  [More info in HELP.]
	 Error should not occur during normal operation.  Contact NCL Support.
.
     # number = 6, severity = 0
    error - too many picks;?
.
     # number = 7, severity = 0
    error - invalid pick identifier.  [More info in HELP.]
	 Error should not occur during normal operation.  Contact NCL Support.
.
		# number = 8, severity = 0
eature display not not available for point entities.  [More info in HELP.]
 Features can only be displayed for the following geometric 
 entities:
 Line
 Circle and circular arc
 Ellipse and elliptical arc
 Spline
.
		# number = 9, severity = 1
m_feareal: graphics pick id of %d is out of rang.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
		# number = 10, severity = 1
m_feavect: graphics pick id of %d is out of vector rang.  [More info in HELP.]
This error should not occur during normal operation.  Contact NCL Support.
.
		# number = 11, severity = 1
m_feacoord: graphics pick id of %d is out of coordinate rang.  [See HELP.]
This error should not occur during normal operation.  Contact NCL Support.
.
		# number = 12, severity = 0
Too many entities for Feature display (type 'real').  [More info in HELP.]
 You have selected more entities for feature display than the 
 feature subsystem can handle at one time.  If this message occurs
 for a small number of entities, a system error may have occurrred;
 contact NCL support.  Otherwise, retry feature display with fewer
 entities selected.
.
		# number = 13, severity = 0
Too many entities for Feature display (type 'coord').  [More info in HELP.]
 You have selected more entities for feature display than the 
 feature subsystem can handle at one time.  If this message occurs
 for a small number of entities, a system error may have occurrred;
 contact NCL support.  Otherwise, retry feature display with fewer
 entities selected.
.
		# number = 14, severity = 0
Too many entities for Feature display (type 'vector').  [More info in HELP.]
 You have selected more entities for feature display than the 
 feature subsystem can handle at one time.  If this message occurs
 for a small number of entities, a system error may have occurrred;
 contact NCL support.  Otherwise, retry feature display with fewer
 entities selected.
.
   # prompt section
     # number = 1
 	Select Geometry for Feature Display
.

/*-------------------------------------------------------------------------
      NAME         :  imodhep.mg
      CONTAINS: MPE MODEL help, error, and prompt messages
-------------------------------------------------------------------------*/
# Subsystem = MODEL = 7
   /*                                                                 */
   /*                      MODELING ERROR SECTION                    */
   /*                    LAST ERROR NUMBER IS: 328            			*/
   /*                                                                 */
   # error section
   /*                              bsplcrv.c                          */
      # number =  2, severity = 3
MODEL#2; c6_bsplcrv: insufficient memory to allocate.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  3, severity = 1
MODEL#3; c6_bsplcrv: coefficient list full.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  4, severity = 1
Number of control points for spline must be >= %d.  [More info in HELP.]
  A minimum of four points must be selected to create a spline: the end-
  points and two intermediate points.  You have entered fewer than four.
  Retry the operation with four points.
.
      # number =  5, severity = 5
Order of b-spline must be 2, 3 or 4.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  6, severity = 5
MODEL#6; tr06_tranbsplcrv: error on update_var_geom.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  7, severity = 5
MODEL#7; rt06_bsplcrv: error on update_var_geom.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  8, severity = 5
MODEL#8; cp06_bsplcrv: error on create_geom.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  9, severity = 5
MODEL#9; cp06_bsplcrv: error on update_var_geom.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      /*                                     c_srf.c                       */
      # number =  10, severity = 1
Axis direction vector must be longer than %f.  [More info in HELP.]
The length of the axis direction vector you entered is smaller 
than the system's predetermined error threshold.  Use of a
vector smaller than the threshold can introduce errors and 
is not permitted. Enter a vector with the same direction but 
a greater length.
.
      # number = 11, severity = 0
Difference between angles must be between -360 and +360 degrees.  [See HELP.]
At least one of the angles entered was greater than 360 degrees or less
than -360 degrees and the sum of the angles is not between -360 and +360.
(or the radian equivalent values of -2 pi and +2 pi).  The requested option
is not defined for angles outside this range.
.
      # number =  12, severity = 0
Selected entity not a curve.  Re-select.
You must select an entity of one of the following types:
  Line
  Circle or circular arc
  Ellipse or elliptical arc
  Spline
.
      /*                                    calcarclen.c                  */
      # number =  13, severity = 5
MODEL#13; integrate: stack overflow.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      /*                                    chamfer.c                     */ 
      # number =  14, severity = 0
Selected entity not a line.  Re-select.
.
      # number =  15, severity = 0
Angle entered must be between 0 and 180 degrees.  [More info in HELP.]
This option does not allow angles greater than 180 (pi radians),
or negative angles.
.
      # number =  16, severity = 0
Lines 1 and 2 do not intersect.  [More info in HELP.]
The two lines to be chamfered must lie in the same plane and must not
be parallel.  Try changing the view to one which is not orthogonal to 
the view in which the lines lie.  
.
      # number =  17, severity = 0
Chamfer distance entered is too large.  [More info in HELP.]
The first step in chamfering is to trim each line back from the endpoint
closest to the point on the line where it was picked (where the cursor
was positioned.  It trims the first line picked back the "chamfer distance"
you specify from the intersection point of the two lines.  This error
is caused when the distance specified is greater than the distance from that 
intersection point to the opposite end of the line.  

This error can be caused by picking the lines in the wrong place.  Pick them 
close to the end that will be trimmed.  Re-select the chamfer option to 
retry.
.
      # number =  18, severity = 0
Chamfer angle entered is too large.  [More info in HELP.]
The first step in chamfering is to trim each line back from the endpoint
closest to the point on the line where it was picked (where the cursor
was positioned.  It trims the first line picked back the "chamfer distance"
you specify from the intersection point of the two lines.  It then 
constructs the chamfer line at the "chamfer angle" you specify.  
This error is caused when the angle specified is such that the chamfer
line does not intersect the second line picked.

This error can be caused by picking the lines in the wrong place.  Pick them 
close to the end that will be trimmed.  Re-select the chamfer option to 
retry.
.
      /*                                 circ1.c                            */
      # number =  19, severity = 0
Radius too small.  [More info in HELP.]
The circle radius is smaller than the system's predetermined error
threshold.  Use of a radius smaller than the threshold can introduce
errors and is not permitted. Enter a larger radius.
.
      # number =  20, severity = 0
Center and circumference points identical.  [More info in HELP.]
This error can be caused by snapping of located points (points input
with the (+) cursor) to the grid.  Check that snap resolution set with 
the Define Grid Icon in the Axis/Grid subsystem is correct.
.
      # number =  21, severity = 0
Warning: point will be projected onto plane of the arc.  [More info in HELP.]
The endpoint entered does not lie in the plane of the arc, which is 
determined by the start point and the z axis of the working plane.  This
message is a warning that the point entered will be projected onto the 
plane of the arc to determine the endpoint of the arc.
.
      # number =  22, severity = 0
Point on radius same point as circle center.  [More info in HELP.]
This error can be caused by snapping of located points (points input
with the (+) cursor) to the grid.  Check that snap resolution set with 
the Define Grid Icon in the Axis/Grid subsystem is correct.
.
      # number =  23, severity = 0
Wrong type picked; pick a line, circle, or circular arc.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
    only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked.
    'a' key - only an arc or a circle will be picked.

  - zooming in closer to the portion of the view where the pick will 
    occur, or changing to an oblique view.
.
      # number =  24, severity = 0
Points are collinear.  [More info in HELP.]
The middle point of the three points used to define the arc or circle lies on
an imaginary line connecting the other two points.
.
      /*                                  circ2.c                        */
      # number =  25, severity = 0
Center identical to first point.  [More info in HELP.]
This error can be caused by snapping of located points (points input
with the (+) cursor) to the grid.  Check that snap resolution set with 
the Define Grid Icon in the Axis/Grid subsystem is correct.
.
      # number =  26, severity = 0
Center identical to second point.  [More info in HELP.]
This error can be caused by snapping of located points (points input
with the (+) cursor) to the grid.  Check that snap resolution set with 
the Define Grid Icon in the Axis/Grid subsystem is correct.
.
      # number =  27, severity = 0
Length of arc would be too small.  [More info in HELP.]
The length of the arc is smaller than the system's predetermined error
threshold.  Use of an arc with an arc length smaller than the threshold
can introduce errors and is not permitted. 
.
      # number =  28, severity = 0
Angle not in range -360 to 360 degrees.
.
      # number =  31, severity = 0
Entities not coplanar.  [More info in HELP.]
The two lines to be chamfered must lie in the same plane and must not
be parallel.  Try changing the view to one which is not orthogonal to 
the view in which the lines lie.  
.
      # number =  32, severity = 0
MODEL#32; cir_ttr: illegal entities.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  33, severity = 0
Wrong type picked; pick a line, circle, or circular arc.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
    only the entity which you wish to pick
  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked

  - zooming in closer to the portion of the view where the pick will 
    occur, or changing to an oblique view.
.
      # number =  34, severity = 0
Cannot calculate circle tangent to specified entities.  [More info in HELP.]
Only lines, circular arcs, and circles can be picked for this option.
.
      # number =  35, severity = 0
Same entity picked more than once.  [More info in HELP.]
You must pick a different entity for each prompt.  

The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
    only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked

  - zooming in closer to the portion of the view where the pick will 
    occur, or changing to an oblique view.
.
      # number =  36, severity = 0
Circle radius is too small.
The circle radius is smaller than the system's predetermined error
threshold.  Use of a radius smaller than the threshold can introduce
errors and is not permitted. 
.
/*                                     compcrv.c                        */
      # number = 37, severity = 0
Curves to be composited are not connected.  [More info in HELP.]
In order for the composite process to be successful, the following 
conditions must be met:

  - The curves must be selected in the sequence in which they are 
	   connected.  You must either pick them manually in the proper 
	   sequence, or you must use Chain Select in the Select subsystem. 
	   You CANNOT use In Area, etc., to select them as they will not 
	   necessarily be selected in the proper sequence.  
  
  - The curves must be connected endpoint to endpoint within the 
	   error tolerance of the system (a very small number).  To determine
	   where a point of discontinuity that is not readily visable exists, 
	   it is suggested that you initiate a chain select and observe the last 
	   entity that is selected.  The point of discontinuity will be at the 
	   endpoint of that curve.  
.
      # number =  38, severity = 0
Selected curve not part of a composite.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
    only the entity which you wish to pick
  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked

  - zooming in closer to the portion of the view where the pick will 
    occur, or changing to an oblique view.
.
      # number =  39, severity = 0
Dissolve not implemented for this entity type: %d
.
      # number =  40, severity = 0
Illegal entity for composite curve.  [More info in HELP.]
Curves of any type, including splines, can be composited;  drafting
entities, symbols, etc. cannot be composited.
.
      /*                                    dasupt.c                        */
      # number =  41, severity = 3
MODEL#41; get_endpts: illegal relation %d.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  42, severity = 3
MODEL#42; d_pdas: illegal type specification.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  43, severity = 0
MODEL#43; d_pdas: illegal entity picked. 
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  44, severity = 0
MODEL#44; d_pldas: illegal type specification.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  45, severity = 0
MODEL#45; d_pldas: illegal entity picked.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  46, severity = 3
MODEL#46; dsegtomtid: illegal display segment id 0x=%x.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  47, severity = 0
Illegal edge picked for current operation.  [More info in HELP.]
This particular edge of solid may not be used for current operation.
.
      /*                                          dba.c                    */
      # number =  48, severity = 3
MODEL#48; freegeom, illegal relation.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  49, severity = 3
Wrong entity type picked.  You must pick a geometric entity.  [See HELP.]
You can only pick geometric entities to answer this prompt.  You cannot
pick text, drafting or any other non-geometric entities.  

The system may be picking an entity other than the one you intend.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur.
.
      /*                                      evcrv.c                      */
      # number =  50, severity = 0
MODEL#50; illegal evaluation request.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  51, severity = 3
MODEL#51; evcrv, illegal curve relation.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      /*                                       geom.c                      */
      # number =  52, severity = 3
MODEL#52; init_relations: illegal relation %d.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  53, severity = 0
MODEL#53; p_geometry: undefined relation type %d.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  54, severity = 3
MODEL#54; drw_geometry: undefined entity %d.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      /*                                      line.c                        */
      # number =  55, severity = 0
Start and endpoints identical.  [More info in HELP.]
This error can be caused by snapping of located points (points input
with the (+) cursor) to the grid.  Check that snap resolution set with 
the Define Grid Icon in the Axis/Grid subsystem is correct.
.
      # number =  56, severity = 0
Wrong entity type picked; you must pick a curve.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
    only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked.

  - zooming in closer to the portion of the view where the pick will 
    occur, or changing to an oblique view.
.
      # number =  57, severity = 0
Point entered does not lie in the plane of the circle.  [More info in HELP.]
The point must lie in the same plane as the circle.  Try changing the 
view to one which is not orthogonal to the view in which the circle lies
so you can be sure you are entering a point that is in the same plane.
.
      # number =  58, severity = 0
The point entered lies inside the circle.
.
		# number =	59, severity = 0
The new vertex point does not lie in the same plane as the polygon.
.
      # number =  60, severity = 0
Same curve picked both times.  Pick two different curves.  [See HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
    only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked.

  - zooming in closer to the portion of the view where the pick will 
    occur, or changing to an oblique view.
.
		# number = 61, severity = 0
Wrong entity type picked; you must pick a circle.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
    only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked.

  - zooming in closer to the portion of the view where the pick will 
    occur, or changing to an oblique view.
.
      # number =  62, severity = 0
The circles picked do not lie in the same plane.
.
      # number =  64, severity = 0
One circle lies entirely inside the other.
.
      # number =  66, severity = 0
Distance is too small.
The length of the line that to be constructed would be smaller than the
system's predetermined error threshold.  Construction of a line smaller
than the threshold can introduce errors and is not permitted. 
.
      # number =  67, severity = 0
Wrong entity type picked.  You must pick a line.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
	 only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
	 'l' key - only a line will be picked
	 'a' key - only an arc or a circle will be picked.

	 - zooming in closer to the portion of the view where the pick will 
ccur.
.
      # number =  68, severity = 0
Line is parallel to working plane normal.  [More info in HELP.]
When the point entered lies on the line picked, the new line is con-
structed parallel to the working plane.  The construction angle cannot
be calculated when the line picked is parallel to the working plane 
normal.  If you wish the line to be drawn in the working plane at an 
angle to the point where the picked line intersects the working plane,
it is suggested that you construct a line using Line Perpendicular from 
the picked line to a point on the working plane x axis and then use Line
at an Angle to construct the desired line.
.
      # number =  69, severity = 0
Enter a non-zero angle > -180 degrees and < 180 degrees [More info in HELP.]
CASE 1: The point through which the line will pass does not lie on the
line: 

the indicated range is sufficient for all possible lines.

CASE 2: The point though which the line will pass lies on the picked
line:

the new line will be constructed on the side of the existing line to
which the cursor lies when it is picked (as with a positive distance
with Line Parallel to a Line). A negative angle constructs a line to
the opposite side.  Therefore angles outside the indicated range are
not necessary.  
.
      # number =  70, severity = 0
Angle must be greater than zero.  [More info in HELP.]
Use Line Parallel Through a Point to create a line parallel to the 
picked line.
.
      /*                                         math.c                  */
      # number =  71, severity = 0
Autotrimmed curve would be to small.  Curve will not be trimmed. [See HELP.]
This error will occur when you are filleting in autotrim mode and the system
finds that the length of one  of the curves would be zero or very small if
trimmed to join the fillet arc.  For example, filleting two perpendicular lines
will a radius of 1 in would cause this error if either line were 1 in long.  

The system will leave the line untrimmed.
.
      /*                                         math1.c                 */
      # number =  72, severity = 0
 MODEL#72;coplanar: test not implemented for this entit.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number =  73, severity = 0
Cannot intersect line with curve %d
.
      # number =  74, severity = 0
Cannot intersect circle with curve %d
.
      # number =  75, severity = 0
Cannot intersect curves.
.
      /*                                        pt.c                   */
      # number =  76, severity = 0
Zero vector (0,0,0) cannot be used as a delta vector.
.
      # number =  77, severity = 0
Center and start points identical.  [More info in HELP.]
This error can be caused by snapping of located points (points input
with the (+) cursor) to the grid.  Check that snap resolution set with 
the Define Grid Icon in the Axis/Grid subsystem is correct.
.
      # number =  78, severity = 0
Radius of circular array of points too small.  [More info in HELP.]
The circle radius is smaller than the system's predetermined error
threshold.  Use of a radius smaller than the threshold can introduce
errors and is not permitted. Enter a larger radius.
.
      # number =  79, severity = 0
Same curve picked twice.  You must pick different curves.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
	 only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
	 'l' key - only a line will be picked
	 'a' key - only an arc or a circle will be picked.

	 - zooming in closer to the portion of the view where the pick will 
ccur.
.
      # number =  80, severity = 0
The selected entities do not intersect.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
	 only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked.

  - zooming in closer to the portion of the view where the pick will 
    occur, or changing to an oblique view.
.
      /*                                       setcpl.c                    */
      # number =  81, severity = 0
Magnitude of the vector entered is too small.  [More info in HELP.]
The length of vector is smaller than the system's predetermined error
threshold.  Use of a vector with length smaller than the threshold can
introduce errors and is not permitted. 
.
      /*                                       solc31lam.c                 */
      # number =  82, severity = 0
Only composite curves consisting of lines and arcs may be swept into solids.
.
      # number =  84, severity = 0
Only planar composite curves can be swept into solids.
.
      # number =  85, severity = 0
Rotation angle is essentially zero; enter larger angle.
.
      # number =  86, severity = 0
Rotation vector is essentially zero; enter longer vector.
.
      # number =  89, severity = 0
Only closed, planar composite curves can be lifted into solids.
.
      # number =  90, severity = 0
Direction vector is essentially zero; enter longer vector.
.
      /*                                      solcmdin.c                    */
      # number =  91, severity = 0
Block dimensions cannot be essentially zero.
.
      # number =  92, severity = 0
Both radii cannot be zero.
.
      # number =  93, severity = 0
Circle centers are identical.
.
      # number =  94, severity = 0
Pick two solids. 
.
      # number =  95, severity = 0
Picked entities are identical.
.
      # number =  97, severity = 0
Pick a solid.
.
      /*                                tran_rot2.c                         */
      # number =  98, severity = 0
Translation not implemented for this entity
.
      # number =  99, severity = 0
Rotation not implemented for this entity
.
      # number =  100, severity = 0
Copy not implemented for this entity
.
      /*                                trim.c                             */
      # number =  101, severity = 0
Trimmed line would be too small.  [More info in HELP.]
The length of the line after trimming would be smaller than the system's
predetermined error threshold.  Construction of lines shorter than the
threshold can introduce errors and is not permitted.
.
      # number =  102, severity = 0
Can not trim to this curve
.
      # number =  103, severity = 0
Trim entity and trim to element(s) are the same.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
	 only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked.

  - zooming in closer to the portion of the view where the pick will 
.
      # number =  104, severity = 0
Composite curves cannot be trimmed.  [More info in HELP.]
The entity selected for trimming is a composite curve and cannot be 
trimmed.  To trim an element of a composite, use the Dissolve icon on 
the Composite sub-menu of the Wire-Frame Modeling menu to change the 
composite back to ordinary curves.  Remember that composites can contain
composties, which would also have to be dissolved.
.
      # number =  105, severity = 0
Illegal trim element.
.
      # number =  106, severity = 0
Filleted line too small to be trimmed.  [More info in HELP.]
The fillet radius specified was such that the endpoint of the fillet arc
is very close to the endpoint of the one of the fillet lines. 
The line that would result from trimming the filleted line back to
the arc would be smaller than the system's predetermined error
threshold.  Construction of a line smaller than the threshold can
introduce errors and is not permitted.

The curve will be left un-trimmed.

This problem can be resolved by deleting the fillet arc and re-filleting
with a slightly smaller radius.
.
      # number =  107, severity = 0
NOTE: circles cannot be auto-trimmed when filleted.  [More info in HELP.]
Since circles cannot be trimmed (they can only be mid-trimmed) automatic
trimming will not be done on filleted circles.  Use the Mid-Trim icon in
the Move/Edit subsystem to manually trim the circle to the arc.
.
      # number =  108, severity = 0
Filleted arc too small to be trimmed.  [More info in HELP.]
The fillet radius specified was such that the endpoint of the new fillet
arc is very close to the endpoint of the one of the filleted arcs. 
The arc that would result from trimming the new arc back to
the filleted arc would be smaller than the system's predetermined error
threshold.  Construction of an arc smaller than the threshold can
introduce errors and is not permitted.
.
      # number =  109, severity = 0
Cannot auto trim entity.  [More info in HELP.]
.
      # number =  110, severity = 0
Only curves other than composite curves can be trimmed.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
	 only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked.

  - zooming in closer to the portion of the view where the pick will 
.
      # number =  111, severity = 0
Arc resulting from trim would be too small.  [More info in HELP.]
The resulting arc would have a length shorter than the system's
predetermined error threshold.  Construction of an arc with an arc 
ength shorter than the threshold can introduce errors and is not permitted. 
.
      # number =  112, severity = 0
The curve does not/cannot intersect the trim-to curve(s).  [See HELP.]
The curves must all lie in the same plane.  Lines must not be parallel.
If the curves appear on the display to intersect, try changing the view
to one which is oblique to the plane in which the curves lie to insure
that they are in fact in the same plane.  If they still appear to 
intersect, try the operation again in the new view to insure that the 
desired entities are being picked.  
.
      # number =  113, severity = 0
The entities intersect on the entity to be trimmed.  [More info in HELP.]
The mid-trim operation was not completed because the two entities 
selected to trim to intersect at a point that lies on the curve to be 
trimmed.
.
      # number =  114, severity = 0
Cannot trim line.
.
      # number =  115, severity = 0
Trimmed line too small for trim.
.
      # number =  116, severity = 0
Circle in ambiguous viewplane; cannot trim.
.
      /*                                   vwcntl.c                        */
      # number =  117, severity = 0
Magnification factor must be greater than zero.
.
      /*                                  compcrv.c                        */
      # number = 118, severity = 0
Entity picked is not a composite curve.  [More info in HELP.]
This option requires that you pick a composite curve.
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

  - using care when positioning the cursor.  The cursor should touch 
	 only the entity which you wish to pick.

  - using a selective pick trigger key instead of the mouse button
    'l' key - only a line will be picked
    'a' key - only an arc or a circle will be picked.

  - zooming in closer to the portion of the view where the pick will 
    occur, or changing to an oblique view.
.
      /*                                  evcrv.c                          */
      # number = 119, severity = 3
MODEL#119; rotatept: direction vector (rotvec) too small.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number = 120, severity = 0
Number of points entered (%d) exceeds maximum allowed (%d) 
.
      # number = 121, severity = 0
Y axis cannot be parallel to normal.
.
      # number = 122, severity = 0
New control point not accepted; too close to previous control point.
.
      # number = 123, severity = 0
New ctl point not accepted; spline would not have continuous 1st derivative.
.
      # number = 124, severity = 0
ROMULUS failure; attempting to recover; press any key to continue.
.
      # number = 125, severity = 0
Pick a solid.
.
      # number = 126, severity = 0
Cannot split a composite curve containing a single constituent
.
      # number = 127, severity = 0
Cannot split a composite curve at the first constituent
.
      # number = 128, severity = 0
Mirroring not implemented for this entity.
.
      # number = 129, severity = 0
Scaling not implemented for this entity.
.
      # number = 130, severity = 0
Entity picked was not a point.  You must enter a point. [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur, or changing to an oblique view.
.
      # number = 131, severity = 0
Entity picked was not a line.  You must enter a line.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur, or changing to an oblique view.
.
      # number = 132, severity = 0
Entity picked was not a circle.  You must pick a circle.  [See HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur, or changing to an oblique view.
.
      # number = 133, severity = 5
MODEL#133, um_tf06_bsplcrv: error on update_geom_var.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
# number = 134, severity = 0
Not an arc; break circle first.
.
      # number = 135, severity = 0
Arc with endpoints cannot be broken.
.
      # number = 136, severity = 5
MODEL#133, um_sc06_scalbsplcrv: error on update_geom_var.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number = 137, severity = 0
Yes, there is interference between these two bodies.
.
      # number = 138, severity = 0
No interference between these two bodies.
.
		# number = 139, severity = 5
um_get_all_geom: not enough space for retrieval of relation %d
.
		# number = 140, severity = 5
umi_setup_evaluator: undefined relation of %d
.
		# number = 141, severity = 5
umi_getarclen: undefined relation of %d
.
		# number = 142, severity = 0
Entity picked was not a curve.  You must pick a curve.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur, or changing to an oblique view.
.
		# number = 143, severity = 0
MODEL#143 An extra %d bytes have been allocated to draw surface(s).
.
		# number = 144, severity = 0
MODEL#144 From um_del_geometry, undefined relation of %d.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number = 145, severity = 5
MODEL#145, font %s symbol %s not define.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number = 146, severity = 0
Current alignment (horiz,vert) = (%s,%s)
.
      # number = 147, severity = 0
Current %s = %s
.
      # number = 148, severity = 5
MODEL#148, font %s file symbol %s invalid forma.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number = 149, severity = 5
MODEL#149, font %s file symbol %s not define.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number = 150, severity = 0
Solids picked didn't intersect.
.
      # number = 151, severity = 0
Sectioning object doesn't intersect the body.
.
      # number = 152, severity = 0
Picked edges do not lie on same face.
.
      # number = 153, severity = 0
You've picked the same edge twice.
.
      # number = 154, severity = 0
A unique face is not defined by this edge.
.
		# number = 155, severity = 0
um_drwsrf_dispatcher: illegal relation of %d.
.
		# number = 156, severity = 0
um_getparampts: illegal relation for a defining curve to draw a surface.
.
		# number = 157, severity = 0
um_create_geom: entity not stored, rel_num = %d
.
		# number = 158, severity = 0
um_create_geom: can't store attributes, unknown class for relation: %d
.
		# number = 159, severity = 1
um_create_geom: can't store attributes, no class for relation: %d
.
		# number = 160, severity = 1
um_update_geom: can't update the entity that has key = %d
.
		# number = 161, severity = 1
um_update_geom: can't update transform for entity that has key = %d
.
		# number = 162, severity = 1
um_create_geom: can't store transform for entity that has key = %d
.
		# number = 163, severity = 1
um_get_transformation: transformation not retrieved for entity with key = %d
.
		# number = 164, severity = 1
um_transform_evaluator: undefined class of %d for entity with key = %d
.
		# number = 165, severity = 1
um_update_transformation: transformation not updated for entity with key = %d
.
      # number = 166, severity = 5
cp7_rbsplcrv: error on um_create_geom.
.
      # number = 167, severity = 1
Invalid color
.
		# number = 168, severity = 5
um_init_rel: UNIBASE relation creation for entity relation %d failed
.
		# number = 169, severity = 5
um_init_rel: UNIBASE relation creation for transformation relation %d failed
.
		# number = 170, severity = 5
um_init_rel: UNIBASE relation creation for attribute relation %d failed
.
		# number = 171, severity = 1
Fillet/chamfer on this edge(s) would result in bad topology.
.
		# number = 172, severity = 1
um_evsrf: surface relation %d not yet implemented.
.
		# number = 173, severity = 1
Plane does not intersect the solid.
.
		# number = 174, severity = 1
Polygon must have at least three vertices.
.
		# number = 175, severity = 1
Not a filled polygon region.
.
		# number = 176, severity = 1
Illegal geometry, body not created.
.
      /*                                  muconic.c                        */
		# number = 177, severity = 0
Ellipse with the given axes lengths would be too narrow.  [See HELP.]
The smaller of the two axes is shorter than the system's predetermined error
threshold.  Construction of an ellipse with an axis smaller than the
threshold can introduce errors and is not permitted. 
.
		# number = 178, severity = 0
Axis length must be shorter.
.
		# number = 179, severity = 0
Axis length must be longer
.
		# number = 180, severity = 0
Vertex too close to center
.
		# number = 181, severity = 0
Range too small
.
		# number = 182, severity = 0
Foci are too close together
.
		# number = 183, severity = 0
Point is too close to axis
.
		# number = 184, severity = 0
Focal length is too short
.
		# number = 185, severity = 0
umu_projcirc: error projecting circle  %d
.
      # number = 186, severity = 0
Entity picked was not an ellipse.  You must pick an ellipse.  [See HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur, or changing to an oblique view.
.
      # number = 187, severity = 0
Hyperbola was not picked; re-enter.
.
      # number = 188, severity = 0
Parabola was not picked; re-enter.
.
      # number = 189, severity = 0
Conic was not picked; re-enter.
.
		# number = 190, severity = 0
Curvature value is too small
.
		# number = 191, severity = 0
No conic satisfies given conditions
.
		# number = 192, severity = 0
Endpoints must be distinct
.
		# number = 193, severity = 0
Can't get endpoint conditions from picked entity
.
		# number = 194, severity = 0
Vector condition rejected: perpendicular to working plane
.
		# number = 195, severity = 0
Cannot modify closed ellipse
.
		# number = 196, severity = 0
Entity with key %d would not report its span
.
		# number = 197, severity = 0
Dimension of space spanned by entities is %d
.
		# number = 198, severity = 0
Pen number must be in the range 1 - 256.
.
		# number = 199, severity = 0
There is no view named %s in the database.
.
		# number = 200, severity = 0
There is already a view named %s in the database.
.
		# number = 201, severity = 0
There is no sheet named %s in the database.  [More info in HELP.]
.
		# number = 202, severity = 0
sheet %s already exists in database
.
		# number = 203, severity = 0
There is no drawing named %s in the database.  [More info in HELP.]
You can see the names of the drawings that have been created by 
selecting the List Drawings icon.
.
		# number = 204, severity = 0
There is already a drawing named %s in the database.  [More info in HELP.]
You can see the names of the drawings that have been created by 
selecting the List Drawings icon.
.
		# number = 205, severity = 0
Layer number must be in the range 0 to 9999
.
		# number = 206, severity = 0
You cannot change the normal vector in a reference view
.
		# number = 207, severity = 0
Reference views cannot be saved
.
		# number = 208, severity = 0
There is no view named %s in the database.  [More info in HELP.]
You can see the names of the views that have been created by 
selecting the List Views icon.
.
		# number = 209, severity = 0
Normal vector cannot be parallel to the up vector.  [More info in HELP.]
The following procedure can often be a convenient way to establish
the up vector for the view:

  - position the cursor to the Up Vector field on the form.
  - press 'pick' on the keyboard or use the middle mouse button to 
    change the input mode to pick.
  - position the pick cursor to the BOTTOM of a line that points
    in the direction you wish to become the 'up' direction.
  - trigger the pick with the 'v' key on the keyboard instead of the
    mouse button.

The form will then come back up and the Up Vector field will contain
the necessary vector.

.
		# number = 210, severity = 0
You have not assigned that name to a layer.
.
		# number = 211, severity = 0
Cannot delete a reference view
.
		# number = 212, severity = 0
Cannot delete a displayed view
.
		# number = 213, severity = 0
MODEL#213: error in retrieving relation number for key=%d.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
		# number = 214, severity = 0
CONFLICT! new layer number %d deleted
.
		# number = 215, severity = 0
CONFLICT! new view name %s deleted
.
		# number = 216, severity = 0
The lines are not coplanar.  [More info in HELP.]
The three lines selected to define the curve must all lie in the same
plane.  If the curves appear to intersect on the display, try changing
the view to one which is oblique to the plane in which the curves lie to
insure that they are in fact in the same plane.  If they still appear to 
intersect, try the operation again in the new view to insure that the 
desired entities are being picked.  
.
		# number = 217, severity = 0
The curve cannot be constructed because all three lines are parallel.  
.
		# number = 218, severity = 0
The curve cannot be constructed because two of the lines are collinear.
.
		# number = 219, severity = 0
The curve cannot be constructed - three of the lines intersect at one point.
.
		# number = 220, severity = 0
The location of the circle is ambiguous from the picked locations
.
		# number = 221, severity = 0
No surface picked, (%s).
.
		# number = 222, severity = 0
Can't copy default attributes, (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
		# number = 223, severity = 0
Invalid modeling relation: %d, (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
		# number = 224, severity = 0
No active drawing
.
		# number = 225, severity = 0
Enitity picked was not a curve. You must pick a curve.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur, or changing to an oblique view.
.
		# number = 226, severity = 0
Cannot multifillet intersections of arcs with arcs.  [More info in HELP.]
The Multifillet option does not support filleting between two arcs.  You
can fillet between arcs with the Single Fillet option.
.
		# number = 227, severity = 0
Scale factor must be positive.  [More info in HELP.]
The valid ranges of scale factors are:
     
     - less than one but greater than zero to scale DOWN

     - Greater than one to scale UP.
.
		# number = 228, severity = 0
Illegal screen name %s.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
		# number = 229, severity = 0
No form file for screen name %s.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
		# number = 230, severity = 0
Drawing capacity exceeded.  [More info in HELP.]
The capacity of the drawing management system for a single drawing
has been exceeded.  This error should not occur during normal system
operation.  Contact NCL Customer Support.
.
		# number = 231, severity = 0
Locate cursor (+) cannot be used when const. plane perp. to view.  [See HELP.]
You attempted to use the locate cursor to enter a point in a viewport
in which the view normal vector is perpendicular to the construction 
plane normal vector.  This is not allowed because the system must 
project the located screen position onto the construction plane.  
You can

  - change the view or use a different view in another viewport, or
  - change the construction plane.
.
		# number = 232, severity = 0
Grid spacing is smaller that 1/20 of aperture.  [More info in HELP.]
The system will not let you set the grid spacing smaller than 1/20
of the viewport aperture.  This prevents you from accidentally drawing 
a grid with spacing so fine that it takes several minutes to draw.  You
will see the aperture displayed at the bottom of the Activate Grid 
form.  Note that the snap resolution can be set as small as desired.  
.
		# number = 234, severity = 0
drawing archive file %s does not exist
.
		# number = 235, severity = 0
don't have read access to archive file %s
.
		# number = 236, severity = 0
don't have write access to archive file %s
.
		# number = 237, severity = 0
no methods available to modify this entity
.
		# number = 238, severity = 0
entities not connected
.
		# number = 239, severity = 0
Fillet radius is too large.
The fillet arc does not intersect the line segements. Enter a smaller radius.
.
		# number = 240, severity = 0
Line and arc are tangent, cannot fillet
.
		# number = 241, severity = 0
Illegal (sub)curve to offset of relation type, %d  (%s).
.
		#number = 242, severity = 0
Offset direction vector is too small, try again,  (%s).
.
		# number = 243, severity = 0
Can't determine how to offset, try again  (%s).
.
		# number = 244, severity = 0
Can't determine offset direction for subcurve curve with key: %d,  (%s).
.
		# number = 245, severity = 0
Invalid trim status, (%s).
.
		# number = 246, severity = 0
Can't determine offset direction for curve, %d,  (%s).
.
		# number = 247, severity = 0
Can't determine which side to offset a subcurve of composite,  (%s).
.
		# number = 248, severity = 0 
Intersector failed on entities with relation numbers of: %d and %d,  (%s).
.
		# number = 249, severity = 0
No points found from which to trace offset  (%s).
.
		# number = 250, severity = 0
%d is too many intersections between offset subcurves,  (%s).
.
		# number = 251, severity = 0
Can't find next intersection on offset,  (%s).
.
		# number = 252, severity = 0
No offset curve found to trim/extend between current intersections,  (%s).
.
		# number = 253, severity = 0
Trimmed offset curve of entity, %d, is too small,  (%s).
.
		# number = 254, severity = 0
Can't find next intersection of offset,  (%s).
.
		# number = 255, severity = 0
Composite curve picked is not planar  (%s).
.
		# number = 256, severity = 0
Error, curve and picked location are not planar,  (%s).
.
		# number = 257, severity = 0
Error, can't project pick onto working plane,  (%s).
.
		# number = 258, severity = 0
Can't get point beyond intersection of offset subcurves (%s).
.
		# number = 259, severity = 0
Can't get point prior to intersection of offset subcurves (%s).
.
		# number = 260, severity = 0
Solids not available in this version
.
		# number = 261, severity = 5
Illegal relation of %d,  (%s).
.
		# number = 262, severity = 0
Can't offset curve with given direction and distance (%s).
.
		# number = 263, severity = 0
Can't offset picked subcurve, reset minimum offset distance,  (%s).
.
		# number = 264, severity = 0
Can't determine offset direction for subcurve of composite (%s).
. 
		# number = 265, severity = 0
No intersections found from which to construct offset.
.
		# number = 266, severity = 0
Warning there may be large numerical inaccuracies in the offset, (%s).
.
		# number = 267, severity = 0
Warning, an apparently small offset curve may not be included in the offset.
.
		# number = 268, severity = 0
Can't trim the end of the curve which has key:%d, (%s).
.
		# number = 269, severity = 0
Can't determine the 'in' and 'out' curves at an intersection, (%s).
.
		# number = 270, severity = 0
Lift direction must be perpendicular to plane of composite.
.
		# number = 271, severity = 0
Cannot load a displayed view.
.
		# number = 272, severity = 0
Grid space is too small. Please redefine it.
.
		# number = 273, severity = 5
Out of digs segment space.  
Attempted to create more segments than the digs graphics package 
allows.  The number of segments allowed is the  compile time 
constant 'UG_MAXSEGNO' defined in gtbldef.h. 

Please delete entities or exit.
.
		# number = 274, severity = 0
Delta angle would result in closed circle, not an arc.
.
		# number = 275, severity = 0
Point not on curve.
.
		# number = 276, severity = 0
Can't place 'drawing view' on a drawing.
.
		# number = 277, severity = 0
You must pick a line/curve. Retry.
.
		# number = 278, severity = 0
Point and the curve are not on the same plane.
.
		# number = 279, severity = 0
Can't create %s from given constraints.
.
		# number = 280, severity = 0
Unable to calculate line tangent to curve through point.
.
		# number = 281, severity = 0
Unable to calculate line tangent to these two curves.
.
		# number = 282, severity = 0
Unable to trim this curve.
.
		# number = 283, severity = 5
Can't clear save/load bit map.
.
		# number = 284, severity = 0
Polygon picked is not planar.
.
		# number = 285, severity = 0
Polygon picked is not in work plane axes x-y plane.
.
		# number = 286, severity = 0
Composite curve picked is not planar.
.
		# number = 287, severity = 0
Composite curve picked is not in work plane axes x-y plane.
.
		# number = 288, severity = 0
Composite curve picked is not closed.
.
		# number = 289, severity = 0
Unsupported curve for 2-D analysis in composite curve.
.
		# number = 290, severity = 0
Outer radius is smaller than the inner radius.
.
		# number = 291, severity = 0
Outer radius is almost the same as the inner radius.

The outer radius - the inner radius is smaller than the preset
tolerances set in Romulus.
.
		# number = 292, severity = 0
Error: can't selectively save a drawing.
.
		# number = 293, severity = 0
Error occurred in attempting to selectively save solids.
.
		# number = 294, severity = 0
Error occurred in attempting to selectively save splines and/or surfaces.
.
		# number = 295, severity = 0
Can't generate profile for %s(s).
.
		# number = 296, severity = 0
The solids do not fit.
.
		# number = 297, severity = 0
The solids DO NOT FIT at the indicated points.
.
		# number = 298, severity = 0
Error: The curves picked must constitute a simple closed curve.
.
		# number = 299, severity = 0
Error: Entity picked is a solid but not on the cross section.
.
		# number = 300, severity = 0
Error: Edge picked is not on the first cross section body picked.
.
		# number = 301, severity = 0
Can't get 2 unique points, one on 1st cross section, one on it's offset.
.
		# number = 302, severity = 0
The solids do not fit; the original solid cross sections intersect.
.
		# number = 303, severity = 0
The solids do not fit; the 1st cross section intersects the 2nd offset.
.
		# number = 304, severity = 0
Tolerance value is too small; must be greater than %g.
.
		# number = 305, severity = 0
The solids do not fit; the 1st offset intersects the 2nd cross section.
.
		# number = 306, severity = 0
The solids do not fit; the 1st offset intersects the 2nd offset.
.
		# number = 307, severity = 0
The solids do NOT fit.
.
		# number = 308, severity = 0
The solids fit.
.
		# number = 309, severity = 0
Cross section edge is neither a line nor a circle.
.
		# number = 310, severity = 0
Can't make closed curve out of picked cross section.
.
		# number = 311, severity = 0
Too many edges in body.
.
		# number = 312, severity = 0
Error: same solid picked twice.
.
		# number = 313, severity = 0
Can't %s solid body.
.
		# number = 314, severity = 0
Error: length of plane must greater than %g.
.
		# number = 315, severity = 0
Error: Subscripted geometry name not allowed.
.
		# number = 316, severity = 0
Error: Selected name is in use and REDEFINE is NO.
.
		# number = 317, severity = 0
Error: Illegal geometry picked (WIREFRAME only).
.
		# number = 318, severity = 0
Error: Illegal geometry picked (Same Type geometry only).
.
/* added by yurong */
		# number = 319, severity = 0
Error: There is no drawing in the data base.
.
/* added by yurong */
		# number = 320, severity = 0
Error: Empty drawing name is not allowed.
.
/* added by Janet*/
		# number = 321, severity = 0
Error: Must pick a shape.
.
/* added by Janet*/
		# number = 322, severity = 0
Error: Unable to find command.  Please select from the list.
.
/* Eduard                                nuci.c                 */
      # number = 323, severity = 0
Entity picked is not appropriate for the circle construction.  
You must pick a point, a line, a circle, or a curve.  
.
/* Eduard                                nuci.c                 */
      # number = 324, severity = 0
Same entity picked twice.  You must pick different entities.  
.
/* Eduard                                nuci.c                 */
      # number = 325, severity = 0
Circle through a point tangent to a curve is not implemented.
.
/* Eduard                                nuci.c                 */
      # number = 326, severity = 0
Circle tangent to two curves is not implemented.
.
/* added by Janet*/
		# number = 327, severity = 0
Cannot intersect a circle and a plane, please try again.
.
/* added for creation of optimal plane*/
		# number = 328, severity = 0
Entity picked was not a curve/surface. You must pick a curve/surface.
.
/*e@@                                                                     */
/*                           MODELING  PROMPT   SECTION                   */
/*                    LAST ASSIGNED PROMPT NUMBER IS: 340                 */
/*                                                                        */
   # prompt section
      /*                        bsplcrv.c                                  */
      # number = 1
inverted
.
      # number = 2
planar
.
      # number = 3
open  
.
      # number = 4
order
.
      # number = 5
control points
.
      /*                                   c_srf.c                         */
      # number = 6
point on rotation axis:
.
      # number = 7
direction vector of axis:
.
      # number = 8
start angle for surface
.
      # number = 9
terminating angle for surface
.
      # number = 10
distance and direction for lift:
.
      # number = 11
distance (from picked line):
.
/*                                 chamfer.c                        */
      # number = 12
distance (along first line):
Distances and lengths can be entered with or without a units 
specification.  If without, the default set in the Environment subystem
will be used (set to inches when the system is initialized).  Valid 
units are: 

	      mm            millimeters
	      cm            centimeters
	      m             meters
	      km            kilometers
	      in            inches
	      ft            feet
	      mi            miles
.
      # number = 13
angle:
Angles can be entered with or without an angular unit specification.
If without, the default set in the Environment subsystem will be used
(set to degrees when the system is initialized).  Valid angular units
are:

	      deg           degrees
	      rad           radians
.
      /*                                 circ1.c                          */
      # number = 14
Arc/circle center:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 15
radius of circle:
Distances and lengths can be entered with or without a units 
specification.  If without, the default set in the Environment subystem
will be used (set to inches when the system is initialized).  Valid 
units are: 

	      mm            millimeters
	      cm            centimeters
	      m             meters
	      km            kilometers
	      in            inches
	      ft            feet
	      mi            miles
.
      # number = 17
point on circumference:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 19
first point on circle/arc:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 20
second point on circle/arc:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 21
third point on circle/arc:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      /*                              circ2.c                              */
      # number = 22
arc center:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 23
first endpoint of arc:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 24
approximate endpoint of arc (to define arc angle):
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 25
arc/circle radius:
Distances and lengths can be entered with or without a units 
specification.  If without, the default set in the Environment subystem
will be used (set to inches when the system is initialized).  Valid 
units are: 

	      mm            millimeters
	      cm            centimeters
	      m             meters
	      km            kilometers
	      in            inches
	      ft            feet
	      mi            miles
.
      /*                             color.c                              */
      # number = 26
pick entity:
.
      /*                             compcrv.c                            */
      # number = 27
pick curves to merge: 
.
      /*                             drwaxis.c                            */
      # number = 28
axis length:
.
      /*                             line.c                               */
      # number = 29
start point of new line:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 30
endpoint of new line:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
			10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
			d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 31
point on line:
.
      # number = 32
point new line goes through:
.
      # number = 33
length of new line:
Distances and lengths can be entered with or without a units 
specification.  If without, the default set in the Environment subystem
will be used (set to inches when the system is initialized).  Valid 
units are: 

	      mm            millimeters
	      cm            centimeters
	      m             meters
	      km            kilometers
	      in            inches
	      ft            feet
	      mi            miles
.
      # number = 34
approximate endpoint:
.
      /*                                pt.c                                */
      # number = 35
coordinates of point:
The required point can be picked, located, or typed in.  The following
are valid forms for text input of coordinates:
         10,20            (Cartesian - x,y)
	      10,20,30         (          - x,y,z)
	      a10,b45,c90      (Spherical - distance, azimuth, elevation.)
	      r10,t45,z10      (Cylindrical - radius, azimuth, s-depth.)

	      d 10,10          (Delta forms - relative to ref. pt.)
         d a10,b90,c45    (etc.)

Appropriate units can be used for any value in a cordinate. 
Examples: 10 cm, 136 mm    a10ft,b30deg,c1.5rad
.
      # number = 36
starting point:
.
      # number = 37
<delta x, delta y, delta z>
.
      # number = 38
number of points in pattern:
.
      # number = 41
angle between points:
Angles can be entered with or without an angular unit specification.
If without, the default set in the Environment subsystem will be used
(set to degrees when the system is initialized).  Valid angular units
are:

	      deg           degrees
	      rad           radians
.
      /*                               setcpl.c                            */
      # number = 43
working plane z-depth:
.
      # number = 44
working plane base point:
.
      # number = 45
working plane z axis:
.
      # number = 46
working plane y axis: 
.
      /*                              solc31lam.c                         */
      # number = 47
rotation vector:
.
      # number = 48
rotation angle:
.
      # number = 49
direction vector: 
.
      /*                             solcmdin.c                           */
      # number = 50
MODEL#50; enter ROMULUS command: 
Error should not occur during normal operation.  Contact NCL Support.
.
      # number = 51
corner point of block:
.
      # number = 52
block dimensions <x,y,z>: 
.
      # number = 54
radius of cylinder: 
.
      # number = 55
start point of cylinder axis:
.
      # number = 56
distance and direction of cylinder axis:
.
      # number = 57
radius of sphere: 
.
      # number = 58
center of sphere:
.
      # number = 59
radius of circle 1: 
.
      # number = 60
center of circle 1:
.
      # number = 61
radius of circle 2: 
.
      # number = 62
center of circle 2:
.
      # number = 63
chamfer distance:
.
      # number = 64
enter fillet radius:
.
      # number = 65
pick connected edges in sequence: 
.
      /*                              solrender.c                         */
      # number = 66
coordinates of light source: 
.
      # number = 67
file name: 
.
      /*                              test.c                              */
      # number = 68
pick entities to delete:
.
      # number = 69
MODEL#69; enter mti.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number = 70
MODEL#70; display mti.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      /*                              tran_rot2.c                        */
      # number = 71
pick entities to translate:
.
      # number = 72
from point: 
.
      # number = 73
to point: 
.
      # number = 74
pick entities to rotate: 
.
      # number = 75
rotation axis point: 
.
      # number = 76
axis direction vector: 
.
      # number = 77
angle of rotation
.
      # number = 78
pick entities to copy/translate:
.
      # number = 79
pick entities to copy/rotate:
.
      # number = 80
rotation direction vector: 
.
      /*                                 trim.c                           */
      # number = 81
Pick intersection of trim entity and trim to element
.
      /*                                 vwcntl.c                         */
      # number = 82
window center:
.
      # number = 83
Indicate lower left corner
.
      # number = 84
Indicate upper right corner
.
      # number = 85
magnification factor: (smaller than 1 displays more of part - zooms out)
.
      # number = 86
view reference point:
.
      # number = 87
view normal vector:
.
      # number = 88
Continue?
.
      # number = 89
Enter control point:
.
      # number = 90
Should control/interpolating points be on the curve?
.
      # number = 91
Pick a solid.
.
      #  number = 92
point on rotation axis:
.
      # number = 93
axis direction vector:
.
      # number = 94
point lying on u and v axis:
.
      # number = 95
u axis direction vector:
.
      # number = 96
v axis direction vector:
.
      # number = 97
Is this the desired direction for ruling?
.
      # number = 98
Pick entities to mirror:
.
      # number = 99
Mirror plane normal:
.
      # number = 100
Point on mirror plane:
.
      # number = 101
Pick entities to scale:
.
      # number = 102
Point for scale origin:
.
      # number = 103
Scale factor:
.
      # number = 104
Pick point to modify:
.
      # number = 105
New point coordinates:
.
      # number = 106
New endpoint of line:
.
      # number = 107
Pick circle to be modified:
.
      # number = 108
New center point:
.
      # number = 109
New radius:
.
      # number = 110
New endpoint of arc:
.
      # number = 111
Pick arc to be closed:
.
      # number = 112
New angle for circle:
.
		# number = 113
Enter a vector to be rotated:
.
		# number = 114
Enter direction of vector after rotation:
.
		# number = 115
Enter rotation axis direction vector:
.
      # number = 116
ROMULUS failure; attempting to recover; press any key to continue.
.
		# number = 117
Enter interpolation point:
.
      # number = 118
Location of text:
.
      # number = 119
Text string:
.
      # number = 120
Font name:
.
      # number = 121
Character expansion:
.
      # number = 122
Character height:
.
      # number = 123
Character spacing:
.
      # number = 124
Character rotate angle:
.
      # number = 125
Planar hatching pitch value:
.
 # number = 126, severity = 0
Pick first solid for interference check.
.
 # number = 127, severity = 0
Pick second solid for interference check.
.
 # number = 128, severity = 0
Combine this solid
.
 # number = 129, severity = 0
With this solid.
.
 # number = 130, severity = 0
Pick first solid body
.
 # number = 131, severity = 0
Pick second solid body.
.
 # number = 132, severity = 0
Pick composite curve to split.
.
 # number = 133, severity = 0
Pick first constituent of second composite.
.
 # number = 134, severity = 0
Pick composite curve to dissolve.
.
 # number = 135, severity = 0
Pick curve to rotate.
.
 # number = 136, severity = 0
Pick curve to lift into a surface.
.
 # number = 137, severity = 0
Pick a line.
.
 # number = 138, severity = 0
Pick first entity to intersect.
.
 # number = 139, severity = 0
Pick one or more entities to intersect with first.
.
 # number = 140, severity = 0
Pick first surface.
.
 # number = 141, severity = 0
Pick second surface.
.
 # number = 142, severity = 0
Unite this solid
.
 # number = 143, severity = 0
to this solid.
.
 # number = 144, severity = 0
Pick solid.
.
 # number = 145, severity = 0
Subtract this solid
.
 # number = 146, severity = 0
from this solid.
.
 # number = 147, severity = 0
Pick a connected sequence of edges.
.
 # number = 148, severity = 0
Pick a composite curve.
.
 # number = 149, severity = 0
Pick solid to cross section.
.
 # number = 150, severity = 0
Pick the first surface edge.
.
 # number = 151, severity = 0
Pick the direction to chain surface edges.
.
 # number = 152, severity = 0
Pick the last surface edge.
.
 # number = 153, severity = 0
Pick entity.
.
 # number = 154, severity = 0
Pick first line.
.
 # number = 155, severity = 0
Pick second line.
.
 # number = 156, severity = 0
Pick tangent line/circle.
.
 # number = 157, severity = 0
Indicate first curve.
.
 # number = 158, severity = 0
Indicate second curve.
.
 # number = 159, severity = 0
Pick a curve.
.
 # number = 160, severity = 0
Pick first curve.
.
 # number = 161, severity = 0
Pick line near end to be moved.
.
 # number = 162, severity = 0
Pick circle at point to be broken.
.
 # number = 163, severity = 0
Pick arc near end to be moved.
.
 # number = 164, severity = 0
Pick entity to trim/extend on side to be trimmed/extended
.
 # number = 165, severity = 0
Pick element to trim/extend to
.
 # number = 166, severity = 0
Pick part of entity to keep 
.
 # number = 167, severity = 0
Pick first element to trim to
.
 # number = 168, severity = 0
Pick second element to trim to
.
 # number = 169, severity = 0
Pick first curve: ruling direction starts at nearest endpoint.
.
 # number = 170, severity = 0
Pick second curve: ruling direction starts at nearest endpoint.
.
 # number = 171, severity = 0
Number of u paths.
.
 # number = 172, severity = 0
Number of v paths.
.
 # number = 173, severity = 0
Number of points per u path.
.
 # number = 174, severity = 0
Number of points per v path.
.
 # number = 175, severity = 0
Enter new knot value.
.
 # number = 176, severity = 0
Desired multiplicity.
.
 # number = 177, severity = 0
Origin of cross section plane.
.
 # number = 178, severity = 0
Normal vector of cross section plane.
.
 # number = 179, severity = 0
X length of plane.
.
 # number = 180, severity = 0
Y length of plane.
.
 # number = 181, severity = 0
Enter point defining plane.
.
 # number = 182, severity = 0
Enter normal to plane.
.
 # number = 183, severity = 0
Enter radius of cylinder.
.
 # number = 184, severity = 0
Enter point on cylinder axis.
.
 # number = 185, severity = 0
Enter cylinder axis vector.
.
 # number = 186, severity = 0
Enter resolution.
.
 # number = 187, severity = 0
Drawing error (%g of radius)
.
 # number = 188, severity = 0
Pick second curve.
.
      # number = 189
Font arc drawing error (%g of radius):
.
		# number = 190
Pick circles to have their centers changed.
.
		# number = 191
Approximate center point
.
		# number = 192
New location of polygon vertex
.
		# number = 193
Pick a filled polygon region near vertex to be changed
.
		# number = 194
Vertices of polygon
.
		# number = 195
Enter base vector
.
		# number = 196
Pick viewport
.
      /*                                  muconic.c                        */
		# number = 197
Two focus points
.
		# number = 198
Point on ellipse
.
		# number = 199
Center of ellipse
.
		# number = 200
Length of semi-minor axis
.
		# number = 201
Center (intersection of asymptotes) of hyperbola
.
		# number = 202
Length of transverse axis of hyperbola
.
		# number = 203
Length of conjugate axis of hyperbola
.
		# number = 204
Focal point of parabola
.
		# number = 205
Range out major axis
.
		# number = 206
Conic not in working plane.  Supply another point in plane
.
		# number = 207
Six coefficients for quadratic equation
.
		# number = 208
Pick an ellipse
.
		# number = 209
Total length of major axis
.
		# number = 210
Total length of minor axis
.
		# number = 211
Pick a conic curve
.
		# number = 212
Pick a hyperbola
.
		# number = 213
Pick a parabola
.
		# number = 214
Curvature at vertex
.
		# number = 215
Point to be on conic
.
		# number = 216
Tangent vector at this point (OPTIONAL)
.
		# number = 217
Pick first endpoint
.
		# number = 218
Pick first endpoint (OPTIONAL)
.
		# number = 219
Pick second endpoint
.
		# number = 220
Pick starting entity
.
		# number = 221
Pick terminating entity
.
		# number = 222
Entities for test
.
		# number = 223
Pick entities
.
		# number = 224
Pick entities to make invisible
.
		# number = 225
Pen number
.
		# number = 226
Enter view name
.
		# number = 227
Enter drawing name
.
		# number = 228
Enter new drawing name
.
		# number = 229
enter drawing to rename
.
		# number = 230
enter drawing to be archived
.
		# number = 231
enter archival name
.
		# number = 232
enter drawing to be retrieved
.
		# number = 233
enter drawing to delete
.
		# number = 234
Layer number
.
		# number = 235
Layer name
.
		# number = 236
Indicate lower left of area
.
		# number = 237
Indicate upper right of area
.
		# number = 238
Plot area border?
.
		# number = 239
enter logical pen number for border
.
		# number = 240
select area for view placement
.
		# number = 241
select area to delete
.
		# number = 242
procede with area delete?
.
		# number = 243
indicate area to align
.
		# number = 244
indicate base point
.
		# number = 245
indicate new base point
.
		# number = 246
View name
.
		# number = 247
Parallel to which vector?
.
		# number = 248
Indicate base point
.
		# number = 249
Enter offset delta
.
		# number = 250
Diametrically opposite point
.
		# number = 251
Indicate curve to lift
.
		# number = 252
Number of layer to name
.
		# number = 253
Offset layer numbers by what value
.
		# number = 254
Length of semi-major axis
.
		# number = 255
Orientation angle
.
		# number = 256
Pick first tangent line
.
		# number = 257
Pick second tangent line
.
		# number = 258
Pick third tangent line
.
		# number = 259
Pick entities to blank
.
		# number = 260
working plane x axis
.
		# number = 261
Enter number of translation copies of each entity
.
		# number = 262
Enter number of rotation copies of each entity
.
		# number = 263
Point on drawing to place view center
.
		# number = 264
Pick a connected sequence of lines
.
		# number = 265
Pick a connected sequence of lines/arcs
.
		# number = 266
Pick next line/arc
.
		# number = 267
Pick splitting curve
.
		# number = 268
Pick curve to split
.
		# number = 269
Pick line/circle.
.
		# number = 270
Pick entity to modify
.
		# number = 271
Can't dynamically allocate storage for form (%s). 
.
		# number = 272
Warning, form truncated to fit in the graphics area (%s).
.
		# number = 273
Error in loading form (%s).
.
		# number = 274
Form data fill error (%s).
.
		# number = 275
Form presentation error (%s).
.
		# number = 276
Form activate error (%s).
.
		# number = 277
Unknown form error (%s).
.
      # number = 278
start angle:
Angles can be entered with or without an angular unit specification.
If without, the default set in the Environment subsystem will be used
(set to degrees when the system is initialized).  Valid angular units
are:

	      deg           degrees
	      rad           radians
.
      # number = 279
end angle:
Angles can be entered with or without an angular unit specification.
If without, the default set in the Environment subsystem will be used
(set to degrees when the system is initialized).  Valid angular units
are:

	      deg           degrees
	      rad           radians
.
		# number = 280
Enter distance to offset.
.
		# number = 281
Pick planar curve to offset; side picked on is offset side.
.
		# number = 282
Pick solid to tweak:
.
		# number = 283
Coordinate of point to move:
.
		# number = 284
New coordinate of point:
.
		# number = 285
Enter number of scaled copies of each entity
.
		# number = 286
Render solids for drawing (Hidden lines on) ?
.
		# number = 287
Pick polygons or composite curves for positive areas
.
		# number = 288
Pick polygons or composite curves for negative areas
.
		# number = 289
Pick polygon to calculate perimeter
.
		# number = 290
Pick curve near new point
.
		# number = 291
Pick curve that line is to be tangent to
.
		# number = 292
Pick point on curve that tangent line starts at
.
		# number = 293
Pick curve that line is to be normal to
.
		# number = 294
Pick point on curve that normal line starts at
.
		# number = 295
Distance to extend line
.
		# number = 296
Projection point used to extend line
.
		# number = 297
Enter a point:
.
		# number = 298
Pick a line/curve:
.
		# number = 299
Pick a curve:
.
		# number = 300
Pick entities to include in group
.
		# number = 301
Pick group to dissolve
.
		# number = 302
Pick an entity to calculate the minimum distance.
.
		# number = 303
Pick an entity to calculate the perpendicular distance.
.
		# number = 304
Do you really want to delete all displayable entities?
.
		# number = 305
Enter center of torus.
.
		# number = 306
Enter axis of torus.
.
		# number = 307
Enter major radius of torus.
.
		# number = 308
Enter minor radius of torus.
.
		# number = 309
Enter corner point.
.
		# number = 310
Enter first point of parabola.
.
		# number = 311
Enter second point of parabola.
.
		# number = 312
Enter third point of parabola.
.
		# number = 313
Enter fourth point of parabola.
.
		# number = 314
Enter focus of parabola.
.
		# number = 315
Enter vertex of parabola.
.
		# number = 316
Pick a curve to trim conic to.
.
		# number = 317
Enter tangent vector at point.
.
		# number = 318
Pick entity to project onto plane.
.
		# number = 319
Center of inscribed circle.
.
		# number = 320
Number of sides of polygon.
.
		# number = 321
Define plane to project entities onto.
.
		# number = 322
Pick entity to Rename
.
		# number = 323
Enter U arc length parameter on curve
.
		# number = 324
Enter U arc length parameter on surface
.
		# number = 325
Enter V arc length parameter on surface
.
		# number = 326
Pick curve to evaluate.
.
		# number = 327
Pick surface to evaluate.
.
		# number = 328
Pick entities to save.
.
		# number = 329
Pick entities to make editable.
.
		# number = 330
Pick entities to make non-editable.
.
		# number = 331
Enter selective save file name: 
.
		# number = 332
Enter outer radius of tube
.
		# number = 333
Enter inner radius of tube
.
		# number = 334
Enter starting point of tube
.
		# number = 335
Enter distance and direction of tube axis
.
		# number = 336
Pick first body to cross section.
.
		# number = 337
Pick closed curve of x-section (pick on tolerance side).
.
		# number = 339
Press any key to continue.
.
		# number = 340
Pick second body to cross section.
.
      # number = 341
Model system origin:
.
      # number = 342
Model system x axis:
.
      # number = 343
Model system y axis:
.
      # number = 344
Model system z axis:
.
      # number = 345
Pick entity to redefine.
.
      # number = 346
Pick entities to unblank:
.
      # number = 347
Pick entities to get:
.
      # number = 348
Pick entities to put:
.
      # number = 349
Pick entities to redisplay:
.
     # number = 350
Pick solid or net-surface to decompose.
.

/*-------------------------------------------------------------------------
      NAME         :  iubhep.mg
      CONTAINS: MPE UBASE help, error, and prompt messages
-------------------------------------------------------------------------*/
/*            UNIBASE ERRORS & PROMPTS           */
# Subsystem = UB = 8
  # error section
  /* r_save                */
    # number = 1, severity = 0
Cannot save; empty part. [See Help]
This error should not occur during normal operation.  Contact NCL Support.
.
    # number = 2, severity = 0
File could not be replaced; save with a different name.  [More info in HELP.]
The system was not able to replace the existing file.  This error can
occur when you have permission to read the file but not to write to 
it.  You should either:

    -  obtain permission from the owner of the file to write to it and 
       retry the operation; or
    -  Save the file with a different name resolve the problem by using
       appropriate operating system commands after exiting NCL.
.
    # number = 3, severity = 0
  UB#3; cannot open specified save file
.
    # number = 4, severity = 0
  Cannot allocate necessary memory. [See Help]
  This error should not occur during normal operation.  Contact NCL Support.
.
    # number = 5, severity = 0
  UB#5; error saving variable list [See Help]
  This error should not occur during normal operation.  Contact NCL Support.
.
    # number = 6, severity = 0
  UB#6; error writing to file [See Help]
  This error should not occur during normal operation.  Contact NCL Support.
.
  /* r_load                */
    # number = 7, severity = 0
  Cannot open load file.
.
    # number = 8, severity = 0
  Cannot load incompatible file.
.
    # number = 9, severity = 0
  UB#9; illegal geometry relation in mtid [See Help]
  This error should not occur during normal operation.  Contact NCL Support.
.
    # number = 10, severity = 0
  UB#10; error reading mtid relations [See Help]
  This error should not occur during normal operation.  Contact NCL Support.
.
    # number = 11, severity = 0
  UB#11; unable to allocate necessary memory [See Help]
  This error should not occur during normal operation.  Contact NCL Support.
.
    # number = 12, severity = 0
  UB#12; encountered illegal relation control block [See Help]
  This error should not occur during normal operation.  Contact NCL Support.
.
    # number = 13, severity = 0
  UB#13; error reading atom in variable list [See Help]
  This error should not occur during normal operation.  Contact NCL Support.
.
    # number = 14, severity = 0
  UB#14; cannot load non-existent variable list.  [More info in HELP.]
  Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 15, severity = 0
  UB#15; illegal mtid relation found in geometry
.
    # number = 16, severity = 0
  UB#16; error reading mtid relations
.
    # number = 17, severity = 0
  UB#17; RDBMS not available.
.
    # number = 18, severity = 0
  UB#18; relation not found.  [More info in HELP.]
  Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 19, severity = 0
  UB#19; No such part.
.
    # number = 20, severity = 0
  UB#20; Database error in transcription.  [More info in HELP.]
  Error should not occur during normal operation.  Contact NCL Support.
.
    # number = 21, severity = 0
  Autosave occurring.   [More info in HELP.]
  The Autosave option is ON.  The system will periodically Snapsave 
  the database according to the parameters set when the option was 
  invoked.  You can restore the most recent Autosaved database by 
  selecting the Restore icon in the File Management subsystem.
.
    # number = 22, severity = 0
  Answer must begin with t or n.
.
    # number = 23, severity = 0
  Part contains no environment info.
.
    # number = 24, severity = 0
  Unable to open autosave file.
.
    # number = 25, severity = 0
  Data dictionary file is illegal or damaged.
.
    # number = 26, severity = 0
  Cannot load partial part file, must merge data. 
.
	# number = 27, severity = 0
Error, Geometry label %s already exists, Unibase did not merge.
.
    # number = 28, severity = 0
  Answer must begin with a or b.
.
  # prompt section
  /* r_save                      */
    # number = 1
  Enter Unibase filename to save:
.
    # number = 2
  Overwrite existing part?
.
  /* r_load                      */
    # number = 3
  Remove existing geometry?
.
    # number = 4
  Enter load filename:
.
    # number = 5
  Enter part name:
.
    # number = 6
  Delete existing part?
.
    # number = 7
  Autosave by Time or Number of Unibase changes? (t/n)
.
    # number = 8
  Enter time interval in minutes:
.
    # number = 9
  Enter number of Unibase changes:
.
    # number = 10
  Destroys existing work. Are you sure?
.
    # number = 11
  Save existing Unibase?
.
    # number = 12
  Enter merge filename:
.

/*-------------------------------------------------------------------------
      NAME         :  iviewhep.mg
      CONTAINS: MPE VIEW help, error, and prompt messages
-------------------------------------------------------------------------*/
/*            VIEWING ERRORS & PROMPTS           */
# Subsystem = VIEW = 9
  # error section
   # number = 1, severity = 1
  Cannot write to filename; enter new name. 
.
  # prompt section
    # number = 1
  Enter light source direction vector:
.
    # number = 2
  Enter polygon output file name: 
.
    # number = 3
  File exists; overwrite?
.
   # number = 4
  Pick surfaces to render:
.
   # number = 5
  Press any key to return to NCL.
.
   # number = 6
  Pick rendering(s) to convert to wireframe.
.
   # number = 7
  File does not exist; enter new file name.
.
   # number = 8
  Escape NCL for maximum rendering?
.

/*-------------------------------------------------------------------------
      NAME         :  iudoshep.mg
      CONTAINS: MPE UDOS help, error, and prompt messages
-------------------------------------------------------------------------*/
/*            UX_UDOS ERRORS & PROMPTS           */
# Subsystem = UX_UDOS = 10
  # error section
   # number = 1, severity = 1
File %s wasn't successfully opened for reading
.
	# number = 2, severity = 1
File %s doesn't have %s access from ux_access0
.
	# number = 3, severity = 1
Operation %s isn't implemented for modenv operations
.
	# number = 4, severity = 1
Bad syntax in file: no '='
.
	# number = 5, severity = 1
Can't find symbol name %s in table to replace it
.
	# number = 6, severity = 1
Environmental variable %s was not found
.
	# number = 7, severity = 1
Unable to expand identifier %s in ux_expand_string
.
	# number = 8, severity = 1
Unable to write block to STREAM file
.
	# number = 9, severity = 1
Unable to read a block from STREAM file
.
	# number = 10, severity = 1
Unable to read file that isn't STREAM file
.
	# number = 11, severity = 1
Unable to write to file that isn't STREAM file
.
	# number = 12, severity = 1
Unable to get the extra header information
.
	# number = 13, severity = 1
Error in VAX pathname to filearea syntax
.
	# number = 14, severity = 1
Error, %s already exists (%s)
.
	# number = 15, severity = 1
Error loading file %s into UNIBASE (%s)
.
	# number = 16, severity = 1
Error is: File %s doesn't exist and can't create it (%s)
.
	# number = 17, severity = 1
Error -- file %s does not exist or is not xio compatible (%s)
.
	# number = 18, severity = 1
File %s has a UNIBASE file header
.
	# number = 19, severity = 1
File %s has no UNIBASE file header
.
	# number = 20, severity = 1
No file area or archive found at %s (%s)
.
	# number = 21, severity = 1
Error: %s is a file area (%s)
.
	# number = 22, severity = 1
Error: File %s doesn't exist or no access to it (%s)
.
	# number = 23, severity = 1
Error: Target File %s already exists (%s)
.
	# number = 24, severity = 1
Error: Filename too long (> 40 characters). Enter another name. 
.
	# number = 25, severity = 1
Error: Your filename has illegal characters in it. Enter another name.
.
	# number = 26, severity = 1
Error: No %s library was found at %s. (%s)
.
	# number = 27, severity = 1
Error: Error in the pathname to the %s library. (%s)
.
	# number = 28, severity = 1
Error: The %s archive area, %s, is not a directory. (%s)
The library must be a directory.  You must create the library
before attempting to archive to the library.
.
	# number = 29, severity = 1
Error: The library name %s is not legal. (%s)
.
	# number = 30, severity = 1
Can't remove %s archive, %s,  (%s) not empty.  [More info in HELP.]
The archive library must be empty before it can be deleted.
.
	# number = 31, severity = 1
Error, can't determine if library, %s, is in area, %s, (%s).
.
	# number = 32, severity = 1
Can't create %s library; %s already exists (%s).
.
	# number = 33, severity = 0
Archive path, %s, is out of user local area,  (%s). 
.
/* xe@@ */

  # prompt section
   # number = 1
  Hit any key to continue
.
	# number = 2
Desired file not found or unreadable in local area; search system area?
.
	# number = 3
Ignore this file and continue loading other files?
.
	# number = 4
Type the operating system command string.
.
	# number = 5
Type the filename of a file to send the results to.
.
	# number = 6
Send the results of the operating system command to the screen?
.
	# number = 7
Type the filename of the executable image to spawn.
.
	# number = 8
Illegal %s name, was changed to: %s, ok?
.
	# number = 9
Give new library name. Local area is used unless prefix \"sys:\" is given.
.
	# number = 10
Enter l to list local area; s to list system area.
.
	# number = 11
Give library name; for system area, prefix name with \"sys:\".
.

/*-------------------------------------------------------------------------
      NAME         :  isymhep.mg
      CONTAINS: MPE SYM help, error, and prompt messages
-------------------------------------------------------------------------*/
/*                      SYMBOL                    */
# Subsystem = SYMBOL = 11
  # error section
	# number = 2, severity = 0
No allowable geometric entities picked (ubu_create_symbol).
.
	# number = 3, severity = 2
Error in loading form.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 4, severity = 1
Form data fill error.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 5, severity = 1
Form presentation error.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 6, severity = 1
Form activate error.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 7, severity = 1
Unknown form error.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 8, severity = 0
Symbol contains maximum number of text nodes allowed (%d).  [See HELP.]
NCL set the indicated limit based on anticipated user requirements.  
If the limit is too low for your specific requirements, contact NCL 
Customer support.

.
	# number = 9, severity = 1
Error in UNIBASE data retrieval (ubi_get_snap_nodes).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 10, severity = 1
Error in attribute update (ubi_get_snap_nodes).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 11, severity = 0
Symbol contains maximum number of snap nodes allowed (%d).  [See HELP.]
NCL set the indicated limit based on anticipated user requirements.  
If the limit is too low for your specific requirements, contact NCL 
Customer support.
.
	# number = 12, severity = 1
Error in UNIBASE symbol setup from symbol subsystem.   [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 13, severity = 1
Cannot create entity tuple for rel. %d (%s).  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 14, severity = 1
Cannot store transform for entity that has key: %d.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 15, severity = 1
Cannot store attribute bundle for entity that has key: %d.  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 16, severity = 1
Can't delete composite entity record, %d, (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 17, severity = 1
Error in loading form (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 18, severity = 1
Form data fill error (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 19, severity = 1
Form presentation error for symbol instance (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 20, severity = 1
Form activate error for symbol instance (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 21, severity = 1
Unknown symbol instance form error (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 22, severity = 1
Cannot retrieve data for key = %d (ubi_fix_unibase_geom_for_symbol). [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 23, severity = 1
Relation number is: %d.  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 24, severity = 1
Can not retrieve master symbol data for key: %d (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	#number = 25, severity = 1
Can not find master symbol, %s, (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 26, severity = 1
Error on retrieve of symbol record with key=%d (ub_retrieve_sym).  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 27, severity = 1
Error on retrieve of attributes, symbol key=%d (ub_get_sym_attr).  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 28, severity = 1
Error on retrieve of transform, symbol key=%d (ub_get_sym_transf).  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 29, severity = 1
Error, nothing in UNIBASE to save (%s).
.
	# number = 30, severity = 1
Can't delete version of master sym at: %s (%s). Use another name.[See HELP.]
The system could not delete the master symbol in the library.  This error
will occur when you do not have the permission of the owner of the library
to write or delete symbols in the library.  

You should save the symbol using a different name or obtain permission from the
owner of the old symbol to delete it.

.
	# number = 31, severity = 1
Can't set flag to save master sym. (sub)entity with key = %d (%s).  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 32, severity = 1
Error in archiving master symbol: %s  (%s).
.
	# number = 33, severity = 1
Master symbol archive path name not found (%s).  [More info in HELP.]
The library in which symbols are to be archived must be created prior
to starting NCL.  Use a different path name or exit the system and
create the archive library.
.
	# number = 34, severity = 1
Unable to delete geometry with key=%d (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 35, severity = 1
Unable to delete symbol instance with key=%d (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 36, severity = 1
Unknown relation type: %d, for key = %d, (%s).  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	#number = 37, severity = 1
Entity picked has relation type %d and is not a symbol instance (%s).;?
.
	# number = 38, severity =1
Unable to retrieve display segment id for key=%d, (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 39, severity = 1
Master symbol not found in local library and no system library specified.
.
	# number = 40, severity = 1
Master symbol %s not found with prefix path name: %s.
.
	# number = 41, severity = 1
Can't retrieve attribute bundle for key=%d, relation number=%d.  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 42, severity = 1
Can't update UNIBASE master symbol record having key = %d  (%s).  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 43, severity = 1
Master symbol does not exist in %s (%s).;?library
.
	# number = 44, severity = 1
No master symbol library found at %s  (%s).
.
	# number = 45, severity = 1
Can't create symbol library; %s already exists (%s).
.
	# number = 46, severity = 1
Can't update views for symbol instance (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 47, severity = 1
Can't set displayability for symbol: %d, relation: %d  (%s).  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 48, severity = 1
Error, symbol %s does not exist or is not compatible (%s).  [See HELP.]
The named symbol does not exist in the library specified.  To see the 
names of the symbols in the library, select the List Symbol Library 
option.
.
	# number = 49, severity = 1
Invalid file area type: %s  (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 50, severity = 1
Entity type %s, not allowed in symbol; master not created (%s).  [See HELP.]
Symbols cannot currently contain drafting entities nor other symbols.
Drafting entities must be simulated with ordinary curves and text nodes.
.
	# number = 51, severity = 1
Unable to delete snap node with key=%d, (%s).
.
	# number = 52, severity = 1
Unable to delete text node with key=%d, (%s).  [More info in HELP.]
.
	# number = 53, severity = 1
Can't delete entity record, %d, from UNIBASE  (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 54, severity = 1
Can't store symbol instance in UNIBASE, (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 55, severity = 1
Can't dynamically allocate storage for forms  (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 56, severity = 1
Illegal color number of %d, (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 57, severity = 1
Illegal text node visibility specification of %d,  (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 58, severity = 1
Error in path name to archive, (%s).
.
	# number = 59, severity = 1
Error in symbol archive path name, symbol created but not archived. (%s).
.
	# number = 60, severity = 1
Can't remove master symbol archive, %s,  (%s) not empty.  [More info in HELP.]
The archive library must be empty before it can be deleted.  Use the
List Master Symbols in Library option to list the symbols remaining 
in the library.  Use the Delete Master Symbol From Library option to
delete symbols from the library.
.
	# number = 61, severity = 1
Illegal text node type,  (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 62, severity = 1
Symbol archive area, %s, is not a directory  (%s).  [More info in HELP.]
The symbol archive library must be a directory.  You must use the Create
Symbol Library option to create the library before attempting to archive 
symbols into the library.
.
	# number = 63, severity = 1
Can't set displayability for subentity with key:%d  (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 64, severity = 1
Error in retrieving UNIBASE index of loaded symbol  (%s).  [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 65, severity = 1
Can't set displayability for subentity with key: %d, rel.: %d (%s).  [See HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
	/* NOTE: This error message is needed to be
	   a blank line (actually a dash).  Notify him before 
	   changing it */
	# number = 66, severity = 1
-
.
	# number = 67, severity = 1
%s name, \"%s\", is illegal  (%s).
.
	# number = 68, severity = 0
Warning, form truncated to fit in the graphics area (%s).
.
	# number = 69, severity = 1
Error, can't determine if library, %s, is in area, %s, (%s).
.
	# number = 70, severity = 1
Master symbol not created  (%s).
.
	# number = 71, severity = 1
No symbol library by this name exists (%s).
.
	# number = 72, severity = 1
Bad text node visibility number, %d,  (%s).
.
    # number = 73, severity = 0
Too many master symbols. You selected %d; the maximum is %d (%s). [See HELP.]
If the number of selected symbol entities indicated in the error message is not
the correct number for the symbol, try again making sure that only the
correct entities are selected.
.
	# number = 74, severity = 0
%s symbol area, %s, doesn't exist (%s).
.
	# number = 75, severity = 0
Error in updating application list, %d, for key, %d,  (%s).
.
	# number = 76, severity = 4
Can't unload master symbol, %s; current library version not loaded (%s).
.
	# number = 77, severity = 5
Can't clear save/load bit map (%s).
.
	# number = 78, severity = 2
Can't get version of master file, %s, at %s; no auto load done.
.
	# number = 79, severity = 2
Can't retrieve list, %d, for entity with key, %d,  (%s).
.
	# number = 80, severity = 2
Can't find data packet for instance with key, %d, (%s).
.
	# number = 81, severity = 2
Error in deleting from list, %d, master sym with key, %d, (%s).
.
	# number = 82, severity = 2
Error in updating UNIBASE list, %d, for key, %d, (%s).
.
	# number = 83, severity = 2
Can't mark currently loaded master symbols (%s).
.
	# number = 84, severity = 0
Can't find master symbol, %s, loaded from lib. %s, (%s).
.
	# number = 85, severity = 2
Can't set displayability for instance with key, %d, (%s).
.
	# number = 86, severity = 2
Too many master symbols to process during a load/merge (%s).
.
	# number = 87, severity = 2
Can't transform snap node with key:%d  (%s).
.
	# number = 88, severity = 1
Can't find master symbol, %s, (%s).
.
	# number = 89, severity = 2
Test of uu_uerror3, %d, %d, (%s).
.
	# number = 90, severity = 2
Error in updating UNIBASE symbol record with key:%d,  (%s).
.
	# number = 91, severity = 2
Symbol library path, %s, doesn't exist (%s).
.
	# number = 92, severity = 5
Data base maybe inconsistent; sign off and start over. (%s).
.
	# number = 93, severity = 5
Failure on UNIBASE deletion from list: %d of key: %d  (%s).
.
	# number = 94, severity = 0
Mandatory symbol UB_UPDATE_LOAD_SYMS not defined, (%s).
.
	# number = 95, severity = 0
Can't update position of text entity %d in old instance %d (%s).
.
	# number = 96, severity = 0
Symbol %s name, %s, is illegal, (%s).
.
	# number = 97, severity = 0
No check for %s being current; file path not associated with %s's name.
.
	# number = 98, severity = 0
Warning, rename does not change symbol file names in libraries.
.
	# number = 99, severity = 0
Can't delete master, %s, instances of it exist, (%s).
.
	# number = 100, severity = 5
Error in freeing dynamic storage for instance with key: %d.
.
	# number = 101, severity = 5
Error in retrieving relation type for entity with key: %d  (%s).
.
	# number = 102, severity = 0
Subentities of symbol instances can't be queried.
.
	# number = 103, severity = 0
No snap node picked.
.
	# number = 104, severity = 0
Error updating master sym, %s; data base inconsistency req'd a reset.
During a load or merge of parts containing symbol instances, the data 
base may become inconsistent. There may be more than one master symbol 
of the same name or some master symbol can not be instanced. In this 
case, the data base is reset and the part has been deleted.
.
	# number = 105, severity = 0
Note, reloading %s from symbol lib, %s.
.
	# number = 106, severity = 0
Can't rename, symbol %s is loaded in Unibase: save part, unload master. (%s)
.
	# number = 107, severity = 0
Archive path, %s, is out of user local area,  (%s). 
.
	# number = 108, severity = 0
Unable to create a description file for the symbol %s.
.
	# number = 109, severity = 0
Use %s script file to enter the description.
.
	# number = 110, severity = 0
Unable to create the script file for entering a symbol description. 
.
	# number = 111, severity = 0
Unable to create a modify script for the description file: symbol %s.
.
	# number = 112, severity = 0
Use %s script file to modify the description.
.
	# number = 113, severity = 0
Unable to show a description for symbol %s.
.
	# number = 120, severity = 1
No standard parts library found at %s  (%s).
.
	# number = 121, severity = 1
No standard parts library by this name exists (%s).
.
	# number = 122, severity = 1
The system can't delete the standard part in the library: %s (%s).
.
	# number = 123, severity = 1
Can't set flag to save standard part (sub)entity with key = %d (%s).
Error should not occur during normal operation.  Contact NCL Support.
.
	# number = 124, severity = 1
Standard part does not exist in %s (%s) library.
.
/* end sym errors se@@ */

  # prompt section
   # number = 1
  Enter symbol name
.
	# number = 2
	Enter symbol origin
.
	# number = 3
	Pick symbol geometry
.
	# number = 4
	Enter next snap node.
.
	# number = 5
	Enter text.
.
	# number = 6
	Continue entering text nodes? If so, current node must be redone.
.
	# number = 7
	Enter angle of symbol label from working plane x-axis.
.
	# number = 8
	Is text to be a prompt?
.
	# number = 9
	Non-geometric or text entity in UNIBASE, delete it and continue?.
.
	# number = 10
	Enter location of next snap node.
.
	# number = 11
	Enter location of instance origin.
.
	# number = 12
	Master symbol exists; replace old version?.
.
	# number = 13
	Pick instance to be decomposed.
.
	# number = 14
	Enter new name of master symbol.
.
	# number = 15
Give symbol lib. name; for system area, prefix name with \"sys:\".
.
	# number = 16
	Pick entities to be in master symbol.
.
	# number = 17
Symbol not found or unreadable in local area; search system area?
.
	# number = 18
	Symbol created but not archived; enter a previously created lib.
.
	# number = 19
	Enter new library name.
.
	# number = 20
	Ignore this file and continue loading master symbols? 
.
	# number = 21
	Master symbol, %s, is already in the data base; overwrite?
.
	# number = 22
	Pick entities to be put in master symbol.
.
	# number = 23
	Enter text node position.
.
	# number = 24
Change color of: geometry (g); snap nodes (s); text (t); all (a).
.
	# number = 25
	Pick symbol instances to change visibility.
.
	# number = 26
Make visible: snap nodes (s); text: all (t), graphic only (g); all (a).
.
	# number = 27
Number of copies to be created is %d, ok?
.
	# number = 28
Enter l to list local symbol area; s to list system area.
.
	# number = 29
Ok to create default symbol library, \"symlib\" in local area?
.
	# number = 30
Illegal %s name, was changed to: %s, ok?
.
	# number = 31
%s already exists; overwrite?.
.
	# number = 32
Hit any key to continue.
.
	# number = 33
Reloading, %s, from lib, will regenerated instances use current text node data?
.
	# number = 34
Redo this instance? Otherwise, required text will be \"***\" in this instance.
.
	# number = 35
Updating instances of %s; shall current text nodes be reused?
.
	# number = 36
Currently loaded master, %s, is from a different library, overwrite?
.
	# number = 37
Enter symbol library name:
.
	# number = 100
Pick snap node on symbol to start new connector
.
	# number = 101
Pick snap node on symbol to end new connector
.
	# number = 102
Enter intermediate coodinates of new connector
.
	# number = 103
Enter text of new connector
.
	# number = 104
To remove master, %s, from UNIBASE all instances of it will be removed; ok?
.
	# number = 105
Give new symbol lib. name; for system area, prefix name with \"sys:\".
.
	# number = 106
Enter new symbol file name:
.
	# number = 107
Must reload master sym, %s; give lib (for sys area, prefix \"sys\")
.
	# number = 108
Can't construct library path to master, %s; enter lib name.
.
	# number = 109
Master symbol file for %s not readable at %s; reenter lib.
.
	# number = 110
No access to get status of master, %s, at %s; enter new lib.
.
	# number = 111
%s doesn't correspond to a library; enter new lib.
.
	# number = 112
Do you want to use the master in the part file regardless of lib path? 
.
	# number = 113
Do you want to use the master in Unibase prior to load?
.
	# number = 114
Do you want the text node data of current instances of %s inherited?
.
	# number = 115
Use currently loaded master version without updating?
.
	# number = 116
2 masters named %s loaded; use newest loaded?
.
	# number = 117
Target symbol filename %s already exists. Continue, overwriting the file?
.
	# number = 118
Do you want to create a description for the symbol %s? 
.
	# number = 119
Symbol %s from diff. lib. or int.rename already in Unibase, overwrite it? 
.
	# number = 120
Give standard part lib. name; for system area, prefix name with \"sys:\".
.
   # number = 121
Enter standard part name
.
	# number = 122
Enter location of the part copy's origin.
.
	# number = 123
Enter new standard part file name:
.
	# number = 124
Pick entities to be put in standard part.
.
	# number = 125
Target standard part filename %s already exists. Continue, overwriting the file?
.
	# number = 126
Ok to create default standard part library, \"splib\" in local area?
.
	# number = 127
Standard Part of this name already exists; replace it?.
.

/*-------------------------------------------------------------------------
      NAME         :  isuphep.mg
      CONTAINS: MPE SUPPORT help, error, and prompt messages
-------------------------------------------------------------------------*/
# Subsystem = SUPPORT = 12
  # error section
    # number = 1, severity = 0
Setup file %s doesn't exist or contains error information. [More info in HELP.]
The system could not find the named plot setup file.  

You must define the setup file using the Create/Update Setup
file option.
.
    # number = 2, severity = 0
Pen table %s doesn't exist or contains error information.
The system could not find the named pen table file.  

You must define the pen table using the Create/Update Pen Table option.
.
    # number = 3, severity = 0
Illegal paper size.  [More info in HELP.]
This error should not occur during normal system operation. 
Contact NCL Support.
.
/*
.....changed by Yurong
.....start and finished plot msg
.....Starting to create plot file. 
.....Finished creating plotttt file.
.....no longer need here, instead
.....we want some other msgs
.....Yurong 9/5/97
*/
	 # number = 4, severity = 0  
Null drawing name !!
.
	 # number = 5, severity = 0
Null setup file name !!
.
	 # number = 6, severity = 0
Construction plane not parallel to the view selected for Quickplot.  [See HELP.]
Quickplot projects the view of the model onto the construction plane, 
therefore, the construction plane must be the same as the viewing plane
in the viewport selected for Quickplot. 

To set the construction plane parallel to the view, select the Axis/Grid
subsystem and invoke the Align to Viewing Plane option.
.
	 # number = 7, severity = 0
Can't open file %s
.
	 # number = 8, severity = 0
Illegal input
.
	 # number = 9, severity = 0
Item number out of range
.
	 # number = 10, severity = 0
Grid is not on on the picked view port
.
	 # number = 11, severity = 0
Drawing '%s' does not exist  [More info in HELP.]
To list the names of all drawings that have been defined for the model,
select the List Drawings option in the Plot Management subsystem.
.
	 # number = 12, severity = 0
%s
.
	 # number = 13, severity = 0
Null plotfile name
.
	 # number = 14, severity = 0
Grid spacing is smaller that 1/20 of aperture.  [More info in HELP.]
The system will not let you set the grid spacing smaller than 1/20
of the viewport aperture.  This prevents you from accidentally drawing 
a grid with spacing so fine that it takes several minutes to draw.  You
will see the aperture displayed at the bottom of the Activate Grid 
form.  Note that the snap resolution can be set as small as desired.  
.
	# number = 15, severity = 1
Can't dynamically allocate storage for form  (%s).  [More info in HELP.]
This error should not occur during normal system operation. 
Contact NCL Support.
.
	# number = 16, severity = 1
Error in loading form  (%s).   [More info in HELP.]
This error should not occur during normal system operation. 
Contact NCL Support.
.
	# number = 17, severity = 1
Form data fill error (%s).  [More info in HELP.]


This error should not occur during normal system operation. 
Contact NCL Support.
.
	# number = 18, severity = 1
Form presentation error (%s).  [More info in HELP.]
This error should not occur during normal system operation. 
Contact NCL Support.
.
	# number = 19, severity = 1
Form activate error (%s).  [More info in HELP.]
Error should not occur during normal system operation.  Contact NCL Support.
.
	# number = 20, severity = 1
Unknown form error (%s).  [More info in HELP.]
Error should not occur during normal system operation.  Contact NCL Support.
.
	# number = 21, severity = 1
Illegal toggle index of %d for %s  (%s).  [More info in HELP.]
Error should not occur during normal system operation.  Contact NCL Support.
.
	# number = 22, severity = 0
Warning, form truncated to fit in the graphics area (%s).
.
	# number = 24,	severity = 1
Error: Unable to remove the plot file %s.
.
	# number = 25,	severity = 1
Error: Unable to rename the plot file %s to %s.
.
	# number = 26,	severity = 1
Error: Unable to copy the plot file %s to %s.
.
	# number = 27, severity = 1
Error: query line too long.
.
	# number = 28, severity = 2
Can't find name of entity with key: %d,  (%s).
.
	# number = 29, severity = 0
Null pen table name!!
.
	# number = 30, severity = 0
Can not create file %s !!
.
/* end support errors je@@ */


  # prompt section
	# number = 1
	Enter plotter setup file name: 
.
	# number = 2
	Enter pen table name: 
.
	# number = 3
	Enter drawing name: 
.
	# number = 4
	Enter setup file name to be copied:
.
	# number = 5
	Enter setup file name to copy to:
.
	# number = 6
	Enter pen table name to be copied:
.
	# number = 7
	Enter pen table name to copy to:
.
	# number = 8
	Enter pathname to list the file:
.
	# number = 9
	Enter filename to delete:
.
	# number = 10
	Delete %s?
.
	# number = 11
	Enter filename to rename:
.
	# number = 12
	Enter newfile name:
.
	# number = 13
	Enter filename to copy from:
.
	# number = 14
	Enter filename to copy to:
.
	# number = 15
	Enter plotfile name:
.
	# number = 16
	Select entity for data display:
.
	# number = 17
	END OF DATA, press RETURN to continue.
.
	# number = 18
%s picked; this entity is hierarchical; do you want a subentity?
.
	# number = 19
	Plot an empty drawing?
.
	# number = 20
	Plot file %s already exists - overwrite?
.

/*-------------------------------------------------------------------------
      NAME         :  idrafhep.mg
      CONTAINS: MPE DRAFT help, error, and prompt messages
-------------------------------------------------------------------------*/
/* DRAFTERROR */
# Subsystem = DRAFTING = 13
 # error section
   #number = 1, severity = 0
Cannot open drafting default file named %s.  [More info in HELP.]
The system has attempted to load drafting standard modal
settings from the named file.  The file name is specified in the 
DRAFTSTD parameter in the U.INIT initialization file.  

You should exit the system and use the host operating system
utilities to list the contents of the U.INIT file and to verify the 
existance of the specified default file.  A valid U.INIT file and 
default file are provided on the NCL distribution file.  If you are
using these files as distributed, contact NCL support.
.
   #number = 2, severity = 0
Defaults loaded from %s
.
   #number = 3, severity = 0
Defaults saved to %s
.
   #number = 4, severity = 1
WARNING -- Version of %s not current
.
   #number = 5, severity = 0
%s not valid font name.  Font not changed.  [More info in HELP.]
The following fonts are provided by NCL:

NAME                     DESCRIPTION
--------------           --------------------------
ROMSIM                   Roman simplex
ANSI                     Modified Roman Simplex
GRKSIM                   Greek Simplex
DUPLEX                   
ITALIC


.
   #number = 6, severity = 0
Wrong entity type picked.  You must pick and arc or a circle.  [See HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur, or changing to an oblique view.
.
   #number = 7, severity = 0
First and second points picked are the same.  [More info in HELP.]
This error can be caused by snapping of located points (points input
with the (+) cursor) to the grid.  Check that snap resolution set with 
the Define Grid Icon in the Axis/Grid subsystem is correct.
.
   #number = 8, severity = 0
Entity picked not a line.  [More info in HELP.]
The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur, or changing to an oblique view.
.
   #number = 9, severity = 0
Selected lines are parallel.  [More info in HELP.]
The system determined that the lines selected for angular dimensioning
are parallel.

The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur, or changing to an oblique view.
.
   #number = 10, severity = 0
Invalid choice selection
.
	#number = 11, severity = 0
More than %d boundary curves picked for crosshatching.  [More info in HELP.]
NCL set the indicated limit based on anticipated user requirements.  
If the limit is too low for your specific requirements, contact NCL 
Customer support.
.
	#number = 12, severity = 0
Line pick at intersection center
.
	#number = 13, severity = 0
Boundary geometry not co-planar.  [More info in HELP.]
All of the curves selected for crosshatching must lie in the same plane.

The system may have picked an entity other than the one you intended.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur.
.
	#number = 14, severity = 0

.
	#number = 15, severity = 0
The curves selected do not form a closed boundary.  [More info in HELP.]
The system determined that the selected curves were not connected
endpoint-to-endpoint to form a closed planar boundary.  If the 
correct curves appear to have been picked, you can find a point of
discontinuity which is not visually apparent by attempting to 
change the color of the boundary by doing a chain select.  Select
the Edit/Move subsystem Modify Attributes option followed by the
Select subsystem Chain Select option.  Pick an endpoint of a curve
on the boundary.  Observe the attention diamonds resulting from the
pick.  No diamonds will appear on the curves following the point
of discontinuity.
.
	#number = 16, severity = 0
Geometry projects to zero length - invalid for this type dimension.  [See HELP.]
Selected geometry is projected onto the working plane to determine the 
dimension distance.  If the working plane is not parallel to the 
drafting viewing plane, the points input for dimensioning can appear
to be separated even though they are coincident on the working plane.
You can input the points to the Calculator by picking them to 
determine whether they have the same x,y coordinates in the working
plane.  
.
	#number = 17, severity = 0
Arc plane and working plane are not parallel
.
	#number = 18, severity = 0
Cannot open drafting standards file -- DRAFTSTD symbol not defined
The drafting standards file name should be specified in the DRAFTSTD
parameter in the U.INIT initialization file.  The DRAFTSTD parameter
was not specified.  NCL provides a valid U.INIT file on the 
distribution media.  Exit the system and use the host operating
system utilities to list the U.INIT file and compare it to the 
distributed file.  Contact NCL support if they compare.
.
	#number = 19, severity = 0
Lines are not parallel  [More info in HELP.]
The system may be picking an entity other than the one you intend.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur.
.
	#number = 20, severity = 0
First and second lines picked are the same  [More info in HELP.]
The system may be picking an entity other than the one you intend.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur.
.
	#number = 21, severity = 0
No user modifiable text found in drafting entity selected.  [See HELP.]
The drafting entity selected either does not contain any text
(possibly the wrong entity was selected); or it is not
text which can be modified by the user.  Some drafting entities 
contain text which is automatically generated by the system and
cannot be modified by the user.
.
	#number = 22, severity = 0
Wrong entity type picked.  You must pick a drafting entity.  [See HELP.]
The system may be picking an entity other than the one you intend.  You 
can exercise better control over which entity is picked by;

- using care when positioning the cursor.  The cursor should touch 
  only the entity which you wish to pick.

- using a selective pick trigger key instead of the mouse button
  'l' key - only a line will be picked
  'a' key - only an arc or a circle will be picked.

- zooming in closer to the portion of the view where the pick will 
  occur.
.
	#number = 23, severity = 0
Cannot add tolerance to user dimension text.
.
	#number = 24, severity = 0
Tolerance method not set, cannot add tolerance.
.
	#number = 25, severity = 0
Appended text method not set, cannot add text.
.
	#number = 26, severity = 0
Cannot fit text between extension lines, reposition.
.
	#number = 27, severity = 0
Form-Positional Tolerance text not editable, delete and recreate.
.
	#number = 28, severity = 0
Currently, only lines and arcs are supported by this function.
.
	#number = 29, severity = 0
Exceeded internal limit for entries in this line.
.
	#number = 30, severity = 0
Exceeded internal limit for characteristic entries in this line.
.
	#number = 31, severity = 0
Exceeded internal limit for tolerance entries in this line.
.
	#number = 32, severity = 0
Exceeded internal limit for datum entries in this line.
.
	#number = 33, severity = 0
Exceeded internal limit for lines.
.
	#number = 34, severity = 0
Current drafting entities inconsistent with new drafting standard. Load failed.
.
	#number = 35, severity = 0
Text origin inconsistent with entity selected - try again.
.
	#number = 36, severity = 0
No Alt. Act. available for this prompt.
.
   #number = 37, severity = 0
Cannot open text file named %s.  
.
   #number = 38, severity = 0
Invalid line number.
.
   #number = 39, severity = 0
Input exceeded character storage limit - last line ignored.
.
   #number = 40, severity = 0
Wrong entity type picked for this function - try again.
.
   #number = 41, severity = 0
Centerline pattern does not fit between selected entities - try again.
.
   #number = 42, severity = 0
Cannot delete only line in a note.
.
   #number = 43, severity = 0
Dimension not associated with geometry - cannot regenerate.
.
   #number = 44, severity = 0
Wrong entity picked. You must pick a Parts List Table.
.
   #number = 45, severity = 0
Limited to 3 characters for ID numbers.
.
   #number = 46, severity = 0
Wrong entity picked. You must pick a BALLOON.
.
   #number = 47, severity = 0
Wrong BALLON picked, or BALLOON has only one leader.
.
   #number = 48, severity = 0
Can not add a multiplier to a BALLOON with more than one leader.
.
   #number = 49, severity = 0
Can not add a leader to a BALLOON with a multiplier.
.
   #number = 50, severity = 0
Duplicate FIND NUMBER (re-enter line).
.
   #number = 51, severity = 0
Table contains duplicate FIND NUMBER entries. (not loaded).
.
   #number = 52, severity = 0
Non-integer FIND NUMBER data (re-enter line).
.
   #number = 53, severity = 0
Non-integer MULTIPLIER data (re-enter line).
.
   #number = 54, severity = 0
Must select a PARTS LIST table entry.
.
   #number = 55, severity = 0
Pick a PARTS LIST table entry from the wrong table.
.
   #number = 56, severity = 0
PARTS LIST TABLES limited to 40 entries.
.
   #number = 57, severity = 0
Limited to 3 characters for QUANTITY numbers.
.
   #number = 58, severity = 0
Non-integer QUANTITY data (re-enter line).
.
   #number = 59, severity = 0
Dimension values are not equal, cannot use REPETITIVE dimension type.
.
   #number = 60, severity = 0
Repetitive type does not currently support 180 or 360 degree angles.
.


/*DRAFTEND*/
/* DRAFTPROMPT */
# prompt section
   #number = 28
Select first entity
.
   #number = 29
Select second entity
.
   #number = 30
Indicate dimension origin
.
   #number = 31
Select baseline/margin
.
   #number = 32
Select first arc or circle
.
   #number = 33
Select second arc or circle
.
   #number = 34
Select arc to dimension
.
   #number = 35
Indicate dimension line endpoint
.
   #number = 36
Enter flange angle in degrees
.
   #number = 37
Select first line
.
   #number = 38
Select second line
.
   #number = 39
Indicate datum origin
.
   #number = 40
Indicat positive quadrant
.
   #number = 41
Select line or margin
.
   #number = 42
Indicate position
.
   #number = 43
Indicate dogleg position
.
   #number = 44
Select point, line or arc
.
   #number = 45
Enter main dimension text
.
   #number = 46
Enter dual dimension text
.
   #number = 47
Enter dimension text
.
   #number = 48
Enter appended text
.
   #number = 49
Indicate alignment
.
   #number = 50
Indicate new entity origin
.
   #number = 51
Indicate note text location
.
   #number = 52
Enter note text
.
   #number = 53
Enter label text
.
   #number = 54
Indicate label text location
.
   #number = 55
Enter ID-symbol text
.
   #number = 56
Indicat ID-symbol origin
.
   #number = 57
Select point
.
   #number = 58
Select line
.
   #number = 59
Enter margin offset distance
.
   #number = 60
Indicate margin offset position
.
   #number = 61
Enter line spacing ratio
.
   #number = 70
Indicate new entity origin
.
   #number = 71
Select drafting entity
.
   #number = 72
Indicate alignment 
.
   #number = 73
Enter character size offset, Delta X
.
   #number = 74
Enter character size offset, Delta Y
.
   #number = 75
Enter offset, Delta X
.
   #number = 76
Enter offset, Delta Y
.
   #number = 92
Enter dim text A
.
   #number = 93
Enter dim text B
.
   #number = 94
Enter dimension text
.
   #number = 95
Enter appended text
.
	#number = 100
Select entity for Form/Positional tol
.
	#number = 101
Select entity for label
.
	#number = 102
Enter text for tolerance
.
	#number = 103
Enter text for symbol
.
	#number = 104
Enter text for datum
.
	#number = 105
Locate end of witness line
.
 #number = 106
Select arc or circle
.
 #number = 107
Select center line of the shaft
.
 #number = 108
Select shaft diameter section
.
 #number = 109
Indicate endpoint for arrowhead location
.
 #number = 110

.
 #number = 111
Select cross hatch boundary geometry 
.
 #number = 112
Enter load file name (standard file)
.
 #number = 113
Enter save file name (standard file)
.
 #number = 114
Select arrowhead for alignment
.
 #number = 115
Select first entity - extend from
.
 #number = 116
Select second entity - dimension to
.
 #number = 117
Indicate dimension offset
.
 #number = 118
Indicate new dimension origin
.
 #number = 119
Indicate new dimension offset
.
 #number = 120
Select drafting entity for edit
.
 #number = 121
Enter the number of the line to be modified
.
 #number = 122
Enter the number of the line to be deleted
.
 #number = 123
Enter the line number after which text is to be added
.
 #number = 124
Enter the line number before which text is to be added
.
 #number = 125
Modify text using standard text cursor control characters
.
 #number = 126
Enter text for the new line
.
 #number = 127
Enter text file name
.
 #number = 128
Select baseline or datum.
.
 #number = 129
Reposition origin outside extension lines.
.
 #number = 130
Indicate points for center-line (in order).
.
 #number = 131
Locate center point of bolt circle.
.
 #number = 132
Select circles for center-line (in order).
.
 #number = 133
Select circles for center-line (in clockwise order).
.
 #number = 134
Select drafting entities for regeneration.
.
 #number = 135
Select notes for modification.
.
 #number = 136
Select dimensions for modification.
.
 #number = 137
Select dimension for repositioning.
.
 #number = 138
Select line for polar axis.
.
 #number = 139
Select an intersecting line to determine origin.
.
 #number = 140
Select polar coordinate entity.
.
 #number = 141
Select cross-hatch for modification.
.
 #number = 142
Locate Lower Left corner of the table.
.
 #number = 143
Enter number of lines in the table.
.
 #number = 144
Enter text for the current line.
.
 #number = 145
Select the PARTS LIST table to move.
.
 #number = 146
Locate new lower left corner of the table.
.
 #number = 147
Select Parts List Table for editting.
.
 #number = 148
Select Table entry for modification.
.
 #number = 149
Select Table for report generation.
.
 #number = 150
Enter report file name.
.
 #number = 151
Enter the location of the find number.
.
 #number = 152
Select table for delete entry operation.
.
 #number = 153
Select entry for deletion.
.
 #number = 154
Select table for add entry operation.
.
 #number = 155
Select entry after which new entry is to be placed.
.
 #number = 156
Report file exits. Overwrite? (y or n).
.
 #number = 157
Select centerlines for modification.
.
 #number = 158
Locate centerline start point.
.
 #number = 159
Locate centerline end point.
.
 #number = 160
Pick entity for Balloon label.
.
 #number = 161
Locate endpoint of Balloon leader.
.
 #number = 162
Select Table for Update operation.
.
 #number = 163
Select entries for Update operation.
.
 #number = 164
Select BALLOON for Edit operation.
.
 #number = 165
Select BALLOON arrowhead for deletion.
.
 #number = 166
Enter Multiplier value.
.
 #number = 167
Select Table for the Merge operation.
.
 #number = 168
Enter the multiplier value.
.
 #number = 169
Enter the find number.
.
 #number = 170
Enter row text (column entries separated by ;'s).
.
 #number = 171
Select next entity.
.
 #number = 172
Select next arc to dimension.
.

/*-------------------------------------------------------------------------
      NAME         :  idd1hep.mg
      CONTAINS: MPE SIGNON help, error, and prompt messages
-------------------------------------------------------------------------*/
# Subsystem = UU_SIGNON = 14
 # error section
   #number = 1, severity = 0
Function unavailable until after sign on
.
   #number = 2, severity = 0
User is already signed on, sign off first
.
   #number = 3, severity = 0
No user is currently signed on.
.
	#number = 4, severity = 0
'%s' disabled in NCL system.
.
	#number = 5, severity = 0
'%s' not supported on this display device.
.
	#number = 6, severity = 0
'%s' not enabled while in drawing management
.
	#number = 7, severity = 0
Drawing Management currently active
.
	#number = 8, severity = 0
'%s' is only available in drawing management
.
	#number = 9, severity = 0
'%s' disabled in UIMS System.
.
	#number = 10, severity = 0
'%s' disabled while record and playback is active
.
	#number = 11, severity = 5
The system has run out of dynamic memory


				SYSTEM ERROR!!!!!

This application has used up all available dynamic memory on
this system, has run out of swap space, or some such other
catastrophic error.

In order to attempt recovery, an automatic 'jump to root' will
be executed.  Save your part, exit the application, and
re-execute.  If Unibase is not corrupted you should be able
to recover the part.  If not, reload from the last snapsave
or a back-up copy of the part.
.
	#number = 12, severity = 0
'%s' disabled in DDC System.
.
	#number = 13, severity = 5
The system has run out of choice devices for menus


            FATAL SYSTEM ERROR!!!!!

This application has used up all available choice devices that
the menu navigator uses to assign a menu node.  The system will now exit.

In order to run, two solutions are available.

1)	Increase the number of choice devices in DIGS.
2) Reset the choice device alogrithm flag to allocate devices
sequencially.  This flag may be found in dasnog.h
.
	#number = 14, severity = 5
The system has run out of choice devices for popup menus.


            FATAL SYSTEM ERROR!!!!!

This application has used up all available choice devices that
the popup menu handler uses to assign a popup node.  The system will now exit.

In order to run, there is one solution available.

	Increase the start number of the menu choice device (UD_MENU_CHC_DEV_ST) 
	in "dasnog.h" and recompile d1alloc.c.

	Popup menus are allocated from UD_POP_CHC_DEV_ST and end at
	UD_MENU_CHC_DEV_ST-1.  The difference of thos two values must be larger
	than the number of popup menus in the system.

	The default (DDC) values are:

		UD_POP_CHC_DEV_ST  21		/* start choice dev number for popup menus */
		UD_MENU_CHC_DEV_ST 71		/* start choice dev number for normal menus */
.
	#number = 15, severity = 0
'%s' is only available when CADD is enabled.
.
	#number = 16, severity = 0
'%s' is only available when CAM is enabled.
.

  # prompt section
	#number = 1
Enter reference point
.
	#number = 2 
Exit Drawing Management?
.
/*
.....COPYRIGHT notice also set for *ver command in nclf/nclver.f
.....Roberta Zorzynski.
*/
   #number = 3 
- NCL - Copyright 1991-2018 Numerical Control Computer Sciences, Windows 32-bit
.
   #number = 4 
- NCL - Copyright 1991-2018 Numerical Control Computer Sciences, Windows 64-bit
.

/*-------------------------------------------------------------------------
      NAME         :  iclashep.mg
      CONTAINS: MPE CLASS help, error, and prompt messages
-------------------------------------------------------------------------*/
/*            CLASS ERRORS & PROMPTS           */
# Subsystem = CLASS = 15
  # error section
   # number = 1, severity = 1
Class initialization failure - class number disparity, row = %d, class = %d


            FATAL SYSTEM ERROR!!!!!

The class number in the class table is different from the row that
is being initialized.

In order to run, the initialized class number must be equal to the
row number.  The row is out of sequence.  The class table must be
defined at compile time in ascending numerical order.
.

/*-------------------------------------------------------------------------
      NAME         :  iaghep.msg
      CONTAINS: Applied Geometry subsystem help, error, and prompt
-------------------------------------------------------------------------*/
# Subsystem = UM_APPGEO = 16
   /*                                                                 */
   /*                      APP GEOM ERROR SECTION                    */
   /*                      LAST ERROR NUMBER IS: 6 			            */
   /*                                                                 */
   # error section
		# number = 1, severity = 0
Can't create %s from given constraints.
.
		# number = 2, severity = 0
Only 2D curves may be swept into surface.
.
		# number = 3, severity = 0
Unable to calcuate fillet curve.
.
		# number = 4, severity = 0
%s was not created.
.
		# number = 5, severity = 0
Unable to %s shells
.
		# number = 6, severity = 0
Unable to project curve onto surface.
.
/*                                                                        */
/*                    APP GEOM  PROMPT   SECTION                   */
/*                    LAST ASSIGNED PROMPT NUMBER IS: 89                 */
/*                                                                        */
   # prompt section
      # number = 1
Pick curve to convert to bspline.
.
      # number = 2
Lower left point on plane.
.
      # number = 3
Lower right point on plane.
.
      # number = 4
Upper left point on plane.
.
      # number = 5
Center coordinate of sphere.
.
      # number = 6
Radius of sphere.
.
      # number = 7
Top point of cylinder axis.
.
      # number = 8
Direction (and length) of cylinder axis.
.
      # number = 9
Radius of cylinder.
.
      # number = 10
Point defining start of surface segment.
.
      # number = 11
Starting angle of surface segment.
.
      # number = 12
Ending angle of surface segment.
.
      # number = 13
Top point of cone axis.
.
      # number = 14
Radius at top of cone.
.
      # number = 15
Direction (and length) of cone axis.
.
      # number = 16
Radius at bottom of cone.
.
      # number = 17
Pick curve to revolve.
.
      # number = 18
Point defining axis of revolution.
.
      # number = 19
Vector defining axis of revolution.
.
      # number = 20
Vector defining axis of revolution.
.
      # number = 21
Vector defining axis of revolution.
.
      # number = 22
Vector defining axis of revolution.
.
      # number = 23
Vector defining axis of revolution.
.
      # number = 24
Vector defining axis of revolution.
.
      # number = 25
Enter point for cubic.
.
      # number = 26
Direction of curve at start point.
.
      # number = 27
Direction of curve at end point.
.
      # number = 28
Radius of fillet.
.
      # number = 29
Pick curve near start point of fillet.
.
      # number = 30
Pick curve near end point of fillet.
.
      # number = 31
Pick first curve or point of ruled surface.
.
      # number = 32
Pick second curve or point of ruled surface.
.
      # number = 33
Pick center curve of swept surface.
.
      # number = 34
Pick rail curve of swept surface.
.
      # number = 35
Pick 2D cross-section curve of swept surface.
.
      # number = 36
Pick surface to reverse normal.
.
      # number = 37
Enter coordinate of corner of box.
.
      # number = 38
Enter diagonal vector of box.
.
      # number = 39
Enter center point of torus.
.
      # number = 40
Enter axial vector of torus.
.
      # number = 41
Enter axial radius of torus.
.
      # number = 42
Enter circular radius of torus.
.
      # number = 43
Union this shell
.
      # number = 44
Subtract this shell
.
      # number = 45
Intersect this shell
.
      # number = 46
with this shell
.
      # number = 47
from this shell
.
      # number = 48
Enter new tolerance value.
.
      # number = 49
Pick curve to offset.
.
      # number = 50
Enter offset distance.
.
      # number = 51
Pick surface to tessellate.
.
      # number = 52
Pick curve to reverse parameterization.
.
      # number = 53
Pick base curve of tabulated cylinder.
.
      # number = 54
Enter direction of tabulated cylinder.
.
      # number = 55
Pick next curve to fit surface through.
.
      # number = 56
Enter vector defining lift direction.
.
      # number = 57
Pick curve to lift into shell.
.
      # number = 58
Pick first curve defining sum shell.
.
      # number = 59
Pick second curve defining sum shell.
.
      # number = 60
Pick surface to project curves onto.
.
      # number = 61
Enter vector to project along (optional).
.
      # number = 62
Pick curves to project onto surface.
.
      # number = 63
Pick entity to extract curve.
.
      # number = 64
Enter parameter value.
.
      # number = 65
Pick closed, planar curve to define face boundary.
.
      # number = 66
Pick first (U0) boundary curve.
.
      # number = 67
Pick second (V1) boundary curve.
.
      # number = 68
Pick third (U1) boundary curve.
.
      # number = 69
Pick fourth (V0) boundary curve.
.
      # number = 70
Pick face to trim outer boundary.
.
      # number = 71
Pick trimming curve.
.
      # number = 72
Pick face inner boundary lies on.
.
      # number = 73
Pick interior curve.
.
      # number = 74
Pick edge curve to extract.
.
      # number = 75
Bottom point of cylinder axis.
.
      # number = 76
Bottom point of cone axis.
.
      # number = 77
End point of surface segment.
.
      # number = 78
Pick surfaces for analysis.
.
      # number = 79
Pick bspline to change shape.
.
      # number = 80
Pick control point to move.
.
      # number = 81
Enter coordinates to move it to.
.
      # number = 82
Pick curves to reparameterize.
.
      # number = 83
Pick curve(s) to convert to curve with one bspline. 
.
      # number = 84
Pick face on which to define outer boundary.
.
      # number = 85
Pick entity to extract face from.
.
      # number = 86
Pick surface(s) to convert into face(s).
.
      # number = 87
Pick composite face(s) to convert into simple faces.
.
      # number = 88
Pick first face to join.
.
      # number = 89
Pick second face to join.
.
      # number = 90
Pick closed curve known to lie on face.
.
      # number = 91
Enter up vector.
.
      # number = 92
Enter angle of twist.
.
      # number = 93
Constant up vector (0) or propagate up vector (1);
.
      # number = 94
Pick a surface of revolution.
.

/*-------------------------------------------------------------------------
      NAME         :  inclhep.mg which doesn't exist on its own ...
      CONTAINS: NCL help, error and prompt messages.
-------------------------------------------------------------------------*/
# Subsystem = UA_NCL = 17
/*                                                                     */
/*                      NCL ERROR SECTION                              */
/*                    LAST ERROR NUMBER IS: 19                          */
/*                                                                     */
# error section
      # number =  1, severity = 0
'%s' not enabled while in NCLCAM
.
      # number =  2, severity = 0
Must pick points.
.
      # number =  3, severity = 0
error message goes here [More info in HELP.]
Error should not occur during normal operation.  Contact NCL Support.
.
      # number = 4, severity = 0
Must select INVERS before selecting CONST
.
	  # number = 5, severity = 0
Can't open file %s
.
	  # number = 6, severity = 0
Picked geometry does not use a subscripted (RESERV) variable.
.
      # number = 7, severity = 0
Wireframe geometry not allowed at this time.
.
      # number = 8, severity = 0
   Z coordinate not allowed when entering X,Y coord.
.
      # number = 9, severity = 0
Incorrect length must be 1 to 6 character. 
.
      # number = 10, severity = 0
Incorrect Prefix must be 1 to 20 alphabetic characters.  No blanks. 
.
      # number = 11, severity = 0
Prefix must contain at least 1 alphabetic character and no blanks. 
.
      # number = 12, severity = 0
Valid 2 character vocabulary words are not allowed as Prefix. 
.
      # number = 13, severity = 0
Unibase file has incorrect version number. Use UNIBASE VERSION CONVERTER in NIS.
.
      # number = 14, severity = 0
Unibase file is a different machine type.  Use PLATFORM CONVERTER in NIS.
.
      # number = 15, severity = 0
No TRACUT is in effect.  No change to view made.
.
      # number = 16, severity = 0
No REFSYS is in effect.  No change to view made.
.
      # number = 17, severity = 0
Illegal geometry picked.  Picking limited to %s.
.
		# number = 18, severity = 0
Post-processor commands are not allowed on last entity.
.
		# number = 19, severity = 0
Nested Line is too long. Please break it up.
.

/*                                                                         */
/*                           NCL  PROMPT   SECTION                         */
/*                     LAST ASSIGNED PROMPT NUMBER IS: 525(with some holes)*/
/*                                                                         */
# prompt section

   #number = 1
Label of new entity
.

/*                                                                         */
/*                      POINT DEFINITIONS                                  */
/*                                                                         */

/* pt/x,y */
   #number = 2
X,Y coordinate of point
.
/* pt/x,y,z */
   #number = 3
Z coordinate
.
/* pt/circle, ATANGLE, angle */
   #number = 4
Label of circle new point is on
.
   #number = 5
Angle (from x axis) of new point
.
/* pt/INTOF, circle, curve, near-point */
   #number = 6
Label of circle to intersect
.
   #number = 7
Label of curve to intersect
.
   #number = 8
Label of point near intersection
.
/* pt/point, circle, angle */
   #number = 9
Point label
.
   #number = 10
Circle label
.
   #number = 11
Angle
.
/* pt/point, x-offset, y-offset, z-offset */
   #number = 12
Label of point to offset
.
   #number = 13
X,Y,Z offset values
.
/* pt/<xl|xs|yl|ys>, ENDPT, line or circle */
   #number = 14
Label of line/circle to get endpoint of
.
/* pt/INTOF, line, plane */
   #number = 15
Label of line to intersect
.
   #number = 16
Label of plane to intersect
.
/* pt/TE */
/* pt/CENTER, circle */
   #number = 17
Label of circle to define center point
.
/* pt/INTOF, plane, plane, plane */
   #number = 18
Label of first plane to intersect
.
   #number = 19
Label of second plane to intersect
.
   #number = 20
Label of third plane to intersect
.
/* pt/<xl|xs|yl|ys>, INTOF, line, circle/arc */
   #number = 21
Label of line to intersect
.
   #number = 22
Label of circle/arc to intersect
.
/* pt/<xl|xs|yl|ys>, INTOF, circle1, circle2 */
   #number = 23
Label of first circle to intersect
.
   #number = 24
Label of second circle to intersect
.
/* pt/INTOF, line1, line2 */
   #number = 25
Label of first line to intersect
.
   #number = 26
Label of second line to intersect
.
/* pt/INTOF, plane, curve, near-point */
   #number = 27
Label of plane to intersect 
.
   #number = 28
Label of curve to intersect
.
   #number = 29
Label of point near intersection
.
/*                                                                         */
/*                      LINE DEFINITIONS                                   */
/*                                                                         */

/* ln/point1, point2 */
   #number = 30
Label of first point of line
.
   #number = 31
Label of second point of line
.
/* ln/point, PERPTO, curve, near-point */
   #number = 32
Label of point line is to go through
.
   #number = 33
Label of curve line is to be perpendicular to
.
   #number = 34
Label of point near point of intersection
.
/* ln/point, TANTO, curve, near-point */
   #number = 35
Label of point line is to go through
.
   #number = 36
Label of curve line is to be tangent to
.
   #number = 37
Label of point near point of tangency
.
/* ln/x1, y1, x2, y2 */
   #number = 38
X,Y coordinate of startpoint 
.
   #number = 39
X,Y coordinate of endpoint 
.
/* ln/x1, y1, z1, x2, y2, z2 */
   #number = 40
Z coordinate of startpoint 
.
   #number = 41
Z coordinate of endpoint 
.
/* ln/INTOF, plane1, plane2 */
   #number = 42
Label of first plane to intersect 
.
   #number = 43
Label of second plane to intersect 
.
/* ln/<xl|xs|yl|ys>, circle, ATANGLE, angle, line */
   #number = 44
Label of circle new line is tangent to
.
   #number = 45
Angle that line is to make
.
   #number = 46
Label of line new line is to make angle with
.
/* ln/line, x-offset, y-offset, z-offset */
   #number = 47
Label of line to offset
.
   #number = 48
X,Y,Z offset
.
/* ln/point, PARLEL, line */
   #number = 49
Label of point line is to go through
.
   #number = 50
Label of line new line is parallel to
.
/* ln/point, PERPTO, line */
   #number = 51
Label of point line is to go through
.
/* 
.....Changed to include curve. JLS 2/19/99
*/
   #number = 52
Label of line or curve new line is perpendicular to
.
/* ln/PARLEL, line, <xl|xs|yl|ys>, distance */
   #number = 53
Label of line new line is parallel to
.
   #number = 54
Distance from line
.
/* ln/FWD */
/* ln/point, <right|left>, TANTO, circle */
   #number = 55
Label of point line is to go through
.
   #number = 56
Label of circle line is tangent to
.
/* ln/<right|left>, TANTO, circle1, <right|left>, TANTO, circle2 */
   #number = 57
Label of first circle line is tangent to
.
   #number = 58
Label of second circle line is tangent to
.
/* ln/<xaxis|yaxis>, distance */
   #number = 59
Distance from axis
.
/* ln/point, ATANGLE, angle, <xaxis|yaxis|line> */
   #number = 60
Label of point new line is to go through
.
   #number = 61
Angle of new line
.
   #number = 62
Label of line 
.
/* ln/x1,y1,z1, ..., xn,yn,zn */
   #number = 63
X,Y coordinate of point in string line
.
/* ln/point1, ..., pointn */
   #number = 64
Label of new point in string line
.
/*                                                                         */
/*                      CIRCLE DEFINITIONS                                 */
/*                                                                         */

/* ci/x, y, radius */
   #number = 65
X,Y coordinate of the center
.
   #number = 66
Radius of a circle
.
/* ci/x, y, z, radius */
   #number = 67
X,Y coordinate of the center
.
   #number = 68
Radius of a circle
.
/* ci/CENTER, point, TANTO, line */
   #number = 69
Label of the center point of a circle
.
   #number = 70
Label of a line the new circle is tangent to
.
/* ci/CENTER, point, RADIUS, radius */
   #number = 71
Label of the center point of a circle
.
   #number = 72
Radius of a circle
.
/* ci/CENTER, point, point */
   #number = 73
Label of the center point of a circle
.
   #number = 74
Label of a circumference point on a circle
.
/* ci/CENTER, point, <large|small>, TANTO, circle */
   #number = 75
Label of the center point of a circle
.
   #number = 76
Label of a circle the new circle is tangent to 
.
/* ci/<xl|xs|yl|ys>, point1, point2, RADIUS, radius */
   #number = 77
Label of the first circumference point of a circle
.
   #number = 78
Label of the second circumference point of a circle
.
   #number = 79
Radius of a circle
.
/* circle:   TT-TT-RA */
   #number = 80
Pick first entity (point/line/circle/curve) new circle is tangent to
.
   #number = 81
Pick second entity (point/line/circle/curve) new circle is tangent to
.
   #number = 82
Pick entity (line/circle/curve) new circle is tangent to
.
   #number = 84
Radius of circle 
.
/* ci/circle, x-offset, y-offset, z-offset */
   #number = 85
Label of circle to offset
.
   #number = 86
X,Y,Z offset
.
/* ci/point1, point2, point3 */
   #number = 87
Label of first point on circle
.
   #number = 88
Label of second point on circle
.
   #number = 89
Label of third point on circle
.
/* ci/point1, point2, vector */
/* ci/point1, vector, point2 */
   #number = 90
Label of a circumference point on a circle
.
   #number = 91
Label of tangent vector at first point or of another circumference point 
.
   #number = 92
Label of tangent vector at second point
.
/* ci/TANTO, line, <xl|xs|yl|ys>, point, RADIUS, radius */
   #number = 93
Label of line/circle new circle is tangent to
.
   #number = 94
Label of point on circumference of circle
.
   #number = 95
Radius of circle
.
/* ci/<xl|xs|yl|ys>, line1, <xl|xs|yl|ys>, line2, RADIUS, radius */
   #number = 96
Label of first line circle is tangent to
.
   #number = 97
Label of second line circle is tangent to
.
   #number = 98
Radius of circle
.
/* ci/<xl|xs|yl|ys>, <in|out>, circle1, <in|out>, circle2, RADIUS, radius */
   #number = 99
Label of first circle new circle is tangent to 
.
   #number = 100
Label of second circle new circle is tangent to 
.
   #number = 101
Radius of new circle
.
/* ci/point, <xl|xs|yl|ys>, <in|out>, circle, RADIUS, radius */
   #number = 102
Label of point new circle is to go through
.
   #number = 103
Label of circle new circle is tangent to
.
   #number = 104
Radius of new circle
.
/* ci/<xl|xs|yl|ys>, line, <xl|xs|yl|ys>, <in|out>, circle, RADIUS, radius */
   #number = 105
Label of line new circle is tangent to
.
   #number = 106
Label of circle new circle is tangent to
.
   #number = 107
Radius of new circle
.
/* ci/<xl|xs|yl|ys>, line, <xl|xs|yl|ys>, line, <xl|xs|yl|ys> line */
   #number = 108
Label of first line circle is tangent to
.
   #number = 109
Label of second line circle is tangent to
.
   #number = 110
Label of third line circle is tangent to
.
/* ci/CANON, x,y,z, i,j,k, radius, i,j,k,distance */
   #number = 111
X,Y,Z of center point
.
   #number = 112
i,j,k of normal vector 
.
   #number = 113
Optional: I,J,K of limit plane
.
   #number = 114
distance of limit plane
.
/* ci/CANON, point, vector, radius, plane */
   #number = 115
Label of center point of circle
.
   #number = 116
Label of normal vector of circle
.
   #number = 117
Radius of circle
.
   #number = 118
Label of limit plane
.
/*                                                                         */
/*                      VECTOR DEFINITIONS                                 */
/*                                                                         */

/* ve/i,j,k */
   #number = 119
I,J,K coordinates of vector
.
/* ve/point, surface */
   #number = 120
Label of point vector starts at
.
   #number = 121
Label of surface vector ends at
.
/* ve/FWD */
/* ve/TLAXIS */
/* ve/point1, point2 */
   #number = 122
Label of start point of vector
.
   #number = 123
Label of end point of vector
.
/* ve/PERPTO, plane, <posx|negx|posy|negy|posz|negz> */
   #number = 124
Label of plane vector is perpendicular to
.
/* ve/UNIT, vector */
   #number = 125
Label of vector to unitize
.
/* ve/vector1, <CROSS|PLUS|MINUM>, vector2 */
   #number = 126
Label of first vector
.
   #number = 127
Label of second vector
.
/* ve/PARLEL, INTOF, plane1, plane2, <posx|negx|posy|negy|posz|negz> */
   #number = 128
Label of first plane to intersect
.
   #number = 129
Label of second plane to intersect
.
/*                                                                         */
/*                      PLANE DEFINITIONS                                  */
/*                                                                         */

/* pl/i,j,k,distance */
   #number = 130
I,J,K coodinates of plane normal
.
   #number = 131
Distance of plane from origin
.
/* pl/point1, point2, point3 */
   #number = 132
Label of first point defining plane
.
   #number = 133
Label of second point defining plane
.
   #number = 134
Label of third point defining plane
.
/* pl/point, PARLEL, plane */
   #number = 135
Label of point new plane is to go through
.
   #number = 136
Label of plane new plane is parallel to
.
/* pl/PARLEL, plane, <xl|xs|yl|ys|zl|zs>, distance */
   #number = 137
Label of plane new plane is parallel to
.
   #number = 138
Distance of new plane
.
/* pl/point, PERPTO, vector */
/*
.....Since we have two "Label of point new plane is to go through"
.....(#135) Changed it to ask for display point.  JLS 2/19/99
*/
   #number = 139
Pick display point.
.
   #number = 140
Label of vector new plane is perpendicular to
.
/* pl/point1, point2, PERPTO, plane */
   #number = 141
Label of first point new plane is to go through
.
   #number = 142
Label of second point new plane is to go through
.
   #number = 143
Label of plane new plane is perpendicular to
.
/* pl/point, PERPTO, plane1, plane2 */
   #number = 144
Label of point new plane is to go through
.
   #number = 145
Label of first plane new plane is perpendicular to
.
   #number = 146
Label of second plane new plane is perpendicular to
.
/* pl/line, PERPTO, plane */
   #number = 147
Label of line lying on new plane
.
   #number = 148
Label of plane new plane is perpendicular to
.
/*                                                                         */
/*                      MATRIX DEFINITIONS                                 */
/*                                                                         */

/* mx/p1,...p12 */
   #number = 149
Twelve parameter values defining matrix
.
/* mx/INVERSE, mx */
   #number = 150
Label of matrix to invert
.
/* mx/matrix1, matrix2 */
   #number = 151
Label of first matrix to multiply
.
   #number = 152
Label of second matrix to multiply
.
/* mx/point, vector1, vector2 */
   #number = 153
Label of point defining origin
.
   #number = 154
Label of xaxis vector
.
   #number = 155
Label of vector giving approximate yaxis
.
/* mx/MIRROR, plane */
   #number = 156
Label of plane to define mirror transformation
.
/* mx/SCALE, scale */
   #number = 157
Scale factor: x [,y [,z]]
.
/* mx/<xrot|yrot|zrot>, angle */
   #number = 158
Angle of rotation about chosen axis
.
/* mx/TRANSL, xoffset, yoffset, zoffset */
   #number = 159
X,Y,Z values to translate by
.
/*                                                                         */
/*                      CURVE DEFINITIONS                                  */
/*                                                                         */

/* cv/point1, vector1, ..., pointn, vectorn */
   #number = 160
Label of point defining curve
.
   #number = 161
Label of vector defining curve
.
/* cv/FIT, point1, vector1, ..., pointn, vectorn */
   #number = 162
Label of point defining curve
.
   #number = 163
Label of vector defining curve
.
/*                                                                         */
/*                      SURFACE DEFINITIONS                                */
/*                                                                         */

/* sf/boundary1, boundary2 */
   #number = 164
Label of first boundary curve of ruled surface
.
   #number = 165
Label of second boundary curve of ruled surface
.
/* sf/boundary1, slope1, ..., boundary20, slope20 */
/* sf/boundary, 0, ..., boundary20, 0 */
   #number = 166
Label of boundary curve
.
   #number = 167
Label of slope curve
.
/* sf/curve1, curve2, ..., curve-n */
   #number = 168
Label of boundary curve
.
   #number = 169
Label of slope curve
.
/* sf/<xl|xs|yl|ys|zl|zs>, plane1, <xl|xs|yl|ys|zl|zs>, plane2, RADIUS, radius */
   #number = 170
Label of first plane of fillet surface
.
   #number = 171
Label of second plane of fillet surface
.
   #number = 172
Radius of fillet surface
.
/* sf/MESH, file-name, surf-name, start-patch, end-patch */
   #number = 173
Name of file containing mesh surface definition
.
   #number = 174
Label of mesh surface
.
   #number = 175
Number of starting patch of mesh surface
.
   #number = 176
Number of ending patch of mesh surface
.
/* sf/QUILT, file-name, patch1, ..., patchn */
   #number = 177
Name of file containing quilt surface
.
   #number = 178
Number of patch
.
/*                                                                         */
/*                         ZSURF DEFINITIONS                            */
/*                                                                         */
      # number = 179
Z value
.
      # number = 180
Label of Z surf plane
.
/*                                                                         */
/*                         REFSYS DEFINITIONS                              */
/*                                                                         */
      # number = 181
Label of refsys matrix
.
/*                                                                         */
/*                         MODSYS DEFINITIONS                              */
/*                                                                         */
      # number = 182
Label of modsys matrix
.
/* mx/pt1,pt2,pt3,pt4,pt5,pt6 */
   # number = 183
Enter 1st input coord:
.
   # number = 184
Enter 2nd input coord:
.
   # number = 185
Enter 3rd input coord:
.
   # number = 186
Enter 1st output coord:
.
   # number = 187
Enter 2nd output coord:
.
   # number = 188
Enter 3rd output coord:
.
/*                                                                         */
/*                         PROGRAM CONTROL                                 */
/*                                                                         */

/* call/macro, parameterlist */
      # number = 190
Macro name
.
      # number = 191
Complete macro parameter list
.
/* if (scalar exp) label1, label2, label3 */
      # number = 192
Expression to evaluate
.
      # number = 193
Label to jumpto if expression less than 0 
.
      # number = 194
Label to jumpto if expression equal to 0 
.
      # number = 195
Label to jumpto if expression greater than 0 
.
/* jumpto/ label */
      # number = 196
Label to jumpto
.
/* simple statement */
      # number = 197
Enter NCL language statement
.
/* redef statement */
      # number = 198
Label of geometry to redefine 
.
/* redef statement */
      # number = 199
Label of geometry to trim/extend to
.
/* remove statement */
      # number = 200
Label of geometry to remove
.
/* Vector times Vector */
      # number = 201
Scale factor:
.
/* show geometry statement */
      # number = 202
Pick geometry to show
.
/* show source statement */
      # number = 203
Enter source line number
.
/* surf/mesh_name, surf_name, start_patch, end_patch */
      # number = 204
Name of file containing mesh surface
.
      # number = 205
Label of surface
.
      # number = 206
Number of starting patch
.
      # number = 207
Number of ending patch
.
/* surf/quilt, file_name, patch, ..., patch */
      # number = 208
Name of file containing quilt surface
.
      # number = 209
Next patch number
.
/* skip prompt */
      # number = 210
Enter increment value
.
/*  indent prompt  */
      # number = 211
Enter column number
.
/*  cl tapeft prompt  */
      # number = 212
Enter maximum length of tape (ft)
.
/*  delete source line prompt */
      # number = 213
Enter source line number(s) to be deleted (L1, Ln)
.
/*  find souce line prompt */
      # number = 214
Enter character string to search for
.
/*  findtk prompt */
      # number = 215
Enter TOKEN to search for
.
      # number = 216
Pick a point
.
      # number = 217
Pick a line
.
      # number = 218
Pick a vector
.
      # number = 219
Pick a plane
.
      # number = 220
Pick a circle
.
      # number = 221
Pick a curve
.
      # number = 222
Pick a surface
.
      # number = 223
Enter a scalar value
.
      # number = 224
Enter distance along curve
.
      # number = 225
Pick optional starting point
.
      # number = 226
Enter number of segments for circle displays
.
      # number = 227
Enter number of U segments for surface displays
.
      # number = 228
Enter number of V segments for surface displays
.
      # number = 229
Enter DO loop label
.
	  # number = 230
Enter name of loop counter
.
	  # number = 231
Enter initial value for loop counter
.
	  # number = 232
Enter the limit value for loop counter
.
	  # number = 233
Enter increment size for loop counter (optional)
.
/*                                                                         */
/*                         MISC  PROMPT                                    */
/*                                                                         */
	  #number = 234
Enter a scalar variable name.
.
	  # number = 235
Enter a reserved point array name.
.
	  # number = 236 
Enter optional forward vector array name.
.
	  # number = 237 
Enter optional tool axis vector array name.
.
      # number = 238
Pick geometry
.
      # number = 239
Pick subscripted geometry (RESERV-VARIABLE geometry)
.
      # number = 240
Pick a point vector
.
      #number = 241
Label of point-vector defining plane
.
      # number = 242
Label of geometry to reverse 
.
      # number = 243
Enter DONE to accept pick or REJECT OP to continue picking.
.
      # number = 244
Enter geometry to open/close
.
      # number = 245
Enter number of calls to reset. Default [ALL]
.
      # number = 246
Enter a reserved point vector array name.
.
      # number = 247
Pick a surface or plane to calculate projection vectors (optional)
.
      # number = 248
Pick a surface/curve.
.
/*                                                                         */
/*                         MOTION DEFINITIONS                              */
/*                                                                         */
      # number = 390
Enter distance of plane parallel to starting tool axis (leave-dist)
.
      # number = 391
Enter distance of plane parallel to final tool axis (approach-dist)
.
      # number = 392
Pick drive surface vector.
.
      # number = 393
Pick check surface vector.
.
      # number = 400
Pick Retract plane, point or By Text for distance
.
      # number = 401
Enter NCL command:
.
      # number = 402
Pick point.
.
      # number = 403
Pick boundary curve.
.
      # number = 404
Pick slope control curve.
.
      # number = 405
Enter: diameter [radius, height, angle/BARREL radius, z-height, flt angle]
.
      # number = 406
Enter: i, j, k
.
      # number = 407
Enter: vector label
.
      # number = 408
Enter: angle(degrees)
.
      # number = 409
Enter: cutter height
.
      # number = 410
Enter: MAXANG
.
      # number = 411
Enter: MAXDP
.
      # number = 412
Enter: NUMPTS
.
      # number = 413
Enter: THICK
.
      # number = 414
Enter: TOLER
.
      # number = 415
Pick part surface
.
      # number = 416
Enter: matrix label
.
      # number = 417
Enter: index value
.
      # number = 418
Enter: number of times
.
      # number = 419
Enter: x, y, z offsets
.
      # number = 420
Enter: index value
.
      # number = 421
Enter: x, y 
.
      # number = 422
Enter: x, y, z 
.
      # number = 423
Pick drive surface
.
      # number = 424
Pick check surface
.
      # number = 425
Edit line if required
.
      # number = 426
Enter: z or x,y,z or a plane label
.
      # number = 427
Enter part program file name
.
      # number = 428
Pick surface to scrub
.
      # number = 429
Enter number of passes, points per pass
.
      # number = 430
Enter clfile file name
.
      # number = 431
Enter the feed rate value
.
      # number = 432
Enter the distance from the check surface
.
      # number = 433
Enter the feed rate scaling factor
.
      # number = 434
Enter the offset factor
.
      # number = 435
Pick boundary points
.
      # number = 436
Pick curve (spline) to offset
.
      # number = 437
Enter layer number
.
      # number = 438
Pick a viewport for motion to be displayed in
.
      # number = 439
Pick circle to trim/extend
.
      # number = 440
Pick 1st line to trim/extend circle to
.
      # number = 441
Pick 2nd line to trim/extend circle to
.
      # number = 442
Pick surface to rmill
.
      # number = 443
Pick 1st check surface, line, circle, curve, or plane
.
      # number = 444
Pick 2nd check surface, line, circle, curve, or plane
.
      # number = 445
Pick 1st drive_surface line, or plane 
.
      # number = 446
Pick 2nd drive_surface line, or plane 
.
      # number = 447
Pick Clearance option plane or By Text for distance
.
      # number = 448
Enter height up cutter to contact drive surface
.
      # number = 449
Enter X, Y, Z delta values
.
/*                                                                         */
/*                         PATERN PROMPTS                                  */
/*                                                                         */
      # number = 450
Pick a patern
.
      # number = 451
Pick patern or point
.
      #number = 452
Enter points to retain
.
      #number = 453
Enter points to omit
.
	  #number = 454
Enter height to raise tool
.
	  #number = 455
Enter points to avoid
.
      #number = 456
Enter a point in the patern
.
	  # number = 457
Pick a point or vector
.
      # number = 458
Enter number of points in patern
.
      # number = 459
Enter start angle of patern
.
	  # number = 460
Enter end angle of patern
.
      # number = 461
Enter increment distance(s) between patern(s) 
.
      # number = 462
Enter number of points
.
	  # number = 463
Enter distance between patern(s) 
.
      # number = 464
Enter number of parlel paterns including original patern 
.
      # number = 465
Pick a point
.
      # number = 466
Enter number of paterns to increment
.
      # number = 467
Enter increment angle(s) between points 
.
      # number = 468
Enter number of points to increment
.
      # number = 469
Enter angle(s) between point(s) 
.
      # number = 470
Enter increment distance(s) between point(s) 
.
      # number = 471
Enter distance(s) between point(s) 
.
/*                                                                         */
/*                POKMOD CLEARANCE PLANE PROMPT                            */
/*                                                                         */
      # number = 472
Pick Clearance Plane or Planar Surface
.
/*                                                                         */
      # number = 474
Enter APT source file name 
.
/*                                                                         */
/*                                                                         */
/*                         NET SURFACE PROMPT                              */
/*                                                                         */
      # number = 475
Pick first surface of net surface
.
      # number = 476
Pick last surface of net surface
.
      # number = 477
Pick surface(s) of net surface
.
      # number = 478
Pick surface(s)
.
/*                                                                         */
/*                                                                         */
/*                         SHAPE PROMPT                                    */
/*                                                                         */
      # number = 479
Pick line, circle, or curve
.
      # number = 480
Pick starting line
.
      # number = 481
Pick line or circle
.
      # number = 482
Pick ending line
.
/* cv/io,sf,sf|pl  */
	  # number = 483
Pick a surface or plane
.
/* cv/io,sf,sf|pl,near-point  */
	  # number = 484
Pick a near point (optional)
.
/*                                                                         */
/*                                                                         */
/*                   ADDITIONAL CURVE DEFINITION PROMPTS                   */
/*                                                                         */
/* cv/line1, circle1, ..., linen, circlen */
      # number = 485
Label of line or circle defining curve
.
/* cv/sf,edge */
	  # number = 486
Enter edge number (1:U=0, 2:U=1, 3:V=0, 4:V=1)
.
/* cv/sf,edge,num-pts  */
	  # number = 487
Enter number of points to use in generating curve
.
/* cv/sf,edge,num-pts,U/V-offset  */
	  # number = 488
Enter amount to offset from edge (0 to 1)
.
/* cv/(pt/pv),thru,(pt/pv) */
	  # number = 489
Pick point or point-vector.
.
/*                                                                         */
/*                                                                         */
/*                         LATHE PROMPT                                    */
/*                                                                         */
      # number = 490
Pick the material shape
.
      # number = 491
Pick the part shape
.
      # number = 492
Pick the shape to modify
.
/*                                                                         */
/*                                                                         */
/*                         POCKET PROMPT                                    */
/*                                                                         */
      # number = 493
Pick plane or surface for bottom of pocket
.
      # number = 494
Pick plane or planar surface for top of pocket
.
      # number = 495
Pick perimeter geometry: (surface,curve,patern,circle or subscripted points)
.
      # number = 496
Pick island geometry: (surface,curve,patern,circle or subscripted points)
.
      # number = 497
Pick point for ending location
.
      # number = 498
Enter number of loops around pocket periphery: (return for full pocket)
.
/*                                                                         */
/*                         ANALYZE SURFACE PROMPT                          */
/*                                                                         */
      # number = 499
Pick surface(s) to analyze
.
      # number = 500
Pick first surface 
.
      # number = 501
Pick last surface 
.
/* cv/sf,edge,num-pts,U/V-percent-offset  */
	  # number = 502
Enter percentange to offset from edge (0 to 100)
.
      # number = 503
Pick points for entry locations
.
/*                                                                        */
/*                 MORE POCKETING                                         */
/*                                                                        */
      # number = 504
Pick surface to pocket:
.
      # number = 505
Pick curve to use as pocket perimeter:
.
      # number = 506
Pick start component of open side(s):
.
      # number = 507
Pick end component of open side(s):
.
/*                                                                         */
/*                 EDIT GEOMETRY                                          */
/*                                                                         */
		# number = 510
Label of  entity to blank
.
		# number = 511
Label of entity to remove
.
		# number = 512
Select new label location (DONE for the default location)
.
		# number = 513
Select end location of leader line on the entity(DONE for the default location)
.
/*                                                                         */
/*                 OFFSET SURFACE                                          */
/*                                                                         */
      # number = 520
Label of surface to offset
.
/*                                                                         */
/*                 VARIABLE FILLET SURFACE                                 */
/*                                                                         */
      # number = 521
Label of first surface fillet is tangent to
.
      # number = 522
Label of second surface fillet is tangent to
.
      # number = 523
Label of near point to indicate position of fillet
.
      # number = 524
Starting radius of fillet surface
.
      # number = 525
Ending radius of fillet surface (optional)
.
      # number = 526
Label of curve defining variable fillet radius values
.
      # number = 527
Label of curve defining end limits of fillet surface (optional)
.
/*                                                                         */
/*                 TRIMMED SURFACES                                        */
/*                                                                         */
      # number = 528
Enter trimmed surface to extract from
.
      # number = 529
Enter boundary curve on a trimmed surface to extract from
.
      # number = 530
Select surface to trim or planar outer boundary curve
.
      # number = 531
Enter outer boundary curve
.
      # number = 532
Enter inner boundary curve (optional)
.
      # number = 533
Enter boundary curve on a trimmed surface to remove
.
/*                                                                         */
/*                 Expanded FILLET Surface                                 */
/*                                                                         */
      # number = 543
Select 1st bounding point
.
      # number = 544
Select 2nd bounding point
.
/*                                                                         */
/*                 TOOL AXIS PROMPT                                        */
/*                                                                         */
      # number = 545
Enter angle to tilt forward from normal
.
      # number = 546
Enter heel clearance distance
.
      # number = 547
Pick 4-axis control vector
.
      # number = 548
Enter start angle [, end angle] (degrees)
.
/*                                                                         */
/*                 MORE VECTOR PROMPTS                                     */
/*                                                                         */
      # number = 550
Label of curve that vector is to be tangent to
.
      # number = 551
Label of point on/near curve where vector is to be tangent to
.
/*                                                                         */
/*                                                                         */
/*                   STILL MORE CURVE DEFINITION PROMPTS                   */
/*                                                                         */
 # number = 577, severity = 0
Pick first THRU component of composite entity.
.
 # number = 578, severity = 0
Pick last THRU component of composite entity.
.
      # number = 579
Pick curve (spline) to translate
.
/* cv/composite  */
      # number = 580
Label of line, circle or curve defining composite curve
.
/* cv/conic  */
      # number = 581
Label of 1st point or point-vector on CONIC curve
.
/* cv/conic  */
      # number = 582
Label of 2nd point on CONIC curve or 1st vector
.
/* cv/conic  */
      # number = 583
Label of 3rd point on CONIC curve
.
/* cv/conic  */
      # number = 584
Label of 4th point or point-vector on CONIC curve
.
/* cv/conic  */
      # number = 585
Label of 5th point on CONIC curve or 2nd vector
.
/* cv/conic  */
      # number = 586
Label of 2nd point on CONIC curve
.
/* cv/conic  */
      # number = 587
Label of 3rd point or point-vector on CONIC curve
.
/* cv/conic  */
      # number = 588
Label of 4th point or vector on CONIC curve or 2nd vector
.
/*
.....NCLIPV
*/
      # number = 590
Pick curve to revolve or surface of revolution.
.
      # number = 591
Pick surface(s) to compare
.
      # number = 592
Pick shape to compare
.
/*
.....Nurb Surfaces
*/
		# number = 600
Enter interpolation type (1 = Linear, 2 = Cubic)
.
      # number = 601
Pick first (U0) boundary curve.
.
      # number = 602
Pick second (V1) boundary curve.
.
      # number = 603
Pick third (U1) boundary curve.
.
      # number = 604
Pick fourth (V0) boundary curve.
.
      # number = 605
Pick curve to revolve.
.
      # number = 606
Point (point-vector) defining axis of revolution.
.
      # number = 607
Vector defining axis of revolution.
.
      # number = 608
Starting angle (between 0 and 360 degrees).
.
      # number = 609
Ending angle (between 0 and 360 degrees).
.
/*                                                                         */
/*                 POINT-VECTOR PROMPTS                                    */
/*                                                                         */
      # number = 610
Pick the first pointvector.
.
      # number = 611
Pick the second pointvector.
.
      # number = 612
Pick pointvector.
.
/*										*/
/*			Tool Axis Prompts			*/
/*										*/
      # number = 613
Enter Perpto Vector (optional).
.
      # number = 614
Enter Thru Point.
.
      # number = 615
Enter Thru Curve.
.
      # number = 616
Enter Curve Distance (optional).
.
/*
.....More Motion Prompts
*/
      # number = 649
Pick motion segment to step back to
.
      # number = 650
Pick check surface near point
.
      # number = 651
Please enter fillet radius.  
.
      # number = 652
Enter distance along tool axis
.
      # number = 653
Pick first entity for distance
.
      # number = 654
Pick second entity for distance
.
      # number = 655
Pick entity for radius
.
      # number = 656
Pick surface to fmill
.
      # number = 657
Pick optional limit curve
.
      # number = 658
Pick start point
.
      # number = 659
Pick rapto plane or surface
.
      # number = 660
Pick retrct plane or surface
.
      # number = 661
Enter Geometry to Get:
.
	  # number = 662
Pick Beginning Geometry to Get:
.
	  # number = 663
Pick End Geometry to Get:
.
	  # number = 664
Pick Beginning Geometry to Put:
.
	  # number = 665
Pick End Geometry to Put:
.
	  # number = 666
Enter new name of the get geometry:
.
      # number = 667
Enter Geometry to Put:
.
      # number = 668
Pick tool axis control surface:
.
      # number = 669
Pick plane or planar surface for top of profile:
.
/*
.....Tool geometry
*/
      # number = 670
Pick tool geometry
.
      # number = 671
Pick point-vector defining axis of revolution:
.
      # number = 672
Pick annotation(s) to engrave
.
      # number = 673
Enter scalar to receive number of decomposed entities
.
/*
.....Visual solids
*/
      # number = 674
Pick 1st corner of box
.
      # number = 675
Pick 2nd corner of box
.
      # number = 676
Enter upper and lower limits of box
.
      # number = 677
Pick center of box
.
      # number = 678
Enter length, width, and height of box [x,y,z]
.
      # number = 679
Pick 1st point of cone
.
      # number = 680
Pick 2nd point of cone
.
      # number = 681
Enter bottom and top radii of cone [bot,top]
.
      # number = 682
Enter height of cone
.
      # number = 683
Pick center of cone
.
      # number = 684
Pick axis of cone
.
      # number = 685
Pick circle defining cylinder
.
      # number = 686
Enter height of cylinder
.
      # number = 687
Pick 1st point of cylinder
.
      # number = 688
Pick 2nd point of cylinder
.
      # number = 689
Enter radius of cylinder
.
      # number = 690
Pick center of cylinder
.
      # number = 691
Pick axis of cylinder
.
      # number = 692
Pick circle defining sphere
.
      # number = 693
Enter radius of sphere
.
      # number = 694
Pick center of sphere
.
      # number = 695
Pick inner radius circle of torus
.
      # number = 696
Pick outer radius circle of torus
.
      # number = 697
Enter circular radius of torus
.
      # number = 698
Enter axial radius of torus
.
      # number = 699
Pick center of torus
.
      # number = 700
Pick axis of torus
.
      # number = 701
Pick axial radius circle of torus
.
      # number = 702
Pick curve to extrude
.
      # number = 703
Pick extrusion vector
.
      # number = 704
Enter extrusion distance
.
      # number = 705
Pick solid(s)
.
      # number = 706
Enter minimum and maximum Z-values [min,max]
.
/*
.....Direction
*/
      # number = 707
Pick the first drive plane
.
      # number = 708
Pick the second drive plane
.
     # number = 709
Pick direction vector
.
/*
.....More Motion
*/
     # number = 710
Pick motion for distance
.
/*
.....More Shape Prompts
*/
     # number = 711
Pick a solid, surface or plane
.
     # number = 712
Pick a solid or surface
.
     # number = 713
Pick first component of composite curve
.
     # number = 714
Pick second component of composite curve
.
     # number = 715
Pick geometry to set initial direction
.
     # number = 716
Select position on geometry where tool will be positioned
.
     # number = 717
Select geometry to position tool at
.
