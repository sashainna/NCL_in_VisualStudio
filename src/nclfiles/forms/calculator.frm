#HEADER#
/TITLE/ Calculator
/POSITION/ 50,30
/SIZE/385,190

#DISPLAY#
/LABEL/ Results:
/POSITION/ 15,8
/SIZE/ 140,17
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 40
/JUSTIFIED/ RIGHT
/FONT/ 1.6
/COLOR/ BLUE,DEFAULT

#CHOICEBOX#      
/LABEL/
/POSITION/ 225, 8
/SIZE/ 35,40
/TYPE/ UD_DASSTRING
/CHOICES/ "TZS", "0", "1", "2","3","4", "5", "6", "7", "8"

#CHOICEBOX#      
/LABEL/
/POSITION/ 270, 8
/SIZE/ 35,40
/TYPE/ UD_DASSTRING
/CHOICES/ "DEC", "DMS"

#PUSHBUTTON#
/LABEL/ Assign
/POSITION/ 315,6
/SIZE/ 50,17
/TYPE/ UD_DASSTRING
/COLOR/ RED,GREY

#EDIT#
/LABEL/ Formula:
/POSITION/ 15,28
/SIZE/ 245,17
/TYPE/ UD_DASSTRING
/LEN/ 70
/PREC/ 10

#DISPLAY#
/LABEL/
/POSITION/ 340,28
/SIZE/ 25,17
/TYPE/ UD_DASINT
/LEN/ 4
/PREC/ 4
/JUSTIFIED/ RIGHT

#CHECKBOX#
/LABEL/ Inv
/POSITION/ 15,48
/SIZE/ 30,17
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Sin
/POSITION/ 15,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Cos
/POSITION/ 43,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Tan
/POSITION/ 71,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Atan2
/POSITION/ 99,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Angl
/POSITION/ 15,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Dist
/POSITION/ 43,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Dot
/POSITION/ 71,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Lnth
/POSITION/ 99,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Abs
/POSITION/ 15,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Int
/POSITION/ 43,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Nint
/POSITION/ 71,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Exp
/POSITION/ 99,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Sqrt
/POSITION/ 15,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Cbrt
/POSITION/ 43,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Log
/POSITION/ 71,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ Log10
/POSITION/ 99,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ '
/POSITION/ 191,48
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/FONT/ 1.8

#PUSHBUTTON#
/LABEL/ ^
/POSITION/ 219,48
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/FONT/ 1.8

#PUSHBUTTON#
/LABEL/ 7
/POSITION/ 135,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_7
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ 8
/POSITION/ 163,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_8
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ 9
/POSITION/ 191,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_9
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ /
/POSITION/ 219,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_divide
/FONT/ 2.0
/COLOR/ BROWN,SEAGRN

#PUSHBUTTON#
/LABEL/ 4
/POSITION/ 135,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_4
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ 5
/POSITION/ 163,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_5
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ 6
/POSITION/ 191,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_6
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ *
/POSITION/ 219,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_multiply
/FONT/ 2.0
/COLOR/ BROWN,SEAGRN

#PUSHBUTTON#
/LABEL/ 1
/POSITION/ 135,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_1
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ 2
/POSITION/ 163,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_2
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ 3
/POSITION/ 191,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_3
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ -
/POSITION/ 219,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_subtract
/FONT/ 2.0
/COLOR/ BROWN,SEAGRN

#PUSHBUTTON#
/LABEL/ 0
/POSITION/ 135,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_0
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ .
/POSITION/ 163,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_decimal
/FONT/ 2
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ +/-
/POSITION/ 191,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/FONT/ 1.5
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ +
/POSITION/ 219,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_add
/FONT/ 2.0
/COLOR/ BROWN,SEAGRN

#PUSHBUTTON#
/LABEL/ BS
/POSITION/ 256,50
/SIZE/ 34,17
/TYPE/ UD_DASSTRING
/COLOR/ RED,GREY
/FONT/ 1.4

#PUSHBUTTON#
/LABEL/ CE
/POSITION/ 293,50
/SIZE/ 34,17
/TYPE/ UD_DASSTRING
/COLOR/ RED,GREY
/FONT/ 1.4

#PUSHBUTTON#
/LABEL/ C
/POSITION/ 330,50
/SIZE/ 34,17
/COLOR/ RED,GREY
/FONT/ 1.4

#PUSHBUTTON#
/LABEL/ pi
/POSITION/ 255,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,YELLOW

#PUSHBUTTON#
/LABEL/ x**2
/POSITION/ 283,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ x**3
/POSITION/ 311,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ x**y
/POSITION/ 339,71
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ 1/x
/POSITION/ 255,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ BLUE,PINK

#PUSHBUTTON#
/LABEL/ ,
/POSITION/ 283,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/FONT/ 2.0

#PUSHBUTTON#
/LABEL/ (
/POSITION/ 311,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/FONT/ 1.8
/COLOR/ BROWN,SEAGRN

#PUSHBUTTON#
/LABEL/ )
/POSITION/ 339,94
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/FONT/ 1.8
/COLOR/ BROWN,SEAGRN

#PUSHBUTTON#
/LABEL/ MR
/POSITION/ 255,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ CYAN,BROWN

#PUSHBUTTON#
/LABEL/ MS
/POSITION/ 283,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ CYAN,BROWN

#PUSHBUTTON#
/LABEL/ M+
/POSITION/ 311,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ CYAN,BROWN

#PUSHBUTTON#
/LABEL/ MC
/POSITION/ 339,117
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ CYAN,BROWN

#PUSHBUTTON#
/LABEL/ =
/POSITION/ 255,140
/SIZE/ 53,20
/TYPE/ UD_DASSTRING
/SHORTCUT/ kp_enter
/FONT/ 2.0
/COLOR/ BROWN,SEAGRN

#PUSHBUTTON#
/LABEL/ M-
/POSITION/ 311,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ CYAN,BROWN

#PUSHBUTTON#
/LABEL/ Scalar
/POSITION/ 339,140
/SIZE/ 25,20
/TYPE/ UD_DASSTRING
/COLOR/ RED,GREY

#HELP#

Results
-------

The Results window is the standard display of a calculator.  It will contain
the numeric input/calculation of the calculator functions.  This is a 
display only field and cannot be modified directly by the user. The result 
will always be a current result or current input. When a multiple values 
entered, it always shows the last input.

Precision Choice Box
--------------------

This choice box contains the choices of the result precision you want to use: 
When TZS is used, we will keep 8 digits after decimal point but removed 
trailing 0s, for example, 1.0000 will display as 1.0, 91.000012348 will 
display as 91.00001235. Other choices (1 to 8) will display 'n' (n=1 to 8) 
digits after decimal point but with trailing 0s, for example, we pick '4' as 
the choice, then (1.0000 will display as 1.0000, 91.000012348 will display 
as 91.0000).

Display format Choice Box
-------------------------

This choice box contains the choices of DEC and DMS: DEC will display the 
standard decimal notation for the results field while DMS will display the 
results in degrees, minutes, seconds using the following format:  deg'min^sec
In DMS format, there are 60seconds per minute and 60 minutes per degree. 
The only value that can have a decimal point is the seconds value.

Assign
------

When in choice mode, the Assign button will assign the generated formula 
to a scalar by issuing a standard NCL assignment command, ex. A=3/12*sin(15).
When the Assign button is pressed, then the formula will be finalized as if 
the = key was pressed and the Define Scalar form will be activated with the 
Value field defaulting to the Formula window.  The scalar will actually be 
defined from the Define Scalar form.
When in command/prompt mode or when another form is active, the Assign key 
will act as an accept button, it will assign the formula/result to the 
command window or a form field. It will close the form after that.

Formula Window
--------------

The Formula window contains the formula created by the user interaction 
with the calculator.  The user may also manually type data into this window
and when this field is activated either by using the Enter key or by 
exiting the field, the formula will be checked for validity, processed, 
and the answer will be displayed in the Results window.

Status Window
-------------

The Status Window is used to display the open parenthesis count in the 
format(n), where n is the number of parenthesis levels currently open.

Inv
----

When the Inv box is checked, the trig keys (Sin, Cos, Tan) will be assigned
to their respective inverse functions (Asin, Acos, Atan).  In addition, the 
Dist key will be assigned to an additional distance function Tdist. The text
of the function keys will also be changed to match the inverse function text.
When this box is not checked, then the normal functions will be active.

Backspace  -  BS
----------------

The BS key deletes one keypad key input. It only applies to data input. 
For example, you input "123.45", then push BS key, it will become "123.4". 
It does not apply to other operation/function input, for example, 
input "123.45+", the BS will become disable. If you want to undo "+", 
use "CE" button.

Clear Entry  -  CE
------------------

The Clear Entry key will act as an Undo function and cancel/erase the last
interaction.  "CE" button can only undo once. 

The MS, M+, M- keys are ignored by the CE key, so whatever the previous 
key(s) pressed prior to the MS, M+, M- key will be processed as described 
above. For example:

	Input:	1+2 MS CE
	Results:	2.0 then 1.0
	Formula:	"1+2" then "1+"

Clear  -  C
-----------

The Clear key will reset the Results window back to 0. and clear the Formula 
window.  But it will not clear the memory value. MC clear the memory value.

Trig Functions  -  Sin, Cos, Tan, Atan2
---------------------------------------

The trig function keys perform the standard trig function on the value 
currently displayed in the Results field.  If the Inv box is checked, then
the Sin, Cos, Tan buttons will display Asin, Acos, Atan, respectively and 
will perform the inverse trig functions.

The Atan2 function requires two numeric values.  Therefore, the input will 
be two values separated by a comma.  The Results window only display the 
number entered later.

Geometry Functions  -  Angl, Dist, Dot, Lnth, Tdist
---------------------------------------------------

The geometry functions require the user to select one or more geometric 
entities from the screen.  When the function button is clicked, the 
calculator form hides and prompts the user to select one or 2 entities.
Once the entities are selected, the calculator form will redisplayed and 
the result of the function (with the input as selected) will be displayed in
the Results window and the geometry function will be added to the formula.
When prompted for selection, only the correct types of geometry can be 
selected. 

The Tdist function can be used to find the distance between a point-vector
a line, circle, curve, plane, surface or solid.  After selecting the geometry
a popup menu will be displayed requesting which distance to report.  The
following distances can be reported.

 - Closest:    All intersections between the point-vector and entity will
               be evaluated.  The distance between the starting point of
               the point-vector and the closest intersection of the entity
               will be returned.

 - Farthest:   The distance between the starting point of the point-vector and
               the farthest intersection of the entity will be returned.

 - Near Point: The distance between the starting point of the point-vector and
               the intersection of the entity closest to the selected near
               point will be returned.

The first entity selected for the Tdist function must be a point-vector.

Numeric Functions  -  Abs, Int, Nint, Exp, Sqrt, Cbrt, Log, Log10
-----------------------------------------------------------------

The numeric function buttons perform the standard numeric function on the 
value currently displayed in the Results field.

Numeric Keypad  -  7, 8, 9, 4, 5, 6, 1, 2, 3, 0, ., ', ^.
The Numeric Keypad is used for entering input values as on a standard 
calculator. Also we can use shortcut key kp_0, kp_1,...kp_9 for enter
numbers.

Plus / Minus Key  -  +/-
------------------------

The +/- key will change the sign of the value currently in the Results 
window.

Operation Keys  -  /, *, -, +
-----------------------------

The operation keys perform the following operations on the values entered 
prior to and after the operation key.

	/	Division
	*	Multiplication
	-	Subtraction
	+	Addition

Also we can use shortcut key kp_divide, kp_multiply, kp_subtract, kp_add
for those buttons.

pi
--

The pi key will simply enter the value of pi into the Results window, the 
same as if the digits were entered individually.  If a number is currently 
being input (the last key pressed was a number key), then this number will 
be replaced with pi.

Exponent Keys  -  x^2, x^3, x^y
-------------------------------

The exponent keys will add an exponent to the current value.  The x^2 key 
will square the value, x^3 will cube the value, and x^y will use the next 
value as the exponent.

Inversion Key  -  1/x
---------------------

The inversion key will divide 1.0 by the number displayed in the Results 
window. X=0 is not allowed.

Comma Key
---------

Enters a comma in the Results window and is used for entering values for 
functions that require two values such as the Atan2 function.


Parenthesis Keys  -  ()
-----------------------

The Open parenthesis key will start a new parenthesis level.  The level 
number of open parenthesis will be displayed in the Status Window. 

The Close parenthesis key will end the current parenthesis level.  The 
parenthesis level count in the Status Window will be decremented by 1.  

Memory Keys  -  MR, MS, M+, MC, M-
----------------------------------

The Memory keys are used to access the single memory location of the 
calculator.  The MS key will store the value currently in the Results 
window in memory.  M+ will add the current value to the memory value and 
M- will subtract the current value from the memory value.
The MR key will enter the memory value into the Results window, the same as 
if the digits were entered individually.  If a number is currently being 
input (the last key pressed was a number key), then this number will be 
replaced with the memory value.  

Equals Key  -  = 
----------------

The Equals key will finalize the formula and display the final answer in the
Results field.

Scalar Key
----------

The Scalar key will bring up the Select Scalar form.  Once a scalar is 
selected, its value will be displayed in the Results window and the scalar 
added to the formula.

