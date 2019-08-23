#HEADER#
/TITLE/ Playback Current Motion
/POSITION/ 50,50
/SIZE/260,50

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 10,8
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ <<
/POSITION/ 60,8
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ <
/POSITION/ 85,8
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#DISPLAY#
/LABEL/ 
/POSITION/ 115,8
/SIZE/ 30,14
/TYPE/ UD_DASINT
/PREC/ 6
/LEN/ 6

#PUSHBUTTON#
/LABEL/ >
/POSITION/ 155,8
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ >>
/POSITION/ 180,8
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ -->
/POSITION/ 205,8
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#HELP#
============================
Playback Current Motion Form
============================
This form is used to playback the previous motions that are currently stored
on the Motion Display Stack.  The Motion Display Stack size is defined in the
Motion Display Form.  The Playback Control form can be used to playback any
motion that is not on the Motion Display stack.

View
----
Enters Dynamic Viewing mode.  The Playback Current Motion form will be taken
down while Dynamic Viewing is in effect and redisplayed when it is exited.

<<
--
Rewinds the motion display to the beginning of the active motion or if already
at the beginning of the active motion, the display will be rewound to the start
of the previous motion.

<
_
Steps the motion display back one move.

Text Field
----------
References the motion currently being played back.  '0' refers to the last
programmed motion and positive numbers refer to the previous motion offsets
from this motion.  This is a "display only" field and cannot be modified by
the user.

>
_
Steps the motion display forward one move.

>>
--
Fast forwards the motion display to the beginning of the next motion.

-->
---
Plays back the active motion display on the stack to the end of the motion.  If
the position is already at the end of the current motion, then the next motion
will be dislpayed.  For example, if the motion display is rewound to the
beginning motion on the stack, then the Play button --> can be used to display
each motion one at a time with each press of the button until the end of the
Motion Display stack is reached, without the need of pressing the Fast Forward
button >> to advance to the next motion each time.
