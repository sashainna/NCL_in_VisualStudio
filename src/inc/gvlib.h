
/********************************************************************* 
**  NAME:  gvlib.h
**
**      Include file for output primitives added to GKS (gksvlib.c).
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gvlib.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:25
**
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  none
**
**  SIDE EFFECTS :  none
**
**  WARNINGS     :  none
*********************************************************************/

#ifndef GVLIBH

#include "gviw.h"
/* To add an additional command to the library:
  1. add a #define to this file to define the opcode
  2. Add another case to the switch statement in ug_viwlib.c
  3. Add another procedure in ug_viwlib to draw the object.
  4. Add a procedure named gnxxxx in genviw.c to gen the command.
*/

/* opcodes for library commands */
#define GRECT2OP UG_OPPTR+1
#define GRECT3OP UG_OPPTR+2
#define GCIRCLE2OP UG_OPPTR+3
#define GCIRCLE3OP UG_OPPTR+4
#define GARROW2OP UG_OPPTR+5

#define GVLIBH
#endif
