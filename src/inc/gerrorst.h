/********************************************************************* 
**  NAME:  gkserrorst.h
**
**      symbolic names of GKS error messages.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gerrorst.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:17
**
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  NO_ERROR if all went OK.
**
**  SIDE EFFECTS :  none
**
**  WARNINGS     :  none
*********************************************************************/

#ifndef GKSERRORSTH
#define NCL_NO_ERROR 0
/*  states errors */
#define ENOTGKCL 1			/* not in state GKCL */
#define ENOTGKOP 2			/* not in state GKOP */
#define ENOTWSAC 3			/* not in state WSAC */
#define ENOTSGOP 4			/* not in state SGOP */
#define ENOTACOP 5			/* not in state WSAC or SGOP */
#define ENOTOPAC 6			/* not in state WSOP or WSAC */
#define ENOTWSOP 7			/* not in WSOP, WSAC, or SGOP */
#define ENOTGWWS 8			/* not in state GKOP, WSOP, WSAC, or SGOP */
#define GKSERRORSTH
#endif
