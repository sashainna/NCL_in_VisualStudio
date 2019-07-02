/********************************************************************* 
**  NAME:  gkserrorxf.h
**
**      symbolic names of GKS error messages.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gerrorxf.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:18
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

#ifndef GKSERRORXFH
/* transformation errors */
#define EBADXFRM 50			/* transformation number is invalid */
#define EBADRCTD 51			/* Rectangle definition is invalid */
#define EBDVIEWP 52			/* Viewport not within NDC unit square */
#define EBDWINDW 53			/* workstation window not in the NDC unit square */
#define EVIEWDSP 54			/* workstation viewport not in the display space */
#define EBADPNT  55			/* coordinate out of range (-1.0e+20,1.0e+20) */
#define GKSERRORXFH
#endif
