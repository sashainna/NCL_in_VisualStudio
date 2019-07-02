/********************************************************************* 
**  NAME:  gkserrorinp.h
**
**      symbolic names of GKS error messages.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gerrorinp.h , 25.1
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

#ifndef GKSERRORINPH
/* input errors */
#define ENOINDEV 140			/* specified input device not present on workstation */
#define EREQUEST 141			/* input device not in REQUEST mode */
#define ENSAMPLE 142			/* input device is not in SAMPLE mode */
#define ENOEVSMP 143			/* EVENT and SAMPLE input mode not available at this lvl of GKS */
#define ENOPETWS 144			/* specified prompt and echo type not supported on wkstn */
#define EEBOUNDS 145			/* echo area outside display space */
#define EBADDATA 146			/* contents of input data record are invalid */
#define EINQOVFL 147			/* input queue has overflowed */
#define ENOQOVFL 148			/* input queue has not overflowed */
#define EASWSCLO 149			/* input queue overflow, but assoc wkstn closed */
#define ENOCURIV 150			/* no input value of correct class in current event report */
#define EINVTOUT 151			/* timeout is invalid */
#define EADINITV 152			/* initial value is invalid */
#define GKSERRORINPH
#endif
