/********************************************************************* 
**  NAME:  gkserroratt.h
**
**      symbolic names of GKS error messages.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gerroratt.h , 25.1
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

#ifndef GKSERRORATTH
/* output attribute errors */
#define EBADLINX 60			/* polyline index is invalid */
#define ENOLINEX 61			/* a representation for the specified polyline index has not\
										been defined on this workstation */
#define ELINELEZ 62			/* linetype is <= zero */
#define ENOLINTP 63			/* linetype not supported on this workstation */
#define EBADMRKX 64			/* polymarker index is invalid */
#define ENOMARKX 65			/* a representation for the specified polymarker index\
										has not been defined on this workstation */
#define EMARKLEZ 66			/* marker type <= zero */
#define ENOMRKTP 67			/* specified marker type not supported on this workstation */
#define EBADTXTX 68			/* text index is invalid */
#define ENOTEXTX 69			/* a representation for the specified text index has not\ 
										been defined on this workstation */
#define ETXTFLEX 70			/* text font is less than or equal to zero */
#define ENOTXTTP 71			/* requested text font not supported for the specified\
										precision on this workstation */
#define ECEXFLEZ 72			/* char expansion factor <= zero */
#define ECHHTLEX 73			/* char height <= zero */
#define ECHRUPVZ 74			/* length of char up vector is zero */
#define EBADFILX 75			/* fill area index is invalid */
#define ENOFILTP 76			/* a representation for the specified fill area index\
										has not been defined on the workstation */
#define ENOFSTYL 77			/* Specified fill area interior style not supported\
										on this workstation */
#define ESTYLLEZ 78			/* style (pattern or hatch) index <= zero */
#define EBADPATN 79			/* specified pattern index is invalid */
#define ENOHATCH 80			/* specified hatch style not supported on wkstn */
#define EPATSZLZ 81			/* pattern size value not positive */
#define ENOPATNX 82			/* a representation for the specified pattern index\
										has not been defined on this workstation */
#define ENOPSTYL 83			/* interior style PATTERN is not supported on wkstn */
#define ECADIMEN 84			/* dimensions of color array are invalid */
#define ECINDXLZ 85			/* color index < zero */
#define EBADCOLX 86			/* color index invalid */
#define ENOCOLORX 87			/* a representation for the specified color index 
										has not been defined on thid workstation */
#define ECOLRNGE 88			/* color is outside range (0,1) */
#define EBADPICK 89			/* pick identifier is invalid */
#define GKSERRORATTH
#endif
