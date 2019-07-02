/********************************************************************* 
**  NAME:  gkserrorout.h
**
**      symbolic names of GKS error messages.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gerrorout.h , 25.1
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

#ifndef GKSERROROUTH
/* output primitive errors */
#define ENPOINTS 100			/* number of points is invalid */
#define ECHRCODE 101			/* invalid code in string */
#define EBDGDPID 102			/* GDP identifier is invalid */
#define EGDPDATA 103			/* content of GDP data record invalid */
#define ECANTGDP 104			/* at least one active workstation is not able to\
										generate the specified GDP */
/* segments errors */
#define EBADNAME 120			/* segment name is invalid */
#define ENAMUSED 121			/* segment name is already in use */
#define EWHATSEG 122			/* segment does not exist */
#define EWORKSET 123			/* segment does not exist on specified workstation */
#define EWISSSEG 124			/* segment does not exist on WISS */
#define ESEGOPEN 125			/* segment is open */
#define ESEGPRIR 126			/* segment priority outside range [0,1] */
#define GKSERROROUTH
#endif
