/********************************************************************* 
**  NAME:  gkserrormet.h
**
**      symbolic names of GKS error messages.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gerrormet.h , 25.1
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

#ifndef GKSERRORMETH
/* metafile errors */
#define ERESERVE 160			/* item type not allowed for user items */
#define EBDLNGTH 161			/* item length is invalid */
#define EMNOITEM 162			/* no item is left in GKS metafile input */
#define EMITMINV 163			/* metafile item is invalid */
#define ENOTGKSI 164			/* item type is not a valid GKS item */
#define EBADCNTS 165			/* contents of item data record is invalid for item type */
#define EEBDNXDR 166			/* max item data record length is invalid */
#define EINTERPT 167			/* user item cannot be interpreted */
#define ENOARCF  168			/* no archive file is open */
#define ENOARCOLD 169		/* archive file doesn't exist */
/* escape errors */
#define ENOFUNCT 180			/* specified function is not supported */
#define EESCDATA 181			/* contents of escape data record are invalid */
#define GKSERRORMETH
#endif
