/********************************************************************* 
**  NAME:  gkserrorws.h
**
**      symbolic names of GKS error messages.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gerrorws.h , 25.1
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

#ifndef GKSERRORWSH
/* workstation errors */
#define EWSIDINV 20			/* workstation identifier invalid */
#define ECNIDINV 21			/* connection identifier is invalid */
#define EWSTPINV 22			/* workstation type is invalid */
#define ENOWSTYP 23			/* workstation type does not exist */
#define EWSISOPN 24			/* workstation is open */
#define EWSNOTOP 25			/* workstation is not open */
#define EWSCNTOP 26			/* workstation cannot be opened */
#define EWISSNOP 27			/* workstation indep seg storage is not open */
#define EWISSOPN 28			/* WISS is already open */
#define EWSISACT 29			/* workstation is active */
#define EWSNTACT 30			/* workstation is not active */
#define EWSCATMO 31			/* workstation is of category MO */
#define EWSNOTMO 32			/* workstation is not of category MO */
#define EWSCATMI 33			/* workstation is of category MI */
#define EWSNOTMI 34			/* workstation is not of category MI */
#define EWSCATIN 35			/* workstation is of category INPUT */
#define EWSIWISS 36			/* workstation is WISS */
#define EWSNOTOI 37			/* workstation not of category OUTIN */
#define EWSNOTIO 38			/* workstation is not of category INPUT or OUTIN */
#define EWSNOTOO 39			/* workstation not of category OUTPUT or OUTIN */
#define EWSNOTXL 40			/* workstation has no pixel store/readback facility */
#define EWSNOGDP 41			/* workstation not able to generate the GDP */
#define EBADMSG  42			/* workstation message or prompt too long (>150) */
#define EBADPRMTNO 43		/* bad prompt number (0,10) */
#define GKSERRORWSH
#endif
