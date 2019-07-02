/*********************************************************************
**    NAME         :  drubber.h
**       CONTAINS:
**       	Rubber banding, dragging, and other dynamics
**
**				To do rubber band line the sequence is
**					RUBBER_LINE to initialize
**					RUBBER_ON to invoke rubber banding from the last input point
**					RUBBER_OFF when through
**
**				To do rubber band box the sequence is
**					RUBBER_BOX to initialize
**					RUBBER_ON to invoke rubber box from the last input point
**					RUBBER_OFF when through
**
**				To do drag some graphics the sequence is
**					DRAG_GRAPHICS to initialize
**					DRAG_ON to invoke the graphics as the cursor
**					DRAG_OFF when through
**
**				The macros will take care of cleaning up in the event of
**				a command reject or jump-to-root action by the operator.
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       drubber.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:14
*********************************************************************/

#ifndef DRUBBER
#include "usysdef.h"
#include "dmark.h"
#include "gobas.h"

/*	-- Rubber line, box, and dragging control block -- */

	typedef struct
	{
		UU_LOGICAL rubber; 		/* rubber band supported flag */
		UU_LOGICAL rubberon;		/* rubber band active flag */
		int rubberpet;				/* pet of r.b. requested 
											or number of device in drag */
		int type;					/* save interaction type */
		int dev;						/* save device number */
		int pet;						/* save pet */
		int markval;				/* MARK save cell */
	} UD_RUBBER;

#ifdef __cplusplus 
	extern "C" int ud_onrub(UD_RUBBER *rubinfo);
	extern "C" int ud_strub(UD_RUBBER *rubinfo, int pet);
	extern "C" int ud_endrub(UD_RUBBER *rubinfo);
	extern "C" int ud_ondrag(UD_RUBBER *draginfo);
	extern "C" int ud_stdrag(UD_RUBBER *draginfo, int segno, Gwpoint3 *center, int normtran);
	extern "C" int ud_enddrag(UD_RUBBER *draginfo);
#endif
/*********************************************************************
**    E_FUNCTION :  RUBBER_ON(rcb)
**       activate rubber line or box
**
**    PARAMETERS   
**       INPUT  : 
**          rcb = rubber control block
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define RUBBER_ON(rcb)  ud_onrub(rcb)

/*********************************************************************
**    E_FUNCTION :  RUBBER_OFF(rcb)
**       stop rubber box and line
**
**    PARAMETERS   
**       INPUT  : 
**          rcb = rubber control block
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define RUBBER_OFF(rcb) { ud_endrub(rcb); UD_UNMARK((*rcb).markval); }

/*********************************************************************
**    E_FUNCTION :  DRAG_ON(rcb)
**       activate drag segment
**
**    PARAMETERS   
**       INPUT  : 
**          rcb = rubber control block
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define DRAG_ON(rcb)  ud_ondrag(rcb)

/*********************************************************************
**    E_FUNCTION :  DRAG_OFF(rcb)
**       deactivate drag segment
**
**    PARAMETERS   
**       INPUT  : 
**          rcb = rubber control block
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define DRAG_OFF(rcb) { ud_enddrag(rcb); UD_UNMARK((*rcb).markval); }

/*********************************************************************
**    I_FUNCTION :  RUBBER_INIT(rcb)
**       initialize rubber box and rubber line
**
**    PARAMETERS   
**       INPUT  : 
**          rcb = rubber control block
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

#define RUBBER_INIT(rcb, pet) { \
	ud_strub(rcb, pet); \
	UD_MARK((*rcb).markval, UU_FALSE); \
	if((*rcb).markval != 0) \
		RUBBER_OFF(rcb); }

/*********************************************************************
**    E_FUNCTION :  DRAG_GRAPHICS(rcb, segno, center, normtran)
**       initialize dragging function
**
**    PARAMETERS   
**       INPUT  : 
**          rcb = rubber control block
**				segno = segment number to drag
**				center = origen of drag segment
**				normtran = normalization transform of drag segment
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define DRAG_GRAPHICS(rcb, segno, center, normtran) { \
	ud_stdrag(rcb, segno, center, normtran); \
	UD_MARK((*rcb).markval, UU_FALSE); \
	if((*rcb).markval != 0) \
		DRAG_OFF(rcb); }

/*********************************************************************
**    E_FUNCTION :  RUBBER_LINE(rcb)
**       initilize rubber line function
**
**    PARAMETERS   
**       INPUT  : 
**          rcb = rubber control block
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define RUBBER_LINE(rcb) RUBBER_INIT(rcb, 4)

/*********************************************************************
**    E_FUNCTION :  RUBBER_BOX(rcb)
**       initilize rubber box function
**
**    PARAMETERS   
**       INPUT  : 
**          rcb = rubber control block
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#define RUBBER_BOX(rcb)  RUBBER_INIT(rcb, 5)

#define DRUBBER
#endif
