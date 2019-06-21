/*********************************************************************
**
**    NAME         :  daseg.c
**
**       CONTAINS:
**            ud_addpik_seg()
**            ud_free_pick_segs()
**            ud_getn_pick_segs()
**            ud_get_pickseg()
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       dpikstk.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:14
**
*********************************************************************/
#include "usysdef.h"
#include <math.h>
#include "nclfc.h"
#include "usysg.h"
#include "uhep.h"
#include "dmark.h"
#include "dinput.h"
#include "dasnog.h"
#include "diconm.h"
#include "dselect.h"
#include "udebug.h"
#include "xenv1.h"
#include "gmat4.h"
#include "ginqxf.h"
#include "gtblvar6.h"
#include "mdebug.h"
#include "nclicons.h"
#include "view.h"
#include "gtbl.h"
#include "ulist.h"
/*
.....Added by Paul to implement the "text" and "location" input. 07/10/92
*/
#include <ctype.h>
#include "class.h" 
#include "uims.h"
#include "mdcoord.h"
#include "mfort.h"  
#include "mdpick.h"
#include "driver.h"
#include "nclfc.h"
#include "gsegac.h"
#include "gviw.h"
#define INTSIZ(x) ((x+sizeof(int)-1)/sizeof(int))
#define INTSIZEOF(x) ((sizeof(x)+sizeof(int)-1)/sizeof(int))
#define MAX_PICK_SEG 10000

static int Spick_seg_num = 0;
static UU_LIST Spick_segs;

static int S_getseg_num();

/**************************************************************************
**
**  E_FUNCTION         : ud_addpik_seg(seg) 
**     add a geom into a picking stack, if there is no picking stack
**		create one.
**
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_addpik_seg(seg)
UD_DPICKS seg;
{
	if (Spick_seg_num==0)
	{
		uu_list_init (&Spick_segs, sizeof(UD_DPICKS), 25, 25);
		if (Spick_segs.data == UU_NULL) return 0;
	}
	if (Spick_seg_num+1>MAX_PICK_SEG)
		return 0;
	uu_list_push(&Spick_segs,&seg);
	Spick_seg_num++;
	return Spick_seg_num;
}
/**************************************************************************
**
**  E_FUNCTION         : ud_free_pick_segs() 
**     free picking stack
** 
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
void ud_free_pick_segs() 
{
	int i;
	UD_DPICKS *seg_array;
/*
.....Initialize routine
*/
	if (Spick_seg_num<=0) return;
	seg_array = (UD_DPICKS *)UU_LIST_ARRAY(&Spick_segs);
	if (seg_array==NULL) return;
	for (i=0; i<Spick_seg_num;i++)
	{
		if (seg_array[i].depth>0)
			uu_free(seg_array[i].pickpath);
	}
	Spick_seg_num = 0;
	uu_list_free(&Spick_segs);
}


/**************************************************************************
**
**  E_FUNCTION         : ud_getn_pick_segs()
**     Returns the number of active picking segments.
** 
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  Number of active assist segments.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
int ud_getn_pick_segs()
{
	return(Spick_seg_num);
}

/**************************************************************************
**
**  E_FUNCTION         : ud_get_pickseg(epick, remove)
**     get the top picking segment data and remove it from te stack.
** 
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          epick: picking segment data
**
**  RETURNS      :  Segment id of requested assist segment.  Returns
**                  -1 if there are no more assist segments.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_get_pickseg(epick)
UD_DPICKS *epick;
{
	if (Spick_seg_num>0)
	{
		uu_list_pop(&Spick_segs, epick);
		Spick_seg_num--;
	}
	return 0;
}
