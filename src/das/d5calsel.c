/*********************************************************************
**
**    NAME         :  d5calsel.c
**
**       CONTAINS:	ud_calsel()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d5calsel.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:10
**
*********************************************************************/

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) d5calsel.c 3.3 7/25/88 14:32:10 single"};
#else
static char uu_sccsident[]={"@(#) d5calsel.c 3.3 7/25/88 14:32:10 double"};
#endif

#include "usysdef.h"
#include "umath.h"
#include "dselect.h"
#include "udebug.h"
#include "gtbl.h"
#include "ginq.h"
#include "xenv1.h"		/* used by "ux" calls: UX_PRTERRS value */

/*********************************************************************
**
**    E_FUNCTION        :  ud_calsel(filter_on,vis,xform)
**
**      initializes select buffer and calls ud_region2() 
**
**    PARAMETERS   
**
**       INPUT  :  filter_on = UU_TRUE, UU_FALSE
**						vis = (0=visible, 1=invisible, 2=both)
**						xform = xform of vport to select from
**						hilite = UU_TRUE, UU_FALSE
**
**       OUTPUT :  
**
**          none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : adds entities to global select list
**
**    WARNINGS     : don't allow user input between call to ud_calsel()
**		and call to ud_gnxt() -- select buffer contents could be lost
**
*********************************************************************/

ud_calsel(filter_on,vis,xform,hilite)
UU_LOGICAL filter_on;			/* attribute filtering? */
int vis;								/* vis or invis entities ? */
int xform;							/* xform of viewport */
UU_LOGICAL hilite;				/* hilite selected entities? */
{
	Gnpoint center;					/* center of region */
	Gfloat dx, dy;						/* length & width of region */
	int selmode = UD_SELECTIN;		/* pick only entities entirely inside region */
	int num_match = 0;				/* # of input items matched by sscanf */
	char *selbuf_str;					/* size of select buffer as string */
	char *ux_getenv();
	Gnrect3 *nrect;					/* ndc corners of viewport */
	extern UU_LOGICAL UD_hilite;	/* controls hiliting of selected entities */
	int stat = 0;
	/* start of executable code */

	uu_denter(UU_DTRC,(us,"entering ud_calsel(%d %d %d)",
		filter_on, vis, xform));

	/* malloc space for select buffer if this is the first use of it */
	if (UD_Selinit)
	{
		if ((selbuf_str = ux_getenv("UD_SELBUF_SIZ",UX_PRTERRS)) == NULL)
			UD_Selbuf_size = 10000;
		else
		{
			num_match = sscanf(selbuf_str,"%d",&UD_Selbuf_size);
			if (num_match == 0)
				UD_Selbuf_size = 10000;
		}

		uu_denter2(UU_DTRC,(us,"ud_calsel: num_match %d selbuf_size %d",
			num_match, UD_Selbuf_size));
		uu_dexit;

		UD_Select_buf = (int*)uu_toolmalloc(UD_Selbuf_size * sizeof(int));
		UD_Selinit = UU_FALSE;
	} 

	/* get filter data if needed */
	if (filter_on)
		stat = ud_getfilt();
	if (stat==-1)
		return -1;
	/* initialize select buffer */
	UD_Select_ptr = 0;
	UD_Select_cnt = 0;

	/* calculate region center, width, length */
	nrect = gqvport3(xform);
	center.x = (nrect->urb.x + nrect->llf.x) / 2.0;
	center.y = (nrect->urb.y + nrect->llf.y) / 2.0;
	dx = nrect->urb.x - center.x;
	dy = nrect->urb.y - center.y; 

	uu_denter2(UU_DTRC,(us,"ud_calsel: center %g %g dx dy %g %g",
		center.x, center.y, dx, dy));
	uu_dexit;

	/* set hilite flag */
	UD_hilite = hilite;

	/* now call ud_region2() to fill select buffer */
	ud_region2(&center,dx,dy,selmode,filter_on,vis);

	/* set entities pickable again */
	ud_spstat(UU_TRUE, 0);

	/* reset hilite flag */
	UD_hilite = UU_TRUE;

	uu_dexit;
	return 0;
}
