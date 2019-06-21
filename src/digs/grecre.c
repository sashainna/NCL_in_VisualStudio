/*********************************************************************
**    NAME         :  grecre.c -- fctns to re-create segs on workstation.
**					Only gets linked in if a workstation has its own segs.
**    CONTAINS:
**    ug_drecresegrect(ws,segrect,segno) -- recreate segs within rect.
**  	ug_drecresegs(ws) -- re-create and redraw segments.
**  	ug_drecresegs_1(ws,segno) --  redraw all but 1 seg.
**  	ug_drecreseg(n,erase) -- re-create and redraw segment n. 
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       grecre.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:24
*********************************************************************/

#include <stdio.h>
#include "usysdef.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gsegac.h"
#include "udebug.h"

/*********************************************************************
**    S_FUNCTION :  ug_drecresegrect(ws,segrect,segno)
**						UG_DCHGXFORM simulation routine for workstations which
**						maintain their own segments.
**       delete, recreate and draw all graphics segments whose NDC coordinates
**			have changed, except segment segno.
**    PARAMETERS   
**       INPUT  :  Gnrect *segrect
**						 Gint segno; -- seg no. not to draw (-1 = draw everything).
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_drecresegrect(ws,segrect,segno)
Gws ws;
Gnrect *segrect;				/* not used now */
Gint segno;
{
	Gnrect *rect;
	UG_segstli *p;
	int i;
	int msk;
	Gnrect3 *vp;
	int prms[4],ph;
	UG_prat3 ats;					/* save attributes here */
	
	uu_denter(UU_GITRC,(us,"ug_recresegrect(%g %g, %g %g segno=%d)",
		(*segrect).ll.x,(*segrect).ll.y,(*segrect).ur.x,(*segrect).ur.y,segno));

	gipat3(&ats);					/* save primitive attributes */
	/* first, go thru segs, deleting those that need changing */
	/* we delete first, then recreate since it looks better for the operator*/
	msk = UG_SEGNDCBOX;		/* bit in seg header if NDC coords no good */
	ug_seginitscan();
	while ((p=ug_segscan())!=NULL) {
		if (((*p).segid<UG_MAXSEGNO)&&((*p).segid!=segno)) {
			uu_dprint(UU_GITRC,(us,"ug_recresegrect. seg(%d).xforms=%x msk=%x", 
				(*p).segid,(*p).xforms,msk));
			if (((*p).xforms & msk)!=0) {		/* seg's NDC coords no good */
				prms[0]=UG_DDELSEG;
				prms[2]=(*p).segid;
				ug_wkout(prms,3);	/* tell workstation to delete segment */
			}

			/* Delete associated hilite segment if it exists */
			if( p->segatts.ghilit == UG_HIGHLIGHTED )
			{
/*
.....Do not delete the highlighted segment
.....if it does not exist
.....Bobby  -  6/11/92
*/
				ph = p->segid+UG_MAXSEGNO+1;
				if (ug_segac(ph) != UU_NULL)
				{
					prms[0]=UG_DDELSEG;
					prms[2]=(*p).segid+UG_MAXSEGNO+1;
					ug_wkout(prms,3);	/* tell workstation to delete hilite */
				}
			}
		}
	}															/* end while */
	/* now go thru segs again, recreating the ones we just deleted. */
	ug_seginitscan();
	while ((p=ug_segscan())!=NULL) {
		if (((*p).segid<UG_MAXSEGNO)&&((*p).segid!=segno)) {
			if (((*p).xforms & msk)!=0) 	/* seg's NDC coords no good */
				ug_drecreseg( p->segid,0);
			if( p->segatts.ghilit == UG_HIGHLIGHTED ) 	/* recreate the hilite */
			{
/*
.....The highlighted segment does not exist
.....Do not try to recreate it
.....(Tektronix blinking highlight)
.....Bobby  -  6/11/92
*/
				ph = p->segid+UG_MAXSEGNO+1;
				if (ug_segac(ph) == UU_NULL)
				{
					prms[0]=UG_DHILITE;
					prms[2]=(*p).segid;
					prms[3]=(int)(p->segatts.ghilit);
					ug_wkout(prms,4);	/* tell workstation to delete segment */
				}
				else
				{
					ug_drecreseg( p->segid+UG_MAXSEGNO+1,0 );
				}
			}
		}
	}															/* end while */

	gspat3(&ats);					/* restore primitive attributes */
	uu_dexit;
}

/********************************************************************* 
**  S_FUNCTION:  ug_drecresegs(ws) -- re-create and redraw segments.
**      Workstation simulation routine to re-create and redraw all segments
**			on workstation ws.
**  PARAMETERS   
**      INPUT:  Gws ws -- workstation id.
**      OUTPUT: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int ug_drecresegs(ws)						/* re-create and redraw all segs on a ws*/
Gws ws;
{
	int i;
	UG_segstli *p;

	uu_denter(UU_GITRC,(us,"ug_drecresegs()"));

	ug_wsoff(ws);								/* disable all but one ws */
	ug_seginitscan();
	while((p=ug_segscan())!=NULL) {
		if ((*p).segid<UG_MAXSEGNO)
			ug_drecreseg((*p).segid,0);		/* re-create and re-draw segment on ws */
	}
	ug_wson();									/* re-activate workstations */
	uu_dexit;
}

/********************************************************************* 
**  S_FUNCTION:  ug_drecresegs_1(ws,segno) --  redraw all but 1 seg.
**			Re-create and redraw all segments on a workstation but segno.
**      All segments but segment segno is re-created and re-drawn on 
**			workstation ws. This is useful for workstations which do not
**			maintain their own segment storage to delete a segment. They
**			first erase the screen and then call ug_drecresegs_1 with 
**			segno set to the number of the segment to be deleted.
**  PARAMETERS   
**      INPUT:  Gws ws -- workstation id.
**					 Gseg segno -- segment number of the segment not to be redrawn.
**      OUTPUT: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int ug_drecresegs_1(ws,segno)/* re-create and redraw all segs but segno on ws*/
Gws ws;
Gseg segno;
{
	UG_segstli *p;

	uu_denter(UU_GITRC,(us,"ug_drecresegs_1(segno=%d)", segno));

	ug_wsoff(ws);								/* disable all but one ws */
	ug_seginitscan();
	while ((p=ug_segscan())!=NULL) {
		if (((*p).segid!=segno)&&((*p).segid<UG_MAXSEGNO))
			ug_drecreseg((*p).segid,0);	/* re-create and re-draw segment on ws */
	}
	ug_wson();									/* re-activate workstations */

	uu_dexit;
}

/********************************************************************* 
**  S_FUNCTION:  ug_drecreseg(n,erase) -- re-create and redraw segment n. 
**      Workstation simulation routine called repeatedly by other
**			simulation routines which re-create or redraw segments. This
**			routine might someday prove useful to workstation that do not
**			maintain their own segment storage.
**  PARAMETERS   
**      INPUT:  Gseg n -- segment number of the segment to be redrawn.
**      OUTPUT: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int ug_drecreseg(n,erase)			/* re-create and redraw seg n  */
int n;							/* segment number to re-create and redraw */
int erase;						/* erase or not */
{
	int prms[3],reply[3];
	UG_segstli *sp;

	uu_denter(UU_GITRC,(us,"ug_drecreseg(%d), erase = %d", n,erase));

	prms[0]=UG_DCRESEG;
	prms[2]=n;
	ug_wkout(prms,3);					/* tell ws to create segment */
	ug_gksstli.opnseg=n;				/* pretend the segment is open. */

	sp = ug_segac(n);
	sp->wcboxok = 0;					/* Insure entire seg gets traversed */
	if( sp->segatts.gvis==UG_INVISIBLE ) {	/* If invisible */
		gssegvis(n,UG_VISIBLE);				/* Pretend it's visible for view0 */
		ug_view0(n,erase);					/* This defines segment in workstation */
		gssegvis(n,UG_INVISIBLE);			/* Set seg invisible */
	}
	else
		ug_view0(n,erase);				/* Draw the segment */
	
	prms[0]=UG_DCLOSEG;
	ug_wkout(prms,3);					/* tell ws to close the segment */

	uu_dexit;
}


