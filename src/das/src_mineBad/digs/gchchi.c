/*********************************************************************
**    NAME         :  gchchi.c - choice device hilight routines
**       CONTAINS:
**						gschchilite() - Set choice device hilighting
**						ug_chchilite() - Set choice device hilighting
**
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
** 
**    MODULE NAME AND RELEASE LEVEL
**       gchchi.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:17
**********************************************************************/

#include "udebug.h"
#include "gichoice.h"
#include "gtblvar4.h"
#include "gerrorst.h"
#include "gerrorinp.h"
#include "gdidd.h"
#include "zsysdep.h"

/*********************************************************************
**    E_FUNCTION :  gschoicehilite(ws,devno,choiceno,h)
**    PARAMETERS   
**       INPUT  :
**					 Gws *ws - work station id
**					 Gint devno - device number
**				    Gint choiceno - number of choice in devno to be highlighted
**					 Gseghi highlighting -- either UG_NORMAL or UG_HIGHLIGHTED.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : if devno or choiceno doesn't exist, no action
**				is taken.  Nothing is done if pet!=5 (Not an icon array)
*********************************************************************/
Gerror gschoicehilite(ws,devno,choiceno,h)
Gws *ws;
Gint devno;
Gint choiceno;
Gseghi h;
{
	Gchoicest *choicept;
	int indx, mask=0;
	int irtn;
	Gnrect rect;
	Gfloat dx, dy;

	uu_denter(UU_GTRC,(us,"gschoicehilite(dev=%d, chc=%d, hilite=%d)",
					devno, choiceno, h));
	irtn = NCL_NO_ERROR;
	choicept= &(*ug_gksstli.wsopen[*ws].inptr).choicedata[devno-1];	
#ifdef UU_CHECK

	if((devno < 1) || (devno > (*ug_gksstli.wsopen[*ws].inptr).nchoice))
		{
		/* Invalid device number */
		ug_errorhand(ENOINDEV,"ginitchoice",&devno);
		irtn=ENOINDEV;
		goto ret;
		}

	if(( choiceno < -1) || ( choiceno > choicept->record.number))
		{
		ug_errorhand(ENOINDEV,"ginitchoice",&devno); 
		irtn=ENOINDEV;
		/* Bad choiceno 	*/
		goto ret;
		}

	if(choicept->pet != 5)
		{
		/* Bad Pet 	*/
		goto ret;
		}

#endif

	if(choiceno == -1)
		{
		int i;

		if(h == UG_NORMAL)
			{
			for(i=1;i<=32;i++)
				{
				if(i>choicept->record.number)	break;

				if(((choicept->record.hilite[0] >> (i-1)) & 1) != 0)
					gschoicehilite(ws, devno, i, h);
					
				if((i+32) > choicept->record.number) continue;
					
				if(((choicept->record.hilite[1] >> (i-1)) & 1) != 0)
					gschoicehilite(ws, devno, 32+i, h);

				}
			}
		else		/* UG_HIGHLIGHTED	*/
			{
			for(i=1;i<=32;i++)
				{
				if(i>choicept->record.number)	break;

				if(((choicept->record.hilite[0] >> (i-1)) & 1) == 0)
					gschoicehilite(ws, devno, i, h);
					
				if((i+32) > choicept->record.number) continue;

				if(((choicept->record.hilite[1] >> (i-1)) & 1) == 0)
					gschoicehilite(ws, devno, 32+i, h);
					
				}
			}
		goto ret;
		}
	indx = (int)choiceno/32.0;		/* KLUDGE - 32 is bits/int - shouldn't be hard coded*/
	mask = 1<<(choiceno-1);
	uu_dprint(UU_GTRC,(us,"gschoicehilite: mask = %d", mask));
	uu_dprint(UU_GTRC,(us,"gschoicehilite:start highlite = %d %d",
						choicept->record.hilite[0], choicept->record.hilite[1]));
	if(h == UG_HIGHLIGHTED)
		{
		if(choicept->record.hilite[indx] & mask)	/* If already hilited */
			goto ret;
		dx = choicept->e_area.ur.x - choicept->e_area.ll.x;
		dy = choicept->e_area.ur.y - choicept->e_area.ll.y;
		rect.ll.x=choicept->e_area.ll.x + 
			choicept->record.chposn[choiceno-1].ll.x * dx;
		rect.ll.y=choicept->e_area.ll.y + 
			choicept->record.chposn[choiceno-1].ll.y * dy;
		rect.ur.x=choicept->e_area.ll.x +
			choicept->record.chposn[choiceno-1].ur.x * dx;
		rect.ur.y=choicept->e_area.ll.y +
			choicept->record.chposn[choiceno-1].ur.y * dy;
		irtn=ug_choicehilite(ws,devno,choiceno,&rect,h);	/* Hilite the choice in devno */
		choicept->record.hilite[indx] |= mask;	/* Update mask	*/
		}
	else
		{
		if ( !(choicept->record.hilite[indx] & mask)) /* Not hilited	*/
			goto ret;
		dx = choicept->e_area.ur.x - choicept->e_area.ll.x;
		dy = choicept->e_area.ur.y - choicept->e_area.ll.y;
		rect.ll.x=choicept->e_area.ll.x + 
			choicept->record.chposn[choiceno-1].ll.x * dx;
		rect.ll.y=choicept->e_area.ll.y + 
			choicept->record.chposn[choiceno-1].ll.y * dy;
		rect.ur.x=choicept->e_area.ll.x +
			choicept->record.chposn[choiceno-1].ur.x * dx;
		rect.ur.y=choicept->e_area.ll.y +
			choicept->record.chposn[choiceno-1].ur.y * dy;
		irtn=ug_choicehilite(ws,devno,choiceno,&rect,h);	/* Hilite the choice in devno */
		choicept->record.hilite[indx] &= (~mask);	/* Update mask	*/
		}
	uu_dprint(UU_GTRC,(us,"gschoicehilite:end highlite = %d %d",
						choicept->record.hilite[0], choicept->record.hilite[1]));
ret:;
	uu_dexit;
	return(irtn);
}


/*********************************************************************
**    I_FUNCTION :  ug_choicehilite(ws,devno, choiceno, rect,h);
**    PARAMETERS   
**       INPUT  : 
**              Gws *ws - workstation
**					 Gint devno - device number
**				    Gint choiceno - number of choice in devno to be highlighted
**					 Gnrect *rect - ll and ur of icon to be highlighted
**					 Gseghi h - UG_HIGHLIGHTED or UG_NORMAL
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : No error checking
*********************************************************************/
Gerror ug_choicehilite(ws,devno,choiceno,rect,h)
Gws *ws;
Gint devno;         		/* Not used for Raster Devices */
Gint choiceno;         	/* Not used for Raster Devices */
Gnrect *rect;
Gseghi h;
	{
	int irtn;
	struct { Gws ws; Gint devno; Gint choiceno; Gnrect rect; Gseghi h; } prms;

	uu_denter(UU_GTRC,(us,"ug_choicehilite(dev=%d, chc=%d)",
					devno, choiceno));

	irtn = NCL_NO_ERROR;

	/* call workstation's UG_DCHOICEHILITE jump table entry */
	prms.ws = *ws;
	prms.devno = devno;
	prms.choiceno = choiceno;
	zbytecp(prms.rect,*rect);
	prms.h = h;
	(*(ug_gksstli.wsopen[*ws].connid)[UG_DCHOICEHILITE])(&prms);

	uu_dexit;
	return(irtn);
	}
