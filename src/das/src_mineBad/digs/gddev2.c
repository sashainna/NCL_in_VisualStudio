/*********************************************************************
**    NAME         :  gddev2.c -- service and sim routines for wsdev.c
**       CONTAINS:
**		int ug_dloctoq(wid,devno) -- put loc event on queue.
**		int ug_dstroketoq(wid,devno) -- put stroke event on queue.
**		int ug_dvaltoq(wid,devno,val) -- put val event on q.
**		int ug_dchoicetoq(wid,ch,devno) -- put choice event on q.
**		int ug_dstringtoq(wid,strbuf) -- put string event on q.
**		int ug_case0(wid,k,j) -- Check for devsel key.
**		int ug_updpick(wid,devno,seg,depth,j)
**		int ug_updchoice(wid,k,j,devno)
**		int ug_updloc(wid,xy,j,devno)
**		int ug_updstroke(wid,xy,j,devno)
**		int ug_updstring(wid,str,j,devno)
**		int ug_updval(wid,value,j,devno)
**		ug_dfindpick(loc,seg,depth,wid) -- find closest segment. 
**		ug_dfindpk(loc,seg,depth,aper,xform) 
**		int ug_dtxt(ws,pos,str) -- put up text on ws.
**		int ug_drect(wid,x1,y1,x2,y2) -- draw a rectangle.
**		int ug_dborder(wid,earea,rasll,rasur) -- draw a border.
**		int ug_detlin(s) -- get line into string s.
**		int ug_dfndlet(c,p,n) -- find letter c in menu strings.
**		int ug_dsquare(wid,xy)  -- tablet square.
**		int ug_ckpuck(wid,devtype,k,xy)
**		int ug_dmenuno(wid,xy,item) 
**		int ug_dmenun(wid,xy) 
**		int ug_dmenuin(loc,choicept)
**		ug_dchht(prms) -- char height. Not impl yet.
**		ug_noop() -- graphics no op.
**		int ug_dchoice(ws,devno,xy,k,choice,menu)
**		ug_drastorc(xy,wdtpt,row,col,round) -- convert raster to row,col
**		int ug_drctoras(xy,wdtpt,row,col)
**		Gint ug_dshrink(wdtpt,irasll,irasur,orasll,orasur,rc,nrnc)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gddev2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:18
*********************************************************************/
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "gcolors.h"
#include "gsegac.h"
#include "ginq.h"
#include "driver.h"

extern struct {
	Giclass curdevclas;		/* currently being used device class */
	Gint curdevno;			/* currently being used device number */
	Giclass curreqclas;		/* currently requested device class, or -1 */
	Gint curreqno;			/* currently requested device number */
} ug_wsdev;

extern UU_REAL NCL_pick_aper;
void ug_dfindpk();
void ug_dborderras();
void ug_drastorc();
/*********************************************************************
**    I_FUNCTION     :  int ug_dloctoq(wid,devno) -- put loc event on queue.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dloctoq(wid,devno)			/* put loc event on queue */
Gws wid;
Gint devno;
{
	Glocst *locpt;
	locpt= &(*ug_gksstli.wsopen[wid].inptr).locdata[devno-1];
	uu_denter2(UU_GITRC,(us,"ug_dloctoq(%d,%d) x,y=%g %g",wid,devno,
				(*locpt).loc.position.x,(*locpt).loc.position.y));
	ug_inqueue[ug_inqlen].type.ws= &ug_gksstli.wsopen[wid].id;
	ug_inqueue[ug_inqlen].type.dev=devno;
	ug_inqueue[ug_inqlen].type.class=UG_E_LOCATOR;
	ug_inqueue[ug_inqlen].time=ug_etime;
	ug_inqueue[ug_inqlen].event.locevent.transform=(*locpt).loc.transform;
	ug_inqueue[ug_inqlen].event.locevent.position.x=(*locpt).loc.position.x;
	ug_inqueue[ug_inqlen].event.locevent.position.y=(*locpt).loc.position.y;
	ug_inqlen++;
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  int ug_dstroketoq(wid,devno)--put stroke event on queue.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dstroketoq(wid,devno)			/* put stroke event on queue */
Gws wid;
Gint devno;
{
	Glocst *strokept;
	strokept= &(*ug_gksstli.wsopen[wid].inptr).locdata[devno-1];

	uu_denter2(UU_GITRC,(us,"ug_dloctoq(%d,%d)",wid,devno));
	ug_inqueue[ug_inqlen].type.ws= &ug_gksstli.wsopen[wid].id;
	ug_inqueue[ug_inqlen].type.dev=devno;
	ug_inqueue[ug_inqlen].type.class=UG_E_STROKE;
	ug_inqueue[ug_inqlen].time=ug_etime;
	ug_inqueue[ug_inqlen].event.strokeevent.transform=0;
	ug_inqlen++;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION   :  int ug_dvaltoq(wid,devno,val) -- put val event on q.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dvaltoq(wid,devno,val)				/* put valuator devno on q */
Gws wid;
Gint devno;
Gfloat val;
{
	uu_denter(UU_GITRC,(us,"ug_dvaltoq(%d,%f,%d)",wid,devno,val)); 
	ug_inqueue[ug_inqlen].type.ws= &ug_gksstli.wsopen[wid].id;
	ug_inqueue[ug_inqlen].type.dev=devno;
	ug_inqueue[ug_inqlen].type.class=UG_E_VALUATOR;
	ug_inqueue[ug_inqlen].time=ug_etime;
	ug_inqueue[ug_inqlen].event.valevent=val;
	ug_inqlen++;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION   :  int ug_dchoicetoq(wid,ch,devno) -- put choice event on q.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dchoicetoq(wid,ch,devno)		/* put choice event on queue 
												for choice device devno */
Gws wid;
int ch;									/* choice value */
int devno;								/* choice device number */
{
	Gchoicest *choicept;
	choicept= (*ug_gksstli.wsopen[wid].inptr).choicedata;
	uu_denter2(UU_GITRC,(us,"ug_dchoicetoq(%d,%d,%d)",wid,ch,devno));

	ug_inqueue[ug_inqlen].type.ws= &ug_gksstli.wsopen[wid].id;	/* ptr to ws */
	ug_inqueue[ug_inqlen].type.dev=devno;						/* device number */
	ug_inqueue[ug_inqlen].type.class=UG_E_CHOICE;
	ug_inqueue[ug_inqlen].time=ug_etime;
	ug_inqueue[ug_inqlen].event.choiceevent=ch;				/* choice value */
	ug_inqlen++;											/* bump queue length */
	uu_dexit;
}
/* 
   	ug_inqueue[ug_inqlen].type.ws= &ug_gksstli.wsopen[wid].id;
   	ug_inqueue[ug_inqlen].type.dev=devno;
   	ug_inqueue[ug_inqlen].type.class=UG_E_LOCATOR;
   	ug_inqueue[ug_inqlen].time=ug_etime;
   	ug_inqueue[ug_inqlen].event.locevent.transform=(*locpt).loc.transform;
   	ug_inqueue[ug_inqlen].event.locevent.position.x=(*locpt).loc.position.x;
   	ug_inqueue[ug_inqlen].event.locevent.position.y=(*locpt).loc.position.y;
   	ug_inqlen++;
*/
/*********************************************************************
**    I_FUNCTION   :  int ug_dstringtoq(wid,strbuf) -- put string event on q.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dstringtoq(wid,strbuf)			/* put string event on queue */
Gws wid;
char *strbuf;								/* string to go on queue */
{
	uu_denter2(UU_GITRC,(us,"ug_dstringtoq. strbuf=%s",strbuf));
	ug_inqueue[ug_inqlen].type.ws= &ug_gksstli.wsopen[wid].id;
	ug_inqueue[ug_inqlen].type.dev=1;
	ug_inqueue[ug_inqlen].type.class=UG_E_STRING;
	ug_inqueue[ug_inqlen].time=ug_etime;
	strcpy(ug_inqueue[ug_inqlen].event.stringevent,strbuf);
	ug_inqlen++;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_case0(wid,k,j) -- Check for devsel key.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_case0(wid,k,j)			/* k is keypad2 value. check for devsel key.
												Return 0 if so. Else update choice dev 5
												put on Q if UG_EVENT mode, return 1 */
Gws wid;						/* workstation id */
int k;						/* keypad 2 value */
int *j;						/* return j=1 if an UG_EVENT went to q, else leave alone*/
{
	int irtn;
	Gchoicest *choicept;
	if ((k>=1)&&(k<=6)) {	/* user hit a device select key */
		irtn=0;						/* return 0 */
	}
	else {						/* ordinary keypad 2 key */
		irtn=1;
		choicept= &(*ug_gksstli.wsopen[wid].inptr).choicedata[3];
		(*choicept).choice=k;		/* update choice dev 4  measure */
		if ((*choicept).mode==UG_EVENT) {
			ug_dchoicetoq(wid,k,5);	/* choice device 5 to q */
			*j=1;							/* remember something went on q */
		}
	}
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_updpick(wid,devno,seg,depth,j)
**			Update pick device devno, put on Q if in UG_EVENT mode.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_updpick(wid,devno,seg,depth,j)	/* update pick device devno,
											put on Q if in UG_EVENT mode. Set j=1 if
											an event went on Q, else leave j alone */
Gws wid;							/* workstation id */
Gidevno devno;					/* pick device number. */
int seg[];						/* list of segment numbers with pickid at end */
int depth;						/* length of seg */
int *j;
{
	Gpickst *pickpt;
	pickpt= &(*ug_gksstli.wsopen[wid].inptr).pickdata[devno-1];
	(*pickpt).pick.pickpath=seg;
	if (depth>0) {
		(*pickpt).pick.status=UG_OKPICK;
		(*pickpt).pick.depth=depth;
	}	
	else {
		(*pickpt).pick.status=UG_NOPICK;
		(*pickpt).pick.depth=0;
	}
	if ((*pickpt).mode==UG_EVENT) {
		/* put a pick event on queue (even if nothing found) */
		ug_inqueue[ug_inqlen].type.ws= &ug_gksstli.wsopen[wid].id;
		ug_inqueue[ug_inqlen].type.dev=1;
		ug_inqueue[ug_inqlen].type.class=UG_E_PICK;
		ug_inqueue[ug_inqlen].time=ug_etime;
		ug_inqueue[ug_inqlen].event.pickevent.status=(*pickpt).pick.status;
		ug_inqueue[ug_inqlen].event.pickevent.pickpath=seg;
		ug_inqueue[ug_inqlen].event.pickevent.depth=(*pickpt).pick.depth;
		ug_inqlen++;
		uu_denter2(UU_GITRC,(us,"updpick to q"));
		uu_dexit;
		*j=1;
	}
	uu_denter2(UU_GITRC,(us,"updpick(devno=%d, seg=%d %d depth=%d j=%d",
						devno,seg[0],seg[1],depth,*j));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_updchoice(wid,k,j,devno)
**			Update measure of choice device, and put on Q if UG_EVENT mode.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_updchoice(wid,k,j,devno)		/* update measure of choice
									device devno, and put on Q if UG_EVENT mode */
Gws wid;						/* workstation id */
int k;						/* choice value */
int *j;						/* return j=1 if choice went on Q, else leave alone*/
int devno;					/* choice device number */
{
	Gchoicest *choicept;
	choicept= &(*ug_gksstli.wsopen[wid].inptr).choicedata[devno-1];
	(*choicept).choice=k;		/* update choice dev devno measure */
	if ((*choicept).mode==UG_EVENT) {
		ug_dchoicetoq(wid,k,devno);	/* choice device to Q */
		uu_denter2(UU_GITRC,(us,"updchoice to q"));
		uu_dexit;
		*j=1;							/* remember something went on q */
	}
	uu_denter2(UU_GITRC,(us,"updchoice(choice=%d, devno=%d, j=%d",k,devno,*j));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_updloc(wid,xy,j,devno)
**			Update loc measure of device devno, and put on Q if UG_EVENT mode.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_updloc(wid,xy,j,devno)
Gws wid;						/* workstation id */
Gnpoint *xy;				/* location */
int *j;						/* return j=1 if put an event on Q */
int devno;					/* loc device number */
{
	Glocst *locpt;

	uu_denter(UU_GITRC,(us,"ug_updloc(%f, %f)", xy->x, xy->y));

	locpt= &(*ug_gksstli.wsopen[wid].inptr).locdata[devno-1];
	(*locpt).loc.position.x=(*xy).x;
	(*locpt).loc.position.y=(*xy).y;

	/* Find which viewport locator is in.  */
	locpt->loc.transform = ug_locvp(xy);

	if ((*locpt).mode==UG_EVENT) {
		ug_dloctoq(wid,devno);
		*j=1;
		uu_dprint(UU_GITRC,(us,"updloc to q"));
	}

	uu_dprint(UU_GITRC,(us,"updloc(xy=%g %g,devno=%d,j=%d xform=%d)",
			xy->x, xy->y,devno, *j, locpt->loc.transform)); 

	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  int ug_updstroke(wid,xy,j,devno)
**			Update stroke measure of device devno, and put on Q if UG_EVENT mode.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_updstroke(wid,xy,j,devno)		/* update stroke measure of device devno.
													If event mode, put on Q and set j=1 */
Gws wid;						/* workstation id */
Gnpoint *xy;				/* location */
int *j;						/* return j=1 if put an event on Q */
int devno;					/* loc device number */
{
	Gstrokest *strokept;

	strokept = &(*ug_gksstli.wsopen[wid].inptr).strokedata[devno-1];

/*	(*strokept).loc.position.x=(*xy).x; */
/*	(*strokept).loc.position.y=(*xy).y; */
	if ((*strokept).mode==UG_EVENT) {
		ug_dstroketoq(wid,devno);
		*j=1;
		uu_denter2(UU_GITRC,(us,"updstroke to q"));
		uu_dexit;
	}
	uu_denter2(UU_GITRC,(us,"updstroke(devno=%d,j=%d)",devno,*j)); 
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_updstring(wid,str,j,devno)
**			Update string  measure, and put on Q if UG_EVENT mode.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_updstring(wid,str,j,devno)	/* update string  measure,
											put on Q if in UG_EVENT mode */
Gws wid;						/* workstation id */
char *str;					/* string */
int *j;						/* set j=1 if an event was put on Q, else leave alone*/
int devno;					/* string device number */
{
	Gstringst *stringpt;
	static char s[120];
	uu_denter(UU_GITRC,(us,"updstring(devno=%d, str=%s)",devno,str));
	stringpt= &(*ug_gksstli.wsopen[wid].inptr).stringdata[devno-1];
	strcpy(s,str);
	(*stringpt).string=s;		/* this won't work if many callers use updstr*/
	if ((*stringpt).mode==UG_EVENT) {
		ug_dstringtoq(wid,str);		/* put string onto queue */
		uu_denter2(UU_GITRC,(us,"updstring to q"));
		uu_dexit;
		*j=1;								/* remember something went to Q */
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_updval(wid,value,j,devno)
**			Update measure of valuator device. Put on Q if UG_EVENT mode.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_updval(wid,value,j,devno)		/* update measure of valuator device
										devno. Put on Q if in UG_EVENT mode */
Gws wid;							/* workstation id */
Gfloat value;					/* value of valuator */
int *j;							/* Set to 1 if anything to Q. Else leave alone */
int devno;						/* valuator device number */
{
	Gvalst *valpt;
	valpt= &(*ug_gksstli.wsopen[wid].inptr).valdata[devno-1];
	(*valpt).val=value;					/* update current measure */
	if ((*valpt).mode==UG_EVENT) {		/* put valuator event on queue */
		ug_dvaltoq(wid,devno,value);
		*j=1;
	}
}

/********************************************************************* 
**  S_FUNCTION:  ug_dfindpick(loc,seg,depth,wid) -- find segment. 
**
**      Workstation simulation routine to find closest graphics segment to loc
**			Also calls workstation's UG_DUPDATE entry.
**  PARAMETERS   
**      INPUT:  Gnpoint *loc -- NDC point.
**      OUTPUT: int seg[] -- will contain segment list, ending with pickid.
**					 int * depth -- will be length of seg.
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dfindpick(loc,seg,depth,wid)	/* find closest segment to loc */
Gnpoint *loc;
int seg[];
int *depth;
Gws wid;
{
/*
.....Improved picking.  Allow user to specify aperture for picking.
.....WAS: #define APER (UU_REAL) .005
.....Now: NCL_pick_aper
.....Roberta Zorzynski.
*/

/*
....Modify call to use NCL_pick_aper for the picking aperture.
*/
	ug_dfindpk(loc,seg,depth,NCL_pick_aper,-1);		/* find closest seg to loc */

}

/********************************************************************* 
**  S_FUNCTION:  ug_dfindpk(loc,seg,depth,aper,xform) 
**      Workstation simulation routine to find closest segment to loc.
**  PARAMETERS   
**      INPUT:  Gnpoint *loc -- point.
**					 Gfloat aper -- pick aperture.
**      OUTPUT: int seg[] -- segment id list, with pickid at end.
**					 int *depth -- length of seg.
**
**  RETURNS      :  0 if nothihg found close to loc, else 1.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dfindpk(loc,seg,depth,aper,xform)	/* find closest graphics seg to loc,
							Return seg=segment ids and pickid, depth=length of seg.
							If not found, return depth=0. If xform>=0,
							Only look thru segs that depend on normtran xform. */
Gnpoint *loc;
int seg[];
int *depth;
Gfloat aper;						/* pick aperture */
Gint xform;				/* normtran to look thru, or -1 */
{
	UG_segstli *p;
	int i,j,k,pkid,irtn;
	UG_segat ats;
	int newseg[14],newdepth,segno;
	Gfloat closedist,newdist;
	int mask;

	uu_denter(UU_GITRC,(us,"ug_dfindpk(loc=%g %g, xform=%d)",
		(*loc).x,(*loc).y,xform));
	/* find item picked */
	zbytecp(ats,ug_defsegat);					/* default atts */
	irtn=0;
	if (xform<0) 
		mask= -1;
	else 
		mask = (1<<xform)|UG_SEGINNTRAN;		/* this xform, or any xform*/
	uu_dprint(UU_GITRC,(us,"mask=x%x",mask));
	closedist=aper;
	*depth=0;

	uu_dprint(UU_GITRC,(us,"ug_vislistok=%d",ug_vislistok));
	for( ug_vislistok ? k=0 : ug_seginitscan();
		  ug_vislistok ? k<UU_IALEN(ug_vislist) : (p=ug_segscan())!=NULL;
		  k++ ) {

		if( ug_vislistok ) {
			i = UU_IAVAL(ug_vislist, k);
			p = ug_segac(i);
			if( p == NULL ) continue;
		}
		else {
			i = p->segid;
		}

		if (i<=UG_MAXSEGNO) {		/* look thru only graphics segments */
			if( p!=NULL && ((xform<0)||(((*p).xforms&mask)!=0))) {
				if (ug_finxy(i,&ats,(*loc).x,
						(*loc).y,closedist,newseg,&newdepth,&pkid,&newdist)==1) {
					closedist=newdist;
					*depth=newdepth;
					irtn=1;
					segno=i;
					newseg[*depth]=pkid;		/* add pkid to pickpath */
					(*depth)++;
					for (j=0; j<(*depth); j++) seg[j]=newseg[j];
				}
			}
		}
	}
	uu_denter2(UU_GITRC,(us,"ug_dfindpk returns. depth=%d, seg=%d %d",
							*depth,seg[0],seg[1]));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dtxt(ws,pos,str) -- put up text on ws.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dtxt(ws,pos,str)		/* put up text on ws */
Gws ws;
Gwpoint3 *pos;					/* text position */
Gchar str[];					/* text */
{
	UG_dtext txtprms;
	uu_denter(UU_GITRC,(us,"ug_dtxt(ws=%d,pos=%g %g %g,%s)",ws,(*pos).x,(*pos).y,
					(*pos).z,str));
	txtprms.op=UG_DTEXT;
	txtprms.id=ws;
	zbytecp(txtprms.pos,(*pos));			/* structure assignment */
	txtprms.slen=strlen(str);
	strcpy(txtprms.s,str);				/* copy prompt string */
	/* call workstation's entry point to put up text */
	(*(ug_gksstli.wsopen[ws].connid)[UG_DTEXT])(&txtprms);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_drect(wid,x1,y1,x2,y2) -- draw a rectangle.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_drect(wid,x1,y1,x2,y2)				/* draw a rectangle */
int x1,x2,y1,y2;
Gws wid;
{
	(*(ug_gksstli.wsopen[wid].connid)[UG_DRASLINE])(x1,y1,x2,y1);
	(*(ug_gksstli.wsopen[wid].connid)[UG_DRASLINE])(x2,y1,x2,y2);
	(*(ug_gksstli.wsopen[wid].connid)[UG_DRASLINE])(x2,y2,x1,y2);
	(*(ug_gksstli.wsopen[wid].connid)[UG_DRASLINE])(x1,y2,x1,y1);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dborder(wid,earea,rasll,rasur) -- draw a border.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dborder(wid,earea,rasll,rasur)		/* draw a border */
										/* return rasll, rasur as raster
										corners of the border */
Gdrect *earea;						/* area for border */
int rasll[2],rasur[2];			/* returned raster coords */
Gws wid;
{
	Gint tmp,i;

	(*(ug_gksstli.wsopen[wid].connid)[UG_DNDCDEV])(&(earea->ll),rasll,wid);	
	(*(ug_gksstli.wsopen[wid].connid)[UG_DNDCDEV])(&(earea->ur),rasur,wid);	
	for (i=0; i<2; i++) {	/* swap coords so that rasll is smaller than rasur */
		if (rasur[i]<rasll[i]) {
			tmp=rasll[i]; rasll[i]=rasur[i]; 
			rasur[i]=tmp;
		}
	}
	ug_dborderras(wid,rasll,rasur);		/* draw the border */
}

/*********************************************************************
**    I_FUNCTION :  ug_dborderras(wid,rasll,rasur) Draw a border
**       Draw a border around the given raster coords.
**    PARAMETERS   
**       INPUT  :  int rasll[2],rasur[2] -- raster rect.
**						 Gws ws; -- workstation.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dborderras(wid,rasll,rasur)		/* draw a border */
Gws wid;
int rasll[2],rasur[2];
{
	int i;
	Gipoint pts[4];
	Gscale save_width;
	/* draw fill area background for menu	*/
	pts[0].x = rasll[0]; pts[0].y = rasll[1];
	pts[1].x = rasll[0]; pts[1].y = rasur[1];
	pts[2].x = rasur[0]; pts[2].y = rasur[1];
	pts[3].x = rasur[0]; pts[3].y = rasll[1];
	ug_flaras(4,pts,wid);
	/* print rectangle around menu area */
	save_width = ug_gksstli.curprats.lnbundl.width;
	ug_gksstli.curprats.lnbundl.width = 1;
	ug_drect(wid,rasll[0],rasll[1],rasur[0],rasur[1]);		
	/* draw smaller rect */
	ug_drect(wid,rasll[0]+2,rasll[1]+2,rasur[0]-2,rasur[1]-2);	
	for (i=1; i<6; i++) {						/* draw 5 line border at top */
		(*(ug_gksstli.wsopen[wid].connid)[UG_DRASLINE])
				(rasll[0],rasur[1]-i,rasur[0],rasur[1]-i);
	}
	ug_gksstli.curprats.lnbundl.width = save_width;
}
/*********************************************************************
**    I_FUNCTION     :  int ug_detlin(s) -- get line into string s.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_detlin(s)							/* get line into string s */
char *s;
{
	char *c;
	c=s;
	while ((*c=getchar())==10) ;	/* ignore leading carriage returns */
	c++;
	while ((*c=getchar())!='\n') c++;
	*c='\0';			/* string terminator overwrites newline */
}

/*********************************************************************
**    I_FUNCTION   :  int ug_dfndlet(c,p,n) -- find letter c in menu strings.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dfndlet(c,p,n)				/* find letter c in menu strings 
											pointed to by p. n=number of strings.
											Return index of string, or zero */
char c,**p;					/* p is pointer to array of pointers to strings */
int n;
{
	int i;
	for (i=1; i<=n; i++) 
	{
		if (**p == c) 
			return(i);	/* found it */
		p++;
	}
	/* gets here if didn't find it */
	return(0);
}

/********************************************************************* 
**  S_FUNCTION:  int ug_dsquare(wid,xy)  -- tablet square.
**      Workstation simulation routine which returns which tablet square
**			xy is in. Tablet assumed to be a 22x22 grid, with square 1 at
**			upper left, numbered consecutively from 1 to 484 by rows.
**  PARAMETERS   
**      INPUT:  Gws wid -- workstation id.
**					 int xy[2] -- tablet location in raster coordinates.
**      OUTPUT: none
**  RETURNS      :  square number between 1 and 484.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int ug_dsquare(wid,xy)	/* return which tablet square xy is in */
Gws wid;							/* workstation id */
int xy[2];
{
	int k;						/* square number */
	int row,col;				/* 22 rows, 22 columns on tablet */
	int devxmax,devymax;		/* device maximum coordinates */
	UG_wdt *wdtpt;
	wdtpt=ug_gksstli.wsopen[wid].wdtptr;
	devxmax=(*wdtpt).dspsize.raster.x-1;
	devymax=(*wdtpt).dspsize.raster.y-1;
	row=(devymax-xy[1])*22/devymax; 
	col=xy[0]*22/devxmax;
	k=row*22+col+1;			/* k=1..484 */
	uu_denter2(UU_GITRC,(us,"%d=ug_dsquare(%d,%d %d) row,col=%d %d",
				k,wid,xy[0],xy[1],row,col));
	uu_dexit;
	return(k);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_ckpuck(wid,devtype,k,xy)
**			Check for tablet square puck key.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_ckpuck(wid,devtype,k,xy)	/* check for tablet square puck key */
Gws wid;
int devtype;						/* 3=tablet puck hit */
int *k;								/* on call: key number. 
											on return: tablet square number */
int xy[2];
{
	int irtn;
	irtn = devtype;
	return(irtn);
}

/********************************************************************* 
**  S_FUNCTION:  int ug_dmenuno(wid,xy,item) 
**
**      Return which menu xy is in, or zero. If not zero, set item to
**			menu choice number. Item will be non-zero only for text menus
**			created by ug_dmenu, or icon menus. If returned menu no is
**			non-zero, then item will be >0 also.
**  PARAMETERS   
**      INPUT:  Gws wid -- workstation id.
**					 int xy[2] -- location in raster coordinates.
**      OUTPUT: int *item -- menu choice number.
**
**  RETURNS      :  Choice device number of the menu xy is in, or 0. 
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int ug_dmenuno(wid,xy,item)				/* return which menu xy is in */
int xy[2];
Gws wid;
int *item;							/* menu choice number, or zero */
{
	int devno,n,i;
	int row,rowy,coly;
	int pet;
/*	char us[130];*/
	Gchoicest *choicept;
	Gfloat s;
	int rasll[2],rasur[2];
	Gdpoint loc;				/* xy in device coords */
	UG_wdt *wdtpt;
	int rc[2],nrnc[2];
	int nitems;					/* number items in the menu */
	Gfloat areadx,aready;
	Gdpoint locfract;
	Gnrect *fp;					/* pointer to choice posn's within menu*/

	wdtpt=ug_gksstli.wsopen[wid].wdtptr;
	n=(*(*wdtpt).inwdtpt).nchoice;
	s=(*wdtpt).dspsize.raster.x;
	/* convert xy to dev coord */
	loc.x=xy[0]/s;
	loc.y=xy[1]/s;
	devno=ug_dmenun(wid,xy);				/* devno=which menu xy is in, or 0 */
	*item=0;
	if (devno>0) {						/* found a menu. Now calc item */
		choicept= &(*ug_gksstli.wsopen[wid].inptr).choicedata[devno-1];
		nitems=(*choicept).record.number;
		pet=(*choicept).pet;
		if ((pet==3)||(pet>=22)) {		/* is a text menu */
			/* convert menu area to raster */
			rasll[0]=(*choicept).e_area.ll.x*s;
			rasll[1]=(*choicept).e_area.ll.y*s;
			rasur[0]=(*choicept).e_area.ur.x*s;
			rasur[1]=(*choicept).e_area.ur.y*s;
			/* calc row=top row of menu */
			ug_dshrink ( wdtpt, rasll, rasur, rasll, rasur, rc, nrnc ) ;
			row=rc[0]+2;				/* row above 1st choice row */
			uu_denter2(UU_GITRC,(us,
				"ug_dmenuin. row above 1st ch=%d, raster.y=%d, earea.ur.y=%g",
				row,(*wdtpt).dspsize.raster.y,(*choicept).e_area.ur.y));
			uu_dexit;
			/* calculate rowy=row xy[1] is on */
			ug_drastorc(xy,wdtpt,&rowy,&coly,0);
			*item=rowy-row;			/* *item=choice number */
			if ((*item<1)||(*item>nitems)) *item=0;
		}
		if (pet==5) {					/* is a user defined icon menu */
			
			/* convert loc to a fraction of this menu's e_area */
			areadx=(*choicept).e_area.ur.x-(*choicept).e_area.ll.x;
			aready=(*choicept).e_area.ur.y-(*choicept).e_area.ll.y;
			locfract.x=(loc.x-(*choicept).e_area.ll.x)/areadx;
			locfract.y=(loc.y-(*choicept).e_area.ll.y)/aready;
			uu_denter2(UU_GITRC,(us,"ug_dmenuno. loc=%g %g, locfract=%g %g",
				loc.x,loc.y,locfract.x,locfract.y));
			uu_dexit;
			/* see if this posn is in any choice */
			fp=(*choicept).record.chposn;
			uu_denter2(UU_GITRC,(us,"ug_dmenuno. chposn=%x, nitems=%d",fp,nitems));
			uu_dexit;
			for (i=0; i<nitems; i++) {				/* for each choice */	
				uu_denter2(UU_GITRC,(us,"ug_dmenuno. fp[%d]=%g %g %g %g",i,fp[i].ll.x,fp[i].ll.y,fp[i].ur.x,fp[i].ur.y));
				uu_dexit;
				if ((locfract.x>=fp[i].ll.x)&&(locfract.x<=fp[i].ur.x) &&
					 (locfract.y>=fp[i].ll.y)&&(locfract.y<=fp[i].ur.y)) {
					 *item=i+1; break; 					/* found it */
				}
			}												/* end for each choice */
		}													/* end if pet==5 */
		if (*item<=0) devno=0;
	}
	uu_denter2(UU_GITRC,(us,
			"%d=ug_dmenuno(wid=%d, xy=%d %d). nchoices=%d, *item=%d",
			devno,wid,xy[0],xy[1],n,*item));
	uu_dexit;
	return(devno);
}

/********************************************************************* 
**  S_FUNCTION:  int ug_dmenun(wid,xy) 
**      Return which menu xy is in, or zero. 
**  PARAMETERS   
**      INPUT:  Gws wid -- workstation id.
**					 int xy[2] -- location in raster coordinates.
**      OUTPUT: none
**
**  RETURNS      :  Choice device number of the menu xy is in, or 0. 
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int ug_dmenun(wid,xy)				/* return which menu xy is in */
int xy[2];
Gws wid;
{
	int devno,n;
	int gotit;
	Gchoicest *choicept;
	Gfloat s;
	Gdpoint loc;				/* xy in device coords */
	UG_wdt *wdtpt;
	wdtpt=ug_gksstli.wsopen[wid].wdtptr;
	n=(*(*wdtpt).inwdtpt).nchoice;
	s=(*wdtpt).dspsize.raster.x;
	loc.x=xy[0]/s;
	loc.y=xy[1]/s;
	devno=ug_wsdev.curreqno;
	gotit=0;
	/* check the requested device first */
	if (ug_wsdev.curreqclas==UG_IC_CHOICE) {
		choicept= &(*ug_gksstli.wsopen[wid].inptr).choicedata[devno-1];
		if (ug_dmenuin(&loc,choicept)==1) gotit=1;
	}
	if (gotit==0) {						/* wasn't the requested device */
		for (devno=6; devno<=n; devno++) {	/* for each choice device */
			choicept= &(*ug_gksstli.wsopen[wid].inptr).choicedata[devno-1];
			if ((*choicept).mode==UG_EVENT) {
				/* uu_denter2(UU_GITRC,(us,"ug_dmenun devno=%d mode=%d",devno,
							(*choicept).mode));
				uu_dexit; */
				if (ug_dmenuin(&loc,choicept)==1) break;
			}
		}
		if (devno>n) devno=0;					/* didnt find a menu */
	}
	/* uu_denter2(UU_GITRC,(us,"%d=ug_dmenun(wid=%d, xy=%d %d).",
		devno,wid,xy[0],xy[1]));
	uu_dexit; */
	return(devno);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dmenuin(loc,choicept)
**			See if loc is within echo area.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dmenuin(loc,choicept)		/* see if loc is within echo area */
Gdpoint *loc;
Gchoicest *choicept;						/* pointer to choice state */
{
	int pet,irtn;
	Gdpoint ll,ur;				/* corners of echo area */
	irtn=0;
	pet=(*choicept).pet;
	if ((pet==3)||(pet==5)||(pet>=22)) {
		ll.x=(*choicept).e_area.ll.x;
		ll.y=(*choicept).e_area.ll.y;
		ur.x=(*choicept).e_area.ur.x;
		ur.y=(*choicept).e_area.ur.y;
		if (((*loc).x>=ll.x)&&((*loc).y>=ll.y)&&((*loc).x<=ur.x)&&
				((*loc).y<=ur.y)) 
			irtn=1;			/* found menu that xy is in */
	}
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  ug_dchht(prms) -- char height. Not impl yet.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dchht(prms)			/* char height */
{
}

/*********************************************************************
**    I_FUNCTION     :  ug_noop() -- graphics no op.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_noop()					/* graphics no op */
{
}

/********************************************************************* 
**  S_FUNCTION:  int ug_dchoice(ws,devno,xy,k,choice,menu)
**   
**  Workstation simulation routine for UG_DCHOICE secondary entry, useful
**  for bit-mapped workstations which support UG_DRASGET and UG_DRASPUT.
**	 UG_DCHOICE gets called for menus echotype 22,23,24 which use the tablet.
**  However, this simulation routine only works for echotype 24. Workstation
**  must handle types 22,23 itself.
**  Track menu cursor. Set xy=cursor posn. Set choice=number of menu item 
**	 under cursor(1..n), or zero if cursor not over any item.
**  PARAMETERS   
**      INPUT: Gws ws;			 workstation id 
**					int devno;		 choice device number
**				   int *k;			 ending key or choice value
**      OUTPUT:int xy[2];		 loc position in dev coords
**					int *choice;	 menu choice number, or zero 
**					int *menu;		 the menu number, or zero
**
**  RETURNS :  
**		0 =a keypad-2 key (dev sel) was hit, k contains the key number. 
**		1= An ASCII kbd key was hit, k contains ascii value.
**		2=a keypad-1 key was hit, k contains key number. 
**		3=a tablet puck button was used, k contains number. 
**		NOTE: 4,5 below not possible on some workstations.
**		4= the expected device type was used, k contains choice number
**		5= an inappropriate device was used, k contains device type 
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int ug_dchoice(ws,devno,xy,k,choice,menu)	/* UG_DCHOICE --  "choice" device */
Gws ws;				/* workstation id */
int devno;			/* choice device number */
int *k;				/* contains ending key or choice value */
int xy[2];			/* on return contains loc position in dev coords */
int *choice;		/* on return contains menu choice number, or zero */
int *menu;			/* on return contains the menu number, or zero */
{
#define MENUCURSOR 1
	Gchoicest *choicept;
	int done;
	int pet;
	char **p;
	choicept= &(*ug_gksstli.wsopen[ws].inptr).choicedata[devno-1];
	p=(*choicept).record.strings;
	pet=(*choicept).pet;
	uu_denter2(UU_GITRC,(us,"ug_dchoice(pet=%d,devno=%d)",pet,devno));
	*choice = 1;
	/* call workstation's UG_DTRK entry to wait for menu hit */
	done=(*(ug_gksstli.wsopen[ws].connid)[UG_DTRK])
				(ws,xy,k,MENUCURSOR,0,0,NULL,NULL);			/* use all locators */
	*menu=ug_dmenuno(ws,xy,choice);		/* find which menu and choice xy is in*/
	uu_denter2(UU_GITRC,(us,
			"ug_dchoice returns %d, xy=%d %d, k=%d, menu=%d choice=%d",
			done,xy[0],xy[1],*k,*menu,*choice));
	uu_dexit;
	uu_dexit;
	return (done);
}

/********************************************************************* 
**  S_FUNCTION:  ug_drastorc(xy,wdtpt,row,col,round) -- convert raster to row,col
**  		:  ug_drctoras(xy,wdtpt,row,col) -- convert row,col to raster
**
**      Convert raster coordinates to row,col, or vice versa.
**			If raster size is not an integral multiple of char height,
**			assume leftover raster lines are at top of screen.
**			If Round==0 round down,left. If round==1, round nearest. 
**			Else round up,right.
**			Rows assumed to start with row 1 at top. Columns start with
**			column 1 at left.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_drastorc(xy,wdtpt,row,col,round)	/* convert raster to row,col */
int xy[2];								/* raster coordinates */
UG_wdt *wdtpt;							/* pointer to workstation desc table */
int *col,*row;							/* resulting row,col */
int round;					/* 0=round down,left. else round up,right */
{
	int rowht,colwd,xmax,ymax,colmax,rowmax;
	uu_denter(UU_GITRC,(us,"ug_drastorc  xy = (%d,%d);  round = %d",xy[0],xy[1],round));
	ymax=wdtpt->dspsize.raster.y;
	xmax=wdtpt->dspsize.raster.x;
	rowmax=wdtpt->rowmax;
	colmax=wdtpt->colmax;
	rowht=ymax/rowmax;
	if( rowht < 1 ) rowht = 1;
	colwd=xmax/colmax;
	switch (round) {
		case 0:								/* round down,left */
			*col=(xy[0])/colwd + 1;
			*row=rowmax-xy[1]/rowht;
			break;
		case 1:								/* round nearest */
			*col=(xy[0]+colwd/2)/colwd+1;
			*row=rowmax-(xy[1]+rowht/2)/rowht;
			break;
		default: 							/* round up, right */
			*col=(xy[0]+colwd-1)/colwd+1;
			*row=rowmax-(xy[1]+rowht-1)/rowht;
			break;
	}														/* end switch(round) */
	uu_denter2(UU_GITRC,(us,"ug_drastorc returns  row,col = (%d,%d)",*row,*col));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_drctoras(xy,wdtpt,row,col)
**			Convert row,col  to raster.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_drctoras(xy,wdtpt,row,col)	/* convert row,col  to raster */
int xy[2];								/* resulting raster coordinates */
UG_wdt *wdtpt;							/* pointer to workstation desc table */
int row,col;							/* row,col */
{
	int rowht,colwd,xmax,ymax,colmax,rowmax;
/*	int leftovery,leftoverx;*/
	uu_denter(UU_GITRC,(us,"ug_drctoras  row,col = (%d,%d)",row,col));
	ymax=wdtpt->dspsize.raster.y;
	xmax=wdtpt->dspsize.raster.x;
	rowmax=wdtpt->rowmax;
	colmax=wdtpt->colmax;
	rowht=ymax/rowmax;
	colwd=xmax/colmax;
	/* leftovery=ymax-rowht*rowmax; */
	/* leftoverx=xmax-colwd*colmax; */

	xy[0]=colwd*(col-1);
	xy[1]=(rowmax-row)*rowht;
	uu_denter2(UU_GITRC,(us,"ug_drctoras  returns  xy = (%d,%d)",xy[0],xy[1]));
	uu_dexit;
}

/********************************************************************* 
**  I_FUNCTION:  Gint ug_dshrink(wdtpt,irasll,irasur,orasll,orasur,rc,nrnc)
**      This routine accepts as input the tentative raster coordinates
**			of an area to be used for alphanumeric data. The coordinates
**			are adjusted so as to fall on exact character boundaries.
**			The adjusted raster coordinates are returned along with
**			row and column indices and the number of rows and columns
**			in the rectangle. Adjustments are always made so that the
**			resulting rectangle is never bigger than the original; it
**			may be smaller.
**  PARAMETERS   
**      INPUT:  
**				wdtpt  -- pointer to workstation description table
**				irasll -- two element array containing x and y coordinate
**								of lower left corner of tentative alphanumeric
**								rectangle
**				irasur -- array containing x and y of upper right corner
**      OUTPUT:
**				orasll -- array containing adjusted coordinates of lower
**								left corner
**				orasur -- array containing adjusted coordinates of upper
**								right corner
**				rc     -- array containing row and column number of character
**								in the top left corner
**				nrnc   -- array containing the number of rows and the number
**								of columns in the rectangle
**
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gint ug_dshrink ( wdtpt, irasll, irasur, orasll, orasur, rc, nrnc )
	UG_wdt *wdtpt ;
	int irasll[], irasur[], orasll[], orasur[], rc[], nrnc[] ;
{
	int truncate,round_up;
	int row = 0 , col = 1 ;
	int cll[2], cur[2], cul[2], clr[2] ; /* character coordinates of lower
														 left, upper right, upper left,
														 and lower right, respectively. */
   uu_denter(UU_GITRC,(us,"ug_dshrink irasll = (%d,%d);  irasur = (%d,%d)",
		irasll[0],irasll[1],irasur[0],irasur[1]));

/*
.....Round numbers up on Iris
*/
	truncate = 0;
	round_up = 1;
/*
#if UU_COMP == UU_IRIS4D
	if (UD_host == 1)
	{
		round_up = 2;
	}
#endif
*/
	/* get row & column locations of new rectangle */
	ug_drastorc ( irasll, wdtpt, &cll[row], &cll[col], round_up ) ;
	ug_drastorc ( irasur, wdtpt, &cur[row], &cur[col], truncate ) ;

	/* get new raster coordinates of rectangle corners */
	ug_drctoras ( orasll, wdtpt, cll[row], cll[col] ) ;
	ug_drctoras ( orasur, wdtpt, cur[row], cur[col] ) ;

	/* determine cul and clr coordinates from cll and cur */
	cul[row] = cur[row] ;
	cul[col] = cll[col] ;
	clr[row] = cll[row] ;
	clr[col] = cur[col] ;

	/* character coordinates are pivoted on the lower left corner
		of the font grid. to adjust row&col numbers to fit within
		the maintained rectangle, the following operations must
		be made. */
	cul[row]++ ;
	clr[col]-- ;

	/* the arguments we need to return are cul and nrow/ncol */
	rc[row] = cul[row] ;
	rc[col] = cul[col] ;
	nrnc[row] = clr[row] - cul[row] + 1 ;
	nrnc[col] = clr[col] - cul[col] + 1 ;

   uu_denter2(UU_GITRC,(us,"ug_dshrink returns:  orasll = (%d,%d);  orasur = (%d,%d);  rc = (%d,%d);  nrnc = (%d,%d)",
			orasll[0],orasll[1],orasur[0],orasur[1],rc[0],rc[1],nrnc[0],nrnc[1]));
	uu_dexit;
	uu_dexit;

	return ( NCL_NO_ERROR ) ;
}


/********************************************************************* 
**  I_FUNCTION:  Gint ug_locvp(xy)
** Find which viewport locator is in.  If a locator is inside multiple
** viewports, the highest priority viewport is selected.  If multiple
** viewports of the same priority are found, the lowest indexed viewport
** is used.
**
**  PARAMETERS   
**      INPUT:  xy		Ndc coordinates of locator measure.
**      OUTPUT: none
**
**  RETURNS      :  Viewport of locator input.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gint ug_locvp(xy)
Gnpoint *xy;
{
	int priority;
	int index;
	int i;
	Gnrect3 *vp;

	uu_denter(UU_GITRC,(us,"ug_dlocvp(%f %f)", xy->x, xy->y));

	/* Find which viewport locator is in.  If a locator is inside multiple
	 * viewports, the highest priority viewport is selected.  If multiple
	 * viewports of the same priority are found, the lowest indexed viewport
	 * is used.
	 */
	priority = index = -1;				/* init to illegal values */
	for (i=0; i<ug_gksstli.novtran; i++) {
		vp= &ug_gksstli.vtran[i].vport;
		uu_dprint(UU_GITRC,(us,"vp %d, %f %f %f %f",
			i, vp->llf.x, vp->llf.y, vp->urb.x, vp->urb.y));
		if( (xy->x >= vp->llf.x) && (xy->y >= vp->llf.y) &&
			 (xy->x <= vp->urb.x) && (xy->y <= vp->urb.y)) {	/* inside vport */

			/* If priority is higher than current priority, use this vport */
			uu_dprint(UU_GITRC,(us,"inside vp, pri=%d, current pri=%d",
				ug_gksstli.vtran[i].inputprio, priority));
			if( ug_gksstli.vtran[i].inputprio > priority ) {
				index = i;
				priority = ug_gksstli.vtran[i].inputprio;
			}
		}
	}

	uu_dprint(UU_GITRC,(us,"ug_locvp returns %d", index));
	uu_dexit;
	return(index);
}
