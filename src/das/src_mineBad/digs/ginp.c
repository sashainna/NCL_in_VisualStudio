/*********************************************************************
**    NAME         :  ginp.c -- DIGS input functions.
**	All calls to workstations have 2 arguments. First is
**	an array of input args. First element is command. Second is
** workstation ID. Third element on is parameter structure
** which is different for each command. For gdwaitdevop command,
** 3rd element is device class, 4th elt is device number.
** Second arg is the reply array. 
**	First element of this array is the reply op-code.
**	Second element on is the reply structure which is
** different for each reply op-code.
** For gdtstdevop command, 2nd element is device class, 3rd element
** is device number.
**       CONTAINS:
**		Gerror ginitloc(wd,dev,init,pet,area,record)
**		Gerror gchglocinit(ws,dev,init)
**		Gerror ginitstroke(wd,dev,init,pet,area,record)
**		Gerror ginitval(ws,dev,init,pet,area,record) 
**		Gerror ginitchoice(wkid,devnum,inchval,pet,area,record)
**		Gerror gchgchoicearea(ws,dev,ll)
**		Gerror ginitpick(ws,dev,init,pet,area,record)
**		Gerror ginitstring(ws,dev,init,pet,area,record) 
**		Gerror gchgstringinit(ws,dev,init)
**		Gerror gslocmode(ws,dev,mode,echo) 
**		       gglocmode(ws,dev,mode,echo) 
**		Gerror gsstrokemode(ws,dev,mode,echo) 
**		Gerror gsvalmode(ws,dev,mode,echo) 
**		Gerror gschoicemode(ws,dev,mode,echo) 
**		Gerror gspickmode(ws,dev,mode,echo) -- Set pick mode.
**		Gerror gsstringmode(ws,dev,mode,echo) -- Set string mode. 
**		Gqloc *greqloc(ws,dev) -- Request locator. 
**		Gqstroke *greqstroke(ws,dev) -- Request stroke. 
**		Gqval *greqval(ws,dev) -- Request valuator. 
**		Gqchoice *greqchoice(ws,dev) -- Request choice. 
**		Gqpicks *greqpick(ws,dev) -- Request pick. 
**		Gqstring *greqstring(ws,dev) -- Request string.
**		Gloc *gsampleloc(ws,dev) -- Sample locator.
**		Gstroke *gsamplestroke(ws,dev) -- Sample stroke.
**		Gfloat gsampleval(ws,dev) -- Sample valuator.
**		Gint gsamplechoice(ws,dev) -- Sample choice.
**		Gpicks *gsamplepick(ws,dev) -- Sample pick. PHIGS extension.
**		Gchar *gsamplestring(ws,dev) -- Sample string.
**		Gevent *gawaitevent(timeout) -- Await event.
**		Gint gqsize() -- get input queue size.
**		Gerror gflushevents(ws,dev,class)
**		Gloc *ggetloc() -- Get locator.
**		Gstroke *ggetstroke() -- Get stroke.
**		Gfloat ggetval() -- Get valuator.
**		Gint ggetchoice() -- Get choice.
**		Gpicks *ggetpick() -- Get picks. PHIGS enhancement.
**		Gchar *ggetstring() -- Get string.
**		gputstring -- write to scrolling text window string device.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ginp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:20
*********************************************************************/
#ifdef trace
char gops[49][12]=			/* also used in gksviw.c */
      {"ug_noop","ug_pageop","gmova3op","gmova2op","gmovr3op","gmovr2op",
       "ug_lina3op","glina2op","glinr3op","glinr2op","gmrka3op",
       "gmrka2op","gmrkr3op","gmrkr2op","ug_polylna3op","ug_polylna2op",
       "ug_polylnr3op","ug_polylnr2op","ug_polymka3op","ug_polymka2op",
       "ug_polymkr3op","ug_polymkr2op","gtextop","gprocop","gcallop",
       " "," "," "," "," "," "," ",
       "ug_dfatsop","gcolorop","gintensop","glstylop","glwidop",
       "gpenop","gfontop","gchsizop","gchplaneop","gchup2op",
       "gchup3op","gchpathop","gchspaceop","gchjustop","gchprecop",
       "gsymbolop","gpickidop"};
#endif
static char sstat[2][8]={"UG_OK","UG_NONE"};
static char sclass[7][14]={"UG_E_NONE","UG_E_LOCATOR","UG_E_STROKE",
		"UG_E_VALUATOR","UG_E_CHOICE","UG_E_PICK","UG_E_STRING"};
#include "zsysdep.h"
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "gvlib.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gsegac.h"
#include "udebug.h"
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) ginp.c 3.3 4/13/88 10:11:28 single"};
#else
static char uu_sccsident[]={"@(#) ginp.c 3.3 4/13/88 10:11:28 double"};
#endif
#define EPS (UU_REAL) .0001

static	UG_queue sav_curevent;
extern struct {
	Giclass curdevclas;		/* currently being used device class */
	Gint curdevno;			/* currently being used device number */
	Giclass curreqclas;		/* currently requested device class, or -1 */
	Gint curreqno;			/* currently requested device number */
} ug_wsdev;

static struct {
	Giclass curdevclas;		/* currently being used device class */
	Gint curdevno;			/* currently being used device number */
	Giclass curreqclas;		/* currently requested device class, or -1 */
	Gint curreqno;			/* currently requested device number */
} ug_wsdev_save;

/********************************************************************* 
**  E_FUNCTION:  Gerror ginitloc(wd,dev,init,pet,area,record)
**      Initialize locator device.
**  PARAMETERS   
**      INPUT:  Gws *wd -- workstation id, returned by gopenws.
**					 Gidevno dev -- locator device number to be initialized.
**					 Gloc *init -- pointer to initial location 
**					 Gpet pet -- prompt and echo type:
**									0		Picking cursor 
**									1		Choice cursor
**									2		Locator cursor
**									4		Rubberband line
**									5		Rubberband rectangle
**									6		Digital
**									21		User defined cursor (dragging)
**									40		Pan
**									41		Rotate X-Y
**									42		Rotate Z
**									43		Zoom
**					 Gdrect *area -- rectangular echo area.
**					 Glocrec *record -- locator data record.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ginitloc(wd,dev,init,pet,area,record)
/*$ INPUT */
Gws *wd;					/* workstation id */
Gidevno dev;			/* device number */
Gloc *init;				/* initial location */
Gpet pet;				/* prompt and echo type */
Gdrect *area;			/* echo area */
Glocrec *record;		/* locator data record */
{
	UG_initdev prms;		/* parameters to workstation init a device */
	int reply[10];		/* reply from workstation */
	int npets,i;
	Gerror irtn;
	Glocst *locpt;
	UG_wdt *wdtpt;
	UG_inwdt *inwdtpt;
	Gdefloc *deflocpt;

	uu_denter(UU_GTRC,(us,"ginitloc(%d,%d,%g %g,%d,%g %g %g %g,record)",
						*wd,dev,(*init).position.x,(*init).position.y,pet,
						(*area).ll.x,(*area).ll.y,(*area).ur.x,(*area).ur.y));
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*wd].inptr).nloc)) {
		ug_errorhand(ENOINDEV,"ginitloc",&dev);
		irtn=ENOINDEV;
	}
	/* make sure pet is supported by this device */
	/* get ptr to input wdt */
	wdtpt=ug_gksstli.wsopen[*wd].wdtptr;
	inwdtpt=(*wdtpt).inwdtpt;		
	deflocpt= &(((*inwdtpt).defloc)[dev-1]);		/* Gdefloc ptr for this dev */
	npets=(*deflocpt).n_pets;
	uu_denter2(UU_GITRC,(us,"ginitloc npets=%d",npets));
	uu_dexit;
	for (i=0; i<npets; i++) {
		uu_denter2(UU_GITRC,(us,"ginitloc. pets[%d]=%d",i,(*deflocpt).pets[i]));
		uu_dexit;
		if ((*deflocpt).pets[i]==pet) break;
	}
	if (i>=npets) {									/* illegal pet */
		ug_errorhand(ENOPETWS,"ginitloc",&pet);
		irtn=ENOPETWS;
	}
#endif
	if (irtn==NCL_NO_ERROR) {						/* no input parm errors */
		/* save initialization data workstation state list */
		locpt= &(*ug_gksstli.wsopen[*wd].inptr).locdata[dev-1];
		zbytecp((*locpt).loc, *init);			/* structure assignment*/
		(*locpt).pet=pet;
		zbytecp((*locpt).e_area, *area);		/* structure assignment */
		zbytecp((*locpt).record, *record);	/* structure assignment */
		(*locpt).mode=UG_REQUEST;
		/* copy initialization data into prms */
		prms.op=UG_DINITLOC;
		prms.id= *wd;
		prms.iclass=UG_IC_LOCATOR;
		prms.devno=dev;
		ug_wkcal(*wd,&prms,reply);		/* call the workstation */
		if (reply[0]==(int)(UG_OK)) irtn=NCL_NO_ERROR;
	}													/* end of no input parm errors */
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  Gerror gchglocinit(ws,dev,init)
**       change initial locator position.
**    PARAMETERS   
**       INPUT  :  Gws *ws;  workstation.
**						 int dev;  locator device no.
**						 Gloc *init;  initial position.
**       OUTPUT :  
**    RETURNS      : NCL_NO_ERROR  if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gchglocinit(ws,dev,init)
/*$ INPUT */
Gws *ws;
int dev;									/* locator device no. */
Gloc *init;							/* new initial position */
{
	Glocst *locpt;
	uu_denter(UU_GTRC,(us,"gchglocinit(%d,%d, %d, %g %g)",
		*ws,dev,(*init).transform,(*init).position.x,(*init).position.y));
	/* save initial loc */
	locpt= &(*ug_gksstli.wsopen[*ws].inptr).locdata[dev-1];
	zbytecp((*locpt).loc, *init);			/* structure assignment*/
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror ginitstroke(wd,dev,init,pet,area,record)
**      Initialize locator device.
**  PARAMETERS   
**      INPUT:  Gws *wd -- workstation id, returned by gopenws.
**					 Gidevno dev -- stroke device number to be initialized.
**					 Gloc *init -- pointer to initial location 
**					 Gpet pet -- prompt and echo type.(currently 1-7).
**					 Gdrect *area -- rectangular echo area.
**					 Gstrokerec *record -- stroke data record.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ginitstroke(wd,dev,init,pet,area,record)
/*$ INPUT */
Gws *wd;					/* workstation id */
Gidevno dev;			/* device number */
Gstroke *init;			/* initial stroke values */
Gpet pet;				/* prompt and echo type */
Gdrect *area;			/* echo area */
Gstrokerec *record;	/* stroke data record */
{
	UG_initdev prms;	/* parameters to workstation init a device */
	int reply[10];		/* reply from workstation */
	Gerror irtn;
	Gstrokest *strokept;

	uu_denter(UU_GTRC,(us,"ginitstroke(%d,%d,%d %d, %g %g %g %g,record)",
						*wd,dev,(*init).transform,(*init).n_points,
						(*area).ll.x,(*area).ll.y,(*area).ur.x,(*area).ur.y));

	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*wd].inptr).nstroke)) {
		ug_errorhand(ENOINDEV,"ginitstroke",&dev);
		uu_dexit; return(ENOINDEV);
	}

	/* save initialization data workstation state list */
	strokept = &(*ug_gksstli.wsopen[*wd].inptr).strokedata[dev-1];
	uu_denter2(UU_GITRC,(us,"pts: %x",strokept->stroke.points)); uu_dexit;

	(*strokept).pet  = pet;
	(*strokept).mode = UG_REQUEST;
	zbytecp((*strokept).e_area, *area);		/* structure assignment */
	zbytecp((*strokept).record, *record);	/* structure assignment */
	uu_denter2(UU_GITRC,(us,"ginitstroke(dev=%d, interval=%g %g",
		dev,(*strokept).record.interval.x,(*strokept).record.interval.y));
	uu_dexit;

	/* copy initialization data into prms */
	prms.op     = UG_DINITSTROKE;
	prms.id     = *wd;
	prms.iclass = UG_IC_STROKE;
	prms.devno  = dev;

	/* call the workstation */
	ug_wkcal(*wd,&prms,reply);	

	if ( reply[0]==(int)(UG_OK) ) irtn=NCL_NO_ERROR;

	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror ginitval(ws,dev,init,pet,area,record) 
**      Initialize valuator. 
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation.
**					Gidevno dev; -- valuator device number.
**					Gfloat init; -- initial value.
**					Gpet pet; -- prompt and echo type.
**					Gdrect *area; -- echo area.
**					Gvalrec *record; -- valuator data record.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ginitval(ws,dev,init,pet,area,record)
/*$ INPUT */
Gws *ws;
Gidevno dev;
Gfloat init;
Gpet pet;
Gdrect *area;
Gvalrec *record;
{
	UG_initdev prms;
	int reply[4];
	Gvalst *valpt;			/* pointer to value state */
	uu_denter(UU_GTRC,(us,"ginitval(%d,%d,%g,%d,%g %g %g %g,%s)",
				*ws,dev,init,pet,(*area).ll.x,(*area).ll.y,(*area).ur.x,
				(*area).ur.y,(*record).prompt));
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nval)) {
		ug_errorhand(ENOINDEV,"ginitval",&dev);
		uu_dexit; return(ENOINDEV);
	}
	/* save initialization data */
	valpt= &(*ug_gksstli.wsopen[*ws].inptr).valdata[dev-1];
	(*valpt).val=init;
	(*valpt).pet=pet;
	zbytecp((*valpt).e_area,(*area));
	zbytecp((*valpt).record,(*record));
	(*valpt).mode=UG_REQUEST;
	prms.op=UG_DINITVAL;
	prms.id= *ws;
	prms.iclass=UG_IC_VALUATOR;
	prms.devno=dev;
	ug_wkcal(*ws,&prms,reply);
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror ginitchoice(wkid,devnum,inchval,pet,area,record)
**      Initialize choice device.
**  PARAMETERS   :  Gws *wkid;					/* workstation id
**							Gidevno devnum;			/* device number
**							Gint inchval;				/* initial value
**							Gpet pet;					/* echo type
**							Gdrect *area;				/* choice echo area (DC)
**							Gchoicerec *record;		/* choice data record
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ginitchoice(wkid,devnum,inchval,pet,area,record)
/*$ INPUT */
Gws *wkid;						/* workstation id */
Gidevno devnum;				/* device number */
Gint inchval;					/* initial value */
Gpet pet;						/* echo type */
Gdrect *area;					/* choice echo area (DC) */
Gchoicerec *record;			/* pointer to choice data record */
{
	Gchoicest *choicept;/* pointer to choice state */
	UG_segstli *segptr;
	int segno,newsegno;
	int i,dev,nitems;
	Gnrect *fp;					/* pointer to array of choice positions */
	Gerror irtn;				/* returned function value */
	UG_initdev parms;				/* parameters to workstation */
	Gstatus reply;				/* reply from workstation */
	uu_denter(UU_GTRC,(us,"ginitchoice(%d,%d,%d,%d,%g %g %g %g,%d,%s)",
							*wkid,devnum,inchval,pet,
							(*area).ll.x,(*area).ll.y,
							(*area).ur.x,(*area).ur.y,(*record).number,
							*(*record).strings));
	irtn=NCL_NO_ERROR;
	/* first get pointer to choice state array */
	choicept= &(*ug_gksstli.wsopen[*wkid].inptr).choicedata[devnum-1];	
#ifdef UU_CHECK
	if ((devnum<1)||(devnum>(*ug_gksstli.wsopen[*wkid].inptr).nchoice)) {
		ug_errorhand(ENOINDEV,"ginitchoice",&devnum);
		irtn=ENOINDEV;
	}
	else {											/* devnum OK */
		if (pet==5) {
			nitems=(*record).number;
			fp=(*record).chposn;					/* positions of icons */
			uu_denter2(-1,(us,"ginitchoice. chposn=%x, nitems=%d",fp,nitems));
			uu_dexit;
			if ((nitems<1)||(nitems>485)) {
				uu_denter2(-1,(us,"ginitchoice. error bad nitems=%d, chposn=%x",
					nitems,fp));
		  		uu_dexit;
				irtn=ENOINDEV;					/* change this to correct error*/
				ug_errorhand(irtn,"ginitchoice",&nitems);
			}
			for (i=0; i<nitems; i++) {
				if ((fp[i].ll.x<0.0)||(fp[i].ll.y<0.0)||
					((fp[i].ur.x-1.0)>EPS)||((fp[i].ur.y-1.0)>EPS)) {	/* bad posn */
					uu_dprint(-1,(us,
						"ginitchoice. error bad posn[%d]=%g %g %g %g",
						i,fp[i].ll.x,fp[i].ll.y,fp[i].ur.x,fp[i].ur.y));
					irtn=ENOINDEV;					/* change this to correct error*/
					ug_errorhand(irtn,"ginitchoice",fp);
					break;
				}
			}											/* end for each item */
		}												/* end if pet==5 */
	}													/* end devnum OK */
	if (irtn==NCL_NO_ERROR) {
#endif
	dev=devnum-1;
	(*choicept).choice=inchval;			/* save initial choice */
	(*choicept).pet=pet;		/* save prompt and echo type */
	zbytecp((*choicept).e_area, *area);	/* save echo area */
	/* menulen[dev]=0; */
	zbytecp((*choicept).record, *record);/* save choice record */
	(*choicept).mode=UG_REQUEST;
	parms.op=UG_DINITCHOICE;
	parms.id= *wkid;
	parms.iclass=UG_IC_CHOICE;
	parms.devno=dev+1;
	ug_wkcal(*wkid,&parms,&reply);	/* call workstation to init choice dev*/
	if (reply!=UG_OK) {
		ug_errorhand(ENOINDEV,"ginitchoice",&devnum);
		irtn=ENOINDEV;			/*** change this to correct return code */
	}
	else {								/* no errors from driver */
		irtn=NCL_NO_ERROR;
		/* see if we need to make a raster copy of the menu segment */
		if ((*choicept).pet==5) {				/* menu is of correct type */
			segno=(*choicept).record.seg;		/* segment id of users's menu seg */
			/* create a raser segment of the menu */
			/*newsegno=2*UG_MAXSEGNO+devnum;	/* segment id of the raster seg */
			/* if (ug_segac(newsegno)==NULL) {	
				/* workstation didn't create a raster copy.  Ws should call
					ug_drasdrw if it wants a raster copy. 
					Driver's DMENU routine might need this
					Probably better not to do this here, but
					let driver do it if it needs to. */
				/* ug_drasseg(*wkid,segno,newsegno,&(*choicept).e_area); 
				/* segptr=ug_segac(newsegno);
				/* (*segptr).segatts.gdtect=UG_UNDETECTABLE;
				/* (*segptr).segatts.gvis=UG_INVISIBLE; */
			/*}							/* end workstation didn't create raster copy*/
		}									/* end pet==5 */
	zbytecp(*record,(*choicept).record);/* save choice record */
	}										/* end no errors from driver */
#ifdef UU_CHECK
	}										/* end if irtn==NCL_NO_ERROR */
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  Gerror gchgchoicearea(ws,dev,ll)
**       change lower left corner of choice area
**    PARAMETERS   
**       INPUT  : 	Gws *ws -- workstation.
**							int dev -- device number.
**							Gdpoint *ll -- new lower left corner of echo area.
**       OUTPUT :  
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gchgchoicearea(ws,dev,ll)		/* change ll corner of choice e_area*/
/*$ INPUT */
Gws *ws;
int dev;										/* chocie device number */
Gdpoint *ll;								/* new lower left corner */
{
	Gerror irtn;
	Gdrect newarea;
	Gdrect *earea;
	Gdspsize  *siz;
	UG_initdev parms;				/* parameters to workstation */
	Gstatus reply;					/* reply from workstation */
	
	uu_denter(UU_GTRC,(us,"gchgchoicearea(%d,%d,%g %g)",*ws,dev,
				(*ll).x,(*ll).y));
	irtn=NCL_NO_ERROR;
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nchoice)) {
		ug_errorhand(ENOINDEV,"ginitchoice",&dev);
		uu_dexit;
		irtn=ENOINDEV;
	}
	else {								/* NCL_NO_ERROR */
		earea= &(*ug_gksstli.wsopen[*ws].inptr).choicedata[dev-1].e_area;
		newarea.ll.x=(*ll).x; newarea.ll.y=(*ll).y;
		newarea.ur.x=newarea.ll.x+(*earea).ur.x-(*earea).ll.x;
		newarea.ur.y=newarea.ll.y+(*earea).ur.y-(*earea).ll.y;
#ifdef UU_CHECK
		/* insure the requested echo area is within screen limits. 
		clip if needed*/
		siz=gqdisplaysize(ws);
		if ((newarea.ll.x<0.) || (newarea.ll.y<0.) || 
			(newarea.ur.x>(*siz).device.x) || 
			(newarea.ur.y>(*siz).device.y)) {
			ug_errorhand(EBADRCTD,"gchgchoicearea",&newarea);
			irtn=EBADRCTD;
			if (newarea.ll.x<0.) newarea.ll.x=0.;
			if (newarea.ll.y<0.) newarea.ll.y=0.;
			if (newarea.ur.x>(*siz).device.x) newarea.ur.x=(*siz).device.x;
			if (newarea.ur.y>(*siz).device.y) newarea.ur.y=(*siz).device.y;
		}
#endif
	}										/* end of NCL_NO_ERROR */
	if (irtn==NCL_NO_ERROR) {
		zbytecp((*earea),newarea);	/* save new echo area */
		parms.op=UG_DCHGCHOICEAREA;
		parms.id= *ws;
		parms.iclass=UG_IC_CHOICE;
		parms.devno=dev;
		ug_wkcal(*ws,&parms,&reply);	/* call workstation to chg choice area*/
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror ginitpick(ws,dev,init,pet,area,record)
**      Initialize pick device.
**  PARAMETERS   : Gws *ws;				/* workstation ID 
**						Gidevno dev;		/* pick device number
**						Gpicks *init;		/* initial pick value 
**						Gpet pet;			/* prompt and echo type
**						Gdrect *area;		/* echo area
**						Gpickrec *record;	/* pick data record 
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ginitpick(ws,dev,init,pet,area,record)
/*$ INPUT */
Gws *ws;				/* workstation ID */
Gidevno dev;		/* pick device number */
Gpicks *init;		/* initial pick value */
Gpet pet;			/* prompt and echo type */
Gdrect *area;		/* echo area */
Gpickrec *record;	/* pick data record */
{
	UG_initpick prms;
	Gpickst *pickpt;			/* pointer to pick state */
	int reply[4];
	char us[200];
	uu_denter2(UU_GTRC,(us,"ginitpick(%d,%d,%d %d,%d,%g %g %g %g,%s)",
					*ws,dev,(*init).status,(*init).depth,
					pet,(*area).ll.x,(*area).ll.y,(*area).ur.x,(*area).ur.y,
					(*record).prompt));
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).npick)) {
		ug_errorhand(ENOINDEV,"ginitpick",&dev);
		uu_dexit; return(ENOINDEV);
	}
	/* save initialize pick data in ws state list */
	pickpt= &(*ug_gksstli.wsopen[*ws].inptr).pickdata[dev-1];
	zbytecp((*pickpt).pick, *init);	/* structure assignment */
	(*pickpt).pet=pet;
	zbytecp((*pickpt).e_area, *area);	/* structure assignment */
	zbytecp((*pickpt).record, *record);	/* structure assignment */
	(*pickpt).mode=UG_REQUEST;
	prms.op=UG_DINITPICK;
	prms.id= *ws;
	prms.iclass=UG_IC_PICK;
	prms.devno=dev;
	ug_wkcal(*ws,&prms,reply);
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror ginitstring(ws,dev,init,pet,area,record) 
**      Initialize string device. 
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation.
**					Gidevno dev; -- string device number
**					Gchar *init;-- initial string
**					Gpet pet; -- prompt and echo type.
**					Gdrect *area; -- echo area.
**					Gstringrec *record; -- string data record .
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ginitstring(ws,dev,init,pet,area,record)
/*$ INPUT */
Gws *ws;
Gidevno dev;
Gchar *init;			/* initial string */
Gpet pet;
Gdrect *area;			/* echo area */
Gstringrec *record;	/* string data record */
{
	Gdspsize  *siz;
	UG_initdev prms;
	int reply[4];
	Gstringst *stringpt;		/* pointer to string state */
	int maxsiz;					/* max buffer size for this string dev, from wdt*/

	uu_denter(UU_GTRC,(us,"ginitstring(%d,%d,%s,%d,%g %g %g %g,%s)",
			*ws,dev,init,pet,(*area).ll.x,(*area).ll.y,(*area).ur.x,
			(*area).ur.y,(*record).prompt));
#ifdef UU_CHECK
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nstring)) {
		ug_errorhand(ENOINDEV,"ginitstring",&dev);
		uu_dexit; return(ENOINDEV);
	}
#endif
	/* save initial string data in string state */
	stringpt= &(*ug_gksstli.wsopen[*ws].inptr).stringdata[dev-1];
	strcpy((*stringpt).initstring,init);	
	(*stringpt).pet=pet;
	zbytecp((*stringpt).e_area, *area);	/* structure assignment */
#ifdef UU_CHECK
	/* insure the requested echo area is within screen limits. clip if needed*/
	siz=gqdisplaysize(ws);
	if (((*area).ll.x<0.) || ((*area).ll.y<0.) || 
		((*area).ur.x>(*siz).device.x) || ((*area).ur.y>(*siz).device.y)) {
		ug_errorhand(EBADRCTD,"ginitstring",area);
		if ((*area).ll.x<0.) (*stringpt).e_area.ll.x=0.;
		if ((*area).ll.y<0.) (*stringpt).e_area.ll.y=0.;
		if ((*area).ur.x>(*siz).device.x) (*stringpt).e_area.ur.x=(*siz).device.x;
		if ((*area).ur.y>(*siz).device.y) (*stringpt).e_area.ur.y=(*siz).device.y;
	}
#endif
	zbytecp((*stringpt).record, *record);	/* structure assignment */
	/* get maximum buffer size for this string device from workstation desc tbl*/
	maxsiz=(*(*ug_gksstli.wsopen[*ws].wdtptr).inwdtpt).defstr[dev-1].bufsiz;
	if ((*stringpt).record.bufsiz>maxsiz) (*stringpt).record.bufsiz=maxsiz;
	if ((*stringpt).record.bufsiz<1) (*stringpt).record.bufsiz=1;
	(*stringpt).mode=UG_REQUEST;
	prms.op=UG_DINITSTRING;
	prms.id= *ws;
	prms.iclass=UG_IC_STRING;
	prms.devno=dev;
	ug_wkcal(*ws,&prms,reply);
	uu_dexit;
	/* if pet==22 the driver may have reduced lines,cols, so update record*/
	(*record).lins=(*stringpt).record.lins;
	(*record).cols=(*stringpt).record.cols;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gchgstringinit(ws,dev,init,buflen)
**      change initial string and buffer length.
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation id.
**					Gidevno dev; -- string device number.
**					Gchar *init; -- new initial string.
**					int buflen; -- new string input buffer size.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gchgstringinit(ws,dev,init,buflen)
/*$ INPUT */
Gws *ws;
Gidevno dev;
Gchar *init;			/* initial string */
int buflen;
{
	Gstringst *stringpt;		/* pointer to string state */
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gchgstringinit(%d,%d,%s,%d)",*ws,dev,init,buflen));
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nstring)) {
		ug_errorhand(ENOINDEV,"gchgstringinit",&dev);
		irtn=ENOINDEV;
	}
	if (irtn==NCL_NO_ERROR) {
#endif
	/* save initial string data in string state */
	stringpt= &(*ug_gksstli.wsopen[*ws].inptr).stringdata[dev-1];
	strcpy((*stringpt).initstring,init);	
	(*stringpt).record.bufsiz=buflen;
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gslocmode(ws,dev,mode,echo) 
**      Set locator mode. 
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation id.
**					Gidevno dev; -- locator device number.
**					Gimode mode; -- new mode (UG_REQUEST, UG_EVENT, or UG_SAMPLE).
**					Gesw echo; -- echo switch (UG_ECHO or UG_NOECHO. 
**										UG_NOECHO Not implemented yet).
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gslocmode(ws,dev,mode,echo)
/*$ INPUT */
Gws *ws;
Gidevno dev;
Gimode mode;
Gesw echo;
{
	Gerror irtn;
	Glocst *locpt;				/* pointer to loc state */
	struct { int op; Gws wsid; Gidevno dev; Gimode mode; Gesw echo;} prms;
	int reply[4];
	uu_denter(UU_GTRC,(us,"gslocmode(%d,%d,%d,%d)",*ws,dev,mode,echo));
	irtn=NCL_NO_ERROR;
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nloc)) {
		ug_errorhand(ENOINDEV,"gslocmode",&dev); 
		irtn=ENOINDEV;
		uu_denter2(UU_GITRC,(us,"nloc=%d",(*ug_gksstli.wsopen[*ws].inptr).nloc));
		uu_dexit;
	}
	if (irtn==NCL_NO_ERROR) {
		locpt= &(*ug_gksstli.wsopen[*ws].inptr).locdata[dev-1];
		(*locpt).mode=mode;
		(*locpt).esw=echo;
		/* inform the workstation of mode change */
		prms.op=UG_DLOCMODE; 
		prms.wsid= *ws;
		prms.dev=dev;
		prms.mode=mode;
		prms.echo=echo;
		ug_wkcal(*ws,&prms,reply);
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gglocmode(ws,dev,mode,echo) 
**      Get locator mode. 
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation id.
**					Gidevno dev; -- locator device number.
**					Gesw echo; -- echo switch (UG_ECHO or UG_NOECHO. 
**										UG_NOECHO Not implemented yet).
**      OUTPUT: Gimode mode; -- current mode (UG_REQUEST, UG_EVENT, or UG_SAMPLE).
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gglocmode(ws,dev,mode,echo)
/*$ INPUT */
Gws *ws;
Gidevno dev;
Gimode *mode;
Gesw *echo;
{
	Glocst *locpt;				/* pointer to loc state */
/*
.....Return location state
*/
	locpt= &(*ug_gksstli.wsopen[*ws].inptr).locdata[dev-1];
	*mode = (*locpt).mode;
	*echo = (*locpt).esw;
	return;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsstrokemode(ws,dev,mode,echo) 
**      Set stroke mode. 
**  PARAMETERS   
**      INPUT: Gws *ws; -- workstation id.
**					Gidevno dev; -- stroke device number.
**					Gimode mode; -- new mode (UG_EVENT, UG_SAMPLE, or UG_REQUEST).
**					Gesw echo; -- (UG_ECHO or UG_NOECHO. 
**										UG_NOECHO not implemented yet.)
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsstrokemode(ws,dev,mode,echo)
/*$ INPUT */

Gws *ws;
Gidevno dev;
Gimode mode;
Gesw echo;
{
	Gerror irtn;
	Gstrokest *pt;				/* pointer to stroke state */
	struct { int op; Gws wsid; Gidevno dev; Gimode mode; Gesw echo;} prms;
	int reply[4];

	uu_denter(UU_GTRC,(us,"gsstrokemode(%d,%d,%d,%d)",*ws,dev,mode,echo));

	irtn = NCL_NO_ERROR;

	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nstroke)) {
		ug_errorhand(ENOINDEV,"gsstrokemode",&dev); 
		irtn=ENOINDEV;
	}
	if (irtn==NCL_NO_ERROR) {
		pt = &(*ug_gksstli.wsopen[*ws].inptr).strokedata[dev-1];
		(*pt).mode = mode;
		(*pt).esw  = echo;

		/* inform the workstation of mode change */
		prms.op   = UG_DSTROKEMODE; 
		prms.wsid = *ws;
		prms.dev  = dev;
		prms.mode = mode;
		prms.echo = echo;
		ug_wkcal(*ws,&prms,reply);
	}

	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsvalmode(ws,dev,mode,echo) 
**      Set valuator mode. 
**  PARAMETERS   
**      INPUT: Gws *ws; -- workstation id.
**					Gidevno dev; -- device number.
**					Gimode mode; -- new mode (UG_EVENT, UG_SAMPLE, or UG_REQUEST).
**					Gesw echo; -- (UG_ECHO or UG_NOECHO. 
**										UG_NOECHO not implemented yet.)
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsvalmode(ws,dev,mode,echo)
/*$ INPUT */

Gws *ws;
Gidevno dev;
Gimode mode;
Gesw echo;
{
	Gerror irtn;
	Gvalst *pt;				/* pointer to val state */
	struct { int op; Gws wsid; Gidevno dev; Gimode mode; Gesw echo;} prms;
	int reply[4];
	uu_denter(UU_GTRC,(us,"gsvalmode(%d,%d,%d,%d)",*ws,dev,mode,echo));
	irtn=NCL_NO_ERROR;
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nval)) {
		ug_errorhand(ENOINDEV,"gsvalmode",&dev); 
		irtn=ENOINDEV;
	}
	if (irtn==NCL_NO_ERROR) {
		pt= &(*ug_gksstli.wsopen[*ws].inptr).valdata[dev-1];
		(*pt).mode=mode;
		(*pt).esw=echo;
		/* inform the workstation of mode change */
		prms.op=UG_DVALMODE; 
		prms.wsid= *ws;
		prms.dev=dev;
		prms.mode=mode;
		prms.echo=echo;
		ug_wkcal(*ws,&prms,reply);
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gschoicemode(ws,dev,mode,echo) 
**      Set choice mode. 
**  PARAMETERS   
**      INPUT: Gws *ws; -- workstation id.
**					Gidevno dev; -- device number.
**					Gimode mode; -- new mode (UG_EVENT, UG_SAMPLE, or UG_REQUEST).
**					Gesw echo; -- (UG_ECHO or UG_NOECHO. 
**										UG_NOECHO not implemented yet.)
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gschoicemode(ws,dev,mode,echo)
/*$ INPUT */
Gws *ws;
Gidevno dev;
Gimode mode;
Gesw echo;
{
	Gerror irtn;
	struct { int op; Gws wsid; Gidevno dev; Gimode mode; Gesw echo;} prms;
	int reply[3];
	Gchoicest *pt;				/* pointer to choice state */
	Gnrect *fp;					/* for debugging */
	int i,nitems;				/* for debugging */

	uu_denter(UU_GTRC,(us,"gschoicemode(%d,%d,%d,%d)",*ws,dev,mode,echo));
	irtn=NCL_NO_ERROR;
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nchoice)) {
		ug_errorhand(ENOINDEV,"gschoicemode",&dev); 
		irtn=ENOINDEV;
	}
	if (irtn==NCL_NO_ERROR) {
		pt= &(*ug_gksstli.wsopen[*ws].inptr).choicedata[dev-1];
		(*pt).mode=mode;
		(*pt).esw=echo;
		if ((*pt).pet==5) {
			fp=(*pt).record.chposn;
			nitems=(*pt).record.number;
			uu_denter2(UU_GTRC,(us,"gschoicemode. chposn=%x, nchoice=%d",
				fp,nitems));
			uu_dexit;
			if ((nitems<1)||(nitems>485)) {
				uu_denter2(-1,(us,"gschoicemode. error bad nitems=%d, chposn=%x",
					nitems,fp));
			  	uu_dexit;
				irtn=ENOINDEV;					/* change this to correct error*/
				ug_errorhand(irtn,"gschoicemode",&nitems);
			}
			for (i=0; i<nitems; i++) {
				if ((fp[i].ll.x<0.0)||(fp[i].ll.y<0.0)||
					(fp[i].ur.x>1.0)||(fp[i].ur.y>1.0)) {	/* bad posn */
					uu_denter2(-1,(us,
						"gschoicemode. error bad posn[%d]=%g %g %g %g",
						fp[i].ll.x,fp[i].ll.y,fp[i].ur.x,fp[i].ur.y));
					uu_dexit;
					irtn=ENOINDEV;					/* change this to correct error*/
					ug_errorhand(irtn,"gschoicemode",fp);
					break;
				}
			}											/* end for each item */
		}										/* end of if pet==5 */
		/* inform the workstation of mode change */
		prms.op=UG_DCHOICEMODE; 
		prms.wsid= *ws;
		prms.dev=dev;
		prms.mode=mode;
		prms.echo=echo;
		ug_wkcal(*ws,&prms,reply);
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gspickmode(ws,dev,mode,echo) -- Set pick mode.
**  PARAMETERS   
**      INPUT: Gws *ws; -- workstation id.
**					Gidevno dev; -- device number.
**					Gimode mode; -- new mode (UG_EVENT, UG_SAMPLE, or UG_REQUEST).
**					Gesw echo; -- (UG_ECHO or UG_NOECHO. 
**										UG_NOECHO not implemented yet.)
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gspickmode(ws,dev,mode,echo)
/*$ INPUT */
Gws *ws;
Gidevno dev;
Gimode mode;
Gesw echo;
{
	Gerror irtn;
	Gpickst *pt;				/* pointer to pick state */
	struct { int op; Gws wsid; Gidevno dev; Gimode mode; Gesw echo;} prms;
	int reply[4];
	uu_denter(UU_GTRC,(us,"gspickmode(%d,%d,%d,%d)",*ws,dev,mode,echo));
	irtn=NCL_NO_ERROR;
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).npick)) {
		ug_errorhand(ENOINDEV,"gspickmode",&dev); 
		irtn=ENOINDEV;
	}
	if (irtn==NCL_NO_ERROR) {
		pt= &(*ug_gksstli.wsopen[*ws].inptr).pickdata[dev-1];
		(*pt).mode=mode;
		(*pt).esw=echo;
		/* inform the workstation of mode change */
		prms.op=UG_DPICKMODE; 
		prms.wsid= *ws;
		prms.dev=dev;
		prms.mode=mode;
		prms.echo=echo;
		ug_wkcal(*ws,&prms,reply);
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsstringmode(ws,dev,mode,echo) -- Set string mode. 
**  PARAMETERS   
**      INPUT: Gws *ws; -- workstation id.
**					Gidevno dev; -- device number.
**					Gimode mode; -- new mode (UG_EVENT, UG_SAMPLE, or UG_REQUEST).
**					Gesw echo; -- (UG_ECHO or UG_NOECHO. 
**										UG_NOECHO not implemented yet.)
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsstringmode(ws,dev,mode,echo)
/*$ INPUT */
Gws *ws;
Gidevno dev;
Gimode mode;
Gesw echo;
{
	Gerror irtn;
	Gstringst *pt;				/* pointer to string state */
	struct { int op; Gws wsid; Gidevno dev; Gimode mode; Gesw echo;} prms;
	int reply[4];
	uu_denter(UU_GTRC,(us,"gsstringmode(%d,%d,%d,%d)",*ws,dev,mode,echo));
	irtn=NCL_NO_ERROR;
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nstring)) {
		ug_errorhand(ENOINDEV,"gsstringmode",&dev); 
		irtn=ENOINDEV;
	}
	if (irtn==NCL_NO_ERROR) {
		pt= &(*ug_gksstli.wsopen[*ws].inptr).stringdata[dev-1];
		(*pt).mode=mode;
		(*pt).esw=echo;
		/* inform the workstation of mode change */
		prms.op=UG_DSTRINGMODE; 
		prms.wsid= *ws;
		prms.dev=dev;
		prms.mode=mode;
		prms.echo=echo;
		ug_wkcal(*ws,&prms,reply);
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gqloc *greqloc(ws,dev) -- Request locator. 
**  PARAMETERS   
**      INPUT: Gws *ws -- pointer to workstation.
**					Gidevno dev -- locator device number.
**  RETURNS      :  Pointer to a structure of type Gqloc, defined as:
**							typedef struct {
**									Gstatus status;	** OK or UG_NONE **
**									Gloc loc;
**							} Gqloc;
**							typedef struct {
**									Gint transform;
**									Gwpoint position;
**							} Gloc;
**  SIDE EFFECTS :  none
**  WARNINGS     : Currently loc.position is returned in NDC space and
**						 loc.transform is not set. This will change.
*********************************************************************/
Gqloc *greqloc(id,devnum)         /* request locator */
/*$ INPUT */
Gws *id;						/* workstation id */
Gidevno devnum;			/* device number */
{
	static Gqloc locat;	/* locator struct to hold status and loc */
  UG_reqdev parms;
  UG_reqloc reply;
  Glocst *locpt;
  Gqloc *irtn;
  int n;
	uu_denter(UU_GTRC,(us,"greqloc(%d,%d,status) ug_etime=%d",*id,devnum,
		ug_etime));
	irtn=NULL;
	n=(*ug_gksstli.wsopen[*id].inptr).nloc;	/* number of loc devices */
	if ((devnum<1)||(devnum>n)) ug_errorhand(ENOINDEV,"greqloc",&devnum);
	else {
		locpt= &(*ug_gksstli.wsopen[*id].inptr).locdata[devnum-1];
		if ((*locpt).mode!=UG_REQUEST) ug_errorhand(EREQUEST,"greqloc",&devnum);
		else {				/* no errors */
			ug_curevent.time=ug_etime;		/* update current event time */
			irtn= &locat;
			parms.op=UG_DREQLOC;
			parms.id=(*id); parms.iclass=UG_IC_LOCATOR;
  			parms.devno=devnum;
  			ug_wkcal(*id,&parms,&reply);
  			locat.status=reply.stat;
  			if (reply.stat!=UG_NONE) {
    			locat.loc.position.x=reply.loc.x;
    			locat.loc.position.y=reply.loc.y;
  				locat.loc.transform=reply.tran;
			}
		}
	}
	uu_denter2(UU_GTRC,(us,"greqloc(%d,%d,%d,%d,%g,%g)\n",
				*id,devnum,locat.status,locat.loc.transform,
				locat.loc.position.x,locat.loc.position.y));
	uu_dexit;
	uu_dexit;	
	return(irtn);
}


/********************************************************************* 
**  E_FUNCTION:  Gqstroke *greqstroke(ws,dev) -- Request stroke. 
**  PARAMETERS   
**      INPUT: Gws *ws -- pointer to workstation.
**					Gidevno dev -- stroke device number.
**  RETURNS      :  Pointer to a structure of type Gqstroke, defined as:
**							typedef struct {
**  SIDE EFFECTS :  none
**  WARNINGS     : Currently loc.position is returned in NDC space and
**						 loc.transform is not set. This will change.
*********************************************************************/
Gqstroke *greqstroke(id,devnum)        /* request stroke */
/*$ INPUT */
Gws *id;											/* workstation id */
Gidevno devnum;								/* device number */
{
	static Gqstroke strok;	/* struct to hold status and stroke buffer */
	UG_reqdev parms;
	UG_reqstroke reply;
	Gstrokest *strokept;
	Gqstroke *irtn;
	int n;
	
	uu_denter(UU_GTRC,(us,"greqstroke(%d,%d,status) ug_etime=%d",
		*id,devnum,ug_etime));

	irtn = NULL;
	n = (*ug_gksstli.wsopen[*id].inptr).nstroke;	/* number of stroke devices */

	if ((devnum<1)||(devnum>n)) 
		ug_errorhand(ENOINDEV,"greqstroke",&devnum);
	else {	
		strokept = &(*ug_gksstli.wsopen[*id].inptr).strokedata[devnum-1];

		if ((*strokept).mode!=UG_REQUEST) 
			ug_errorhand(EREQUEST,"greqstroke",&devnum);
		else {				/* no errors */
			ug_curevent.time=ug_etime;		/* update current event time */
			irtn = &strok;
			parms.op    = UG_DREQSTROKE;
			parms.id    = (*id); parms.iclass=UG_IC_STROKE;
  			parms.devno = devnum;

  			ug_wkcal(*id,&parms,&reply);

  			strok.status=reply.stat;
  			if (reply.stat!=UG_NONE) {
    			strok.stroke.transform = reply.tran;
				strok.stroke.n_points  = reply.n_points;
    			strok.stroke.points    = (Gwpoint *)reply.buf;
			}
		}								/* end of no errors */
	}
	if (irtn!=NULL) {
		uu_denter2(UU_GITRC,(us,"greqstroke status=%d,n_points=%d, xform=%d",
			strok.status,strok.stroke.n_points,strok.stroke.n_points));
		uu_dexit;	
	}
	uu_dexit;	
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gqval *greqval(ws,dev) -- Request valuator. 
**  PARAMETERS   
**      INPUT:  Gws *ws -- pointer to workstation.
**					 Gidevno dev -- valuator device number.
**  RETURNS      :  Pointer to structure of type Gqval, defined as:
**							typedef struct {
**									Gstatus status;    ** OK or UG_NONE **
**									Gfloat val;   	 	 ** the value ***
**							} Gqval;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gqval *greqval(id,devnum)
/*$ INPUT */
Gws *id;					/* workstation id */
Gidevno devnum;		/* input device number */
{
	Gfloat *valp;
	UG_reqdev parms;
	static Gqval reply;
	Gqval *irtn;
	Gvalst *valpt;
	Gint n;
	uu_denter(UU_GTRC,(us,"greqval(%d,%d) ug_etime=%d",*id,devnum,ug_etime));
	irtn=NULL;
	n=(*ug_gksstli.wsopen[*id].inptr).nval;	/* number of val devices */
	if ((devnum<1)||(devnum>n)) ug_errorhand(ENOINDEV,"greqval",&devnum);
	else {
		valpt= &(*ug_gksstli.wsopen[*id].inptr).valdata[devnum-1];
		if ((*valpt).mode!=UG_REQUEST) ug_errorhand(EREQUEST,"greqval",&devnum);
		else {				/* no errors */
			ug_curevent.time=ug_etime;		/* update current event time */
			irtn= &reply;
			parms.op=UG_DREQVAL; parms.id=(*id); parms.iclass=UG_IC_VALUATOR;
			parms.devno=devnum;
			ug_wkcal(*id,&parms,&reply);
		}
	}
	uu_denter2(UU_GTRC,(us,"greqval returns val=%g, status=%d\n",
						reply.val,reply.status));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gqchoice *greqchoice(ws,dev) -- Request choice. 
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation pointer.
**					 Gidevno dev -- choice device number.
**
**  RETURNS      :  Pointer to structure of type Gqchoice.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gqchoice *greqchoice(id,devnum)
/*$ INPUT */
Gws *id;					/* workstation id */
Gidevno devnum;		/* device number */
{
	int i,j;
	UG_reqdev parms;
	Gint n;
	Gqchoice *irtn;
	Gchoicest *choicept;
	static Gqchoice reply;	/* contains status and choice */
	uu_denter(UU_GTRC,(us,"greqchoice(%d,%d) ug_etime=%d", 
		*id,devnum,ug_etime));
	irtn=NULL;
	n=(*ug_gksstli.wsopen[*id].inptr).nchoice;	/* number of choice devices */
	if ((devnum<1)||(devnum>n)) ug_errorhand(ENOINDEV,"greqchoice",&devnum);
	else {
		choicept= &(*ug_gksstli.wsopen[*id].inptr).choicedata[devnum-1];
		if ((*choicept).mode!=UG_REQUEST) ug_errorhand(EREQUEST,"greqchoice",&devnum);
		else {				/* no errors */
			ug_curevent.time=ug_etime;		/* update current event time */
			irtn= &reply;
			parms.op=UG_DREQCHOICE; parms.id=(*id); parms.iclass=UG_IC_CHOICE;
			parms.devno=devnum;
			ug_wkcal(*id,&parms,&reply);
		}
	}
	uu_denter2(UU_GTRC,(us,"greqchoice returns status=%d, choice=%d",
						reply.status,reply.choice));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gqpicks *greqpick(ws,dev) -- Request pick. 
**			PHIGS enhancement. 
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gidevno dev -- pick device number.
**  RETURNS      :  Pointer to structure of type Gqpicks, defined as:
**						typedef struct {
**							Gstatus status;		/* request status 
**							Gint depth;				/* depth of pick path
**							Gpickid *pickpath;	/* pointer to array of segments 
**															and pickid at end.
**						} Gqpicks;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gqpicks *greqpick(id,devnum)	/* request picks - line pick, but
								returns array of pickids instead of 1 */
/*$ INPUT */
Gws *id;
Gidevno devnum;
{
UG_reqdev prms;
static Gqpicks reply;
Gqpicks *irtn;
Gpickid *seg;						/* pointer to array of integers */
Gpickst *pickpt;
Gint n;
char us[120];
	uu_denter2(UU_GTRC,(us,"greqpick(%d,%d) ug_etime=%d",*id,devnum,ug_etime));
	irtn=NULL;
	n=(*ug_gksstli.wsopen[*id].inptr).npick;	/* number of pick devices */
	if ((devnum<1)||(devnum>n)) ug_errorhand(ENOINDEV,"greqpick",&devnum);
	else {
		pickpt= &(*ug_gksstli.wsopen[*id].inptr).pickdata[devnum-1];
		if ((*pickpt).mode!=UG_REQUEST) ug_errorhand(EREQUEST,"greqpick",&devnum);
		else {				/* no errors */
			ug_curevent.time=ug_etime;		/* update current event time */
			irtn= &reply;
			prms.op=UG_DREQPICK;
			prms.id=(*id);
			prms.iclass=UG_IC_PICK;
			prms.devno=devnum;
			ug_wkcal(*id,&prms,&reply);
			seg=reply.pickpath;
		}
	}
	uu_denter2(UU_GTRC,(us,"greqpick returning status=%d,depth=%d",
			reply.status,reply.depth));
	uu_dexit;
	if (reply.depth >= 2)
	{
		uu_dprint(UU_GTRC,(us,"greqpick returning seg=%d,pkid=%d",
			seg[reply.depth-2],seg[reply.depth-1]));
	}
		
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gqstring *greqstring(ws,dev) -- Request string.
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation.
**					 Gidevno dev -- string device number.
**  RETURNS      :  Pointer to structure of tyep Gqstring, defined as:
**							typedef struct {
**									Gstatus status;	** OK or UG_NONE **
**									Gchar *string;		** pointer to the string **
**							} Gqstring;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gqstring *greqstring(id,devnum)		/* request string */
/*$ INPUT */
Gws *id;
Gidevno devnum;
{
	static Gchar buf[1024];		/* holds actual char string */
	static Gqstring qstr={UG_NONE,buf};	/* contains status, string */
	Gqstring *irtn;
	UG_reqdev parms;
	Gstringst *stringpt;
	int i,n;
	UG_rreqstr reply;
	uu_denter(UU_GTRC,(us,"greqstring(%d,%d) ug_etime=%d",*id,devnum,ug_etime));
	irtn=NULL;
	n=(*ug_gksstli.wsopen[*id].inptr).nstring;	/* number of string devices */
	if ((devnum<1)||(devnum>n)) ug_errorhand(ENOINDEV,"greqstring",&devnum);
	else {
		stringpt= &(*ug_gksstli.wsopen[*id].inptr).stringdata[devnum-1];
		/* check for UG_REQUEST mode if pet!=22 */
		if (((*stringpt).mode!=UG_REQUEST)&&((*stringpt).pet!=22)) 
			ug_errorhand(EREQUEST,"greqstring",&devnum);
		else {				/* no errors */
			ug_curevent.time=ug_etime;		/* update current event time */
			parms.id=(*id); parms.op=UG_DREQSTRING; parms.iclass=UG_IC_STRING;
  			parms.devno=devnum;
  			ug_wkcal(*id,&parms,&reply);
  			qstr.status=reply.stat;
			if (reply.stat!=UG_NONE) {
    			strcpy(buf,reply.str);
  			}
		}
	}
	uu_denter2(UU_GITRC,(us,"greqstring returns stat=%s",
		sstat[(int)qstr.status]));
	uu_dexit;
	uu_dexit;
	return(&qstr);
}

/********************************************************************* 
**  E_FUNCTION:  Gloc *gsampleloc(ws,dev) -- Sample locator.
**  PARAMETERS   
**      INPUT: Gws *ws; -- workstation id.
**					Gidevno dev; -- locator device number.
**  RETURNS      :  Pointer to structure of type Gloc.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gloc *gsampleloc(ws,dev)
/*$ INPUT */
Gws *ws;
Gidevno dev;
{
	Glocst *pt;
	Gloc *irtn;
	uu_denter(UU_GTRC,(us,"gsampleloc(%d,%d)",*ws,dev));
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nloc)) 
		irtn=NULL;
	else {
		pt= &(*ug_gksstli.wsopen[*ws].inptr).locdata[dev-1];
		irtn= &(*pt).loc;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gstroke *gsamplestroke(ws,dev) -- Sample stroke.
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation id.
**					Gidevno dev; -- stroke device number.
**  RETURNS      :  Pointer to structure of type Gstroke, defined as:
**						typedef struct	{
**							Gint		transform;--normalization transformation number
**							Gint		n_points; -- number of points in stroke
**							Gwpoint	*points; -- points in stroke
**						} Gstroke;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gstroke *gsamplestroke(ws,dev)
/*$ INPUT */
Gws *ws;
Gidevno dev;
{
	Gstrokest *pt;
	Gstroke *irtn;

	uu_denter(UU_GTRC,(us,"gsamplestroke(%d,%d)",*ws,dev));

	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nstroke)) 
		irtn = NULL;
	else {
		pt   = &(*ug_gksstli.wsopen[*ws].inptr).strokedata[dev-1];
		irtn = &(*pt).stroke;
	}

	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gfloat gsampleval(ws,dev) -- Sample valuator.
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation id.
**					Gidevno dev; -- valuator device number.
**  RETURNS      :  Valuator value.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gfloat  gsampleval(ws,dev)
/*$ INPUT */
Gws *ws;
Gidevno dev;
{
	Gvalst *pt;
	Gfloat irtn;
	uu_denter(UU_GTRC,(us,"gsampleval(%d,%d)",*ws,dev));
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nval)) 
		irtn=0.0;
	else {
		pt= &(*ug_gksstli.wsopen[*ws].inptr).valdata[dev-1];
		irtn=(*pt).val;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gint gsamplechoice(ws,dev) -- Sample choice.
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation id.
**					Gidevno dev; -- choice device number.
**  RETURNS      :  Choice number.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gint gsamplechoice(ws,dev)
/*$ INPUT */
Gws *ws;
Gidevno dev;
{
	Gchoicest *pt;
	Gint irtn;
	uu_denter(UU_GTRC,(us,"gsamplechoice(%d,%d)",*ws,dev));
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nchoice)) 
		irtn=0;
	else {
	pt= &(*ug_gksstli.wsopen[*ws].inptr).choicedata[dev-1];
		irtn=(*pt).choice;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gpicks *gsamplepick(ws,dev) -- Sample pick. 
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation id.
**					Gidevno dev; -- pick device number.
**  RETURNS      :  Pointer to structure of type Gpicks defined as:
**						typedef struct {
**							Gpstat status;			/* pick status (UG_OK or UG_NONE). 
**							Gint depth;				/* depth of pick path.
**							Gpickid *pickpath;	/* pointer to array of segments 
**															and pickid at end.
**						} Gpicks;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gpicks *gsamplepick(ws,dev)
/*$ INPUT */
Gws *ws;
Gidevno dev;
{
	Gpickst *pt;
	Gpicks *irtn;
	uu_denter(UU_GTRC,(us,"gsamplepick(%d,%d)",*ws,dev));
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).npick)) 
		irtn=NULL;
	else {
		pt= &(*ug_gksstli.wsopen[*ws].inptr).pickdata[dev-1];
		irtn= &(*pt).pick;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gchar *gsamplestring(ws,dev) -- Sample string.
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation id
**					Gidevno dev; -- string device number.
**  RETURNS      :  Pointer to string.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gchar *gsamplestring(ws,dev)
/*$ INPUT */
Gws *ws;
Gidevno dev;
{
	Gstringst *pt;
	Gchar *irtn;
	uu_denter(UU_GTRC,(us,"gsamplestring(%d,%d)",*ws,dev));
	if ((dev<1)||(dev>(*ug_gksstli.wsopen[*ws].inptr).nstring)) 
		irtn=NULL;
	else {
		pt= &(*ug_gksstli.wsopen[*ws].inptr).stringdata[dev-1];
		irtn=(*pt).string;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gevent *gawaitevent(timeout) -- Await event.
**  PARAMETERS   
**      INPUT:  Gfloat timeout -- how long to wait.
**  RETURNS      :  Pointer to structure
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gevent *gawaitevent(timeout)
/*$ INPUT */
Gfloat timeout;
{
	Gevent *irtn;
	int i;
	int prms[3];
	int prmsiz;
	uu_denter(UU_GTRC,(us,"gawaitevent(%g)",timeout));
	prms[0]=UG_DAWAITDEV;
	while (ug_inqlen<=0) {			/* queue is empty */
		if (timeout<=0.0) {
			uu_dexit; return(NULL); 
		}
		else {					/* wait for something */
			ug_wkout(prms,prmsiz=sizeof(prms)/sizeof(int));
		}
	}
	/* something is on queue */
	zbytecp(ug_curevent,ug_inqueue[0]);	/* structure assignment */
	for (i=0; i<ug_inqlen-1; i++)
		zbytecp(ug_inqueue[i],ug_inqueue[i+1]);		/* copy rest of queue up */
	ug_inqlen=ug_inqlen-1;
	irtn= &(ug_curevent.type);
#ifdef UU_DEBUG
	if (((int)(*irtn).class<=6)&&((int)(*irtn).class>=0)) {
		uu_denter2(UU_GITRC,(us,"gawaitevent()dev=%d, class=%s",
			(*irtn).dev,sclass[(int)(*irtn).class]));
	}
	else {
		uu_denter2(-1,(us,"gawaitevent() error. illegal class. dev=%d, class=%d",
			(*irtn).dev,(*irtn).class));
	}
#endif
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     :  ug_save_event
**						save current event
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :   none
**    RETURNS      : number of events on input queue.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_save_event()
{
	int i;
	zbytecp(sav_curevent, ug_curevent);	
	ug_wsdev_save.curdevclas = ug_wsdev.curdevclas;
	ug_wsdev_save.curdevno = ug_wsdev.curdevno;
	ug_wsdev_save.curreqclas = ug_wsdev.curreqclas;
	ug_wsdev_save.curreqno = ug_wsdev.curreqno;
}

/*********************************************************************
**    E_FUNCTION     :  ug_reset_event
**						reset current event and time stamp
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :   none
**    RETURNS      : number of events on input queue.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_reset_event()
{
	int i;
	zbytecp(ug_curevent, sav_curevent);
	ug_etime = ug_curevent.time;	
	ug_wsdev.curdevclas = ug_wsdev_save.curdevclas;
	ug_wsdev.curdevno = ug_wsdev_save.curdevno;
	ug_wsdev.curreqclas = ug_wsdev_save.curreqclas;
	ug_wsdev.curreqno = ug_wsdev_save.curreqno;
}
/*********************************************************************
**    E_FUNCTION     :  Gint gqsize() -- get input queue size.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :   none
**    RETURNS      : number of events on input queue.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gint gqsize()			/* return number of events on queue
							with same time as current event */
{
	int n,tim;
	char us[50];
	tim=ug_curevent.time;
	for (n=0; n<ug_inqlen; n++) {
		if (tim!=ug_inqueue[n].time) break;
	}
	uu_denter2(UU_GTRC,(us,"gqsize(time %d)=%d",tim,n));
	uu_dexit;
	return(n);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gflushevents(ws,dev,class)
**		Flush device events (of a given input device)
**  PARAMETERS   
**      INPUT:  Gws *ws; -- workstation id.
**					Gidevno dev; -- device number
**					Geclass class; -- device class.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gflushevents(ws,dev,class)
/*$ INPUT */
Gws *ws;
Gidevno dev;
Geclass class;
{
	int i,k;
	uu_denter(UU_GTRC,(us,"gflushevents(%d,%d,%d)",*ws,dev,class));
	i=0;
	while (i<ug_inqlen) {
		if ((ug_inqueue[i].type.class==class)&&(ug_inqueue[i].type.dev==dev)) {
			for (k=i; k<ug_inqlen-1; k++) {			/* found one to delete. */
				ug_inqueue[k]=ug_inqueue[k+1];
			}
			ug_inqlen=ug_inqlen-1;
		}
		else i++;			/* didnt find one to delete */
	}							/* while */
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gloc *ggetloc() -- Get locator.
**  PARAMETERS   
**      INPUT:  none 
**  RETURNS      :  Pointer to structure of type Gloc.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gloc *ggetloc()
{
	Gloc *irtn;
	uu_denter(UU_GTRC,(us,"ggetloc()"));
	if (ug_curevent.type.class==UG_E_LOCATOR) 
		irtn= &ug_curevent.event.locevent;
	else {
		ug_errorhand(ENOCURIV,"ggetloc",&ug_curevent.type.class);
		irtn=NULL;
	}
	if (irtn!=NULL) {	
		uu_denter2(UU_GTRC,(us,"ggetloc returns. xform=%d, x,y=%g %g",
					(*irtn).transform,(*irtn).position.x,
					(*irtn).position.y));
		uu_dexit;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gstroke *ggetstroke() -- Get stroke.
**  PARAMETERS   
**      INPUT:  none 
**  RETURNS      :  Pointer to structure of type Gstroke.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gstroke *ggetstroke()
{
	Gstroke *irtn;

	uu_denter(UU_GTRC,(us,"ggetstroke()"));

	if (ug_curevent.type.class==UG_E_STROKE)
		irtn = &ug_curevent.event.strokeevent;
	else{
		ug_errorhand(ENOCURIV,"ggetstroke",&ug_curevent.type.class);
		irtn = NULL;
	}

	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gfloat ggetval() -- Get valuator.
**  PARAMETERS   
**      INPUT:  none 
**  RETURNS      :  Valuator value.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gfloat ggetval()
{
	Gfloat irtn;
	uu_denter(UU_GTRC,(us,"ggetval()"));
	if (ug_curevent.type.class==UG_E_VALUATOR)
		irtn=ug_curevent.event.valevent;
	else {
		ug_errorhand(ENOCURIV,"ggetval",&ug_curevent.type.class);
		irtn=0.0;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gint ggetchoice() -- Get choice.
**  PARAMETERS   
**      INPUT:  none 
**  RETURNS      :  Choice number, or 0 if unsuccessful.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gint ggetchoice()
{
	Gint irtn;
	uu_denter(UU_GTRC,(us,"ggetchoice()"));
	if (ug_curevent.type.class==UG_E_CHOICE)
		irtn=ug_curevent.event.choiceevent;
	else {
		ug_errorhand(ENOCURIV,"ggetchoice",&ug_curevent.type.class);
		irtn=0;
	}
	uu_denter2(UU_GTRC,(us,"getchoice returns choice=%d",irtn));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gpicks *ggetpick() -- Get picks. PHIGS enhancement.
**  PARAMETERS   
**      INPUT:  none 
**  RETURNS      :  Pointer to structure.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gpicks *ggetpick()
{
	Gpicks *irtn;
	uu_denter(UU_GTRC,(us,"ggetpick()"));
	if (ug_curevent.type.class==UG_E_PICK)
		irtn= &ug_curevent.event.pickevent;
	else {
		ug_errorhand(ENOCURIV,"ggetpick",&ug_curevent.type.class);
		irtn=NULL;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gchar *ggetstring() -- Get string.
**  PARAMETERS   
**      INPUT:  none 
**  RETURNS      :  Pointer to string, or NULL if unsuccessful.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gchar *ggetstring()
{
	Gchar *irtn;
	uu_denter(UU_GTRC,(us,"ggetstring()"));
	if (ug_curevent.type.class==UG_E_STRING)
		irtn=ug_curevent.event.stringevent;
	else {
		ug_errorhand(ENOCURIV,"ggetstring",&ug_curevent.type.class);
		irtn=NULL;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  gputstring -- write to scrolling text window string device.
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation.
**					 Gidevno dev -- string device number
**					 char s[] -- string.
**      OUTPUT: none
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gputstring(ws,dev,s)
/*$ INPUT */
Gws *ws;					/* workstation */
Gidevno dev;			/* string device number */
char s[];				/* string */
{
	struct {int op; Gws id; Gidevno dev; char *str;} prms;
	int irtn,n;
	int reply[3];
	Gstringst *spt;
	uu_denter(UU_GTRC,(us,"gputstring(%d,%d,%s)",*ws,dev,s));
	irtn=NCL_NO_ERROR;
	n=(*ug_gksstli.wsopen[*ws].inptr).nstring;	/* number string devices */
	if ((dev<1)||(dev>n)) {
		irtn=ENOINDEV; ug_errorhand(ENOINDEV,"gputstring",&dev);
	}
	else {
		spt= &(*ug_gksstli.wsopen[*ws].inptr).stringdata[dev-1];
		if ((*spt).pet!=22) {
			irtn=ENOPETWS; ug_errorhand(ENOPETWS,"gputstring",&dev);
		}
	}
	if (irtn==NCL_NO_ERROR) {	/* call the workstation to write the string*/
		prms.op=UG_DPUTSTRING;
		prms.dev=dev;
		prms.str=s;
		prms.id= *ws;
		ug_wkcal(*ws,&prms,reply);
	}
	uu_dexit;
	return(irtn);
}
/*********************************************************************
**    E_FUNCTION     :  ug_reset_input
**						reset current input state
**			This function only reset the input state, it will not reset the actual data
**				because we use it for makeup of longjump for C++ to C. The data (such as 
**				stringdata.string,...) should still remain the same. (it will have problem
**				otherwise for some reason.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :   none
**    RETURNS      : number of events on input queue.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_reset_input(save_inptr_int)
int *save_inptr_int;
{
	int i, j;
	UG_inwssl *save_inptr_pt;
	UG_inwssl save_inptr;
	UG_inwssl *tmp_inptr;

	save_inptr_pt = (UG_inwssl *)save_inptr_int;
	save_inptr = *save_inptr_pt;
	tmp_inptr = (ug_gksstli.wsopen[0]).inptr;

	for (i=0; i<tmp_inptr->nloc; i++) 
	{	
		tmp_inptr->locdata[i].mode = save_inptr.locdata[i].mode; 
		tmp_inptr->locdata[i].esw = save_inptr.locdata[i].esw;
		tmp_inptr->locdata[i].pet = save_inptr.locdata[i].pet;
	}

	for (i=0; i<tmp_inptr->nstroke; i++) 
	{
		tmp_inptr->strokedata[i].mode = save_inptr.strokedata[i].mode;
		tmp_inptr->strokedata[i].esw = save_inptr.strokedata[i].esw;
		tmp_inptr->strokedata[i].pet = save_inptr.strokedata[i].pet;
	}

	for (i=0; i<tmp_inptr->nval; i++) 
	{
		tmp_inptr->valdata[i].mode = save_inptr.valdata[i].mode;
		tmp_inptr->valdata[i].esw = save_inptr.valdata[i].esw;
		tmp_inptr->valdata[i].pet = save_inptr.valdata[i].pet;
	}

	for (i=0; i<tmp_inptr->nchoice; i++) 
	{
		tmp_inptr->choicedata[i].mode = save_inptr.choicedata[i].mode;
		tmp_inptr->choicedata[i].esw = save_inptr.choicedata[i].esw;
		tmp_inptr->choicedata[i].pet = save_inptr.choicedata[i].pet;
	}

	for (i=0; i<tmp_inptr->npick; i++) 
	{
		tmp_inptr->pickdata[i].mode = save_inptr.pickdata[i].mode;
		tmp_inptr->pickdata[i].esw = save_inptr.pickdata[i].esw;
		tmp_inptr->pickdata[i].pet = save_inptr.pickdata[i].pet;
	}

	for (i=0; i<tmp_inptr->nstring; i++) 
	{
		tmp_inptr->stringdata[i].mode = save_inptr.stringdata[i].mode;
		tmp_inptr->stringdata[i].esw = save_inptr.stringdata[i].esw;
		tmp_inptr->stringdata[i].pet = save_inptr.stringdata[i].pet;
	}
	uu_toolfree(save_inptr.stringdata);
	uu_toolfree(save_inptr.pickdata);
	uu_toolfree(save_inptr.choicedata);
	uu_toolfree(save_inptr.valdata);
	uu_toolfree(save_inptr.strokedata);
	uu_toolfree(save_inptr.locdata);
	uu_toolfree(save_inptr_pt);
}

/*********************************************************************
**    E_FUNCTION     :  ug_save_input
**						save current input state 
**			This function only save the input state, it will not save the actual data
**				because we use it for makeup of longjump for C++ to C. The data (such as 
**				stringdata.string,...) should still remain the same if we reset the input
**				so there is no need to save
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :   none
**    RETURNS      : number of events on input queue.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_save_input(save_inptr_intpt)
int** save_inptr_intpt;
{
	int i, j;
	UG_inwssl *save_inptr_pt;
	UG_inwssl *tmp_inptr = (ug_gksstli.wsopen[0]).inptr;

	save_inptr_pt = (UG_inwssl *)uu_toolmalloc(sizeof (UG_inwssl));
	zbytecp(*save_inptr_pt, *tmp_inptr);

	save_inptr_pt->locdata = 
					(Glocst *)uu_toolmalloc(save_inptr_pt->nloc*sizeof(Glocst));
	for (i=0; i<save_inptr_pt->nloc; i++) 
	{	
		save_inptr_pt->locdata[i].mode = tmp_inptr->locdata[i].mode; 
		save_inptr_pt->locdata[i].esw = tmp_inptr->locdata[i].esw;
		save_inptr_pt->locdata[i].pet = tmp_inptr->locdata[i].pet;
	}

	save_inptr_pt->strokedata =
					(Gstrokest *)uu_toolmalloc(save_inptr_pt->nstroke*sizeof(Gstrokest));
	for (i=0; i<save_inptr_pt->nstroke; i++)
	{
		save_inptr_pt->strokedata[i].mode = tmp_inptr->strokedata[i].mode;
		save_inptr_pt->strokedata[i].esw = tmp_inptr->strokedata[i].esw;
		save_inptr_pt->strokedata[i].pet = tmp_inptr->strokedata[i].pet;
	}

	save_inptr_pt->valdata = 
					(Gvalst *)uu_toolmalloc(save_inptr_pt->nval*sizeof(Gvalst));
	for (i=0; i<save_inptr_pt->nval; i++) 
	{
		save_inptr_pt->valdata[i].mode = tmp_inptr->valdata[i].mode;
		save_inptr_pt->valdata[i].esw = tmp_inptr->valdata[i].esw;
		save_inptr_pt->valdata[i].pet = tmp_inptr->valdata[i].pet;
	}

	save_inptr_pt->choicedata = 
					(Gchoicest *)uu_toolmalloc(save_inptr_pt->nchoice*sizeof(Gchoicest));
	for (i=0; i<save_inptr_pt->nchoice; i++) 
	{
		save_inptr_pt->choicedata[i].mode = tmp_inptr->choicedata[i].mode;
		save_inptr_pt->choicedata[i].esw = tmp_inptr->choicedata[i].esw;
		save_inptr_pt->choicedata[i].pet = tmp_inptr->choicedata[i].pet;
	}

	save_inptr_pt->pickdata = 
					(Gpickst *)uu_toolmalloc(save_inptr_pt->npick*sizeof(Gpickst));
	for (i=0; i<save_inptr_pt->npick; i++) 
	{
		save_inptr_pt->pickdata[i].mode=tmp_inptr->pickdata[i].mode;
		save_inptr_pt->pickdata[i].esw=tmp_inptr->pickdata[i].esw;
		save_inptr_pt->pickdata[i].pet=tmp_inptr->pickdata[i].pet;
	}

	save_inptr_pt->stringdata = 
					(Gstringst *)uu_toolmalloc(save_inptr_pt->nstring*sizeof(Gstringst));
	for (i=0; i<save_inptr_pt->nstring; i++) 
	{
		save_inptr_pt->stringdata[i].mode=tmp_inptr->stringdata[i].mode;
		save_inptr_pt->stringdata[i].esw=tmp_inptr->stringdata[i].esw;
		save_inptr_pt->stringdata[i].pet=tmp_inptr->stringdata[i].pet;
	}
	*save_inptr_intpt = (int*)(save_inptr_pt);
}

