#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"
#include <stdio.h>
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gsegac.h"
#include "gerrorst.h"
#include "driver.h"
/*
.....Include so we will known when a pick occurred outside of the 
.....graphics area. Roberta Zorzynski
*/
#include "uims.h"

void ug_dawaitdev();
typedef struct { int op; Gws wsid; Gidevno dev; Gimode mode; Gesw echo;} Sprms;

/*********************************************************************
**    NAME         :  wsdev.c  
**			Logical Input device workstation simulation library.
**    CONTAINS:
**		int ug_dinitchoice(prms,reply) -- init choice device.
**		int ug_dinitchoice2(prms,reply)	
**		int ug_dinitpick(prms,reply)		
**		int ug_dinitloc(prms,reply) -- init locator device.
**		int ug_dinitloc2(prms,reply) -- init locator device (raster devices).
**		int ug_dinitstroke(prms,reply) -- init stroke device.
**		int ug_dinitval(prms,reply) -- init valuator device.
**		ug_dinitstr(prms,reply) -- init string device.
**		int ug_dstrmode(prms,reply) -- set string mode.
**		int ug_dchoicemode(prms,reply)
**		int ug_dreqval(prms,reply) -- request valuator.
**		int ug_dreqpick(prms,reply) -- request pick device.
**		int ug_dreqloc(prms,reply) -- request locator.
**		int ug_dreqstroke(prms,reply) -- request stroke.
**		int ug_dreqchoice(prms,reply) -- request choice.
**		int ug_dreqstr(prms,reply) -- request string.
**		int ug_dawaitev(prms) -- await any device.
**		int ug_dawaitdev(wid) -- await event from any  device.
**		int ug_dawaitpick(wid,k) -- await pick input.
**		int ug_dawaitchoice(wid,k) -- await choice.
**		int ug_dawaitloc(wid,k) -- await loc.
**		int ug_dawaitstroke(wid,k) -- await stroke.
**		int ug_dawaitstr(wid,k) -- await string.
**		int ug_dawaitval(wid,k) -- await valuator.
**		int ug_dpik(wid,seg,depth,xy,k,cursorno)
**
**	These routines handle the workstation table enties of GDINITxxx,
**	GDREQxxx, and GDAWAITxxx (xxx is PICK, STRING, LOC, VAL, or CHOICE).
** When using these simulation routines, GDxxxMODE and GDSAMPLExxx
** may be set to ug_noop (graphics no-op).
**	The logical device simulation used herein assumes that each
** workstation contains the following physical devices:
**		Ascii keyboard (returns character).
**		Keypad (optional) with b buttons (returns number 1 to b ).
**		Second keypad with at least 5 buttons, used by simulation lib
**			for device select keys.
**		Tablet (optional) with p button puck (returns x,y and ending key).
**		Cursor controlled by thumbwheels, etc (returns x,y and ending key).
**		Pick device (optional) (returns x,y, segid, pickid and ending key).
**		Choice device(s) (optional), e.g. buttons (returns positive integer).
**		Valuator device(s) (optional), e.g. knobs (return 16-bit integer values).
**		Stream device (optional) (returns list of x,y pairs and ending key).
**
**	The simulation routines herein call the following device dependent 
**	physical input device routines through the workstation access table:
**		UG_DKBD -- get keyboard string in buf.
**		UG_D1CHAR -- get 1 char from keyboard.
**		UG_DKEYPAD -- get keypad key from keypad n (n=1 or 2).
**		UG_DTRK -- get locactor locno.
**									locno=0: all locators available,
**											1=t-wheels.
**											2=tablet.
**		UG_DPIK -- pick something.
**		UG_DCHOICE -- get choice from a choice 
**														(menu-like) device
**		UG_DBUTTON -- get settings from a button (switch-like) device
**		UG_DVAL -- get a 32-bit integer value.
**		UG_DSTREAM -- get stream. The stream device will
**		usually be the tablet.
**
**		Each workstation must supply the above physical device routines.
**	Calling one of these physical device routines enables that
**	device for use, waits for the operator to do something, disables 
**	the device (if necessary), and reports what the operator did.
**	On most hardware(including 4115) it is possible to end use of the
**	requested device with a keystroke from the keyboard, either keypad,
**	or the tablet puck (if tablet enabled).
**	We are assuming that the hardware will not allow the operator to
**	use another device than the one requested (such as request kbd and
**	use the tablet). For this reason, this simulation package will
**	interpret the first 5 keys of the 2nd keypad as device select keys and
** call another physical device input routine.
**
**	GDSETxxxMODE() algorithm:
**		if event mode put up prompt for device xxx,
**		else take down prompt if currently up;
**
** GDREQxxx() algorithm:
**		set current_request_device and current_device to xxx;
**		ug_dawaitdev(wsid);	// wait for user to use some device.
**								If he used 1st 5 keys of keypad-2 (device select keys),
**								wait for him to use the new device. 
**								Update all logical device measures he uses.
**								Put on input Q all EVENT mode devices he uses.
**								Set current_device to which device he finally used.
**		if (current_device == xxx) return(UG_OK);		// user used device requested.
**		Else return(UG_NONE);		// user didn't use device requested.
**
** ug_dawaitdev(wsid) algorithm:
**		while (user uses device_select keys) {
**			done=GDAWAITcurrent_device(wsid,k);	// wait for user to use some 
**												device, update measures and put on Q.
**			if (user used dev_sel keys) set current_device to the one selected;
**		}
**
** GDAWAITxxx(wsid,k) algorithm:
**		put up prompt for device xxx if not in event mode, calling workstation
**			functions to do so;
**		call a physical input device function to wait for user to enter something.
**		take down prompt if not in event mode and regenerate display if needed;
**		Update measures of logical device(s) used;
**		Put on input Q EVENT mode logical device(s) used;
**		If he didn't use the current device and didn't use a device select key, 
**		set current device to the device he did use.
**		If a device select key used, set k=its number(1 to 6).
**		return(0=a dev sel key used, else 1).
**	
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       gddev.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:18
*********************************************************************/
struct {
	Giclass curdevclas;		/* currently being used device class */
	Gint curdevno;			/* currently being used device number */
	Giclass curreqclas;		/* currently requested device class, or -1 */
	Gint curreqno;			/* currently requested device number */
} ug_wsdev;
static int scrollup=0;			/* devno if scrolling area is up, else 0 */

/*static char sclas[7][15]={"UG_IC_LOCATOR","UG_IC_STROKE","UG_IC_VALUATOR",
		"UG_IC_CHOICE","UG_IC_PICK","UG_IC_STRING","UG_IC_NONE"};*/	/* for debug, must be 
											in order of Giclass enumerated type */
/* static char messages[10][81];	// for scrolling msg area */
static char menuprmt[3][17]={"choice(F keys): ","choice number","select option"};
static int curxy[2]={0,0};	/* latest graphics pick or loc posn */
extern UG_findit ug_find;

/* Array of pointers to hold the user-defined cursors */
#define MAXUSRCURSORS 10
static int *ug_usrcursor[MAXUSRCURSORS]={0,0,0,0,0,0,0,0,0,0};
/* Array of attach points for above cursors */
static Gipoint ug_usrattach[MAXUSRCURSORS];

Gerror ug_drasseg();

int UD_picking_active = 0;
			/************ DEVICE INITIALIZE ROUTINES ************/

/*********************************************************************
**    S_FUNCTION   :  int ug_dinitchoice(prms,reply) -- init choice device.
**		Initialize choice device. No action required. Just return UG_OK
**		Note: this simulation routine doesn't handle graphics (icon)
**		menus in which pet==5. Use ug_dinitchoice2 to handle graphical menus.
**			Valid Choice PETS :
**				3. Text Menu. "choice number", numbers displayed, kbd input only.
**				5. Icon Menu. "select option", no numbers, mouse input only.
**				21.Text Menu. "choice(F-keys)",numbers displyed, fkey input only.
**						(probably never used.)
**				22.Text Menu. "select option", numbers displayed, mouse or kbd.
**						(probably used most often.)
**				23.Text Menu. same as 22 but with help messages per item.
**				24.Text Menu. same as 22 but with addition of backlighting.
**				25.Text Menu. "select option", no numbers, mouse input only.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dinitchoice(prms,reply)	/* UG_DINITCHOICE - initialize choice device.*/
UG_initdev *prms;						/* UG_initdev defined in gksdidd.h */
Gstatus *reply;
{
	uu_denter(UU_GITRC,(us,"ug_dinitchoice(dev=%d)",(*prms).devno));
	/* ug_drmmenu((*prms).id,(*prms).devno); */
	(*reply)=UG_OK;
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  ug_dinitchoice2(prms,reply)	
**		Init choice device simulation routine. Build a raster segment 
**		for iconmenus, pet=5.
**			Valid Choice PETS :
**				3. Text Menu. "choice number", numbers displayed, kbd input only.
**				5. Icon Menu. "select option", no numbers, mouse input only.
**				21.Text Menu. "choice(F-keys)",numbers displyed, fkey input only.
**						(probably never used.)
**				22.Text Menu. "select option", numbers displayed, mouse or kbd.
**						(probably used most often.)
**				23.Text Menu. same as 22 but with help messages per item.
**				24.Text Menu. same as 22 but with addition of backlighting.
**				25.Text Menu. "select option", no numbers, mouse input only.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dinitchoice2(prms,reply)	/* UG_DINITCHOICE - initialize choice device.
											Just like ug_dinitchoice except builds raster
											segments for iconmenus, pet==5. */
UG_initdev *prms;						/* UG_initdev defined in gksdidd.h */
Gstatus *reply;
{
	Gws ws;							/* workstation id */
	int dev;							/* number of an icon menu */
	int pet,segno,newsegno;
	Gchoicest *choicept;
	UG_segstli *segptr;

	uu_denter(UU_GITRC,(us,"ug_dinitchoice2(dev=%d)",(*prms).devno));

	dev=(*prms).devno;
	ws=(*prms).id;
	choicept= &(*ug_gksstli.wsopen[ws].inptr).choicedata[dev-1];
	pet=(*choicept).pet;

	if (pet==5) {				/* icon menu */
		segno=(*choicept).record.seg;		/* segment id of users's menu seg */
		/* create a raser segment of the menu */
		newsegno=2*UG_MAXSEGNO+dev;		/* segment id of the raster seg */
		if (ug_drasseg(ws,segno,newsegno,&(*choicept).e_area)!=NCL_NO_ERROR) {
			(*reply)=UG_NONE; goto rtn;
		}
		segptr=ug_segac(newsegno);
		(*segptr).segatts.gdtect=UG_DETECTABLE;
		if ((*choicept).mode==UG_EVENT) { 	/* seg visible if UG_EVENT mode. */
			(*segptr).segatts.gvis=UG_VISIBLE;
			ug_view0(newsegno,0);				/* display the segment */
		}
		else (*segptr).segatts.gvis=UG_INVISIBLE;
	}
	(*reply)=UG_OK;
rtn:	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dinitpick(prms,reply)		
**		Initialize pick device simulation routine.
**		No driver action required.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dinitpick(prms,reply)		/* UG_DINITPICK -- initialize pick device.
											No action required. Just return UG_OK */
UG_initpick *prms;					/* UG_initpick defined in gksdidd.h */
Gstatus *reply;
{
	Gidevno dev;
	UG_wdt *wdtpt;
	uu_denter(UU_GITRC,(us,"ug_dinitpick(devno=%d)",(*prms).devno));
	dev=(*prms).devno-1;
	wdtpt=ug_gksstli.wsopen[(*prms).id].wdtptr;
	/* should put this check in device independent ginitpick */
	if ((dev<0)||(dev>((*(*wdtpt).inwdtpt).npick-1))) {
/*		fprintf(ug_gksos.erfile,"ginitpick(ws=%d) - bad device no=%d\n",
									(*prms).id,dev+1);*/
		*reply=UG_NONE; uu_dexit; return;
	}
	*reply=UG_OK;
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dinitloc(prms,reply) -- init locator device.
**				No driver action required.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dinitloc(prms,reply)		/* UG_DINITLOC - initialize locator device.
										No action required. Just return UG_OK */
UG_initdev *prms;						/* Ginitdev defined in gksdidd.h */
Gstatus *reply;
{
	int dev;
	uu_denter(UU_GITRC,(us,"ug_dinitloc(id=%d devno=%d)",
					(*prms).id,(*prms).devno));
	dev=(*prms).devno-1;
	/* put this check for legal device number in device independent ginitloc */
	if ((dev<0)||(dev>1)) {
/*		fprintf(ug_gksos.erfile,"ginitloc(ws=%d) - bad device no=%d\n",
									(*prms).id,dev+1);*/
		*reply=UG_NONE;
	}
	else {									/* device number ok */
		*reply=UG_OK;
	}
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dinitloc2(prms,reply) -- init locator device.
**		A version of ug_dinitloc for raster display devices.  Maintains
**		user defined cursors using the raster get, put, and alloc jump
**		table entries.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dinitloc2(prms,reply)	
UG_initdev *prms;
Gstatus *reply;
{
	int pet,dev,ws;
	Glocst *locpt;
	UG_segstli *segpt;		/* pointer to segment to be drug */
	Gsegvis v;					/* visibility of segment to be drug */
	int *tmpbox;				/* pointer for temporarily saving what's behind seg */
	int attach_pt[2];			/* Attach point for user cursors */
	int segno;
	int ll[2],ur[2];
	int offll[2],offur[2];
	Gnrect nrect;

	uu_denter(UU_GITRC,(us,"ug_dinitloc2(id=%d devno=%d)",
					(*prms).id,(*prms).devno));

	offll[0] = 0; offll[1] = 0; offur[0] = 0; offur[1] = 0;

	dev = prms->devno - 1;
	ws  = prms->id;

	/* Check for legal device number */
	if ( dev > MAXUSRCURSORS-1 ) {
		uu_dprint(-1,(us,"ug_dinitloc2. ERROR-dev (%d) > MAXUSRCURSORS",dev));
/*		fprintf(ug_gksos.erfile,"ginitloc(ws=%d) - bad device no=%d\n",
									(*prms).id,dev+1);*/
		*reply=UG_NONE;
		uu_dexit;
		return;
	}
	else {									/* device number ok */
		*reply=UG_OK;
	}
          
	/* Delete any current cursor */
	if (ug_usrcursor[dev] != NULL) {
		(*(ug_gksstli.wsopen[ws].connid)[UG_DRASDEALLOC])(ws, ug_usrcursor[dev]);
	}

	locpt = &((*ug_gksstli.wsopen[ws].inptr).locdata[dev]);
	pet = locpt->pet;

	if(pet==21) {
		segno = locpt->record.seg;		/* segment number of cursor */
	
		uu_denter2(UU_GITRC,(us,"ug_dinitloc2. drag segno=%d",segno));
		uu_dexit;

		/* Make a raster copy of the segment to be drug */
		segpt = ug_segac(segno);			/* get pointer to segment */
		if ((*segpt).wcboxok==0) {		/* ndc box not defined yet */

			/* KLUDGE -- really should copy viewsgpk code to make a version that
			 * only calculates the ndc box. */
			uu_denter2(UU_GITRC,(us,"ug_dinitloc2 -WARNING; wc box undefined!"));
			uu_dexit;
/*			ug_calcbox(segno);			// calculate the ndc box */
			ug_segwcbox(segno);
		}
     
		/* Change bounding rectangle to raster coords */
		ug_segndcbox(segno,&nrect);
		(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])(&nrect.ll,ll,ws);
		(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])(&nrect.ur,ur,ws);
		(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
			(&locpt->record.attach,attach_pt,ws);

		/* Allocate the memory to hold the cursor	(size of bounding rect) */
 		(*(ug_gksstli.wsopen[ws].connid)[UG_DRASALLOC])
			(ws, ll, ur, &ug_usrcursor[dev]);

		/* Save and clear the "rasbox" */
 		(*(ug_gksstli.wsopen[ws].connid)[UG_DRASALLOC])
			(ws, ll, ur, &tmpbox);
		(*(ug_gksstli.wsopen[ws].connid)[UG_DRASGET])
			(ll, ur, tmpbox, UG_COPY) ;
		(*(ug_gksstli.wsopen[ws].connid)[UG_DRASPUT])
			(ll, ur, NULL, UG_CLEAR,offll,offur) ;

		/* Now draw the segment */
		v = segpt->segatts.gvis;			/* save visibility of segment */
		segpt->segatts.gvis=UG_VISIBLE;	/* make VISIBLE */
		ug_view0(segno,0);					/* draw the segment */
		segpt->segatts.gvis=v;				/* restore visibility */

		/* Copy the image in the "rasbox" to the memory allocated */
		(*(ug_gksstli.wsopen[ws].connid)[UG_DRASGET])
			(ll, ur, ug_usrcursor[dev], UG_COPY);

		/* Save the attach point */
		ug_usrattach[dev].x = attach_pt[0] - ll[0] + 1;
		ug_usrattach[dev].y = attach_pt[1] - ll[1] + 1;

		/* Restore the saved contents of the "rasbox" */
		(*(ug_gksstli.wsopen[ws].connid)[UG_DRASPUT])
			(ll, ur, tmpbox, UG_COPY,offll,offur) ;

		/* free the storage pointed to by tmpbox */
		(*(ug_gksstli.wsopen[ws].connid)[UG_DRASDEALLOC])(ws, tmpbox);

	}

	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dinitstroke(prms,reply) -- init stroke device.
**					No driver action required.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dinitstroke(prms,reply)	/* UG_DINITSTROKE - initialize stroke device.
														No action required. Just return UG_OK */
UG_initdev *prms;						/* Ginitdev defined in gksdidd.h */
Gstatus *reply;
{
	int dev;
	uu_denter(UU_GITRC,(us,"ug_dinitstroke(id=%d devno=%d)",
					(*prms).id,(*prms).devno));

	dev=(*prms).devno-1;
	/* put this check for legal device number in device independent ginitloc */
	if ((dev<0)||(dev>1)) {
/*		fprintf(ug_gksos.erfile,"ginitloc(ws=%d) - bad device no=%d\n",
									(*prms).id,dev+1);*/
		*reply=UG_NONE;
	}
	else {									/* device number ok */
		*reply=UG_OK;
	}
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dinitval(prms,reply) -- init valuator device.
**					No driver action required.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dinitval(prms,reply)		/* UG_DINITVAL -- initialize valuator device.
										No action required, just return OK */
UG_initdev *prms;						/* Ginitdev defined in gksdidd.h */
Gstatus *reply;
{
	Gidevno dev;
	dev=(*prms).devno-1;							/* device number indexes valdata */
}

/*********************************************************************
**    S_FUNCTION     :  ug_dinitstr(prms,reply) -- init string device.
**			If pet==22 (ANSI text area), calculate number rows, cols and
**			call UG_DANSION to create the area (invisible).
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dinitstr(prms,reply)			/* initialize string device */
/* if a scrolling text area, reduce lines,cols if necessary */ 
UG_initdev *prms;
Gstatus *reply;
{
	static int onlist[10];			/* list of windows that are on */
	static int onlen=0;				/* length of onlist */
	int i,j;
	Gstringst *spt;
	int rasll[2],rasur[2];			/* echo area in dev coords */
	int rc[2],nrnc[2];
	int dev;
	/* int maxlines,maxcols; */
	UG_wdt *wdtpt;						/* pointer to workstation's wdt */
	int ymax,xmax;	/* ws parameters from wdt */
/*	int rowmax,colmax */

	dev=(*prms).devno;
	spt= &(*ug_gksstli.wsopen[(*prms).id].inptr).stringdata[(*prms).devno-1];
	/* if this string device is already on our list of scrolling
		text areas, call workstation's UG_DANSIOFF entry and take it off
		the list. */
	for (i=0; i<onlen; i++) {
		if (onlist[i]==dev) {
			scrollup=0;
			(*(ug_gksstli.wsopen[(*prms).id].connid)[UG_DANSIOFF])
				((*prms).id,dev);
			/* delete this device from on list */
			for (j=i; j<onlen-1; j++) onlist[j]=onlist[j+1];
			onlen--;
			break;
		}
	}
	if ((*spt).pet==22) {		/* see if lines,cols needs reducing */
		Gfloat tmp;
		wdtpt=ug_gksstli.wsopen[(*prms).id].wdtptr;
		/*rowmax=(*wdtpt).rowmax;*/
		/*colmax=(*wdtpt).colmax; */
		ymax=(*wdtpt).dspsize.raster.y;
		xmax=(*wdtpt).dspsize.raster.x;
		/* convert echo area to raster coordinates */
		rasll[0]= (int)((*spt).e_area.ll.x*xmax);
		rasll[1]=(int)((*spt).e_area.ll.y*xmax);
		rasur[0]=(int)((*spt).e_area.ur.x*xmax);
		rasur[1]=(int)((*spt).e_area.ur.y*xmax);
		ug_dshrink ( wdtpt, rasll, rasur, rasll, rasur, rc, nrnc ) ;
		/*maxlines=(rasur[1]-rasll[1])*rowmax/ymax;*/
		/*maxcols=(rasur[0]-rasll[0])*colmax/xmax;*/
		/*if (maxlines<(*spt).record.lins) (*spt).record.lins=maxlines;*/
		/*if (maxcols<(*spt).record.cols) (*spt).record.cols=maxcols; */
		if (nrnc[0]<(*spt).record.lins) (*spt).record.lins=nrnc[0];
		if (nrnc[1]<(*spt).record.cols) (*spt).record.cols=nrnc[1];

		/* return actual ll and ur coordinates. */
		tmp=rasll[0]; (*spt).e_area.ll.x=tmp/xmax;
		tmp=rasll[1]; (*spt).e_area.ll.y=tmp/xmax;
		tmp=rasur[0]; (*spt).e_area.ur.x=tmp/xmax;
		tmp=rasur[1]; (*spt).e_area.ur.y=tmp/xmax;

		/* call workstation's entry to put up scroll area, make it invisible */
		(*(ug_gksstli.wsopen[(*prms).id].connid)[UG_DANSION])
			((*prms).id,&(*spt).e_area,dev,
			(*spt).record.lins,(*spt).record.cols);
		/* remember it is on */
		onlist[onlen]=dev; onlen++;
		/*
		(*(ug_gksstli.wsopen[(*prms).id].connid)[UG_DANSINVIS])
			((*prms).id,dev);  */
	}
	*reply=UG_OK;
}

			/*********** DEVICE MODE SETTING ROUTINES *******/

/*********************************************************************
**    S_FUNCTION     :  int ug_dstrmode(prms,reply) -- set string mode.
**			If the string device has echo type 22 (ANSI text area), 
**				If mode==EVENT call UG_DANSIVIS to make the area visible.
**				If mode!=EVENT but previous mode was EVENT, call UG_DANSINVIS
**				to make the area invisible.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dstrmode(prms,reply)				/* set string mode */
Sprms *prms;
int reply[];
{
	Gstringst *spt;
/*	char us[120];*/
	spt= &(*ug_gksstli.wsopen[(*prms).wsid].inptr).stringdata[(*prms).dev-1];
	uu_denter2(UU_GITRC,(us,"ug_dstrmode(%d %d %d %d) pet=%d",(*prms).wsid,(*prms).dev,
					(*prms).mode,(*prms).echo,(*spt).pet));
	if ((*spt).pet==22) { 
		if ((*prms).mode==UG_EVENT) {	/* change to EVENT mode */
			if (scrollup!=(*prms).dev) {		/* scroll area not already visible */
				/* call workstation to put up scrolling text area */
				(*(ug_gksstli.wsopen[(*prms).wsid].connid)[UG_DANSIVIS])
					((*prms).wsid,(*prms).dev);
				scrollup=(*prms).dev;			/* remember scrolling area is up */
			}
		}
		else {								/* not EVENT mode */
			if (scrollup==(*prms).dev) {		/* scroll area is up */
				/* take down scrolling area */
				(*(ug_gksstli.wsopen[(*prms).wsid].connid)[UG_DANSINVIS])
						((*prms).wsid,(*prms).dev);
				scrollup=0;						/* remember scrolling area is down */
			}
		}
	}
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dchoicemode(prms,reply)
**		Workstation Simulation routine for Set choice mode (UG_DCHOICEMODE).
**		If mode==EVENT, call UG_DMENU to put up menu.
**		If mode!=EVENT and menu is up, call UG_DNMENU to take down menu.
**			Valid Choice PETS :
**				3. Text Menu. "choice number", numbers displayed, kbd input only.
**				5. Icon Menu. "select option", no numbers, mouse input only.
**				21.Text Menu. "choice(F-keys)",numbers displyed, fkey input only.
**						(probably never used.)
**				22.Text Menu. "select option", numbers displayed, mouse or kbd.
**						(probably used most often.)
**				23.Text Menu. same as 22 but with help messages per item.
**				24.Text Menu. same as 22 but with addition of backlighting.
**				25.Text Menu. "select option", no numbers, mouse input only.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dchoicemode(prms,reply)				/* set choice mode */
Sprms *prms;
int reply[];
{
	Gchoicest *choicept;
	int newsegno;
	int choprmt;
	int pet;
	UG_segstli *p;
/*	char us[120];*/
	choicept= &(*ug_gksstli.wsopen[(*prms).wsid].inptr).choicedata[(*prms).dev-1];
	pet=(*choicept).pet;
	uu_denter2(UU_GITRC,(us,"ug_dchoicemode(%d %d %d %d) pet=%d",(*prms).wsid,(*prms).dev,
					(*prms).mode,(*prms).echo,pet));
	if (((pet>=21)||(pet==3)||(pet==5))&&((*prms).dev>=6)) {
		/* is a menu pet */
		if ((*prms).mode==UG_EVENT) {	/* change to EVENT mode */
			switch (pet) {
			case 3:  choprmt=1; 
				break;
			case 5: 
				choprmt=2; 
				newsegno=2*UG_MAXSEGNO+(*prms).dev;	
				p=ug_segac(newsegno);
				/* if any workstation has called ug_dinitchoice2 to create a
					raster segment for this device, make the raster seg VISIBLE*/
				if (p!=NULL) (*p).segatts.gvis=UG_VISIBLE;
				break;
			case 21: choprmt=0; break;
			case 22: case 23: case 24: case 25:
				choprmt=1; break;
			}
			/* call workstation to put up menu */
			(*(ug_gksstli.wsopen[(*prms).wsid].connid)[UG_DMENU])
				((*prms).wsid,(*prms).dev,&menuprmt[choprmt][0]);
		}
		else {								/* not EVENT mode */
			if (pet==5) {		/* icon menu, make seg invisible */
				/* calc segment id of the raster seg */
				newsegno=2*UG_MAXSEGNO+(*prms).dev;	
				/* if any workstation has called ug_dinitchoice2 so that a raster
					segment for this device exists, make the raster seg INVISIBLE*/
				p=ug_segac(newsegno);
				if (p!=NULL) (*p).segatts.gvis=UG_INVISIBLE;
			}
			/* call workstation to take down menu */
			(*(ug_gksstli.wsopen[(*prms).wsid].connid)[UG_DDNMENU])
						((*prms).wsid,(*prms).dev);
		}
	}
	uu_dexit;
}

			/*********** DEVICE REQUEST ROUTINES ***********/

/*********************************************************************
**    S_FUNCTION     :  int ug_dreqval(prms,reply) -- request valuator.
**		Set current_device and current_requested_device.
**		Call gdawaitdev to wait for user to use something,
**		update all logical device measures he uses,
**		and put on input Q all EVENT mode devices he uses. That is:
** 		Call UG_DPROMPT to put up prompt for valuator device 
**			ug_wsdev.curdevno. 
**			If pet=1, call workstation's UG_DKBD entry to await users value. 
**			If pet=21, call workstation's UG_DSTREAM entry to await users value.
**			Call UG_DDNPROMPT to take down prompt.
**		If user used the requested valuator, set reply.stat to OK and
**		put current valuator measure in reply.
**		Else set reply.stat to UG_NONE.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dreqval(prms,reply)		/* UG_DREQVAL - request valuator. */
UG_reqdev *prms;
Gqval *reply;
{
	Gvalst *valpt;
	uu_denter(UU_GITRC,(us,"ug_dreqval(ws=%d,dev=%d)",(*prms).id,(*prms).devno));
	ug_wsdev.curdevno=(*prms).devno;			/* set current_device number */
	ug_wsdev.curreqno=(*prms).devno;			/* set curent_requested device number */
	ug_wsdev.curdevclas=UG_IC_VALUATOR;			/* set current_device class */
	ug_wsdev.curreqclas=UG_IC_VALUATOR;			/* set current_requested device class */
	ug_dawaitdev((*prms).id);				/* await user's input */
	if ((ug_wsdev.curdevclas==ug_wsdev.curreqclas)&&(ug_wsdev.curdevno==ug_wsdev.curreqno)) {	
		/* user used the requested valuator. Use its measure. */
		valpt= &(*ug_gksstli.wsopen[(*prms).id].inptr).valdata[ug_wsdev.curdevno-1];
		(*reply).val=(*valpt).val;
		(*reply).status=UG_OK;
	}
	else {
		(*reply).val=0.0;
		(*reply).status=UG_NONE;
	}
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dreqpick(prms,reply) -- request pick device.
**		Set current_device and current_requested_device to requested pick device.
**		Call ug_dawaitdev to:
**			Call UG_DPROMPT to put up pick prompt.
**			Call UG_DPIK to wait for user to enter something.
**			Call UG_DDNPROMPT to take down prompt.
**			Put on event q any devices user used which are in event mode.
**		If user used pick, set reply.stat to OK and put current pick
**		measure in reply.  Else set reply.statuts to NONE.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dreqpick(prms,reply)			/* UG_DREQPICK - request pick device.*/
UG_reqdev *prms;
Gqpicks *reply;
{
	Gpickst *pickpt;		/* pointer to pick state */
	static Gint seg[14];
	uu_denter(UU_GITRC,(us,"wsgreqpick(ws=%d,dev=%d)",(*prms).id,(*prms).devno));
	
	ug_wsdev.curdevno=(*prms).devno;
	ug_wsdev.curreqno=(*prms).devno;
	ug_wsdev.curdevclas=UG_IC_PICK;
	ug_wsdev.curreqclas=UG_IC_PICK;
	ug_dawaitdev((*prms).id);			/* await user input */
	pickpt= &(*ug_gksstli.wsopen[(*prms).id].inptr).pickdata[(*prms).devno-1];
	if ((ug_wsdev.curdevclas==ug_wsdev.curreqclas)&&(ug_wsdev.curdevno==ug_wsdev.curreqno)) {	
		/* user used requested pick device. Use current measure. */
		(*reply).pickpath=(*pickpt).pick.pickpath;	/* pointer to segment list */
		(*reply).depth=(*pickpt).pick.depth;			/* length of pickpath */
		(*reply).status=UG_OK;			/* status OK even if user missed object */
	}
	else {
	   (*reply).status=UG_NONE;
		(*reply).pickpath=seg;   /* make pickpath points to something */
	}
	uu_denter2(UU_GITRC,(us,"ug_dreqpick returns status=%d, depth=%d",
							(*reply).status,(*reply).depth));
	uu_dexit;
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dreqloc(prms,reply) -- request locator.
**		Set current_device and current_requested_device to requested loc.
**		Call ug_dawaitdev to wait for user to use something,
**		update all logical device measures he uses,
**		and put on input Q all EVENT mode devices he uses. That is:
**	 		Call UG_DPROMPT to put up loc prompt for device number 
**			ug_wsdev.curdevno.
**			Call UG_DTRK to await users input.
**			Call UG_DDNPROMPT to take down prompt.
**		If user used locator, set reply.stat to OK and put loc measure
**		in reply.  Else return UG_NONE;
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dreqloc(prms,reply)			/* UG_DREQLOC - request locator. */
UG_reqdev *prms;
UG_reqloc *reply;
{
	Glocst *locpt;
/*	static char chstat[2][5]={"OK","NONE"};		// for debug */

	uu_denter(UU_GITRC,(us,"ug_dreqloc(ws=%d devno=%d,reply)",
							(*prms).id,(*prms).devno));
	ug_wsdev.curdevno=(*prms).devno;
	ug_wsdev.curreqno=(*prms).devno;
	ug_wsdev.curdevclas=UG_IC_LOCATOR;
	ug_wsdev.curreqclas=UG_IC_LOCATOR;
	locpt= &(*ug_gksstli.wsopen[(*prms).id].inptr).locdata[ug_wsdev.curdevno-1];
	ug_dawaitdev((*prms).id);			/* await an event */
	if ((ug_wsdev.curdevclas==ug_wsdev.curreqclas)&&
			(ug_wsdev.curdevno==ug_wsdev.curreqno)) {	
		/* user used the requested locator */
		/* eventually change to translate ndc x,y posn into 3d world */
		/* coordinates  according to viewport it is in.*/
		(*reply).loc.x=(*locpt).loc.position.x;
		(*reply).loc.y=(*locpt).loc.position.y;
		(*reply).stat=UG_OK;

		/* Find which viewport loc is in */
		reply->tran = ug_locvp(&reply->loc);
	}
	else {
		(*reply).stat=UG_NONE;
		(*reply).loc.x=0.0; (*reply).loc.y=0.0;
		(*reply).tran=1;				/* change this */
	}
	uu_denter2(UU_GITRC,(us,"ug_dreqloc returns %s, loc=%g %g, tran=%d",
				&chstat[(int)(*reply).stat][0],(*reply).loc.x,
				(*reply).loc.y,(*reply).tran));
	uu_dexit;
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dreqstroke(prms,reply) -- request stroke.
**
**			Set current_device and current_requested_device to requested stroke.
**			Call ug_dawaitdev to wait for user to use something,
**			update all logical device measures he uses,
**			and put on input Q all EVENT mode devices he uses. That is:
** 			Call UG_DPROMPT to put up loc prompt for device number 
**				ug_wsdev.curdevno.
**				Call UG_DSTREAM to await users input.
**				Call UG_DDNPROMPT to take down prompt.
**			If user used locator, set reply.stat to OK and put stroke measure
**			in reply.  Else return NONE;	
**
**    PARAMETERS   
**       INPUT  : 
**         prms
**       OUTPUT :  
**          reply
**    RETURNS      : NONE or OK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dreqstroke(prms,reply)			/* UG_DREQSTROKE - request stroke. */

UG_reqdev *prms;
UG_reqstroke *reply;
{
	Gstrokest *strokept;
/*	static char chstat[2][5]={"OK","NONE"};		// for debug */

	uu_denter(UU_GITRC,(us,"ug_dreqstroke(ws=%d devno=%d,reply)",
							(*prms).id,(*prms).devno));

	ug_wsdev.curdevno   = (*prms).devno;
	ug_wsdev.curreqno   = (*prms).devno;
	ug_wsdev.curdevclas = UG_IC_STROKE;
	ug_wsdev.curreqclas = UG_IC_STROKE;

	strokept= 
		&(*ug_gksstli.wsopen[(*prms).id].inptr).strokedata[ug_wsdev.curdevno-1];

	/* await an event */
	ug_dawaitdev((*prms).id);	

	if ((ug_wsdev.curdevclas==ug_wsdev.curreqclas)&&
		 (ug_wsdev.curdevno==ug_wsdev.curreqno)) {	

		/* user used the requested stroke device */
		(*reply).buf      = (Gnpoint *)strokept->stroke.points;
		(*reply).n_points = strokept->stroke.n_points;
		(*reply).stat     = UG_OK;
		(*reply).tran     = strokept->stroke.transform;
	}
	else {
		(*reply).stat = UG_NONE;
		(*reply).tran = 0;
	}

	uu_denter2(UU_GITRC,(us,"ug_dreqstroke returns %s",
				&chstat[(int)(*reply).stat][0]));
	uu_dexit;
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dreqchoice(prms,reply) -- request choice.
**		Set current_device and current_requested_device to requested choice.
**		Call ug_dawaitdev to wait for user to use something,
**		update all logical device measures he uses,
**		and put on input Q all EVENT mode devices he uses. That is:
**			Put up choice prompt for device ug_wsdev.curdevno if not already 
**			up by calling UG_DPROMPT or UG_DMENU depending 
**			on echotype to put up prompt.
**			Call UG_DCHOICE or UG_DTRK to wait for user to enter something,
**				depending upon prompt and echo type.
**			Take down prompt or menu by calling UG_DDNPROMPT or UG_DDNMENU.
**			If user made a choice, update current choice measure.
**		If user used requested choice device, set reply.stat to OK
**		and put current choice measure in reply.
**		Else set reply.stat to NONE.
**			Valid Choice PETS :
**				3. Text Menu. "choice number", numbers displayed, kbd input only.
**				5. Icon Menu. "select option", no numbers, mouse input only.
**				21.Text Menu. "choice(F-keys)",numbers displyed, fkey input only.
**						(probably never used.)
**				22.Text Menu. "select option", numbers displayed, mouse or kbd.
**						(probably used most often.)
**				23.Text Menu. same as 22 but with help messages per item.
**				24.Text Menu. same as 22 but with addition of backlighting.
**				25.Text Menu. "select option", no numbers, mouse input only.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dreqchoice(prms,reply)		/* UG_DREQCHOICE - request choice.*/
UG_reqdev *prms;
Gqchoice *reply;
{
/*	static char strstat[2][5]={"OK","NONE"};	// for debugging msg */
	Gchoicest *choicept;

	uu_denter(UU_GITRC,(us,"ug_dreqchoice(dev=%d,echotype=%d)",(*prms).devno,
		(*ug_gksstli.wsopen[(*prms).id].inptr).choicedata[(*prms).devno-1].pet));

	ug_wsdev.curdevno=(*prms).devno;		/* set current_device number */
	ug_wsdev.curreqno=(*prms).devno;		/* set current_requested device number */
	ug_wsdev.curdevclas=UG_IC_CHOICE;
	ug_wsdev.curreqclas=UG_IC_CHOICE;

	/* get a pointer to the choice state */
	choicept= &((*ug_gksstli.wsopen[(*prms).id].inptr).choicedata[ug_wsdev.curdevno-1]);	

	ug_dawaitdev((*prms).id);				/* await event */

	if ((ug_wsdev.curdevclas==ug_wsdev.curreqclas)&&(ug_wsdev.curdevno==ug_wsdev.curreqno)) {	
		/* user used the requested choice device. Use current measure. */
		(*reply).status=UG_OK;
		(*reply).choice=(*choicept).choice;
	}
	else (*reply).status=UG_NONE;

	uu_denter2(UU_GITRC,(us,"ug_dreqchoice returning status=%s, choice=%d",
					&strstat[(int)(*reply).status][0],(*reply).choice));
	uu_dexit;
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dreqstr(prms,reply) -- request string.
**		Set current_device and current_requested_device to this string device.
**		Call ug_dawaitdev to wait for user to use something,
**		update all logical device measures he uses,
**		and put on input Q all EVENT mode devices he uses. That is:
** 		Put up string prompt for device number ug_wsdev.curdevno 
**			if not already up by calling UG_DANSION if prompt is a scrolling 
**			text area, or calling UG_DMSG to put up prompt string.
**			Call UG_DKBD to await users input.
**			Call UG_DANSIOFF to Take down scrolling text prompt if necessary.
**			Update current string measure with whatever he typed.
**		If user used this string device, set reply.stat to OK and
**		put string measure in reply.  Else set reply.stat to NONE;
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dreqstr(prms,reply)		/* UG_DREQSTRING - request string. */
UG_reqdev *prms;
UG_rreqstr *reply;
{
	Gstringst *stringpt;				/* pointer to string state */
	uu_denter(UU_GITRC,(us,"ug_dreqstr(dev=%d)",(*prms).devno));
	/* don't update ws because 4109 takes too long. Ws should update itself*/
	/*	on next input request anyway */
	/* updprms[0]=UG_DUPDATE;			// update workstation */
	/* ug_wkout(updprms,2);			// call workstation */
	ug_wsdev.curdevno=(*prms).devno;		/* set current_device number */
	ug_wsdev.curreqno=(*prms).devno;		/* set current_requested_device number */
	ug_wsdev.curdevclas=UG_IC_STRING;
	ug_wsdev.curreqclas=UG_IC_STRING;
	ug_dawaitdev((*prms).id);			/* await user action */
	if ((ug_wsdev.curdevclas==ug_wsdev.curreqclas)&&(ug_wsdev.curdevno==ug_wsdev.curreqno)) {	
		/* User used requested string. Use current measure */
		stringpt= &(*ug_gksstli.wsopen[(*prms).id].inptr).stringdata[(*prms).devno-1];
		strcpy((*reply).str,(*stringpt).string);
		(*reply).len=strlen((*reply).str);		/* length of returned string */
		(*reply).stat=UG_OK;
	}
	else (*reply).stat=UG_NONE;		/* user didn't enter a string */
	uu_dexit;
}

			/********* AWAIT DEVICE ROUTINES *********/

/*********************************************************************
**    I_FUNCTION     :  int ug_dawaitev(prms) -- await any device.
**    PARAMETERS   
**       INPUT  : UG_reqdev *prms;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dawaitev(prms)				/* GDAWAITEV -- await any device */
UG_reqdev *prms;
{
	uu_denter(UU_GITRC,(us,"ug_dawaitev(ws=%d)",(*prms).id));
	ug_wsdev.curreqclas=UG_IC_NONE;			/* no requested device class */
	ug_dawaitdev((*prms).id);	
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dawaitdev(wid) -- await event from any  device.
**	Algorithm:
**		while (user uses device_select keys) {
**			call GDAWAITcurrent_device	to wait for user to use some device,
**				update measures of devices used, put on input Q all EVENT 
**				mode devices used.
**			if (user used dev_sel keys) set current_device to the one selected.
**		}
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dawaitdev(wid)			/*  await event from any  device. */
Gws wid;
{
	char msg[256];
	/* wsawait data must be in same order as Giclass enum type in gksi.h */ 
	static int wsawait[6]={UG_DAWAITLOC,UG_DAWAITSTROKE,UG_DAWAITVAL,
									UG_DAWAITCHOICE,UG_DAWAITPICK,UG_DAWAITSTRING};
	int done;
	int k;						/* which device select key was hit */
	Gnrect ndcrect2;			/* 2D copy of ug_redrw.ndcrect */
	uu_denter(UU_GITRC,(us,"ug_dawaitdev(%d)",wid));
	/* should put this wid validity check in dev indep part */
	if ((wid<0)||(wid>4)) {
		sprintf(msg, "ug_dtstdevop error. ws id = %d\n",wid);
		ud_printmsg(msg);
		uu_dexit;
		exit(2);
	}
	/* call workstation to tell it an xform has changed */
	if (ug_getredrwflag()==1)
	{
		/* make a 2D copy of ug_redrw.ndcrect in ndcrect2 */
		ug_getredrwrect2(&ndcrect2);
		(*(ug_gksstli.wsopen[wid].connid)[UG_DCHGXFORM])(wid,&ndcrect2,-1);
		ug_resetredrwrect();
		ug_setredrwflag(0);
	}
	/* cycle thru all devices, starting at curdev */	
	done=0;
	while (done==0) {				/* do while user uses dev sel keys */
		if (ug_wsdev.curreqclas==ug_wsdev.curdevclas) ug_wsdev.curdevno=ug_wsdev.curreqno;
		else ug_wsdev.curdevno=1;		/* set to pick device 1 */
		/* call workstation's entry to await this class */
		done=(*(ug_gksstli.wsopen[wid].connid)[wsawait[(int)ug_wsdev.curdevclas]])(wid,&k);
		if (done==0) {			/* user uses dev select key. set current device */
			ug_wsdev.curdevclas=(Giclass)(k-1);
			ug_wsdev.curdevno=1;			/* a dev sel key selects device 1 of that class*/
			
		}
	}									/* end of while user hasnt triggered anything */
	uu_denter2(UU_GITRC,(us,"ug_dawaitdev returns.  ug_wsdev.curdevclas=%s, ug_wsdev.curdevno=%d",
			&sclas[(int)ug_wsdev.curdevclas][0],ug_wsdev.curdevno)); 
	uu_dexit;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dawaitpick(wid,k) -- await pick input.
** 	Call UG_DPROMPT to put up pick prompt.
**		Call UG_DPIK to wait for user to enter something.
**		Call UG_DDNPROMPT to take down prompt.
**		Update current pick measure, put on Q if EVENT mode.
**		Update loc device 1 measure, put on Q if EVENT mode.
**		If user used a device select key, set k=key number
**		and return 0.  
**		If user used normal kbd key, update choice dev 1, return 1.
**		If user used keypad key, update choice device 2, return 1.
**		If user used tablet puck, update choice device 3, return 1.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dawaitpick(wid,k)
Gws wid;							/* workstation id */
int *k;							/* dev select key number */
{
	int done,irtn;
	Gpickst *pickpt;
	Gstringst *stringpt;
	int xy[2];					/* location in dev coords */
	Gnpoint loc;
/*	char us[80];*/
	int i,j;
	static Gpickid seg[14];
	Gint depth;
	int item;
	Gdspsize *siz;
	Gipoint raspt;
	Gfloat scale;
	int dnflag = 0;

	pickpt= &(*ug_gksstli.wsopen[wid].inptr).pickdata[ug_wsdev.curdevno-1];
	uu_denter2(UU_GITRC,(us,"ug_dawaitpick. ug_wsdev.curdevclas=%s",
		&sclas[(int)ug_wsdev.curdevclas][0]));

	/* take down scrolling area if it is up */
	if (scrollup!=0) {
		stringpt= &(*ug_gksstli.wsopen[wid].inptr).stringdata[scrollup-1];
		if(stringpt->record.perm_flag != 1) {
			(*(ug_gksstli.wsopen[wid].connid)[UG_DANSINVIS])(wid,scrollup);
			dnflag = 1;
		}
	}


	/* put up prompt at lower left corner of echo area */
	/* first convert to raster coordinates */
	siz= &((*(ug_gksstli.wsopen[wid].wdtptr)).dspsize);
	scale=(*siz).raster.x; scale=scale/(*siz).device.x;
	raspt.x=(int)((*pickpt).e_area.ll.x*scale);
	raspt.y=(int)((*pickpt).e_area.ll.y*scale);
	(*(ug_gksstli.wsopen[wid].connid)[UG_DPROMPT])
			(wid,(*pickpt).record.prompt,&raspt,
					ug_wsdev.curdevno*7+(int)(UG_IC_PICK)+11);
	xy[0]=curxy[0]; xy[1]=curxy[1];  /* NOW use curxy as initial position */
	/* wait for user to pick something, use graphics pick cursor */
	UD_picking_active = 1;
	done=(*(ug_gksstli.wsopen[wid].connid)[UG_DPIK])(wid,seg,&depth,xy,k,0);
	/* take down prompt*/
	UD_picking_active = 0;
	(*(ug_gksstli.wsopen[wid].connid)[UG_DDNPROMPT])
				(wid,ug_wsdev.curdevno*7+(int)(UG_IC_PICK)+11);
	curxy[0]=xy[0]; curxy[1]=xy[1];		/* update current posn */
	/* convert to NDC */
	(*(ug_gksstli.wsopen[wid].connid)[UG_DDEVNDC])(xy,&loc,wid);	
	done=ug_ckpuck(wid,done,k,xy);		/* check for puck tablet squares */
	j=0;
	irtn=1;
	switch (done) {	
	case 0:								/* user hit a keypad 2 key */
		irtn=ug_case0(wid,*k,&j);		/* check for dev sel key. If not, update*/
											/* choice device 5 */
		if (irtn!=0) {
			/* cause ug_dreqloc to return NONE */
			ug_wsdev.curdevclas=UG_IC_CHOICE; 
			ug_wsdev.curdevno=5;
			/* update current pick measure */
			ug_updpick(wid,ug_wsdev.curdevno,seg,depth,&j);
			/* update loc dev 1 measure. To Q if EVENT*/
			ug_updloc(wid,&loc,&j,1);
		}
		break;

	case 1:									/* user hit a normal kbd char */
	case 3:									/* user hit a tablet puck key */
		if (depth>0) i=0;
		else i=ug_dmenuno(wid,xy,&item);
		if (i>0) {							/* user hit a menu and no graphics */
			/* choicept= &(*ug_gksstli.wsopen[wid].inptr).choicedata[i-1]; */
			ug_updchoice(wid,item,&j,i);	/* upd choice dev i, to Q if */
			ug_wsdev.curdevclas=UG_IC_CHOICE;
			ug_wsdev.curdevno=i;
		}
		else {							/* user didn't hit a legal menu choice */
			/* update cur pick , to Q if EVENT mode */
			ug_updpick(wid,ug_wsdev.curdevno,seg,depth,&j);	
			/* update loc dev 1 measure. To Q if EVENT mode */
			ug_updloc(wid,&loc,&j,1);		
		}
		ug_updchoice(wid,*k,&j,done);		/* update choice dev  1, or 3 measure, 
													put on Q if in EVENT mode */
		break;								/* end of case 2,4 */

	case 2:									/* user used keypad */
	case 4:									/* user hit tablet square */
		/* update loc dev 1 measure. To Q if EVENT*/
		/* ug_updloc(wid,&loc,&j,1);	*/
		ug_updchoice(wid,*k,&j,done);		/* update choice dev  2, or 4 measure, 
													put on Q if in EVENT mode */
		/* cause ug_dreqloc to return NONE */
		ug_wsdev.curdevclas=UG_IC_CHOICE; ug_wsdev.curdevno=2;	
		break;								/* end of case 2,4 */

	case 5:								/* user hit a "arrow" or ctrl key */
		ug_updchoice(wid,*k,&j,1);		/* update choice device 1 */
		ug_wsdev.curdevno=1;
		ug_wsdev.curdevclas=UG_IC_CHOICE;
		uu_dprint(UU_GITRC,(us,"UG_DPIK returned 5"));
		break;

	default: 
		uu_dprint(UU_GITRC,(us,
			"ug_dawaitpick error. UG_DPIK returned %d",done));
		break;
	}											/* end of switch (done) */

 		/* put scroll area back up if needed */
	if (dnflag != 0)
		(*(ug_gksstli.wsopen[wid].connid)[UG_DANSIVIS])(wid,scrollup);
	ug_etime=ug_etime+j;
	uu_dprint(UU_GITRC,(us,
	"gawaitpick returns %d. done=%d, status=%d, depth=%d,loc=%g %g",
				irtn,done,(*pickpt).pick.status,(*pickpt).pick.depth,
				loc.x,loc.y));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dawaitchoice(wid,k) -- await choice.
**		Put up choice prompt for device ug_wsdev.curdevno if not already up by
**		calling UG_DPROMPT or UG_DMENU depending 
**		on echotype to put up prompt.
**		Call UG_DCHOICE or UG_DTRK to wait for user to enter something,
**				depending upon prompt and echo type.
**		Take down prompt or menu by calling UG_DDNPROMPT or UG_DDNMENU.
**		If user made a choice, update current choice measure.
**		If user used keypad 2 key 1-6 (dev sel key), set devkey=number of
**		the key used and return 0.
**		If he used keypad key update choice device 2, put choice event on Q
**		if in EVENT mode, and return 1.
**		If he used tablet puck, update choice device 3, put choice event on Q
**		if in EVENT mode, and return 1.
**			Valid Choice PETS :
**				3. Text Menu. "choice number", numbers displayed, kbd input only.
**				5. Icon Menu. "select option", no numbers, mouse input only.
**				21.Text Menu. "choice(F-keys)",numbers displyed, fkey input only.
**						(probably never used.)
**				22.Text Menu. "select option", numbers displayed, mouse or kbd.
**						(probably used most often.)
**				23.Text Menu. same as 22 but with help messages per item.
**				24.Text Menu. same as 22 but with addition of backlighting.
**				25.Text Menu. "select option", no numbers, mouse input only.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dawaitchoice(wid,k)

Gws wid;						/* workstation id */
int *k;						/* dev select key no */
{
	Gchoicest *choicept;
	Gstringst *stringpt;
	int i,j,n;
	int irtn;
/*	char us[80];*/
	char **p;
	Gdpoint posn;
	int done;								/* return value */
	int xy[2];							/* loc posn in dev coords */
	Gnpoint locndc;						/* loc posn in ndc coords */
	int item;								/* menu item picked */
	char *ss;
	int mdev;								/* which menu user used, or 0 */
	Gfloat scale;
	Gdspsize *siz;
	Gipoint raspt;
	UG_wdt *wdtpt;							/* pointer to this workstations wdt */
	int dnflag = 0;

	choicept= &(*ug_gksstli.wsopen[wid].inptr).choicedata[ug_wsdev.curdevno-1];

	uu_denter2(UU_GITRC,(us,"ug_dawaitchoice. curdevno=%d, pet=%d, strings=%s",
		ug_wsdev.curdevno,(*choicept).pet,(*choicept).record.strings[0]));

	n=(*choicept).record.number;	/* number of choices */
	if (n>128) {
		uu_denter2(UU_GITRC,(us,"greqchoice. ERROR. too many choices=%d",n)); 
		uu_dexit;
		uu_dexit;
		exit(2);
	}

	/* take down scrolling area if it is up */
	if (scrollup!=0)
		{
		stringpt= &(*ug_gksstli.wsopen[wid].inptr).stringdata[scrollup-1];
		if(stringpt->record.perm_flag != 1)
			{
			(*(ug_gksstli.wsopen[wid].connid)[UG_DANSINVIS])(wid,scrollup);
			dnflag = 1;
			}
		}


	p=(*choicept).record.strings;	/*pointer to pointers to menu strings */
	posn.y=(*choicept).e_area.ur.y;
	posn.x=(*choicept).e_area.ll.x;		/* top left of menuarea */
	uu_denter2(UU_GITRC,(us,"ug_dawaitchoice e_area=%g %g, %g %g",
		(*choicept).e_area.ll.x,(*choicept).e_area.ll.y,
		(*choicept).e_area.ur.x,(*choicept).e_area.ur.y));
	uu_dexit;

	/* calculate and use center of menu as initial pick cursor posn */
	wdtpt=ug_gksstli.wsopen[wid].wdtptr;
	xy[0]=(int)((((*choicept).e_area.ll.x+(*choicept).e_area.ur.x)/2.0)
					*(*wdtpt).dspsize.raster.x);
	xy[1]=(int)((((*choicept).e_area.ll.y+(*choicept).e_area.ur.y)/2.0)
					*(*wdtpt).dspsize.raster.x);	/* note x is correct here */

	switch ((*choicept).pet) {				/* switch on echotype */
	case 1:									/* echotype 1 -- use alpha kbd */
		if ((*choicept).mode!=UG_EVENT){		/* if prompt not already up */
			/* call workstation's entry to put up prompt for current device */
			/* first convert to raster coordinates */
			siz= &((*wdtpt).dspsize);
			scale=(*siz).raster.x; scale=scale/(*siz).device.x;
			raspt.x=(int)((*choicept).e_area.ll.x*scale);
			raspt.y=(int)((*choicept).e_area.ll.y*scale);
			(*(ug_gksstli.wsopen[wid].connid)[UG_DPROMPT])
					(wid,(*choicept).record.strings[0],&raspt,
							ug_wsdev.curdevno*7+(int)(UG_IC_CHOICE)+11);
		}
		/* call workstation to await choice, etc. */
		done=(*(ug_gksstli.wsopen[wid].connid)[UG_DCHOICE])
						(wid,ug_wsdev.curdevno,xy,k,&item,&mdev);
		if ((*choicept).mode!=UG_EVENT)	/* if prompt not supposed to stay up */
			(*(ug_gksstli.wsopen[wid].connid)[UG_DDNPROMPT])
				(wid,ug_wsdev.curdevno*7+(int)(UG_IC_CHOICE)+11);

		done=ug_ckpuck(wid,done,k,xy);	/* check for tablet square */

		irtn=1;
		j=0;
		switch (done) {
		case 0:							/* user hit a keypad 2 key */
			irtn=ug_case0(wid,*k,&j);	/* check for dev sel key. If not, update
												choice dev 5 */
			if (irtn!=0) ug_wsdev.curdevno=5;
			break;
		case 1:						/* user hit a normal key */
			ug_updchoice(wid,*k,&j,1);	/* update choice 1. Put on Q if EVENT*/
			ug_wsdev.curdevno=1;
			break;
		case 2:						/* user hit a keypad 1 key */
			ug_updchoice(wid,*k,&j,2);	/* update choice 2. Put on Q if EVENT*/
			ug_wsdev.curdevno=2;
			break;
		case 3:						/* user used tablet puck */
			if ((mdev>0)&&(item>0)) {	/* user hit a menu item */
				ug_updchoice(wid,item,&j,mdev);	/* update menu device, to Q if EVENT*/
				ug_updchoice(wid,*k,&j,done);	/* update choice 3, to Q if EVENT */
				ug_wsdev.curdevno=mdev;
			}
			else {					/* didnt hit a menu item */
				ug_updchoice(wid,*k,&j,done);	/* update choice 3, to Q if EVENT */
				ug_wsdev.curdevno=done;
			}
			break;
		case 4:						/* user hit tablet square */
			ug_updchoice(wid,*k,&j,4);
			ug_wsdev.curdevno=4;
			break;
			
		default: uu_denter2(UU_GITRC,(us,
			"ug_dawaitchoice pet 1 error. UG_DCHOICE returned %d",done));
			uu_dexit;
			break;
		}									/* end of switch (done) */
		break;							/* end of case 1 */

	case 3:					/* echotype 3 -- put up menu, get number*/ 
		if ((*choicept).mode!=UG_EVENT)		/* if menu not already up */
			(*(ug_gksstli.wsopen[wid].connid)[UG_DMENU])
				(wid,ug_wsdev.curdevno,"choice number: "); /* put up menu*/

		done=(*(ug_gksstli.wsopen[wid].connid)[UG_DTRK])(wid,xy,k,1,1,0,NULL,NULL);
		if ((*choicept).mode!=UG_EVENT)		/* if menu not supposed to stay up */
			(*(ug_gksstli.wsopen[wid].connid)[UG_DDNMENU])(wid,ug_wsdev.curdevno);	
		mdev=ug_dmenun(wid,xy);		/* which menu was loc cursor in */
		j=0;
		irtn=1;
		switch (done) {
		case 0:				/* user hit a keypad 2 key */
			irtn=ug_case0(wid,*k,&j);			/* check for dev sel key, update choice 5
												if not */
			if (irtn!=0) ug_wsdev.curdevno=5;
			break;
		case 1:				/* user hit a normal kbd char */
			if (mdev>0) {			/* user hit some menu */
				ug_wsdev.curdevno=mdev;		/* update currently used device number */
				choicept= 
					&(*ug_gksstli.wsopen[wid].inptr).choicedata[ug_wsdev.curdevno-1];
				n=(*choicept).record.number;	/* number of choices */
				/* convert '0'-'9', 'a'-'f' to integer */
				if ((*k>='0')&&(*k<='9')) i= *k - '0';
				else if ((*k>='a')&&(*k<='f')) i= *k - 'a' + 10;
				else i=0;
				if ((i>0)&&(i<=n))  *k=i;
				else ug_wsdev.curdevno=1;		/* choice out of range */
			}
			else ug_wsdev.curdevno=1;			/* user hit no menu */
			ug_updchoice(wid,*k,&j,ug_wsdev.curdevno);	/* update measure, put on Q */

			break;
		case 2:						/* user hit a keypad key */
		case 3:						/* user hit a mouse button key */
		case 5:						/* user hit line editing key, arrow or insert 
											mode */
			ug_updchoice(wid,*k,&j,done);			/* update choice 2, 3 or 5 */
			ug_wsdev.curdevno=done;			/* causes NONE return from reqchoice*/
			break;
		default:
			uu_denter2(UU_GITRC,(us,
						"ug_dawaitchoice pet 3 error. UG_DKBD returned %d",done));
			uu_dexit;
			ug_wsdev.curdevno=done;			/* causes NONE return from reqchoice*/
			break;
		}								/* end of switch (*done) */
		uu_denter2(UU_GITRC,(us,
			"ug_dawaitchoice pet 3 n=%d,done=%d,irtn=%d,*k=%d,ug_wsdev.curdevno=%d",
									n,done,irtn,*k,ug_wsdev.curdevno));
		uu_dexit;
		break;

	case 21:		/* echotype 21 -- use function keys */
		if ((*choicept).mode!=UG_EVENT)	/* if menu not already up, put it up */
			(*(ug_gksstli.wsopen[wid].connid)[UG_DMENU])
				(wid,ug_wsdev.curdevno,"choice(F keys):");

		/* call workstation to await t-wheel loc. Use keypad key that ended loc */
		done=(*(ug_gksstli.wsopen[wid].connid)[UG_DTRK])(wid,xy,k,2,1,0,NULL,NULL);
		if ((*choicept).mode!=UG_EVENT)	/* see if need to take down menu */
			(*(ug_gksstli.wsopen[wid].connid)[UG_DDNMENU])(wid,ug_wsdev.curdevno);

		mdev=ug_dmenuno(wid,xy,&item);			/* which menu was loc cursor in */
		irtn=1;
		j=0;
		switch (done) {
		case 0:							/* user hit a keypad 2 key */
			irtn=ug_case0(wid,*k,&j);	/* check for dev sel key. If not, update
												choice dev 5 */
			if (irtn!=0) ug_wsdev.curdevno=5;
			break;
		case 1:						/* user hit a normal key */
			ug_updchoice(wid,*k,&j,1);	/* update choice 1. Put on Q if EVENT*/
			ug_wsdev.curdevno=1;
			break;
		case 2:						/* user hit a keypad 1 key */
			/* update current choice measure if a legal choice made */
			if (*k<=n) (*choicept).choice= *k;
			else {					/* not a legal choice, update choice device 2 */
				ug_updchoice(wid,*k,&j,2);	/* update choice 2. Put on Q if EVENT*/
				ug_wsdev.curdevno=2;
			}
			break;
		default: uu_denter2(UU_GITRC,(us,
			"ug_dawaitchoice pet 4 error. UG_DKEYPAD returned %d",done));
			uu_dexit;
			break;
		}									/* end of switch (done) */
		break;							/* end of case 21 */

	case 22:	/* These devices are defined in header of this routine */
	case 23:
	case 24:
	case 25:
	case 5:
		ss="select option: ";
		if ((*choicept).mode!=UG_EVENT)	/* if menu not already up */
			/* call workstation's entry to put up menu for current device */
			(*(ug_gksstli.wsopen[wid].connid)[UG_DMENU])(wid,ug_wsdev.curdevno,ss);

		/* call workstation's entry to get a choice */
		done=(*(ug_gksstli.wsopen[wid].connid)[UG_DCHOICE])
					(wid,ug_wsdev.curdevno,xy,k,&item,&mdev);

		if ((*choicept).mode!=UG_EVENT)	/* if menu not supposed to stay up */
			/* call wokrstation's entry to take down menu */
			(*(ug_gksstli.wsopen[wid].connid)[UG_DDNMENU])(wid,ug_wsdev.curdevno);

		done=ug_ckpuck(wid,done,k,xy);	/* check for tablet squares */

		if ((mdev>0)&&(item>0)) {		/* user made a menu selection */
			ug_wsdev.curdevno=mdev;					/* make this menu current device */
			choicept= &(*ug_gksstli.wsopen[wid].inptr).choicedata[ug_wsdev.curdevno-1];
		}

		/* update current choice device measure*/
		if ((mdev>0)&&(item>0)) (*choicept).choice=item;		
		j=0;
		if ((done==0)&&(*k<=6)) irtn=0;		/* dev sel key used */
		else irtn=1;

		/* conv to NDC */
		(*(ug_gksstli.wsopen[wid].connid)[UG_DDEVNDC])(xy,&locndc,wid);	

		switch(done) {
		case 0:								/* user hit a keypad 2 key */
			irtn=ug_case0(wid,*k,&j);		/* check for dev sel key, update choice
													device 5, to Q if EVENT if not*/
			if (irtn==0) ug_wsdev.curdevno=1;	/* ug_dreqchoice returns NONE if 
																dev select key used */
			else {	
				if (item==0) ug_wsdev.curdevno=5; /* ug_dreqchoice returns NONE if
																no menu item picked */
			}
			break;
		case 1:								/* user hit a normal kbd key */
			if ((mdev>0)&&(item>0)) {	/* user hit a menu item */
				/* here should put any EVENT menu device with pet=1 on Q */
			}
			ug_updchoice(wid,*k,&j,done);	/* upd choice dev 1 to Q if EVENT */
			ug_wsdev.curdevno=1;			/* causes ug_dreqchoice to return NONE */
			break;
		case 3:								/* user hit a tablet puck key */
			if ((mdev>0)&&(item>0)) {	/* user hit a menu item */
				/* update menu device, to Q */
				ug_updchoice(wid,item,&j,ug_wsdev.curdevno);	
				ug_updchoice(wid,*k,&j,done);	/* upd choice 3 to Q if EVENT */
			}
			else {							/* user didn't hit a menu item */
				ug_updchoice(wid,*k,&j,done);	/* upd choice 1 or 3 to Q if EVENT */
				ug_wsdev.curdevno=3;		/* causes ug_dreqchoice to return NONE */
			}											
			break;
		case 2:								/* user hit a keypad 1 key */
		case 4:								/* user hit a tablet square */
		case 5:								/* user hit a line editing key (arrow,
													or insert mode) */
			ug_updchoice(wid,*k,&j,done);	/* upd choice 2,4 or 5 to Q if EVENT */
			ug_wsdev.curdevno=done;					/* causes ug_dreqchoice to return NONE
														if no menu item picked */
			break;
		}										/* end of switch(done) */
		uu_denter2(UU_GITRC,(us,
			"ug_dawaitchoice case 22-25,5 done=%d, *irtn=%d,choice=%d,ug_wsdev.curdevno=%d",
									done,irtn,(*choicept).choice,ug_wsdev.curdevno));
		uu_dexit;
		ug_etime=ug_etime+j;					/* bump input Q time */
		break;								/* end of case pet = 22 thru 24 */

	default:
		uu_denter2(-1,(us,"ug_dawaitchoice. ERROR. unknown pet=%d, dev=%d",
				(*choicept).pet,ug_wsdev.curdevno));
		uu_dexit;
		break;
	}										/* end of switch on echotype */
	ug_etime=ug_etime+j;

					/* put scroll area back up if needed */
	if (dnflag != 0)
		(*(ug_gksstli.wsopen[wid].connid)[UG_DANSIVIS])(wid,scrollup);

	uu_denter2(UU_GITRC,(us,"ug_dawaitchoice returns %d. done=%d, k=%d",irtn,done,*k));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dawaitloc(wid,k) -- await loc.
**	 	Call UG_DPROMPT to put up loc prompt for device number ug_wsdev.curdevno.
**		Call UG_DTRK to await users input.
**		Call UG_DDNPROMPT to take down prompt.
**		Update current loc measure, put on Q if in EVENT mode.
**		If he used keypad 2 key 1-6 (dev select keys), set k=number of the 
**		key used and return 0.  If other keypad 2 key, update choice device 
**		4 and return 1.  If he used a normal kbd key, keypad key, or tablet 
**		puck,  update choice 1 2, or 3 respectively and put on Q
**		if in EVENT mode. Return 1.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dawaitloc(wid,k)
Gws wid;						/* workstation id */
int *k;						/* device select key number */
{	
	Glocst *locpt;
	Gstringst *stringpt;
	int i,j;
	int item;
/*	char us[80];*/
	int done,irtn;
	Gnpoint loc;			/* returned location in NDC */
	int xy[2];				/* location in device coords */
	Gdspsize *siz;
	Gipoint raspt;
	Gfloat scale;
	int cursorno;
	int *usrc;				/* pointer to user defined cursor or NULL */
	Gipoint *attach;
	int dnflag = 0;

	locpt= &(*ug_gksstli.wsopen[wid].inptr).locdata[ug_wsdev.curdevno-1];
	uu_denter2(UU_GITRC,(us,"ug_dawaitloc. ug_wsdev.curdevno=%d, init loc=%g %g",
	 			ug_wsdev.curdevno,(*locpt).loc.position.x,(*locpt).loc.position.y));
			/* take down scrolling area if it is up */
	if (scrollup!=0)
		{
		stringpt= &(*ug_gksstli.wsopen[wid].inptr).stringdata[scrollup-1];
		if(stringpt->record.perm_flag != 1)
			{
			(*(ug_gksstli.wsopen[wid].connid)[UG_DANSINVIS])(wid,scrollup);
			dnflag = 1;
			}
		}

	/* put up loc prompt at lower left corner of echo area */
	/* first convert to raster coordinates */
	siz= &((*(ug_gksstli.wsopen[wid].wdtptr)).dspsize);
	scale=(*siz).raster.x; scale=scale/(*siz).device.x;
	raspt.x=(int)((*locpt).e_area.ll.x*scale);
	raspt.y=(int)((*locpt).e_area.ll.y*scale);
	(*(ug_gksstli.wsopen[wid].connid)[UG_DPROMPT])
			(wid,(*locpt).record.prompt,&raspt,
					ug_wsdev.curdevno*7+(int)(UG_IC_LOCATOR)+11);
	/**** use current locator measure for initial loc position. */
	(*(ug_gksstli.wsopen[wid].connid)[UG_DNDCDEV])
			(&(*locpt).loc.position,xy,wid);			/*convert to device coords*/
	/* xy[0]=curxy[0]; xy[1]=curxy[1];	// DON't use last posn as initial posn */
	/* call workstation to track either locator cursor */
	cursorno=(*locpt).pet;
	if (cursorno<2) cursorno=2;
	if(cursorno==21) {
		usrc = ug_usrcursor[ug_wsdev.curdevno-1];
		attach = &(ug_usrattach[ug_wsdev.curdevno-1]);
	}
	else {
		usrc = NULL;
		attach = NULL;
	}
	done=(*(ug_gksstli.wsopen[wid].connid)[UG_DTRK])
			(wid,xy,k,cursorno,0,ug_wsdev.curdevno,usrc,attach);
	(*(ug_gksstli.wsopen[wid].connid)[UG_DDNPROMPT])
			(wid,ug_wsdev.curdevno*7+(int)(UG_IC_LOCATOR)+11);	/* take down prompt */
	curxy[0]=xy[0]; curxy[1]=xy[1];		
	/* convert to NDC*/
	(*(ug_gksstli.wsopen[wid].connid)[UG_DDEVNDC])(xy,&loc,wid);	
	done=ug_ckpuck(wid,done,k,xy);		/* check for tablet square */
	j=0;
	irtn=1;
	switch (done) {		/* switch on type of key user ended tracking*/
	case 0:
		irtn=ug_case0(wid,*k,&j);	/* if dev sel key used, irtn=0. Else
											update choice dev 5 and put on Q if EVENT */
		if (irtn!=0) {
			ug_wsdev.curdevclas=UG_IC_CHOICE; 
			ug_wsdev.curdevno=5;	/* cause ug_dreqloc to return NONE */
			ug_updloc(wid,&loc,&j,ug_wsdev.curdevno);	
		}
		break;
	case 1:					/* user used normal kbd key */
	/* i=ug_dmenuno(wid,xy,&item);*/
	/* if ((i>0)&&(depth==0)) {				// user hit within a menu */
	/* 	choicept= &(*ug_gksstli.wsopen[wid].inptr).choicedata[i-1];*/
	/* 	if ((seg[1]>=1)&&(seg[1]<=(*choicept).record.number)) {	*/
			/* legal menu choice*/
	/* 		ug_updchoice(wid,seg[1],&j,i);	// upd choice dev i, to Q if */
	/* 	}*/
	/* 	else i=0;*/
	/* }*/
	/* if (i<=0) {			// didnt hit within a menu, or not legal choice */
			/* update current loc, to Q if EVENT*/
			ug_updloc(wid,&loc,&j,ug_wsdev.curdevno);	
	/*	} */
		ug_updchoice(wid,*k,&j,1);			/* update choice dev 1  measure. 
													Put on Q if device is in EVENT mode */
		break;
	case 2:					/* user used keypad */
		ug_updchoice(wid,*k,&j,2);	/* update choice dev 2 measure. To Q if EVENT*/
		ug_wsdev.curdevclas=UG_IC_CHOICE; 
		ug_wsdev.curdevno=2;			/* causes ug_dreqloc to return NONE */
		ug_updloc(wid,&loc,&j,1);	/* update loc dev 1 measure. To Q if EVENT*/
		break;
	case 3:					/* user used tablet puck */
		if ((i=ug_dmenuno(wid,xy,&item))>0) {	/* user picked a menu item */
			ug_updchoice(wid,item,&j,i);			/* upd choice dev i, to Q if */
			ug_updchoice(wid,*k,&j,3);				/* update choice dev 3 (puck) */
			ug_wsdev.curdevno=i;						/* update current device */
		}
		else {											/* user didnt hit a menu */
			ug_updloc(wid,&loc,&j,ug_wsdev.curdevno);	/* upd current loc */
			ug_updchoice(wid,*k,&j,3);				/* update choice dev 3 (puck) */
		}
		break;
	case 4:												/* user hit tablet square */
		ug_updchoice(wid,*k,&j,4);	/* tablet squares are choice device 4 */
		ug_wsdev.curdevclas=UG_IC_CHOICE; ug_wsdev.curdevno=4;
		break;
	}								/* end of switch on *done */
	ug_etime=ug_etime+j;
					/* put scroll area back up if needed */
	if (dnflag != 0)
		(*(ug_gksstli.wsopen[wid].connid)[UG_DANSIVIS])(wid,scrollup);
	uu_denter2(UU_GITRC,(us,"ug_dawaitloc returns %d. done=%d, x,y=%g %g",
			irtn,done,(*locpt).loc.position.x,(*locpt).loc.position.y));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dawaitstroke(wid,k) -- await stroke.
**
** 	Call UG_DPROMPT to put up loc prompt for device number ug_wsdev.curdevno.
**		Call UG_DSTREAM to await users input.
**		Call UG_DDNPROMPT to take down prompt.
**		Update stroke, put on Q if in EVENT mode.
**		If he used keypad 2 key 1-6 (dev select keys), set k=number of the key 
**		used and return 0.  If other keypad 2 key, update choice device 4 and 
**		return 1. 
**		If he used a normal kbd key, keypad key, or tablet puck,  
**		update choice 1 2, or 3 respectively and put on Q
**		if in EVENT mode. Return 1.
**
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dawaitstroke(wid,k)
Gws wid;						/* workstation id */
int *k;						/* device select key number */
{	
	Gstrokest *strokept;
	Gstringst *stringpt;
	int j;
	int done,irtn;
	Gnpoint loc;			/* returned location in NDC */
	int xy[2];				/* location in device coords */
	int npts;				/* Number of points returned by workstation */
	int stpt;				/* Position to start adding points in buffer */
	Gdspsize *siz;
	Gipoint raspt;
	Gfloat scale;
/*	char us[80];*/
	int dnflag = 0;

	uu_denter2(UU_GITRC,(us,"ug_dawaitstroke. ug_wsdev.curdevno=%d",
	 			ug_wsdev.curdevno));

	strokept = &(*ug_gksstli.wsopen[wid].inptr).strokedata[ug_wsdev.curdevno-1];

	/* Take down scrolling area if it is up */
		if (scrollup!=0)
			{
			stringpt= &(*ug_gksstli.wsopen[wid].inptr).stringdata[scrollup-1];
			if(stringpt->record.perm_flag != 1)
				{
				(*(ug_gksstli.wsopen[wid].connid)[UG_DANSINVIS])(wid,scrollup);
				dnflag = 1;
				}
			}

	/* Put up stroke prompt at lower left corner of echo area */
	/* first convert to raster coordinates */
	siz= &((*(ug_gksstli.wsopen[wid].wdtptr)).dspsize);
	scale=(*siz).raster.x; scale=scale/(*siz).device.x;
	raspt.x=(int)((*strokept).e_area.ll.x*scale);
	raspt.y=(int)((*strokept).e_area.ll.y*scale);
	(*(ug_gksstli.wsopen[wid].connid)[UG_DPROMPT])
			(wid,(*strokept).record.prompt,&raspt,
					ug_wsdev.curdevno*7+(int)(UG_IC_STROKE)+11);

	xy[0] = curxy[0]; xy[1] = curxy[1];		/* Use last posn as initial posn */

	/* Determine editing position */
	stpt = strokept->record.editpos;

	uu_denter2(UU_GITRC,(us,"editing position %d",stpt)); uu_dexit;

	/* Call workstation to get stroked */
	done = (*(ug_gksstli.wsopen[wid].connid)[UG_DSTREAM])
				(wid,ug_wsdev.curdevno,2,xy,k,
				&(strokept->stroke.points[stpt]),&npts,UG_MAXSTROKEPTS-stpt);

	/* Update stroke data */
	strokept->stroke.n_points  = stpt + npts;
	strokept->stroke.transform = 0;
	curxy[0] = xy[0]; curxy[1] = xy[1];		

	uu_denter2(UU_GITRC,(us,"final # of points: %d",stpt+npts)); uu_dexit;

	/* Take down prompt */
	(*(ug_gksstli.wsopen[wid].connid)[UG_DDNPROMPT])
			(wid,ug_wsdev.curdevno*7+(int)(UG_IC_STROKE)+11);	

	/* Check for tablet square */
	done = ug_ckpuck(wid,done,k,xy);		

	j = 0; irtn = 1;

	switch (done) {		/* Switch on type of key that ended tracking */
	case 0:
		irtn=ug_case0(wid,*k,&j);	/* if dev sel key used, irtn=0. Else
											update choice dev 5 and put on Q if EVENT */
		if (irtn!=0) {
			ug_wsdev.curdevclas=UG_IC_CHOICE; 
			ug_wsdev.curdevno=5;	/* cause ug_dreqstroke to return NONE */
			ug_updstroke(wid,&loc,&j,ug_wsdev.curdevno);	
		}
		break;
	case 1:					/* user used normal kbd key */
		/* update current loc, to Q if EVENT*/
		ug_updstroke(wid,&loc,&j,ug_wsdev.curdevno);	

		/* update choice dev 1  measure. Put on Q if device is in EVENT mode */
		ug_updchoice(wid,*k,&j,1);			
										
		break;
	case 2:					/* user used keypad */
		ug_updchoice(wid,*k,&j,2);	/* update choice dev 2 measure. To Q if EVENT*/
		ug_wsdev.curdevclas=UG_IC_CHOICE; 
		ug_wsdev.curdevno=2;	/* causes ug_dreqstroke to return NONE */

		/* update stroke dev 1 measure. To Q if EVENT*/
		ug_updstroke(wid,&loc,&j,1);
		break;
	case 3:					/* user used tablet puck */
		/* update current stroke measure */
		ug_updstroke(wid,&loc,&j,ug_wsdev.curdevno);
		ug_updchoice(wid,*k,&j,3);			/* update choice dev 3 (puck) */
		break;
	case 4:										/* user hit tablet square */
		ug_updchoice(wid,*k,&j,4);	/* tablet squares are choice device 4 */
		ug_wsdev.curdevclas=UG_IC_CHOICE; ug_wsdev.curdevno=4;
		break;
	}								/* end of switch on done */
	ug_etime=ug_etime+j;
	if (dnflag!=0) 					/* put scroll area back up if needed */
		(*(ug_gksstli.wsopen[wid].connid)[UG_DANSIVIS])(wid,scrollup);
	uu_denter2(UU_GITRC,(us,"ug_dawaitstroke returns %d. done=%d", irtn,done));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dawaitstr(wid,k) -- await string.
** 	Put up string prompt for device number ug_wsdev.curdevno if not already up
**		by calling UG_DANSION if prompt is a scrolling text area, or calling
**		UG_DMSG to put up prompt string.
**		Call UG_DKBD to await users input.
**		Take down scrolling text prompt if necessary by calling UG_DANSIOFF.
**		Update current string measure with whatever he typed.
**		If he used keypad 2 (dev select keys), set k=number of the key used
**		and return 0. 
**		If he used a carriage return, put string event on Q 
**		if in EVENT mode and not subject of a request, and return 1.
**		If he used a control or arrow key,  update choice device 1, put
**		choice event on Q if in EVENT mode, and return 1.
**		If he hit keypad key update choice device 2 or 4
**		put choice event on Q if in EVENT mode, and return 1.
**		If he used tablet puck, put update choice device 3,
**		put choice device 3 event on Q if in EVENT mode, and return 1.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dawaitstr(wid,k)
Gws wid;							/* workstation id */
int *k;							/* dev select key number */
{
	int done,irtn,j;
	Gstringst *stringpt;
/*	char us[180];*/
	struct { Gint op; Gws ws; Gidevno dev; Gchar *str;} mparms;
	int mreply[4];
	static char sbuf[NCL_MAX_COMLINE];			/* keyboard string buffer */
	int putup;
	int xend=0;
	Gfloat scale;
	Gdspsize *siz;
	Gipoint raspt;

	stringpt= &(*ug_gksstli.wsopen[wid].inptr).stringdata[ug_wsdev.curdevno-1];
	uu_denter2(UU_GITRC,(us,
		"ug_dawaitstr(%d) ug_wsdev.curdevno=%d pet=%d, prompt=%s",
		wid,ug_wsdev.curdevno,(*stringpt).pet,(*stringpt).record.prompt));
	
	putup=0;
	switch ((*stringpt).pet) {
	case 22:										/* scrolling area pet */
		if ((scrollup==0)&&((*stringpt).mode!=UG_EVENT)) {
			/* call workstation to put up a scrolling area */
			(*(ug_gksstli.wsopen[wid].connid)[UG_DANSION])
					(wid,&(*stringpt).e_area,ug_wsdev.curdevno,
					(*stringpt).record.lins,(*stringpt).record.cols);
			putup=1;				/* remember we put up scrolling area */
		}
		/* put up prompt in scrolling area */
		mparms.op=UG_DPUTSTRING; mparms.ws=wid;
		mparms.str=(*stringpt).record.prompt;
		mparms.dev=ug_wsdev.curdevno;
		if (strlen(mparms.str)>0)
			(*(ug_gksstli.wsopen[wid].connid)[UG_DPUTSTRING])(&mparms,mreply);
		strcpy(sbuf,(*stringpt).initstring); 	/* put initial string in sbuf */
		/* get keyboard string sbuf from ansi area ug_wsdev.curdevno */
		done = (*(ug_gksstli.wsopen[wid].connid)[UG_DKBD])
				(wid, 0, 0, ug_wsdev.curdevno, sbuf,(*stringpt).record.bufsiz, k);

		if (putup==1)	/* call ws to take down scrolling area if we put it up */
				(*(ug_gksstli.wsopen[wid].connid)[UG_DANSIOFF])
					(wid,ug_wsdev.curdevno);
		break;									/* end of case 22 */
	case 1:										/* ordinary string pet */
		/* put up ordinary string prompt in ll corner of echo area */
		/* first convert to raster coordinates */
		siz= &((*(ug_gksstli.wsopen[wid].wdtptr)).dspsize);
		scale=(*siz).raster.x; scale=scale/(*siz).device.x;
		(*(ug_gksstli.wsopen[wid].connid)[UG_DPROMPT])
				(wid,(*stringpt).record.prompt,&raspt,
					ug_wsdev.curdevno*7+(int)(UG_IC_STRING)+11);
		
		/* Calculate raster coords of end of string,
			using number of rasters per character column */

		strcpy(sbuf,(*stringpt).initstring); 	/* put initial string in sbuf */
		/* Get the string */
		xend = 0;
		done = (*(ug_gksstli.wsopen[wid].connid)[UG_DKBD])
					(wid, raspt.y, xend, -1, sbuf,(*stringpt).record.bufsiz,k);

		(*(ug_gksstli.wsopen[wid].connid)[UG_DDNPROMPT])
				(wid,ug_wsdev.curdevno*7+(int)(UG_IC_STRING)+11); /* take down prompt*/
		break;									/* end of case 1 */
	}												/* end of switch on pet */

	(*stringpt).string=sbuf;		/* update string measure. This won't work
												if many people call ug_dawaitstr */
	irtn=1;
	j=0;
	switch (done) {
	case 0:							/* user hit a keypad 2 key */
		irtn=ug_case0(wid,*k,&j);	/* check for dev sel key. If not, update
											choice device 5 */
		break;
	case 1:							/* user hit a normal kbd char, or c/r */
		if ((*stringpt).mode==UG_EVENT) {
			/* put on q unless this string device is the requested one */
			if (((*stringpt).pet!=22)||(ug_wsdev.curreqclas!=UG_IC_STRING)||
					(ug_wsdev.curreqno!=ug_wsdev.curdevno)) {
				ug_dstringtoq(wid,(*stringpt).string);	/* put string onto queue */
				j=1;						/* remember something went on queue */
			}
		}
		break;
	case 2: 								/* user hit a keypad 1 key */
		ug_updchoice(wid,*k,&j,done);		/* update choice device 2  */
		/* cause reqstring to return NONE by setting choice device 2 */
		ug_wsdev.curdevno=2; ug_wsdev.curdevclas=UG_IC_CHOICE;
		break;
	case 5:								/* user hit a "arrow" or ctrl key */
		ug_updchoice(wid,*k,&j,1);		/* update choice device 1 */
		ug_wsdev.curdevno=1;
		ug_wsdev.curdevclas=UG_IC_CHOICE;	/* cause greqstring to return NONE */
		uu_dprint(UU_GITRC,(us,"ug_dawaitstr. ug_dkbd returned 5"));
		break;
	case 3:								/* user hit a tablet puck button */
		ug_updchoice(wid,*k,&j,3);	/* update choice device 3 */
		ug_wsdev.curdevno=3;
		ug_wsdev.curdevclas=UG_IC_CHOICE; 
		break;
/*
.....Buttons & Dials
*/
	case 4:
		ug_updchoice(wid,*k,&j,done);
		/* cause reqstring to return NONE by setting choice device 2 */
		ug_wsdev.curdevno=4; ug_wsdev.curdevclas=UG_IC_CHOICE;
		break;

	default: 
		uu_denter2(UU_GITRC,(us,"ug_dawaitstr error. UG_DKBD returned %d",
			done)); uu_dexit;
		break;
	}								/* end of switch (done) */
	ug_etime=ug_etime+j;			/* bump queue time if anything put on q */
	uu_denter2(UU_GITRC,(us,
			"ug_dawaitstr returns %d, done=%d, string=%s, strlen=%d, k=%d",
					irtn,done,(*stringpt).string,strlen((*stringpt).string),*k));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_dawaitval(wid,k) -- await valuator.
** 	Call UG_DPROMPT to put up prompt for valuator device ug_wsdev.curdevno. 
**		If pet=1, call workstation's UG_DKBD entry to await users value. 
**		If pet=21, call workstation's UG_DSTREAM entry to await users value.
**		Call UG_DDNPROMPT to take down prompt.
**		Update valuator measure and put val on Q if in EVENT mode.
**		If he used keypad 2 key 1 to 6, set k=number of the key used
**		and return 0. Else update choice device 4 and return 1.
**		If he used a carriage return, put value event on Q 
**		if in EVENT mode and not subject of a request, and return 1.
**		If he hit keypad key update choice device 2
**		put choice event on Q if in EVENT mode, and return 1.
**		If he used tablet puck, update choice device 3,
**		put choice device 3 event on Q if in EVENT mode, and return 1.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dawaitval(wid,k)
Gws wid;							/* workstation id */
int *k;							/* dev select key number(1..6) */
{
	Gvalst *valpt;
	Gstringst *stringpt;
	int j;
	int done,irtn;
	char c[100];					/* for value */
/*	char us[120];*/
#if UU_COMP==UU_IRIS 
#ifdef UU_DOUBLE
	long float value;
#else
	float value;
#endif /* UU_DOUBLE */
#endif /* UU_IRIS */
#if UU_COMP != UU_IRIS
	float value;
#endif
	int gotvalue,gotstring;
	int row, col;
	int xy[2],xyend[2];
	Gfloat f[2];
	Gdspsize *siz;
	Gipoint raspt;
	Gfloat scale;

	valpt= &(*ug_gksstli.wsopen[wid].inptr).valdata[ug_wsdev.curdevno-1];

	uu_denter2(UU_GITRC,(us,
		"ug_dawaitval. ws=%d, curdevno=%d, e_area=%g %g %g %g",
		wid,ug_wsdev.curdevno,(*valpt).e_area.ll.x,(*valpt).e_area.ll.y,
		(*valpt).e_area.ur.x,(*valpt).e_area.ur.y));

	/* put up prompt at lower left corner of echo area */
	/* first convert to raster coordinates */
	siz= &((*(ug_gksstli.wsopen[wid].wdtptr)).dspsize);
	scale=(*siz).raster.x; scale=scale/(*siz).device.x;
	raspt.x=(int)((*valpt).e_area.ll.x*scale);
	raspt.y=(int)((*valpt).e_area.ll.y*scale);
	(*(ug_gksstli.wsopen[wid].connid)[UG_DPROMPT])
			(wid,(*valpt).record.prompt,&raspt,
					ug_wsdev.curdevno*7+(int)(UG_IC_VALUATOR)+11);
	switch ((*valpt).pet) {
	case 1:						/* pet 1 -- type value on keyboard */

		/* Get keyboard string sbuf starting at end of last prompt */

		/* Convert beginning position of prompt to raster coords */
		f[0] = valpt->e_area.ll.x;
		f[1] = valpt->e_area.ll.y;
		(*(ug_gksstli.wsopen[wid].connid)[UG_DNDCDEV])(f,xy,wid);

		/* Convert raster xy to nearest row,col */
		ug_drastorc(xy, ug_gksstli.wsopen[wid].wdtptr, &row, &col, 1);

		/* Update col to end of prompt */
		col = col + strlen( valpt->record.prompt );
		/* convert ending col to raster coord */
		ug_drctoras(xyend,ug_gksstli.wsopen[wid].wdtptr,row,col);

		c[0]=0;						/* null initial string */
		/* Get the string */
		done = (*(ug_gksstli.wsopen[wid].connid)[UG_DKBD])
					(wid, xy[1], xyend[0], -1, c,80,k);	
					
		/* Take down valuator prompt */
		(*(ug_gksstli.wsopen[wid].connid)[UG_DDNPROMPT])
				(wid,ug_wsdev.curdevno*7+(int)(UG_IC_VALUATOR)+11);
		j=0;
		irtn=1;
		gotvalue=0;
		gotstring=0;
		if (strlen(c)>0) {		/* user entered some character(s) from kbd */
			if (((c[0]>='0')&&(c[0]<='9'))||(c[0]=='-')||
							(c[0]=='+')||(c[0]=='.')) {	/* user entered a value */
				sscanf(c,"%f",&value);
				gotvalue=1;
				(*valpt).val=value;			/* update current valuator value */
			}
			else {					/* user entered a non-value string from kbd */
				/* update string device 1 measure */
				stringpt= &(*ug_gksstli.wsopen[wid].inptr).stringdata[0];
				(*stringpt).string=c;
				gotstring=1;
				ug_wsdev.curdevclas=UG_IC_STRING; ug_wsdev.curdevno=1;	/* remember he used string */
			}
		}
		switch (done) {
		case 0:						/* user used a keypad 2 key */
			irtn=ug_case0(wid,*k,&j);		/* check for dev sel key. If not, update
													choice device 5 and put on Q if EVENT*/
			if ((irtn!=0)&&(gotvalue==0)) {
				ug_wsdev.curdevclas=UG_IC_CHOICE; ug_wsdev.curdevno=5;	/* remember he used keypad 2*/
			}
			break;
		case 1:					/* user hit a normal kbd char */
			break;
		case 2:					/* user hit a keypad key */
		case 3:					/* user hit a tablet puck */
			ug_updchoice(wid,*k,&j,done);	/* update choice 2 or 3, 
													put on Q if EVENT */
			if (gotvalue==0) {	/* remember he used choice device 2 or 3 */
				ug_wsdev.curdevclas=UG_IC_CHOICE; ug_wsdev.curdevno=done;
			}
			break;
		default: uu_denter2(UU_GITRC,(us,
				"ug_dawaitval error. UG_DKBD returned %d",done));
			uu_dexit;
			break;
		}								/* end of switch(done) */
		if (irtn==1)	{			/* user didnt' use a dev sel key */
			if (gotvalue==1) 		/* user entered a value */
				ug_updval(wid,value,&j,ug_wsdev.curdevno);	/* update val dev, to Q if EVENT */
			if (gotstring==1) 	/* user entered a non-value string */
				ug_updstring(wid,c,&j,1);	/* update string dev 1, to Q if EVENT*/
		}
		break;
	case 21:						/* pet 21 -- y-axis of stream is value */
		/* not implemented yet */
		break;
	}								/* end of switch on pet */
	ug_etime=ug_etime+j;
	uu_denter2(UU_GITRC,(us,"ug_dawaitval returns %d. done=%d",irtn,done));
	uu_dexit;
	uu_dexit;
	return(irtn);
}

/*
......we are not use 3,4,4 for picking, 3 is for mouse and 4 is the number now
......used for wheel_up, must be the old code, if somewhere else used this,
.....then this change may need to rechange
*/
#define PICKTRIGLEN 5
struct {								/* pick triggers that cause picking */
	int dev;							/* device, 0=keypad 2, 1=kbd, 2=fctn keys,
											3=tablet or mouse, 4=tablet square, 5=arrow
											key */
	int min,max;					/* range of values that cause picking */
} ug_piktrig[PICKTRIGLEN]={1,' ',' ',1, 'a','z', 
		1,'0','9',1,'A','Z',		/* keyboard */
		3,1,1/*,3,4,4 */};				/* 1st and 4th tablet or mouse buttons */
int ug_picktriglen=PICKTRIGLEN;	/* length of piktrig */
/*********************************************************************
**    S_FUNCTION     :  int ug_dpik(wid,seg,depth,xy,k,cursorno)
**			Simulates UG_DPIK in terms of UG_DTRK.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dpik(wid,seg,depth,xy,k,cursorno)/* Simulates UG_DPIK in terms of UG_DTRK */
Gws wid;										/* workstation id */
int seg[];									/* for segment nos, pickid */
int *depth;									/* length of seg */
/*
.....Correct declaraction of incomming parameter. Was int xy; - RAZ
*/
int xy[2];									/* on call init location of pick.
													on return, final location of pick */
int *k;										/* char that ended pick tracking */
int cursorno;								/* which cursor to track:
													0=graphics picking cursor,
													1=menu picking cursor.   */
{
	int done,i;
	Gnpoint loc;
/*
.....Add pointer to graphics area.  This is so we can check to see if
.....pick was within graphics for LARGE PICKING APERTURES.
.....Improved picking.  Roberta Zorzynski.
*/
	UD_AREA *grafarea;

	uu_denter(UU_GITRC,(us,"ug_dpik(wid=%d)",wid));
	/* call workstation entry to track locator cross */
	done=(*(ug_gksstli.wsopen[wid].connid)[UG_DTRK])(wid,xy,k,cursorno,0,0,NULL,NULL);
	/* convert to NDC*/
	(*(ug_gksstli.wsopen[wid].connid)[UG_DDEVNDC])(xy,&loc,wid);	
	for (i=0; i<ug_picktriglen; i++) {
		if ((done==ug_piktrig[i].dev)&&(*k>=ug_piktrig[i].min)
			&&(*k<=ug_piktrig[i].max)) 
			break;						/* legal pick trigger */
	}
/*
.....Initialize graphics area pointer - RAZ.
.....Test to see if we are in the graphics area.
*/
	grafarea = UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF];

	if(loc.x < (*grafarea).posn.ll.x || loc.x > (*grafarea).posn.ur.x ||
		loc.y < (*grafarea).posn.ll.y || loc.y > (*grafarea).posn.ur.y)
		{
/*
........User picked outside of graphics area, pick nothing.
*/
		*depth = 0;
		}	

	else if (i<ug_picktriglen) {				/* pick */
/*
.........We were in the graphics area.  Search for a valid pick. RAZ
*/
		/* set for visible segs, partially within*/
		ug_find.vis=0; ug_find.pikmode=0;	
		/* scan segment storage for closest seg to loc. */
		ug_dfindpick(&loc,seg,depth,wid);
	}
	else {								/* don't pick */
/*
........Found nothing.
*/
		*depth=0;
	}
	uu_denter2(UU_GITRC,(us,
			"ug_dpik returns %d, seg=%d %d, loc=%g %g, k=%d, depth=%d",
			done,seg[0],seg[1],loc.x,loc.y,*k,*depth));
	uu_dexit;
	uu_dexit;
	return(done);
}
