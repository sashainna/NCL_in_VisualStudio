/*******************************************************************
**    NAME         :  d4dvgio.c -- low level forms I/O routines 
**				int ud_tsetup(llx,lly,urx,ury,pgc,txc,txf,txs)
**    		UD_FSTAT ud_getevnt()
**				int ud_trmon()
**				int ud_trmoff()
**				int ud_treset
**				int ud_atoff()
**				int ud_attbld()
**				int ud_eraall()
**    		ud_crspos(r,c) -- move to r,c
**				UD_FSTAT frmgets(bfr,fieldlen)
**
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d4dvgio.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:08
*********************************************************************/


#include "udebug.h"
#include "ustdio.h"
#include "usysdef.h"
#include "uims.h"
#include "g.h"
#include "udfconst.h" 
#include "dwindow.h"
#include "dasnog.h"
#include "dinput.h"
#include "gcolors.h"         		/* color definitions - ud_crwin */

UD_EVENTDEF ud_inparm;				/* initial string and length for ud_gevt*/
int ud_togglefield=0;				/* 1=field is a toggle field */

typedef struct							/* attributes */
{
	char bold;
	char undln;
	char blink;
	char rvs;
} UU_TATTS;

typedef struct							/* row,col coordinate */
{
	int r;
	int c;
} UU_RCLOC;

typedef struct							/* window definition */
{
	UU_REAL pll[2];						/* position: lower left in ndc */
	UU_REAL pur[2];						/*     "     upper right in ndc */
	int pcolor;							/* page backround color */
	int tcolor;							/* text color */
	int tfont;							/* text font size index */
	UU_REAL tsize;						/* text char height */
	UU_RCLOC ll;						/* lower left r/c: should be 1,1 */
	UU_RCLOC ur;						/* upper right r/c: size */
	UU_RCLOC cur;						/* current cursor position r/c */
	char tbs[40];						/* tabstop boolean vector */
} UU_WNDW;

static UU_WNDW ttywind;
static UU_RCLOC ttysiz;

int UD_formdev = 2;				/* form device number */
extern int ud_curfldno;
extern int UD_ksws;
Gdrect ud_frmarea;				/* echo area */
char ud_partial[80];				/* save partial string input here, for ud_filall*/

/*
.....For verify mode, reset to zero first time through picking routines
.....In case of previous reject op.
.....Bobby  -  5/24/94
*/
extern int NCL_nopick_cnt;

/*********************************************************************
**		I_FUNCTION	:		int ud_tsetup(llx, lly, urx, ury, pgc, txc, txf, txs)
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_tsetup(llx, lly, urx, ury, pgc, txc, txf, txs)
UU_REAL llx,lly;									/* lower left ndc x,y of window */
UU_REAL urx,ury;									/* upper right ndc x,y of window */
int pgc;											/* page color */
int txc, txf;									/* text color, font, size */
UU_REAL txs;
{
	uu_denter(UU_DTRC,(us,"ud_tsetup:ll=%g,%g",llx,lly));
	uu_dprint(UU_DTRC,(us,"ud_tsetup:ur=%g,%g",urx,ury));
	uu_dprint(UU_DTRC,(us,"ud_tsetup: pc=%d, tc=%d",pgc,txc));
	uu_dprint(UU_DTRC,(us,"ud_tsetup: tf=%d, ts=%g",txf,txs));

	ud_frmarea.ll.x=llx;
	ud_frmarea.ll.y=lly;
	ud_frmarea.ur.x=urx;
	ud_frmarea.ur.y=ury;

	ttywind.pll[0]=llx;
	ttywind.pll[1]=lly;
	ttywind.pur[0]=ury;
	ttywind.pur[1]=ury;
	ttywind.pcolor=pgc;
	ttywind.tcolor=txc;
	ttywind.tfont=txf;
	ttywind.tsize=txs;

	ttywind.ll.c=1;
	ttywind.ll.r=1;

	ttywind.cur.r=1;
	ttywind.cur.c=1;

	uu_dexit;
	return;
}
/*********************************************************************
**		I_FUNCTION	:		int ud_trmon()
**			turns on (activates) the forms window
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ud_trmon()
{
	int bckgrnd;			/* bckgrnd color to ud_crwin */
	int stat = 0;
	UD_WINDOW_REC wcb;	/* window control block */

	uu_denter(UU_DTRC,(us,"ud_trmon:"));
	if(ud_inquire_window() != UD_FREE)
	{
		stat = 1;
		goto retn;
	}

	bckgrnd = dqwinback();
	ud_initwin_rec(&wcb, &ud_frmarea, UG_C_WHITE, bckgrnd);
	wcb.resflag = UD_RESERVE;
	UD_formdev = ud_crwin(&wcb, &ttysiz.r, &ttysiz.c);
	ttywind.ur.c=ttysiz.c;
	ttywind.ur.r=ttysiz.r;
	uu_dprint(UU_DTRC,(us,"ud_trmon:ll absolute=%d,%d; ur absolute=%d,%d",
								ttywind.ll.r,ttywind.ll.c,
								ttywind.ur.r,ttywind.ur.c));
	uu_dprint(UU_DTRC,(us,"ud_trmon: UD_formdev=%d",UD_formdev));
retn:;
	uu_dexit;
	return(stat);
}

/*********************************************************************
**		I_FUNCTION	:		int ud_trmoff()
**			turns off (deactivates) the forms window
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

int ud_trmoff()
{
	uu_denter(UU_DTRC,(us,"ud_trmoff:"));
	ud_kiwin();
	uu_dexit;
}

/*********************************************************************
**		I_FUNCTION	:		int ud_treset
**			initialize reset
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

int ud_treset()
{
	ud_eraall();
	ud_crspos(1,1);
}

/*********************************************************************
**
**		I_FUNCTION	:		int ud_atoff()
**			turn off all character attributes
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

int ud_atoff()
{
}

/*********************************************************************
**		I_FUNCTION	:		int ud_attbld()
**			Synopsis:		turn on character hilighting
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

int ud_attbld()
{
}

/*********************************************************************
**
**		I_FUNCTION	:		int ud_eraall()
**			erase entire screen
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

ud_eraall()
{
	gansiescr(UD_ksws,UD_formdev);
}

/*********************************************************************
**    I_FUNCTION :  ud_crspos(r,c) -- move to r,c
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ud_crspos(r,c)						/* move to r,c */
int r,c;
{
	int pos[2];
	pos[0]=ttywind.ur.r-r; pos[1]=c;
	gansispos(UD_ksws,UD_formdev,pos);
}

/*******************************************************************
**
**		I_FUNCTION	:	UD_FSTAT frmgets(bfr,fieldlen)
**
**			frmgets gets a (left justified) string from the users 
**			at the current cursor location and returns the string in
**			bfr. It also returns a status which indicates what
**			was in the string.
**
**			PARAMETERS	:
**				INPUT		:	UD_DEVENT *event -- input event buffer.
**								int fieldlen -- field length.
**				OUTPUT	:
**			RETURNS		:
**					UD_ALT - "users  alternate action"
**					UD_FWD - field forward
**					UD_BAK - field back
**					UD_DONE - "done" key hit
**					UD_PASSF - default was approved
**					UD_DATAOK - data string entered
**					UD_TFWD -- toggle forward.
**					UD_TBAK -- toggle backward.
**			SIDE EFFECTS:
**			WARNINGS		:
********************************************************************/

UD_FSTAT frmgets(event,fieldlen)
UD_DEVENT *event;					/* event buffer */
int fieldlen;						/* form field length */
{
	UD_FSTAT irtn;
	UD_FSTAT ud_getevnt();
	Gqstring *greqstring();
	int len,i;
	int orgpos[2];					/* cursor starting position */
	char inpbuf[99];
	char blnks[100];
	char stringdata[100];		/* copy of (*event).indata.stringdata */
	static int doneflag=0;		/* 1=user hit DONE key previously */

	int evtype,evecho,evdev;
	UD_DASTAT status,ud_pick1();
	UD_PPICKREC ret_pck;
	
	uu_denter(UU_DTRC,(us,"frmgets(fieldlen=%d, init str=%s)",
		fieldlen,ud_inparm.defstr));

/* -- ifpreviously got DONE, just exit -- */

	if(doneflag == 1) 
	{
		(*event).evclass=UD_CHOICE;
		(*event).evdev=UD_AUXMENU;
		(*event).indata.choicedata=UD_CDONE;
		irtn=UD_DONE;
		doneflag=0;
		goto rtn;
	}
	ud_inparm.strbfsz=fieldlen;		/* set input field length for ud_gevt */

/* -- ud_actfld has already set ud_inparm.defstr to default string -- */

	gansiqpos(UD_ksws,UD_formdev,orgpos);	/* get starting position */

/* -- print periods over entire field if not a toggle field -- */

	if(ud_togglefield != 1) 
	{
		for (i=0; i<fieldlen; i++) 
			blnks[i]='.';
		blnks[fieldlen]=0;
	}
	irtn=UD_BADREQ;

	while(irtn == UD_BADREQ)
	{
		gansispos(UD_ksws,UD_formdev,orgpos);	/* reset starting position */
		if(ud_togglefield!=1)
		{
			gputstring(UD_ksws,UD_formdev,blnks);
			gansispos(UD_ksws,UD_formdev,orgpos);	/* reset starting position */
		}

/* 	-- get user input. The DIGS initial string will overwrite part of the 
			periods. */

/*
.....Allow VERIFY mode to work when toggling
.....to PICK mode from within a form
.....Bobby  -  5/24/94
*/
	status = DE_AGAIN;
	evtype = UD_STRING;
	NCL_nopick_cnt = 0;
	evdev = UD_formdev;
	evecho = 22;
	while (status == DE_AGAIN)
	{
		ud_gevt(event, evtype, "", 1, evdev,evecho, &ud_inparm);
		if ((*event).evclass == UD_PICK)
		{
			status = ud_pick1(event,&ret_pck);
			if (status == DE_TRUE)
				status = ncl_verify_pick(&ret_pck);
			evtype = UD_PICK;
			evdev = UD_pckdev;
			evecho = UD_pckech;
		}
		else status = DE_TRUE;
	}

		uu_dprint(UU_DTRC,(us,
			"frmgets. ud_gevt returned evclass=%d, evdev=%d UD_formdev=%d",
			(*event).evclass, (*event).evdev,UD_formdev));

/* 	-- see if partial string. If so, ignore it and call ud_gevt again 
			except ifuser hit DONE key. In that case, remember the DONE key
			and use the string. -- */

		ud_partial[0]='\0';
		if((*event).evclass == UD_STRING) 
		{
			strcpy(stringdata,(*event).indata.stringdata);
			len=strlen(stringdata);
			uu_dprint(UU_DTRC,(us,"frmgets. evclass=UD_STRING. len=%d, string=%s",
					len,stringdata));
			if((len>0) && (stringdata[len-1]=='\\')) 
			{

/* 			-- save the partial string, for ud_filall  case UD_TFWD -- */

				strcpy(ud_partial,stringdata);

/* 			-- get rid of trailing backslash -- */

				ud_partial[len-1]='\0';

/* 			-- first erase the entire field -- */

				for(i=0; i<fieldlen; i++) 
					blnks[i]=' ';
				blnks[len]='\0';

/* 			-- don't need to blank the input since ud_filall will echo
					the field contents -- */

				while(((*event).evclass==UD_STRING) &&
						(len>0) && 
						(stringdata[len-1]=='\\'))
				{
					gansispos(UD_ksws, UD_formdev, orgpos);	/* starting position */
					ud_gevt(event, UD_STRING, "", 1, UD_formdev, 22,  &ud_inparm);
					if((*event).evclass == UD_STRING)
					{
						strcpy(stringdata, (*event).indata.stringdata);
						len = strlen(stringdata);
					}
				}

				uu_dprint(UU_DTRC,(us,
				"frmgets. 2nd ud_gevt returned evclass=%d, evdev=%d, UD_formdev=%d",
					(*event).evclass, (*event).evdev, UD_formdev));

/* 			-- hit DONE key-- */

				if(((*event).evclass==UD_CHOICE) &&
					((*event).evdev==UD_AUXMENU) &&
					((*event).indata.choicedata==UD_CDONE)) 
				{

/* 				-- put back string. -- */

					(*event).indata.stringdata=ud_partial;
					(*event).evclass=UD_STRING;

/* 				-- remember we got DONE -- */

					doneflag=1;
				}
			}
		}

		switch ((*event).evclass) 
		{

/* 	-- erase the string -- */

		case UD_STRING:
			len=strlen(stringdata);
			uu_dprint(UU_DTRC,(us,"frmgets. evclass=UD_STRING. len=%d, string=%s",
				len, stringdata));

			gansispos(UD_ksws,UD_formdev,orgpos);	/* get starting position */

/* 		-- user just hit c/r -- */

			if(len == 0)
				irtn = UD_PASSF;
			else 
			{
				if(stringdata[len-1] == '\033')
					irtn = UD_DONE;
				else 
				{						

/* 				-- delete leading blanks -- */

					for (i=0; i<len; i++) 
						if(stringdata[i] != ' ')
							break;

					if(i < len)
					{
						strcpy(inpbuf,&(stringdata[i]));
						strcpy(stringdata,inpbuf);
					}

/* 				-- delete trailing blanks -- */

					len=strlen(stringdata);
					for (i=len-1; i>=0; i--)
						if(stringdata[i] != ' ')
							break;

					stringdata[i+1]='\0';
					irtn=UD_DATAOK;
				}
			}
			break;
	
/* 	-- check for form control input or done key -- */

		case UD_CHOICE:
			uu_dprint(UU_DTRC,(us,"frmgets. evclass=UD_CHOICE, choicedata=%d", 
				(*event).indata.choicedata));

/*			-- switch according to input device -- */

		 	switch((*event).evdev) 
			{

/* 			-- arrow keys -- */

		 		case 4:
					switch((*event).indata.choicedata)
					{

/* 				-- up arrow -- */

		 			case 1:
		 				irtn=UD_BAK;
		 				break;

/* 				-- down arrow -- */

		 			case 2:
		 				irtn=UD_FWD;
		 				break;

/* 				-- tab key -- */

		 			case 5:
		 				irtn=UD_TFWD;
		 				break;

					default: uu_dprint(-1,(us,
						"frmgets. case 4 error unknown choicedata=%d",
						(*event).indata.choicedata));
					break;
		 		}
				break;						/* end of case 4 (arrow keys) */

/*			-- DAS control device -- */

		 	case UD_AUXMENU:
				switch((*event).indata.choicedata) 
				{

/* 				-- user hit done key-- */

					case UD_CDONE:
						irtn=UD_DONE;
						break;

/* 				-- up (previous) field -- */

					case UD_CUPFIELD:
						irtn=UD_BAK;	
						break;

/* 				-- down (next) field -- */

					case UD_CDNFIELD:
						irtn=UD_FWD;
						break;

/* 				-- toggle -- */

					case UD_CTGFIELD:
						irtn=UD_TFWD;
						break;


					default: uu_dprint(-1,(us,
						"frmgets. case UD_AUXMENU error unknown choicedata=%d",
						(*event).indata.choicedata));
						break;
				}
		   	break;								/* end of case UD_AUXMENU */

			default:
				uu_dprint(-1,(us,"frmgets. error unknown (*event).evdev=%d",
							(*event).evdev));
					break;
			}									/* end of switch((*event).evdev) */
			break;							/* end of case UD_CHOICE */


		case UD_LOCATOR:
		case UD_VALUATOR:
		case UD_PICK:
		case UD_VECTOR:
			irtn=UD_DATAOK;
			break;

		default:
			uu_dprint(-1,(us,"frmgets. error unknown evclass=%d",
				(*event).evclass));
			break;
		}										/* end switch(event.evclass) */
	}										/* end while (irtn==UD_BADREQ) */

rtn:;	
	ud_pstat(irtn, "frmgets returns");
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**
**    I_FUNCTION :  UD_FSTAT ud_getevnt()
** 		check event q for up or down arrow keys.
** 		if found, return UD_FWD or UD_BAK 
** 		if ctrl t, return UD_TFWD.
** 		else, return UD_DATAOK.
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

UD_FSTAT ud_getevnt()
{
	Gevent *event;
	Gevent *gawaitevent();
	int devno;
	int chc;
	char us[120];
	UD_FSTAT irtn;

	irtn=UD_DATAOK;
	while ((event=gawaitevent((UU_REAL) 0.))!=NULL) 
	{
		devno = (*event).dev;
		uu_dprint(UU_DTRC,(us,"frmgets. event.class=%d, devno=%d",
			(*event).class, devno));
		switch((*event).class) 
		{
		case UG_E_CHOICE:
			chc=ggetchoice();

/* 		-- got a cursor ctrl char -- */

			if(devno==4) 
			{
				switch(chc)
				{

/* 				-- up arrow -- */

					case 1:
						irtn=UD_BAK;
						break;

/* 				-- down arrow -- */

					case 2:
						irtn=UD_FWD;
						break;

/* 				-- tab forward -- */

					case 5:
						irtn=UD_TFWD;
						break;
				}
			}					/* end ifdevno==4 */
			break;			/* end of case E_CHOICE */
		}						/* end switch on class */

		if(irtn != UD_DATAOK) 
			break;
	}							/* end while event!=NULL */
	uu_dprint(UU_DTRC,(us,"ud_getevnt returns %d",irtn));
	return(irtn);
}
