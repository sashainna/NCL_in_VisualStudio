/*********************************************************************
**
**    NAME         :  d3pipe3.c
**
**       CONTAINS:
**  			ud_revt
**  			ud_eevt
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d3pipe3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:06
**
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "dasg.h"
#include "dinput.h"
#include "driver.h"
#include "gi.h"
#include "uims.h"
#include "uhep.h"
#include "usysg.h"
#include "udebug.h"

/********************************************************************* 
**
**  I_FUNCTION		:  ud_revt(event, inparm)
**      request event for preferred event type
**
**  PARAMETERS   
**      INPUT:  event = event input record
**					 inparm = input parameter block
**      OUTPUT: event buffer filled with event details
**
**  RETURNS      :  UU_TRUE if request satisfied, UU_FALSE if not
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UU_LOGICAL ud_revt(event, inparm)
UD_DEVENT *event;						/* event record */
UD_EVENTDEF *inparm; 				/* input parameter block */
{

#define PARSTRCHAR '\\'

	Glocrec inloc;						/* locator initialize record */
	Gqloc *locrec;						/* locator input record */
	Gqloc *greqloc();
	Gloc *ggetloc();					/* locator input routine */
	Gloc *gsampleloc();				/* locator sample input */
	Gloc *elocrec;						/* locator input record from event mode */

	Gvalrec inval;						/* valuator initialize record */
	Gqval *valrec;						/* valuator input record */
	Gqval *greqval();

	Gpickrec inpick;					/* pick initialize record */
	Gpicks picks;						/* pick record for initial state */
	Gqpicks *pickrec;					/* pick input record */
	Gqpicks *greqpick();

	Gstringrec instring;				/* string initialize record */
	Gqstring *stringrec;				/* string input record */
	Gqstring *greqstring();
	Gchar *gsamplestring();
	static char parstr[1024];		/* partial string buffer */

	Gchoicerec inchoice;				/* choice initialize record */
	Gqchoice *choicerec;				/* choice input record */
	Gqchoice  *greqchoice();
	Gint ggetchoice();

	Gqstroke *qstr;					/* stroke support */
	Gqstroke *greqstroke();			/* stroke support */

	Gevent *answer;
	Gevent *gawaitevent();

	int locstat;						/* local status for return */
	int ecnt;							/* event Q counter */
	int i;
	UD_AREA *areapt;
	UD_AREA poploc;
	Gdrect halfrect;
	Gnrect pos;
	int donefl;
	UU_REAL x, y, z, popdx, popdy;
	UU_REAL cord[3];
	Gloc anchor;
	Gimode mode;
	Gesw echo;
	
	uu_denter(UU_DTRC,(us,"entering ud_revt"));

/*	-- initialize some stuff -- */

	locstat = UU_FALSE;
	(*event).evclass = (*inparm).evtype;
	(*event).evdev = (*inparm).device;
	if((*inparm).evtype != UD_CHOICE)
	{
		if( !(((*inparm).evtype == UD_STRING) && ((*inparm).pet == 22)))
			ud_wrprm((*inparm).prompt);
	}

	ud_halfrect(&pos, &UD_CURLEXPRM_AREA);
	ud_devrect(&pos, &halfrect);

/*	printf("\nevent type = %d\n",(*inparm).evtype);*/
	switch((*inparm).evtype)
	{

		case UD_LOCATOR:

			inloc.prompt = UD_locpmt;

/*			-- assume only non-menu choice devices are in event mode -- */

			if(UD_Eventm == UU_TRUE)
				gslocmode(UD_ksws, (*inparm).device, UG_REQUEST, UG_ECHO);

/*			-- convert the drag coordinate to NDC -- */

			gsnormtran((*inparm).rubloc.transform);
			anchor.transform = (*inparm).rubloc.transform;
			gwndc3(&anchor.position.x, &anchor.position.y, &z,
					(*inparm).rubloc.position.x, 
					(*inparm).rubloc.position.y,
					(*inparm).rubloc.position.z);

/*			-- init the choice device only for non-drag locator devices -- */

			if((*inparm).device == 1)
				ginitloc(UD_ksws, (*inparm).device, &anchor, 
										(*inparm).pet, &halfrect, &inloc);
			else
				gchglocinit(UD_ksws, (*inparm).device, &anchor);

/*			-- get the locator report -- */

/*
.....Save current event
.....In case another menu is selected
.....while in locator mode
.....Bobby  -  11/06/98
*/
			ug_save_event();
			locrec = greqloc(UD_ksws, (*inparm).device);
#if UU_COMP == UU_WIN2K
			ud_prmerr(" ");
#endif
			ug_reset_event();

/*			-- init the new default position to the current point -- */

			if(UD_Eventm == UU_TRUE)
			{
				inloc.prompt = UD_locpmt;
				elocrec = gsampleloc(UD_ksws, (*inparm).device);
				if((*inparm).device != 1)
					gslocmode(UD_ksws, 1, UG_REQUEST, UG_ECHO);
				ginitloc(UD_ksws, 1, elocrec, 1, &halfrect, &inloc);
				gslocmode(UD_ksws, 1, UG_EVENT, UG_ECHO);
			}

			if((*locrec).status == UG_OK)
			{
				locstat = UU_TRUE;

/*			-- move the locator data into the event record -- */

				(*event).indata.locdata.transform = (*locrec).loc.transform;
				(*event).indata.locdata.position[0] = (*locrec).loc.position.x;
				(*event).indata.locdata.position[1] = (*locrec).loc.position.y;

/*				-- save the new crosshair location, if not normtran 0 -- */

				if((*locrec).loc.transform != 0)
				{
					UD_nxhair.transform = (*locrec).loc.transform;
					UD_nxhair.position.x = (*locrec).loc.position.x;
					UD_nxhair.position.y = (*locrec).loc.position.y;
					givref3(UD_nxhair.transform, cord);
					gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
					UD_nxhair.position.z = z;
					UD_GLOC3_NDC2W(&UD_wxhair, &UD_nxhair);
				}

/*				-- get the choice event off the event Q -- */

/*
.....See comment below under UD_PICK mode
.....Bobby  -  11/6/98
*/
				ecnt = gqsize();
				if (ecnt == 2) answer = gawaitevent((UU_REAL) 0.);

				answer = gawaitevent((UU_REAL) 0.);
				if(answer != NULL)
				{

/*				-- set the device number to the device of the key that
					triggered it -- */

					(*event).evdev = (*answer).dev;
					(*event).indata.locdata.choice = ggetchoice();
				}
			}

			break;

		case UD_VALUATOR:
			
			inval.prompt = UD_valpmt;
			inval.low = UD_MINVALUATOR;
			inval.high = UD_MAXVALUATOR;
			if(UD_Eventm == UU_TRUE)
				gsvalmode(UD_ksws, (*inparm).device, UG_REQUEST, UG_ECHO);
			ginitval(UD_ksws, (*inparm).device, (UU_REAL) 0., (*inparm).pet, 
									&halfrect, &inval);
			valrec = greqval(UD_ksws, (*inparm).device);
			if(UD_Eventm == UU_TRUE)
			{
				inval.prompt = UD_valpmt;
				ginitval(UD_ksws, (*inparm).device, (UU_REAL) 0., (*inparm).pet, 
										&halfrect, &inval);
				gsvalmode(UD_ksws, (*inparm).device, UG_EVENT, UG_ECHO);
			}

			if((*valrec).status == UG_OK)
			{
				locstat = UU_TRUE;

/*			-- move the valuator data into the event record -- */

				(*event).indata.valdata = (*valrec).val;
			}
			break;

		case UD_PICK:

			donefl = 0;
			inpick.prompt = UD_pckpmt;
			picks.status = UG_NOPICK;
			picks.depth = 0;
			picks.pickpath = NULL;

/*			-- set pick device in request mode if in event mode. If in request
				mode, set the locator in event mode so that we get the event on
				the Q. --*/

			if(UD_Eventm == UU_TRUE)
			{
				gspickmode(UD_ksws, (*inparm).device, UG_REQUEST, UG_ECHO);
/*
.....Set Locator mode to UG_EVENT (waiting for event)
.....Because if we choose a "By pick" type menu while 
.....we are in locator mode, then the pick record will
.....not be placed on the input queue stack
.....Bobby  -  12/13/96
*/
				gglocmode(UD_ksws, (*inparm).device, &mode, &echo);
				gslocmode(UD_ksws, (*inparm).device, UG_EVENT, UG_ECHO);
			}
			else
				gslocmode(UD_ksws, (*inparm).device, UG_EVENT, UG_ECHO);

			ginitpick(UD_ksws, (*inparm).device, &picks, (*inparm).pet, 
									&halfrect, &inpick);
			pickrec = greqpick(UD_ksws, (*inparm).device);
#if UU_COMP == UU_WIN2K
			ud_prmerr(" ");
#endif

			if(UD_Eventm == UU_TRUE)
			{

/*				-- I have to sample the valuator location in case a pick
					event occurred that moved the cursor. Record and playback
					will capture the wrong point if this is the case -- */

				inpick.prompt = UD_pckpmt;
				ginitpick(UD_ksws, (*inparm).device, &picks, (*inparm).pet, 
										&halfrect, &inpick);
				gspickmode(UD_ksws, (*inparm).device, UG_EVENT, UG_ECHO);
/*
.....Restore locator mode
.....Bobby  -  12/13/96
*/
				gslocmode(UD_ksws, (*inparm).device, mode, echo);
			}
			else
				gslocmode(UD_ksws, (*inparm).device, UG_REQUEST, UG_ECHO);

/*	printf("pickrec.status = %d\n",(*pickrec).status);*/
/*
.....Sometimes when using the selection filter
.....the status will come back with UG_OK when DONE is entered
.....but there will be nothing on the que, so added the 'ecnt' check
.....Bobby  -  2/22/99
*/
			ecnt = gqsize();
			if((*pickrec).status == UG_OK && ecnt > 0)
			{
				answer = gawaitevent((UU_REAL) 0.);
				ecnt = gqsize();
/*	printf("gqsize = %d\n",ecnt);*/
				locstat = UU_TRUE;

/*			-- move the pick data into the event record -- */

				(*event).indata.pickdata.status = (*pickrec).status;
				(*event).indata.pickdata.depth = (*pickrec).depth;
				(*event).indata.pickdata.pickpath = (*pickrec).pickpath;

/*			-- get the locator event off the event Q -- */

/*
.....The only time that I know of the q-size being 2
.....is when a SELECT menu was picked and we are
.....returning from it.  Because the menus are handled
.....thru callbacks instead of this routine, things act
.....a bit differently.  The main problem is that when the
.....SELECT menu is chosen, we do not exit this routine, but
.....we do re-enter it from the SELECT system.
.....Bobby  -  2/8/96
*/
				if(ecnt == 1 || ecnt == 2)
				{
					if (ecnt == 2)
					{
						answer = gawaitevent((UU_REAL) 0.);
						ecnt = gqsize();
						donefl = 1;
/*						printf("ecnt after 1st getloc = %d\n",ecnt);*/
					}
					elocrec = ggetloc();

					(*event).indata.pickdata.transform  = (*elocrec).transform ;
					(*event).indata.pickdata.position[0] = (*elocrec).position.x;
					(*event).indata.pickdata.position[1] = (*elocrec).position.y;

/*				-- save the new crosshair location -- */

					UD_nxhair.transform = (*elocrec).transform;
					UD_nxhair.position.x = (*elocrec).position.x;
					UD_nxhair.position.y = (*elocrec).position.y;
					givref3((*elocrec).transform, cord);
					gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
					UD_nxhair.position.z = z;
					UD_GLOC3_NDC2W(&UD_wxhair, &UD_nxhair);

					ecnt--;
					if (ecnt < 0) ecnt = 0;
				}
			
/*			-- get the choice event off the event Q -- */

				if(ecnt == 0)
				{
					answer = gawaitevent((UU_REAL) 0.);

/*					-- set the device number to the device of the key that
						triggered it -- */

					(*event).evdev = (*answer).dev;
					(*event).indata.pickdata.choice = ggetchoice();
/*
.....Don't let user pick anything after
.....returning from SELECT menu
.....Force response to DONE or REJECT
*/
					if (donefl == 1)
					{
						(*event).evdev = (*answer).dev;
/*
.....Reentrant call to SELECT menu has been added
.....So user can still pick entities after SELECTion
.....Bobby  -  9/26/97
*/
/*						if ((*event).indata.pickdata.choice != 3)
							(*event).indata.pickdata.choice = 2;*/
					}
				}
			}
/*	printf("choice = %d\n",(*event).indata.pickdata.choice);*/
			break;

		case UD_STRING:

			if((*inparm).pet !=22)
			{
				instring.prompt = UD_strpmt;
				instring.bufsiz = (*inparm).strbfsz;
				instring.position = 0 ;
				if(UD_Eventm == UU_TRUE)
					gsstringmode(UD_ksws, (*inparm).device, UG_REQUEST, UG_ECHO);
				ginitstring(UD_ksws, (*inparm).device, (*inparm).defstr, 
										(*inparm).pet, &halfrect, &instring);
			}
			else
			{
				/* set the input prompt and buffer length */
				gchgstringinit(UD_ksws, (*inparm).device, (*inparm).defstr, 
												(*inparm).strbfsz);

/*				-- write out the prompt message -- */

				gputstring(UD_ksws, (*inparm).device, (*inparm).prompt);
			}
			stringrec = greqstring(UD_ksws, (*inparm).device);

/*			-- sample the string in case a partial string was input before we
				re-initialize it --  */

			if((*stringrec).status != UG_OK)
				strcpy(parstr, gsamplestring(UD_ksws, (*inparm).device));

			if((*inparm).pet != 22)
			{
				if(UD_Eventm == UU_TRUE)
				{
					instring.prompt = UD_strpmt;
					ginitstring(UD_ksws, (*inparm).device, "", (*inparm).pet,
												&halfrect, &instring);
					gsstringmode(UD_ksws, (*inparm).device, UG_EVENT, UG_ECHO);
				}
			}

			if((*stringrec).status == UG_OK)
			{
				locstat = UU_TRUE;

/*			-- move the string data into the event record -- */

				(*event).indata.stringdata = (*stringrec).string;

/*				-- set the termination status -- */

				(*inparm).termcon = UD_CRTERM;
			}
			else
			{

/*				-- set the termination status -- */

				(*inparm).termcon = UD_PARTERM;

/* 			-- worry about partial strings -- */

				if(parstr[0] != '\0')
				{
					locstat = UU_TRUE;
					i = strlen(parstr);
					parstr[i] = PARSTRCHAR;
					parstr[i+1] = '\0';

/*				-- move the string data into the event record -- */

					(*event).indata.stringdata = parstr;
				}
			}
			break;

		case UD_CHOICE:

			inchoice.strings = (char **) (*inparm).prompt;
			inchoice.number = (*inparm).number;

			if(UD_Eventm == UU_TRUE && (*inparm).device < UD_start_menu_num)
				gschoicemode(UD_ksws, (*inparm).device, UG_REQUEST, UG_ECHO);

/*			-- compute the extrema window for menu -- */

			if((*inparm).pet < 30)
			{
				areapt = &(UD_duimsdeflt.screen[UD_curlayout.curr_screen].
							areas[UD_MENU])[0];
				inchoice.bkcolor= areapt->color;
				inchoice.bordcolor=areapt->bordercolor;
				inchoice.txcolor=areapt->contcolor;
			}
			else
			{

/*					-- get the current crosshair location -- */

				x = UD_nxhair.position.x;
				y = UD_nxhair.position.y;

				uu_dprint(UU_DTRC,(us, " in popup, x,y = %g %g", x, y));

/*					-- first make sure crosshair is in the graphic area -- */

				if(x <= UD_CURGRAF_AREA.ll.x)
					x = UD_CURGRAF_AREA.ll.x + .001;
				else if(x >= UD_CURGRAF_AREA.ur.x)
					x = UD_CURGRAF_AREA.ur.x - .001;
				if(y <= UD_CURGRAF_AREA.ll.y)
					y = UD_CURGRAF_AREA.ll.y + .001;
				else if(y >= UD_CURGRAF_AREA.ur.y)
					y = UD_CURGRAF_AREA.ur.y - .001;

/*				-- compute menu window dx and dy -- */

				popdx = (*(UD_POPUPREC *)(*inparm).prompt).menusz.x;
				popdy = (*(UD_POPUPREC *)(*inparm).prompt).menusz.y;

				uu_dprint(UU_DTRC,(us, " in popup 1, x,y = %g %g dx,dy=%g %g",
						x, y, popdx, popdy));

				uu_dprint(UU_DTRC,(us, " in popup 1.5, graf=%g %g %g %g", 
					UD_CURGRAF_AREA.ll.x, UD_CURGRAF_AREA.ll.y,
					UD_CURGRAF_AREA.ur.x, UD_CURGRAF_AREA.ur.y));

				if((x + popdx) > UD_CURGRAF_AREA.ur.x)
				{
					poploc.posn.ll.x = x - popdx;
					poploc.posn.ur.x = x;
				}
				else
				{
					poploc.posn.ll.x = x;
					poploc.posn.ur.x = x + popdx;
				}

				if((y - popdy) < UD_CURGRAF_AREA.ll.y)
				{
					poploc.posn.ll.y = y;
					poploc.posn.ur.y = y + popdy;
				}
				else
				{
					poploc.posn.ll.y = y - popdy;
					poploc.posn.ur.y = y;
				}

				areapt = &poploc;

				uu_dprint(UU_DTRC,(us, " in popup 2, rect = %g %g %g %g", 
						poploc.posn.ll.x, poploc.posn.ll.y, 
						poploc.posn.ur.x, poploc.posn.ur.y));

				inchoice.bordcolor = 1;
				inchoice.txcolor = 1;
				inchoice.bkcolor = 0;
			}


/* 			-- we want the position of the choice device to be specified in NDC.
					However, ginitchoice takes DC. Therefore we must run area 
					position thru the workstation xform. */

			ud_devrect(&(*areapt).posn, &pos);

/*			-- only initialize device UD_start_menu_num. Higher number 
			choice devices are assumed to have been gang initialized for 
			popup menus -- */

			if((*inparm).device == UD_start_menu_num)
			{
				ginitchoice(UD_ksws, (*inparm).device, 0, (*inparm).pet,
											&pos, &inchoice);
			}
			else if((*inparm).pet > 30)
			{
				gchgchoicearea(UD_ksws, (*inparm).device, &pos);
				(*inparm).pet = (*inparm).pet - 30;
			}

			choicerec = greqchoice(UD_ksws, (*inparm).device);

			if(UD_Eventm == UU_TRUE && (*inparm).device < UD_start_menu_num)
				gschoicemode(UD_ksws, (*inparm).device, UG_EVENT, UG_ECHO);

			if((*choicerec).status == UG_OK)
			{
				locstat = UU_TRUE;

/*			-- move the choice data into the event record -- */

				(*event).indata.choicedata = (*choicerec).choice;
				answer = gawaitevent((UU_REAL) 0.);

				ecnt = gqsize();
				if(answer != NULL)
				{

/*					-- the only way this case can happen is when the choice was
						made using a puck or mouse.  Check which key was used to make
						the choice. If key 1 was used, then it was a valid choice
						from the menu. If any other button was used then the second
						choice event takes precedence. -- */

					if((*answer).dev == 3)
					{
						i = ggetchoice();
						if(i != 1)
						{
							(*event).evdev = 3;
							(*event).indata.choicedata = i;
							ecnt--;
						}
					}

/*					-- flush any other events hanging around -- */

					while(ecnt > 0)
						answer = gawaitevent((UU_REAL) 0.);
				}
			}
			break;
		
		case UD_STROKE:

			UD_stkinit.prompt = UD_stkpmt;
			ginitstroke(UD_ksws, (*inparm).device, &UD_stkrec,
										(*inparm).pet, &halfrect,&UD_stkinit);
			qstr=greqstroke(UD_ksws, (*inparm).device);
			(*event).indata.strokedata.transform = (*qstr).stroke.transform;
			(*event).indata.strokedata.n_points = (*qstr).stroke.n_points;
			(*event).indata.strokedata.points = (Gnpoint *)(*qstr).stroke.points;

/*			-- get the trigger character -- */

			answer = gawaitevent((UU_REAL) 0.);
			(*event).indata.strokedata.stktrig = ggetchoice();

/*			-- flush any other events hanging around -- */

			ecnt = gqsize();
			while(ecnt > 0)
				answer = gawaitevent((UU_REAL) 0.);

			locstat = UU_TRUE;
			break;

		default:

/*---			derror(" invalid event type to initialize");		---*/

			uu_uerror0(UD_DASHEP, 28);
			break;

	}

/*	-- erase prompt line -- */

	if(((*inparm).evtype!=UD_CHOICE) 
						&& (((*inparm).evtype!=UD_STRING) 
						|| ((*inparm).pet!=22)))
		ud_killmsg(UD_PROMPTMSG);

	 if(locstat == UU_FALSE)
		(*event).evclass = UD_NONE;

#if UU_DEBUG==UU_TRUE
	if(UU_debmask & UU_DTRC)
		ud_prevent("ud_revt", event); 
#endif

	uu_dexit;
/*
	printf("\nevclass = %d\n",event->evclass);
	printf("\nevdev = %d\n",event->evdev);
	printf("Choicedata = %d\n",event->indata.choicedata);
	printf("pickdata.status = %d\n",event->indata.pickdata.status);
	printf("pickdata.depth = %d\n",event->indata.pickdata.depth);
	printf("pickdata.path = %d\n",event->indata.pickdata.pickpath);
	printf("pickdata.xform = %d\n",event->indata.pickdata.transform);
	printf("pickdata.position = %g %g\n",event->indata.pickdata.position[0],
		event->indata.pickdata.position[1]);
	printf("pickdata.choice = %d\n",event->indata.pickdata.choice);
	printf("locstat = %d\n",locstat);
*/
	return(locstat);
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_eevt(event, oldans, inparm)
**		  input an event in event mode
**
**  PARAMETERS   
**      INPUT  : 
**				 inparm = input parameter block
**      OUTPUT :  
**           event = event buffer
**           oldans = old event pointer from gawaitevent
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

int ud_eevt(event, oldans, inparm)
UD_DEVENT *event;						/* event buffer */
Gevent *oldans;						/* report address */
UD_EVENTDEF *inparm; 				/* input parameter block */
{

	int i;
	int ecnt;							/* number of events this interaction called */

	Gevent *gawaitevent();					/* wait for event */
	Gevent *answer;							/* pointer to input record */

	Gloc *ggetloc();
	Gloc *in1;
	Gloc *gsampleloc();

	Gstroke *ggetstroke();
/*	Gstroke *in2;*/

	Gfloat ggetval();

	Gint ggetchoice();

	Gpicks *ggetpick();
	Gpicks *in5;

	Gchar *ggetstring();

	UU_REAL x, y, z, cord[3];

	uu_denter(UU_DTRC,(us, " entering ud_eevt, event=%d, answer=%d",
					event, oldans));

	answer = oldans;

/*	-- get the number of events this interaction caused --  */

	if(answer != NULL)
	{
		(*event).evdev = (*answer).dev;
		ecnt = gqsize();

		switch ((*answer).class)
		{
			case UG_E_NONE:

				(*event).evclass = UD_NONE;
				break;

			case UG_E_LOCATOR:

				(*event).evclass = UD_LOCATOR;

/*					-- get the locator event -- */

				in1 = ggetloc();
				(*event).indata.locdata.transform = (*in1).transform ;
				(*event).indata.locdata.position[0] = (*in1).position.x ;
				(*event).indata.locdata.position[1] = (*in1).position.y ;

/*				-- save the new crosshair location -- */

				UD_nxhair.transform = (*in1).transform;
				UD_nxhair.position.x = (*in1).position.x;
				UD_nxhair.position.y = (*in1).position.y;
				givref3(UD_nxhair.transform, cord);
				gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
				UD_nxhair.position.z = z;
				UD_GLOC3_NDC2W(&UD_wxhair, &UD_nxhair);


/*					-- get the choice event -- */

				if(ecnt == 1)
				{
					answer = gawaitevent((UU_REAL) 0.);

/*				-- set the device number to the device of the key that
					triggered it -- */

					(*event).evdev = (*answer).dev;
					(*event).indata.locdata.choice = ggetchoice();
				}
				break;

			case UG_E_STROKE:

				(*event).evclass = UD_STROKE;
/*---------------------------------------------------------------
				in2 = ggetstroke();
				(*event).indata.strokedata.transform = (*in2).transform ;
				(*event).indata.strokedata.n_points = (*in2).n_points ;
				(*event).indata.strokedata.points = (*in2).points ;
---------------------------------------------------------------*/
				break;

			case UG_E_VALUATOR:

				(*event).evclass = UD_VALUATOR;
				(*event).indata.valdata = ggetval();
				break;

			case UG_E_CHOICE:

				(*event).evclass = UD_CHOICE;
				(*event).indata.choicedata = ggetchoice();
				answer = gawaitevent((UU_REAL) 0.);

				ecnt = gqsize();
				if(answer != NULL)
				{

/*					-- the only way this case can happen is when the choice was
						made using a puck or mouse.  Check which key was used to make
						the choice. If key 1 was used, then it was a valid choice
						from the menu. If any other button was used then the second
						choice event takes precedence. -- */

					if((*answer).dev == 3)
					{
						i = ggetchoice();
						if(i != 1)
						{
							(*event).evdev = 3;
							(*event).indata.choicedata = i;
							ecnt--;
						}
					}

/*					-- flush any other events hanging around -- */

					while(ecnt > 0)
						answer = gawaitevent((UU_REAL) 0.);
				}
				break;

			case UG_E_PICK:

				(*event).evclass = UD_PICK;

/*					-- get the pick event --  */

				in5 = ggetpick();

				if ((*in5).status == UG_OKPICK)
					(*event).indata.pickdata.status = UG_OK ;
				else
					(*event).indata.pickdata.status = UG_NONE ;
				(*event).indata.pickdata.depth = (*in5).depth ;
				(*event).indata.pickdata.pickpath = (*in5).pickpath ;

/*					-- get the locator event -- */

				if(ecnt > 0)
				{
/*					-- if the locator was requested and a pick event was input,
						then no locator event will be on the Q -- */

					if(ecnt == 2)
					{
						answer = gawaitevent((UU_REAL) 0.);
						in1 = ggetloc();
						ecnt = ecnt - 1;
					}
					else
						in1 = gsampleloc(UD_ksws, (*inparm).device);

					(*event).indata.pickdata.transform = (*in1).transform ;
					(*event).indata.pickdata.position[0] = (*in1).position.x ;
					(*event).indata.pickdata.position[1] = (*in1).position.y ;

/*					-- save the new crosshair location -- */

					UD_nxhair.transform = (*in1).transform;
					UD_nxhair.position.x = (*in1).position.x;
					UD_nxhair.position.y = (*in1).position.y;
					givref3(UD_nxhair.transform, cord);
					gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
					UD_nxhair.position.z = z;
					UD_GLOC3_NDC2W(&UD_wxhair, &UD_nxhair);
				}

/*					-- get the choice event -- */

				if(ecnt ==  1)
				{
					answer = gawaitevent((UU_REAL) 0.);

/*				-- set the device number to the device of the key that
					triggered it -- */

					(*event).evdev = (*answer).dev;
					(*event).indata.pickdata.choice = ggetchoice();
				}
				break;

			case UG_E_STRING:

				(*event).evclass = UD_STRING;
				(*event).indata.stringdata = ggetstring();
				break;
		}
	}

#if UU_DEBUG==UU_TRUE
	if(UU_debmask & UU_DTRC)
		ud_prevent("ud_eevt", event);
#endif

uu_dexit;
	return (UU_SUCCESS);
}
