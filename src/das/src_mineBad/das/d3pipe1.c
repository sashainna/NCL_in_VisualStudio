/*********************************************************************
**
**    NAME         :  d3pipe1.c
**
**       CONTAINS:
**  			ud_gevt1
**  			ud_strcomp
**  			ud_subsystem
**  			ud_atgevt
**  			ud_gevt
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**			d3pipe1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:05:06
**
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "zsysdep.h"
#include "dinput.h"
#include "dmark.h"
#include "dasnog.h"
#include "dasg.h"
#include "derror.h"
#include "dpipe.h"
#include "dmenucom.h"
#include "dsubcom.h"
#include "driver.h"
#include "gloc3.h"
#include "uhep.h"
#include "udebug.h"
#include "mfort.h"
#include "nclfc.h"
#include "dselect.h"
#include "mfeatcom.h"
#include "vsegbf.h"

#ifdef STRING
#undef STRING
#endif

#define SUBCHAR '\\'
#define SAFCHAR '!'

#define RECON 		"recon"			/* record on */
#define PLAYB 		"playb"			/* playback */
#define RECOFF 	"recoff"			/* record off */
#define PROMPTER 	"prompt"			/* prompt */
#define ECHO 		"echo"			/* echo control */
#define RPSUSPEND	"susp"			/* suspend record or playback */
#define RPRESUME	"resu"			/* resume record or playback */
#define DELAY		"delay"			/* delay playback n seconds */
#define MOUSEL		"mouse_left"
#define MOUSEM		"mouse_middle"
#define MOUSER		"mouse_right"
#define WHEEL_DOWN		"wheel_down"
#define WHEEL_UP		"wheel_up"

#define RETROOT     '0'				/* go to root of application menu */
#define RLASTCHAR   '1'				/* reject last interaction */
#define DONECHAR    '2'				/* done with input sequence */
#define RCOMCHAR    '3'				/* reject command and return to parser */
#define RESTARTCHAR '4'				/* restart input sequence */
#define DCFCHAR     '6'				/* use default value */
#define REJCHAR     '7'				/* backspace one character (for calculator) */
#define ALTACT      '8'				/* application alternate action */
#define ALTACT1     '9'				/* subsystem alternate action */
#define PICK		  'A'				/* pick event */
#define LOCATOR	  'B'				/* locator event */
#define VALUATOR	  'C'				/* valuator event */
#define STRING		  'D'				/* string event */
#define CHOICE		  'E'				/* choice event */
#define PANIC    	  'F'				/* panic stop */
#define UPFIELD  	  'G'				/* up field */
#define DNFIELD  	  'H'				/* down field */
#define CLFIELD  	  'I'				/* clear field */
#define TGFIELD  	  'J'				/* toggle field */
#define EXITSUB  	  'K'				/* exit subsystem */

#define EXECUTE 	UU_TRUE			/*	execute option for tablet handler */
#define NOEXECUTE UU_FALSE			/* no execute option */
#define SEMACTDEF 0					/* semantic action picked */
#define STRINGDEF 1					/* string picked */
#define UNDEF     2					/* undefined button */
#define NOSUB     3					/* substitution inappropriate on this device */
#define TABLET    4					/* tablet device number */

/* MILLS- To set up RETURN like the use default key. */
extern UU_LOGICAL dflag;
extern int UD_form_bypick;
int UD_current_event = UD_CHOICE;
extern UU_LOGICAL NCL_init_limit;
extern int UZ_key_pickloc;
static UD_DEVENT S_cevent;

void ud_reset_current_event();
extern int LW_nclipv;
/********************************************************************* 
**
**  E_FUNCTION	: ud_gevt1(event, evtype, prompt, number, device, echo)
**      description : Obtains an event from the current source.
**
**						A second function of this routine is to monitor the input
**						stream looking for the tokens to invoke the different
**						subsystems and special features of the UNICAD system.
**
**						The third function of this routine is to handle DAS
**						control functions.
**
**  PARAMETERS   
**      INPUT: 
**					evtype = type of event desired, one of: UD_NONE, UD_LOCATOR,
**							UD_STROKE, UD_VALUATOR, UD_CHOICE, UD_PICK, UD_STRING, 
**							UD_VECTOR
**					devno = device number from which input is desired.
**				   prompt = prompt or array of prompts (if CHOICE request)
**					number = number of prompts
**					device = device number from which input is requested
**					echo = prompt and echotype
**					inparm = input parameter block (catchall for other data)
**
**      OUTPUT: 
**					event = structure containing the event, defined as:
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

void ud_gevt1(event, evtype, prompt, number, device, echo, inparm)
UD_DEVENT *event;		/* event buffer */
int evtype;				/* input type requested */
char *prompt;			/* prompts */
int number;				/* number of prompts */
int device;				/* device number from which input is requested */
int echo;				/* echotype */
UD_EVENTDEF *inparm; /* input parameter block */
{

	char *cptr, ichar;				/* various character buffers and pointers */
	char buffer[100]; 
	UD_DEVENT event1;					/* temporary event buffer */
	int i,isav;								/* temporary integer */
	int markval;						/* mark system cell */
	int savesys;						/* save subsystem number */
	char *prmptr, **prmptrptr;		/* prompt pointer and prompt pointer pointer */
	int cycleptr;						/* save puck cycle state */
	int saveint, savedev, savepet;	/* save interaction, device, pet */
	int ddevice, dpet, dtype;
	UD_EVENTDEF *parm_ptr;			/* local pointer */
	UD_EVENTDEF eventbuf;			/* local pointer */
	Gloc3 UD_wxhair_save;			/* save crosshair location */
	Gloc3 UD_nxhair_save;			/* save crosshair location */
	uv_segbuff(udata);
	char jptr[256];
	UU_REAL cord[3];
	char strbuff[100];

    UM_int2 ifl, mode, mode1;
	int type, status;
	UD_DPICKS epick;
	int save_seg, das_exe;
	int mark, func_mouse,sav_pickloc;

	static char *typemess[8] =  {"",
       								 		"LOCATOR",
								 		"STROKE",
								 		"VALUATOR",
								 		"CHOICE",
								 		"PICK",
								 		"STRING",
								 		"VECTOR"};

	uu_denter(UU_DTRC,(us, "entering ud_gevt1, evtype=%d, device=%d, event=%x",
		evtype, device,event));

	UD_MARK(mark,UU_FALSE);
	if (mark != 0)
		goto done;

	func_mouse = 0;
	if(UD_firsttime == UU_TRUE)
	{
		ud_playinit();
		UD_firsttime = UU_FALSE;
	}

/*	-- set up prompt pointer and prompt pointer pointer -- */

	if(evtype == UD_CHOICE)
	{
		prmptr = "";
		prmptrptr = (char **) prompt;
	}
	else
	{
		prmptr = prompt;
		prmptrptr = &prompt;
	}

/*	-- supply a character string block if none supplied -- */

	if(inparm == NULL)
	{
		parm_ptr = &eventbuf;
		eventbuf.strbfsz = 80;
		eventbuf.defstr = "";
	}
	else
		parm_ptr = inparm;

	ud_qstr(&dtype, &ddevice, &dpet);
	(*parm_ptr).evtype = evtype;
	(*parm_ptr).number = number;
	(*parm_ptr).device = device;
	(*parm_ptr).pet = echo;

/*	-- set up initial rubber band location in world coordinates -- */

	(*parm_ptr).rubloc.transform = UD_wxhair.transform;
	(*parm_ptr).rubloc.position.x = UD_wxhair.position.x;
	(*parm_ptr).rubloc.position.y = UD_wxhair.position.y;
	(*parm_ptr).rubloc.position.z = UD_wxhair.position.z;

/*	-- initlialize the cyclic menus -- */

	ud_insubm(device, evtype);
/*
.....Enable re-entry when a calling routine
.....disables key substitution
.....Bobby  -  9/26/97
*/
again:
	das_exe = 0;
	func_mouse = 0;
	isav = UD_enableim;
	UD_enableim = UU_TRUE;
/* -- get the next substituted event -- */
/*
.....first check if there are picking stack, if yes, get that 
.....and fill into the event and return
*/
	if (((*parm_ptr).evtype == UD_PICK)&&(ud_getn_pick_segs()>0))
	{
		status = ud_get_pickseg(&epick, 1);
		if (status==0)
		{
			(*event).evclass = UD_PICK;
			(*event).evdev = 1;
			(*event).indata.pickdata.transform = epick.transform;
			(*event).indata.pickdata.position[0] = epick.position[0];
			(*event).indata.pickdata.position[1] = epick.position[1];
			(*event).indata.pickdata.status = UG_OK;
			(*event).indata.pickdata.depth = epick.depth;
			(*event).indata.pickdata.pickpath = epick.pickpath;
			(*event).indata.pickdata.choice = 1;
			if (UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
				ud_rpwr(event);
			ud_filter_entity(event);
			goto done;
		}
	}
	UD_pickmode = 1;
	if((*parm_ptr).evtype == UD_CHOICE)
	{
		(*parm_ptr).prompt = (char *) prmptrptr;
		ud_sevt(event, UU_FALSE, parm_ptr);
	}
	else
	{
		(*parm_ptr).prompt = prmptr;
		ud_sevt(event, UU_FALSE, parm_ptr);
	}
	UD_enableim = isav;

restart:
	if(UD_enableim)
	{
		switch ((*event).evclass)
		{
			case UD_NONE:
				break;

			case UD_LOCATOR:
				break;

			case UD_STROKE:
				break;

			case UD_VALUATOR:
				break;

			case UD_CHOICE:
				break;

			case UD_PICK:
				
				if (ud_motion_pick_type()) break;
/*				-- if a pick event has made it this far, then make sure a valid 
					entity has been picked -- */

				if((*event).indata.pickdata.depth == 0)
				{
					if ((func_mouse==1)&&(das_exe==0))
					{
						goto again;
					}
/*
....................Character string holding type of currently limited geometry.
....................Set through the GEOMETRY LIMIT pop-up menu.
....................Issue error message when geo picked is not of type defined by
....................local user limits.
*/
					if ((UD_LIMIT.llsel) && (strlen(UD_LIMIT.llsel_name) > 0))
						{
						uu_uerror1(UA_NCL, 17, UD_LIMIT.llsel_name);
						}

/*				-- "no entity picked, renter" -- */
					else
					{
/*
......change to not display a error window
.....Yurong 5/13/98
*/
/*						uu_uerror0(UD_DASHEP, 2); */
						ud_prmerr("Attempted pick was unsuccessful.  Retry.");
						ug_beep();
					}
					goto again;
				}
				ichar = (*event).indata.pickdata.choice;
				if(ichar != ' ' )
				{

/*				-- illegal character used to pick -- */

					uu_uerror0(UD_DASHEP, 67);
					goto again;
				}
				break;

			case UD_STRING:

				/* to implement return key for use default. kathy */
				if ((*event).indata.stringdata[0] == '\0' && dflag == UU_TRUE)
					{
					/*strcpy((*event).indata.stringdata, SUBCHAR);*/
					(*event).indata.stringdata[0] = SUBCHAR;
					(*event).indata.stringdata[1] = SUBCHAR;
					(*event).indata.stringdata[2] = DCFCHAR;
					}

				cptr = (*event).indata.stringdata;

				if(cptr[0]==SUBCHAR && cptr[1]!='\0')
				{

/*					-- test for DAS control -- */

					if(cptr[1] == SUBCHAR && cptr[2] != '\0')
					{
						ichar = cptr[2];
						(*event).evclass = UD_CHOICE;
						(*event).evdev = UD_AUXMENU;

						if(ichar == DONECHAR)
						{
							(*event).indata.choicedata = UD_CDONE;
							ud_rpwrcom("DONE");
						}
						else if(ichar == RLASTCHAR)
						{
							(*event).indata.choicedata = UD_CRLAST;
							ud_rpwrcom("REJECT LAST");
						}
						else if(ichar == RCOMCHAR)
						{
							ud_rpwrcom("COMMAND REJECT");

/*							-- if jump flag enabled, return to parser, do not
								stop playback -- */
/*							ud_reject(0, 0);  */
							ud_reject(UD_Selrej_ptr, UD_Selrej_cnt);
							ud_jump(-1, UU_FALSE);
							(*event).indata.choicedata = UD_CRCOMMAND;
						}
						else if(ichar == RESTARTCHAR)
						{
							(*event).indata.choicedata = UD_CRESTART;
							ud_rpwrcom("RESTART");
						}
						else if(ichar == DCFCHAR)
						{
							(*event).indata.choicedata = UD_CDEFAULT;
							ud_rpwrcom("USE DEFAULT");
						}
						else if(ichar == REJCHAR)
						{
							(*event).indata.choicedata = UD_CREJCHAR;
							ud_rpwrcom("REJECT CHARACTER");
						}
						else if(ichar == ALTACT)
						{
							(*event).indata.choicedata = UD_CALTACT;
							ud_rpwrcom("DAS ALTERNATE ACTION");
						}
						else if(ichar == ALTACT1)
						{
							(*event).indata.choicedata = UD_CALTACT1;
							ud_rpwrcom("APPLICATION ALTERNATE ACTION");
						}
						else if(ichar == RETROOT)
						{
							ud_rpwrcom("JUMP TO ROOT");

/*							-- if jump flag enabled, return to root, do not
								stop playback -- */

/*
.....Added for NCL501+ mode
.....03/05/92.  Paul
*/
                                                     ifl=35;
                                                     getifl(&ifl,&mode);
                                                     ifl=350;
                                                     getifl(&ifl,&mode1);
                                                     if (mode == 0 && mode1 == 2)
                                                        {}
                                                     else
                                                        {
							UD_dastkfl = UU_FALSE;		/* force init of DAS stack */
							uw_mfmenu_reset(UU_TRUE,UU_TRUE,UU_FALSE);
							ud_jump(UD_markptr, UU_FALSE);
                                                         }
							(*event).indata.choicedata = UD_CRETROOT;
						}
						else if(ichar == PANIC)
						{
							ud_rpwrcom("PANIC STOP");

/*							-- if jump flag enabled, return to root, stop playback -- */

							UD_dastkfl = UU_FALSE;		/* force init of DAS stack */
							ud_jump(UD_markptr, UU_TRUE);
							(*event).indata.choicedata = UD_CPANIC;
						}
						else if(ichar == PICK)
						{

/*							-- set pick event as requested type -- */

							(*parm_ptr).evtype = UD_PICK;
							(*parm_ptr).device = 1;
							(*parm_ptr).pet = 1;

							ud_rpwrcom("PICK MODE");
							goto again;
						}
						else if(ichar == LOCATOR)
						{

/*							-- set locator event as requested type -- */

							(*parm_ptr).evtype = UD_LOCATOR;
							if(evtype == UD_LOCATOR)
							{
								(*parm_ptr).device = device;
								(*parm_ptr).pet = echo;
							}
							else
							{
								(*parm_ptr).device = 1;
								(*parm_ptr).pet = 1;
							}
							ud_rpwrcom("LOCATOR MODE");
							goto again;
						}
						else if(ichar == VALUATOR)
						{

/*							-- set valuator event as requested type -- */

							(*parm_ptr).evtype = UD_VALUATOR;
							(*parm_ptr).device = 1;
							(*parm_ptr).pet = 1;
							ud_rpwrcom("VALUATOR MODE");
							goto again;
						}
						else if(ichar == STRING)
						{

/*							-- set string event as requested type -- */
/*
.....if this event happened from form pick 
.....do nothing and set UD_form_bypick = 0
.....Yurong 12/5/00
*/
							if (UD_form_bypick==1)
								UD_form_bypick = 0;
							else
							{
								(*parm_ptr).evtype = UD_STRING;
								(*parm_ptr).device = ddevice;
								(*parm_ptr).pet = dpet;
								ud_rpwrcom("STRING MODE");
								goto again;
							}
						}
						else if(ichar == CHOICE)
						{

/*							-- set choice event as requested type -- */

							if(evtype != UD_CHOICE)
							{
								(*parm_ptr).evtype = UD_CHOICE;
								(*parm_ptr).device = 1;
								(*parm_ptr).pet = 1;
							}
							else
							{
								(*parm_ptr).evtype = evtype;
								(*parm_ptr).device = device;
								(*parm_ptr).pet = echo;
							}
							ud_rpwrcom("CHOICE MODE");
							goto again;
						}
						else if(ichar == UPFIELD)
						{
							(*event).indata.choicedata = UD_CUPFIELD;
							ud_rpwrcom("UP FIELD");
						}
						else if(ichar == DNFIELD)
						{
							(*event).indata.choicedata = UD_CDNFIELD;
							ud_rpwrcom("DOWN FIELD");
						}
						else if(ichar == CLFIELD)
						{
							(*event).indata.choicedata = UD_CCLFIELD;
							ud_rpwrcom("CLEAR FIELD");
						}
						else if(ichar == TGFIELD)
						{
							(*event).indata.choicedata = UD_CTGFIELD;
							ud_rpwrcom("TOGGLE FIELD");
						}
						else if(ichar == EXITSUB)
						{
							(*event).indata.choicedata = UD_CEXITSUB;
							ud_rpwrcom("EXIT SUBSYSTEM");
						}
						else
						{

/* --						undefined control action encountered	-- */

							uu_uerror0(UD_DASHEP, 12);
							goto again;
						}
					}
					else if(ud_strcomp(&cptr[1], PROMPTER) < 0)
					{

/*							-- if prompt in order, turn off playback flag and get
							next input from the operator -- */

						if (UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
						{

/*							-- get the prompt line and use the error message
								mechanism to output it -- */

							ud_rprd(&event1, UU_FALSE);
							if(event1.indata.stringdata[0] != '\0')
								ud_prmerr(event1.indata.stringdata);

/*							-- get the next event from the operator -- */

							ud_sevt(event, UU_TRUE, parm_ptr);

/*							-- flush the next event in the file -- */

							ud_rprd(&event1, UU_TRUE);

							goto restart;

						}
						else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
						{
							UD_MARK(markval, UU_TRUE);
							if(markval == 0)
							{

/*								-- reset the string pet if a form is up -- */

								ud_qstr(&saveint, &savedev, &savepet);
								ud_dtstr(UD_STRING, 1, 1);

/*								-- get the prompt line -- */

								strcpy(buffer, "");

/* 							--	prompt line -- */

								ud_ldas(UD_DASSTRING, UD_DASHEP, 3, buffer, 100,
													&i, UD_DEFAULT);

/*								-- reset the string pet to what it was -- */

								ud_dtstr(saveint, savedev, savepet);
							}
							UD_UNMARK(markval);
							goto again;
						}
						goto again;
					}
					else if ((ud_strcomp(&cptr[1], MOUSEL)<0)
						|| (ud_strcomp(&cptr[1], MOUSEM)<0)
						|| (ud_strcomp(&cptr[1], MOUSER)<0)
						|| (ud_strcomp(&cptr[1], WHEEL_DOWN)<0)
						|| (ud_strcomp(&cptr[1], WHEEL_UP)<0))
					{
								
						savesys = UD_syssys;
						cycleptr = UD_cycle2ptr;
						zbytecp(UD_wxhair_save, UD_wxhair);
						zbytecp(UD_nxhair_save, UD_nxhair);

						ud_suspwin();
						ud_lpsh(UU_FALSE);

						UD_MARK(markval, UU_TRUE);
						if(markval == 0)
						{
							if((*parm_ptr).evtype == UD_CHOICE)
								type = 0;
							else if ((*parm_ptr).evtype == UD_PICK)
							{
								type = 2;
							}
							else if ((*parm_ptr).evtype == UD_LOCATOR)
							{
								type = 1;
							}
							else if ((*parm_ptr).evtype == UD_STRING)
								type = 3;
							uw_glput_pikgeom2(type, event);
							save_seg = ud_getn_pick_segs();
							das_exe = 0;
							sav_pickloc = UZ_key_pickloc;
							status = uz_mouse_functions(&cptr[1], type);
							if (status==1)
								das_exe = 1;
							func_mouse++;
/*
.....if pick stack is used, reset the stack,
.....otherwise, do nothing
*/
							if (save_seg!=ud_getn_pick_segs())
								ncl_reset_pikgeom();
						}
/*								-- restore old state -- */
						ud_lpop();
						zbytecp(UD_wxhair, UD_wxhair_save);
						zbytecp(UD_nxhair, UD_nxhair_save);
						ud_resuwin();
						UD_cycle2ptr = cycleptr;
						UD_UNMARK(markval);
/*
.....if this is in the input mode, evtype=UD_LOCATOR or evtype=UD_PICK
.....continue the older function
*/
						if (((*parm_ptr).evtype == UD_LOCATOR)||((*parm_ptr).evtype == UD_PICK))
						{
							if (S_cevent.evclass!=-1)
							{
								(*event).evclass = S_cevent.evclass;
								(*event).evdev = S_cevent.evdev;
								(*event).indata = S_cevent.indata;
							}
							if ((*parm_ptr).evtype == UD_PICK)
							{
								if((*event).indata.pickdata.depth > 0)
								{
									ud_filter_entity(event);
									if((*event).evclass == UD_PICK &&
										!ud_isassist_seg((*event).indata.pickdata.pickpath[0]))
									{
										gsegrud((*event).indata.pickdata.pickpath[0], udata);
										if(uv_getrelnum(udata) == UM_FEAT_REL)
										{
											if(um_feasub((*event).indata.pickdata.pickpath[(*event).indata.pickdata.depth-1], &type, cord) == UU_TRUE)

											if(type == UM_FREAL)
											{
												(*event).evclass = UD_VALUATOR;
												(*event).indata.valdata = cord[0];
											}
											else if(type == UM_FCOORD)
											{
												sprintf(strbuff, "<%g,%g,%g>", cord[0], cord[1], cord[2]);
												(*event).evclass = UD_STRING;
												(*event).indata.stringdata = strbuff;
											}
											else if(type == UM_FVECT)
											{
												(*event).evclass = UD_VECTOR;
												for(i=0; i<3; i++)
													(*event).indata.vecdata[i] = cord[i];
											}
											else
												uu_uerror0(UD_DASHEP, 34);
										}
/*
.....the picking segment is used, reset the stack
*/
										if (save_seg!=0)
											ncl_reset_pikgeom();
									}
								}
								else if (((*parm_ptr).evtype == UD_PICK)&&(ud_getn_pick_segs()>0))
								{
									goto again;
								}
							}
/*
......after mouse function continue the old function old once
......
......if das function need execute (das function is posting message)
......we need get the message again to execute
*/
							if ((func_mouse==1)&&(das_exe==0))
							{
/*								
.....if function is KEY_PICK_LOCATE
*/
/*
.....the mouse function uz_mouse_functions is basically empty for standalone NCLIPV
.....so we don't consider this case
*/
								if (LW_nclipv!=1)
								{
									if ((UZ_key_pickloc!=sav_pickloc)||
										((*event).evclass==UD_PICK)||
										((*event).evclass==UD_STRING))
										goto restart;
									else 
										goto again;
								}
								else
									goto restart;
							}
							else
								goto again;
						}
						else
						{
							goto again;
						}
					}
					else if(UD_enabless)
					{
						for(i=0; i<UD_subsyslen; i++)
						{
							if(ud_strcomp(&cptr[1], UD_subsys[i].name) < 0)
							{

/*								-- save current system level help and set new one 
									active -- */

								savesys = UD_syssys;
								cycleptr = UD_cycle2ptr;
								UD_syssys = UD_subsys[i].subhelpno;

/*								-- Save the crosshair location -- */
								zbytecp(UD_wxhair_save, UD_wxhair);
								zbytecp(UD_nxhair_save, UD_nxhair);

/*								-- suspend current window (if active), and set new DAS 
									environment -- */

								ud_suspwin();
								ud_lpsh(UU_FALSE);

								UD_MARK(markval, UU_TRUE);
								if(markval == 0)
								{
									if(UD_subsys[i].nump == 0)
										(*UD_subsys[i].ddentry)();
									else
										(*UD_subsys[i].ddentry)(cptr);
								}

/*								-- restore old state -- */

								ud_lpop();

/*								-- Restore the crosshair location -- */

								zbytecp(UD_wxhair, UD_wxhair_save);
								zbytecp(UD_nxhair, UD_nxhair_save);

								ud_resuwin();
								UD_syssys = savesys;
								UD_cycle2ptr = cycleptr;
								UD_UNMARK(markval);
								goto again;
							}
						}

/* 					--	unsupported subsystem -- */

						uu_uerror1(UD_DASHEP, 14, &cptr[1]);
						goto again;
					}
					else
					{
						uu_uerror0(UD_DASHEP, 105);
						goto again;
					}
				}
				break;
		}
	}

/*	-- if specific interaction limit is on, then handle appropriately -- */

	if(UD_LIMIT.lsint == UU_TRUE)
		if((*event).evclass != UD_LIMIT.fsint)
			if(!((*event).evclass==UD_CHOICE && (*event).evdev==UD_AUXMENU))
			{

/* 				--	input constrained to %s	-- */

					uu_uerror1(UD_DASHEP, 69, typemess[UD_LIMIT.fsint]);
					goto again;
			}
done:;
#if UU_DEBUG==UU_TRUE
	if(UU_debmask & UU_DTRC)
		ud_prevent("ud_gevt1", event);
#endif
	UD_pickmode = 0;
	ud_reset_current_event();
	UD_UNMARK(mark);
	uu_dexit;
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_strcomp(instr, outstr)
**      compare two strings and return which character they differed at
**
**  PARAMETERS   
**      INPUT  : 	instr =  string 1
**						outstr = string 2
**      OUTPUT :  
**          none
**
**  RETURNS      :   if -1 then two strings are identical
**							if >= 0 then the character number they differ at
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_strcomp(instr, outstr)
char *instr;
char *outstr;
{

	register int i;

	for(i=0 ; (instr[i]==outstr[i]) && (instr[i]!='\0') 
			 && (outstr[i]!='\0'); i++)
	;

	if(instr[i]=='\0' || outstr[i]=='\0')
		return(-1);
	else
		return(i);
}

/**************************************************************************
**
**  E_FUNCTION         :  ud_subsystem(subsys, tutorial, trace, stkfl)
**      						activate a subsystem
**
**  PARAMETERS   
**      INPUT  : 
**          subsys = entry point to subsystem
**				tutorial = tutorial definition block 
**				trace = subsystem trace name
**				stkfl = if UU_TRUE, then activate application stack flag
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

void ud_subsystem(subsys, tutorial, trace, stkfl)
int (*subsys)();					/* subsystem entry point */
char *tutorial;					/* tutorial block address */
char *trace;						/* subsystem trace name */
UU_LOGICAL stkfl;					/* if UU_TRUE, then activate application stack */
{

	int savesys;						/* save subsystem state */
	UD_DASSTATE savepromptstate; 	/* save DAS prompt state */
	int savepromptsys;				/* save DAS subsystem number */
	int savepromptnum;				/* save DAS prompt number */
	int cycleptr;						/* save puck cycle state */
	int markval;						/* mark stack variable */
	char *tutsave;						/* tutorial address save cell */
	Gloc3 UD_wxhair_save;			/* save initial crosshair location */
	Gloc3 UD_nxhair_save;			/* save initial crosshair location */
	int save_mark_stack;				/* save mark stack pointer */

	uu_denter(UU_DTRC,(us, "entering ud_subsystem"));
	if(UD_enabless == UU_TRUE)
	{

/*		-- suspend current window (if active), and set new DAS environment -- */

		ud_suspwin();
		ud_lpsh(UU_FALSE);

/*		-- save current system level help and puck cursor state -- */

		savesys = UD_syssys;
		savepromptstate = UD_promptstate;
		savepromptsys = UD_promptsys;
		savepromptnum = UD_promptnum;
		cycleptr = UD_cycle2ptr;

/*		-- save off all the tutorial information -- */

		tutsave = UD_tut_ptr;
		UD_tut_ptr = tutorial;
		if(UD_tut_mode == UD_TUT_ON)
			if(tutorial != NULL)
				uj_tutorial(tutorial);

/*		-- save the crosshair location -- */

		zbytecp(UD_wxhair_save, UD_wxhair);
		zbytecp(UD_nxhair_save, UD_nxhair);

		UD_MARK(markval, UU_TRUE);
		if(markval == 0)
		{
			if(trace != NULL)
			{

/*				-- set up trace back, warn operator if we hit warning depth -- */

				UD_sstrc[UD_sstrc_ptr] = trace;
				UD_sstrc_ptr++;
				if(UD_sstrc_ptr > UDI_MU_WARN+5)
				{
					uu_uerror0(UD_DASHEP, 68);
					UD_dastkfl = UU_FALSE;
					uu_dexit;
					ud_jump(UD_markptr, UU_TRUE);
				}
				else if(UD_sstrc_ptr > UDI_MU_WARN)
					uu_uerror0(UD_DASHEP, 87);
			}

/*			-- set up application stack if indicated -- */

			if(stkfl == UU_TRUE)
				uu_appush_store();

			save_mark_stack = UD_markptr;
			(*subsys)();
		}

/*		-- test mark stack state -- */

		if(save_mark_stack != UD_markptr)
		{
			uu_dprint(UU_DTRC,(us, "MARK imbalance, save=%d, current=%d", 
							save_mark_stack, UD_markptr));
			uu_uerror0(UD_DASHEP, 97);
		}

/*		-- restore application stack if indicated -- */

		if(stkfl == UU_TRUE)
			uu_appop_store();

/*		-- restore old state -- */

		ud_lpop();
		ud_resuwin();

/*		-- restore prompt, error, help stuff -- */

		UD_syssys = savesys;
		UD_promptstate = savepromptstate;
		UD_promptsys = savepromptsys;
		UD_promptnum = savepromptnum;
		UD_tut_ptr = tutsave;
		UD_cycle2ptr = cycleptr;

		if((UD_sstrc_ptr>0) && (trace!=NULL))
			UD_sstrc_ptr--;

/*		-- Restore the crosshair location -- */

		zbytecp(UD_wxhair, UD_wxhair_save);
		zbytecp(UD_nxhair, UD_nxhair_save);

		uu_dprint(UU_DTRC,(us, "leaving ud_subsystem, markval=%d", markval));
		UD_UNMARK(markval);
	}
	else
	{

/*		-- subsystems are currently disabled -- */

		uu_uerror0(UD_DASHEP, 93);
	}
	uu_dexit;
}

/********************************************************************* 
**
**  E_FUNCTION	: ud_atgevt(event, evtype, prompt, number, device, echo)
**      description : front end to input pipe for auto-test record
**							 override
**
**  PARAMETERS   
**      INPUT: 
**					evtype = type of event desired, one of: UD_NONE, UD_LOCATOR,
**							UD_STROKE, UD_VALUATOR, UD_CHOICE, UD_PICK, UD_STRING, 
**							UD_VECTOR
**					devno = device number from which input is desired.
**				   prompt = prompt or array of prompts (if CHOICE request)
**					number = number of prompts
**					device = device number from which input is requested
**					echo = prompt and echotype
**					inparm = input parameter block (catchall for other data)
**
**      OUTPUT: 
**					event = structure containing the event, defined as:
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

void ud_atgevt(event, evtype, prompt, number, device, echo, inparm)
UD_DEVENT *event;		/* event buffer */
int evtype;				/* input type requested */
char *prompt;			/* prompts */
int number;				/* number of prompts */
int device;				/* device number from which input is requested */
int echo;				/* echotype */
UD_EVENTDEF *inparm; /* input parameter block */
{
	UD_EVENTDEF *parm_ptr;			/* local pointer */
	UD_EVENTDEF eventbuf;			/* local pointer */

	if(inparm == NULL)
	{
		parm_ptr = &eventbuf;
		eventbuf.strbfsz = 80;
		eventbuf.defstr = "";
	}
	else
		parm_ptr = inparm;

/*	-- set autotest record overide flag and call input pipe -- */

	if(UD_autotest == UU_TRUE)
		(*parm_ptr).atoveride = UU_TRUE;
	else
		(*parm_ptr).atoveride = UU_FALSE;

	ud_gevt1(event, evtype, prompt, number, device, echo, parm_ptr);
}

/********************************************************************* 
**
**  E_FUNCTION	: ud_gevt(event, evtype, prompt, number, device, echo)
**      description : front end to input pipe for auto-test record
**							 wtih no override
**
**  PARAMETERS   
**      INPUT: 
**					evtype = type of event desired, one of: UD_NONE, UD_LOCATOR,
**							UD_STROKE, UD_VALUATOR, UD_CHOICE, UD_PICK, UD_STRING, 
**							UD_VECTOR
**					devno = device number from which input is desired.
**				   prompt = prompt or array of prompts (if CHOICE request)
**					number = number of prompts
**					device = device number from which input is requested
**					echo = prompt and echotype
**					inparm = input parameter block (catchall for other data)
**
**      OUTPUT: 
**					event = structure containing the event, defined as:
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

void ud_gevt(event, evtype, prompt, number, device, echo, inparm)
UD_DEVENT *event;		/* event buffer */
int evtype;				/* input type requested */
char *prompt;			/* prompts */
int number;				/* number of prompts */
int device;				/* device number from which input is requested */
int echo;				/* echotype */
UD_EVENTDEF *inparm; /* input parameter block */
{
	UD_EVENTDEF *parm_ptr;			/* local pointer */
	UD_EVENTDEF eventbuf;			/* local pointer */

	if(inparm == NULL)
	{
		parm_ptr = &eventbuf;
		eventbuf.strbfsz = 80;
		eventbuf.defstr = "";
	}
	else
		parm_ptr = inparm;

/*	-- do not autotest record overide flag and call input pipe -- */

	(*parm_ptr).atoveride = UU_FALSE;
	ud_gevt1(event, evtype, prompt, number, device, echo, parm_ptr);
}

void ud_save_current_event(event)
UD_DEVENT *event;
{
	S_cevent.evclass =  event->evclass;
	S_cevent.evdev =  event->evdev;
	S_cevent.indata =  event->indata;
}

void ud_reset_current_event()
{
	S_cevent.evclass = -1;
}
