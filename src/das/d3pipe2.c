/*********************************************************************
**
**    NAME         :  d3pipe2.c
**
**       CONTAINS:
**  			ud_sevt
**  			ud_subm
**  			ud_insubm
**				ud_pmcurs
**				ud_is_prompt_open
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**     MODULE NAME AND RELEASE LEVEL 
**       d3pipe2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:06
**
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "uhep.h"
#include "udebug.h"
#include "dinput.h"
#include "calcom.h"
#include "dasnog.h"
#include "dsubcom.h"
#include "dmark.h"
#include "dselect.h"
#include "mfeatcom.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "vsegbf.h"
#include "mfort.h"
#include "nclfc.h"

#define SUBCHAR '\\'					/* escape character */
#define SAFCHAR '!'					/* SAF character */
#define CALCHAR '^'					/* symbol table escape character */

#define EXECUTE UU_TRUE				/*	execute option for tablet handler */
#define NOEXECUTE UU_FALSE			/* no execute option */
#define SEMACTDEF 0					/* semantic action picked */
#define STRINGDEF 1					/* string picked */
#define UNDEF     2					/* undefined button */
#define NOSUB     3					/* substitution inappropriate on this device */
#define TABLET    4					/* tablet device number */

/*
....Flag nclu_limit_pick() that it is ok to define local limits.
....Flag is set to UU_TRUE only if input state is 'by pick'.
*/
extern UU_LOGICAL NCL_init_limit;
static int Sprompt = 0;
extern int NCL_cmdmod;
/**************************************************************************
**
**  I_FUNCTION  :ud_sevt(event, opflag, parm_ptr)
**      monitor input stream and perform token substitution for
**		  symbol table, features, and substitution menus.
**
**  PARAMETERS   
**      INPUT  : 
**				event = event buffer
**				opflag = if UU_TRUE, then input always form operator
**				parm_ptr = pointer to input event parameters
**      OUTPUT :  
**          event = event buffer
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

void ud_sevt(event, opflag, inparm)
UD_DEVENT *event;			/* event buffer */
UU_LOGICAL opflag;		/* if UU_TRUE, then input only from operator */
UD_EVENTDEF *inparm; 	/* input parameter block */
{
	 char *jptr;
	 CRSLT stbptr;								/* symbol table entry pointer */
	 UU_REAL cord[3];
	 UU_LOGICAL ud_rprd();
	 UU_LOGICAL	ud_outevt();
	 uv_segbuff(udata);						/* user segment id data buffer */
	 int i, type;
	 int ud_subm();
	 static char strbuff[100];				/* buffer to convert symbol table 
														entry to string */
	UM_int2 NCL_leave_error_msg, ifl301;
	char buf[256];

	jptr = buf;

	uu_denter(UU_DTRC,(us, " entering ud_sevt, event=%x", event));

restart:

/*	-- remove the error message after coming through here twice -- */

	if(UD_Rpstate[UD_Rpstate_ptr].flag != PLAYBACK)
		UD_errorcnt--;

	if(UD_errorcnt < 0)
		UD_errorcnt = 0;
	else if(UD_errorcnt == 0)
	{
		if(UD_errorsegflag == UU_TRUE)
		{

/*  NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL

 The call to ud_killmsg should not be executed if the last thing
 displayed in the error message area was an NCL status information
 line.  A valid NCL error or IUNIHEP error will cause the line
 to be erased.
														  Mark Gump
														  4-23-90
    NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL  NCL */

			ifl301 = 301;
			getifl (&ifl301, &NCL_leave_error_msg);
			if (NCL_leave_error_msg == 0)
			{
			    ud_killmsg(UD_ERRORMSG);
			    UD_errorsys = 0;
			}
		}
	}

/*	-- get the next input from the procedure buffer -- */

	if(UD_prc_buf_ptr > 0)
	{
		if(ud_outevt(event) != UU_TRUE)
		{
			uu_dprint(UU_DTRC, (us,"ud_outevt=FALSE, buf_ptr=%d", UD_prc_buf_ptr));
			goto restart;
		}
	}

	else if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK && opflag == UU_FALSE)
	{

/*		-- write prompt to auto test file here because we do not get
			to ud_revt in playback mode -- */

		if((*inparm).evtype != UD_CHOICE)
			if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
				ux_write_logfile("SEVT", (*inparm).prompt);

/*		-- get the next input from disk -- */

		if(ud_rprd(event, UU_FALSE) == UU_FALSE)
		{
			UD_Rpstate[UD_Rpstate_ptr].flag = RPOFF;
			ug_reset_event();
			goto restart;
		}
	}

/*	-- get the next event on the queue -- */

	else
	{
		if ((inparm->evtype==UD_STRING) && (UD_enableim))
			Sprompt = 1;
		ud_read(sizeof(UD_DEVENT), event, inparm);

/*		-- see if choice device is a substitution device -- */


		if((*event).evclass==UD_CHOICE && UD_enableim)
		{

/*			-- Subtitution Menu Handler --  */

			type = ud_subm((*event).evdev,
				(*event).indata.choicedata, &jptr, NOEXECUTE);

			if (type == STRINGDEF)
			{
				(*event).indata.stringdata = jptr;
				(*event).evclass = UD_STRING;
			}

/*				-- only the main keyboard may be unassigned at this point -- */

			else if(type == UNDEF && (*event).evdev != 1)
			{
/*
.....remove error message and treat this as "DONE" 
.....yurong
*/
				type = STRINGDEF;
				(*event).indata.stringdata = "\\\\2";
				(*event).evclass = UD_STRING;
/*				--	cursor not on an icon -- */
/*
				uu_uerror0(UD_DASHEP, 32);
				goto restart;
*/
			}
		}
	}

/*	-- if recording then write it out --    */

	if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
	{
		if((*inparm).atoveride == UU_FALSE)
			ud_rpwr(event);
		else if((*event).evclass == UD_STRING) 
		{

/*			-- don't record DAS control functions (\\n) -- */

			if(strlen((*event).indata.stringdata)==3 &&
					 (*event).indata.stringdata[1]=='\\')
		  {
				 if((*event).indata.stringdata[2] == '0')
			 			ud_rpwr(event);
		  }
		  else
			 ud_rpwr(event);
		}
		else if((*event).evclass == UD_CHOICE)
		{

/*			-- don't record menu picks and -- */

			if((*event).evdev < UD_st_menu_num)
				ud_rpwr(event);
		}
	  	else
		 	ud_rpwr(event);
	}
	
/* -- now see if we have to substitute other stuff -- */
/*
......save the current event
*/
	ud_save_current_event(event);
	if(UD_enableim)
	{
	switch ((*event).evclass)
		{
			case UD_NONE:

				break;

			case UD_LOCATOR:

/*					-- if locator event is triggered by an assigned key, the choice
					event takes precedence -- */

				type = ud_subm((*event).evdev,
					(*event).indata.locdata.choice, &jptr, EXECUTE);

				if(type == STRINGDEF)
				{
					if(jptr[0] == SAFCHAR)
					{
						for(i=0; i<UD_num_safs; i++)
							if(ud_strcomp(UD_saf_table[i].safname,
										&jptr[1]) < 0) 
							{
								type=(*UD_saf_table[i].safprc)(jptr, &jptr);
								break;
							}
					}
				}

				if(type == STRINGDEF)
				{
					(*event).evclass = UD_STRING;
					(*event).evdev = UD_AUXMENU;
					(*event).indata.stringdata = jptr;
				}
				else if(type == SEMACTDEF)
				{
					goto restart;
				}
				break;

			case UD_STROKE:

				break;

			case UD_VALUATOR:

				break;

			case UD_PICK:
/*
......when in motion pick, we still need execute the function key ud_subm
......so put "if (ud_motion_pick_type()) break;" after we done that
......Yurong
*/
/*				if (ud_motion_pick_type()) break;  */
/*					-- if pick event is triggered by an assigned key, the choice
					event takes precedence -- */

/*
.................Flag nclu_limit_pick() that it is ok to limit picking
.................NCL_init_limit is reset in nclu_limit_pick()
*/
				NCL_init_limit = UU_TRUE;
				type = ud_subm((*event).evdev, 
					(*event).indata.pickdata.choice, &jptr, EXECUTE);

				if(type == STRINGDEF)
				{
					if(jptr[0] == SAFCHAR)
					{
						for(i=0; i<UD_num_safs; i++)
							if(ud_strcomp(UD_saf_table[i].safname,
										&jptr[1]) < 0) 
							{
								type=(*UD_saf_table[i].safprc)(jptr, &jptr);
								break;
							}
					}
				}

				if(type == STRINGDEF)
				{
					(*event).evclass = UD_STRING;
					(*event).indata.stringdata = jptr;
				}
				else if(type == SEMACTDEF)
				{
					goto restart;
				}
/*
......call "if (ud_motion_pick_type()) break;" here
*/
				else if (ud_motion_pick_type()) break;

				else if((*event).indata.pickdata.depth > 0)
				{

/*						-- invoke pick filter before feature substitution so that
							the features are properly filtered -- */

					ud_filter_entity(event);

/*						-- if pick, then see if a feature was picked.
							Make sure that filter did not change the
							event type -- */

					if((*event).evclass == UD_PICK &&
						!ud_isassist_seg((*event).indata.pickdata.pickpath[0]))
					{
						gsegrud((*event).indata.pickdata.pickpath[0], udata);
						if(uv_getrelnum(udata) == UM_FEAT_REL)
						{

/*						-- go look up the feature -- */

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

/*---							invalid feature pickid		---*/

								uu_uerror0(UD_DASHEP, 34);
						}
					}
				}

				break;

			case UD_STRING:

				if((*event).indata.stringdata[0] == SAFCHAR)
				{
					for(i=0; i<UD_num_safs; i++)
						if(ud_strcomp(UD_saf_table[i].safname,
										&(*event).indata.stringdata[1]) < 0) 
						{
							type = (*UD_saf_table[i].safprc)((*event).indata.stringdata, 
																		&jptr);
							if(type == STRINGDEF)
							{
								(*event).evclass = UD_STRING;
								(*event).indata.stringdata = jptr;
							}
							else
								goto restart;
							break;
						}
				}

				else if((*event).indata.stringdata[0] == CALCHAR)
				{

/*					-- for symbol table, substitute symbol's value an convert to
					a string event -- */

					if(uq_calc2(&(*event).indata.stringdata[1], &stbptr) == UU_TRUE)
					{
						switch(stbptr.ctype)
						{
							case UM_CARTESIAN:
							case UM_CYLINDRICAL:
							case UM_SPHERICAL:

								um_cotovc(stbptr.val.cval, stbptr.ctype, cord);
								sprintf(strbuff, "<%g,%g,%g>", cord[0], cord[1], cord[2]);
								(*event).evclass = UD_STRING;
								(*event).indata.stringdata = strbuff;
								break;

							case UM_VCARTESIAN:
							case UM_VCYLINDRICAL:
							case UM_VSPHERICAL:

								um_cotovc(stbptr.val.cval, stbptr.ctype, cord);
								(*event).evdev = UD_AUXMENU;
								(*event).evclass = UD_VECTOR;
								for(i=0; i<3; i++)
									(*event).indata.vecdata[i] = cord[i];
								break;

							case UQ_SCALAR:
								
								(*event).evdev = UD_AUXMENU;
								(*event).evclass = UD_VALUATOR;
								(*event).indata.valdata = stbptr.val.sval;
								break;

							default:

/*---							unsupported symbol table data type		---*/

								uu_uerror0(UD_DASHEP, 35);
								break;
						}
					}
					else
					{
						goto restart;
					}
				}
				break;

			case UD_CHOICE:

/*				-- now that this event is recorded, go back and execute the
					sematic action if indicated -- */

/*				-- see if choice device is a substitution device -- */

				type = ud_subm((*event).evdev, 
					(*event).indata.choicedata, &jptr, EXECUTE);
				Sprompt = 0;
				if(type == SEMACTDEF)
					goto restart;
				break;

			default:
				break;

		}
	}
	Sprompt = 0;
#if UU_DEBUG==UU_TRUE
	if(UU_debmask & UU_DTRC)
		ud_prevent("ud_sevt", event); 
#endif
	uu_dexit;
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_subm(device, choice, strptr, exflag)
**      perform a substition menu table lookup and substitution
**
**  PARAMETERS   
**      INPUT  : 
**          device = device number
**				choice = choice number
**      OUTPUT :  
**          none
**
**  RETURNS      :  pointer to result string
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

#define EXECUTE UU_TRUE				/*	execute option for tablet handler */
#define NOEXECUTE UU_FALSE			/* no execute option */

int ud_subm(device, choice, strptr, exflag)
int device;						/* device number */
int choice;						/* choice number */
char **strptr;					/* string pointer return */
int exflag;						/* execute flag */
{
	int type;					/* button program type */
	int markval;				/* mark stack flag */

	uu_denter(UU_DTRC,(us,"ud_subm(dev=%d,choice=%d)",device,choice));

/*	-- normal event mode choice devices -- */

	if(device < UD_start_menu_num)
	{	

/*			-- permute iconmenu device / choice number -- */

		if ((device>=6)&&(device<=(UD_start_menu_num-2))) 
		{
			choice=ud_iconnum(device,choice);
			device=6;
		}

		uu_dprint(UU_DTRC, (us,"before lookup, choice=%d", choice));

		if(UD_chctable[device-1] != NULL)
		{
			UD_MARK(markval, UU_FALSE);
			if(markval == 0)
			{

/*				-- activate application stack -- */

				if(exflag == EXECUTE)
					uu_appush_store();

				type = (*(UD_chctable[device-1]))(choice, strptr, exflag);
			}

			if(exflag == EXECUTE)
				uu_appop_store();
			UD_UNMARK(markval);
		}
		
		uu_dprint(UU_DTRC, (us,"after lookup, table=%x, type=%d",
						UD_chctable[device-1], type));

		if(type == STRINGDEF)
		{
			if ((uz_ifmouse_key())||(ud_ifpopup())|| NCL_cmdmod)
			{
				if (strcmp(*strptr, "\\mouse_left")==0)
					*strptr = "";
				if (strcmp(*strptr, "\\mouse_middle")==0)
					*strptr = "\\\\2";
				if (strcmp(*strptr, "\\mouse_right")==0)
					*strptr = "\\\\3";
			}
			if (*strptr[0] == '\0')
				type = UNDEF;
		}
	}

/*	-- if icon user interface is active -- */

	else if((device>UD_start_menu_num) && (device<=UD_chctblen))
	{
		if(UD_chctable[device-1] != NULL)
		{
			UD_MARK(markval, UU_FALSE);
			if(markval == 0)
			{

/*				-- activate application stack -- */

				if(exflag == EXECUTE)
					uu_appush_store();

				type = (*(UD_chctable[device-1]))(choice, strptr, exflag);
			}

			if(exflag == EXECUTE)
				uu_appop_store();
			UD_UNMARK(markval);
		}
		else
		{
			type = UNDEF;
		}
	}

/*	-- substitution not appropriate -- */

	else
		type = NOSUB;

	uu_dprint(UU_DTRC,(us,"leave ud_subm(type=%d)",type));

	uu_dexit;
	return(type);
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_insubm(device, requested)
**      initialize a substition menu table lookup and substitution for cycle
**		  menus
**
**  PARAMETERS   
**      INPUT  : 
**          device = device number
**				requested = input request type
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

void ud_insubm(device, requested)
int device;						/* device number */
int requested;					/* request number */
{

	uu_denter(UU_DTRC,(us, "entering ud_insubm, requested=%d", requested));

/*		-- tablet puck -- */

	switch(requested)
	{
		case UD_CHOICE:
			UD_cycle2ptr = -1;
			break;
		case UD_LOCATOR:
			UD_cycle2ptr = 1;
			break;
		case UD_PICK:
			UD_cycle2ptr = 0;
			break;
		default:
			UD_cycle2ptr = 0;
			break;
	}
	uu_dprint(UU_DTRC,(us, "leave ud_insubm, UD_cycle2ptr=%d", UD_cycle2ptr));
	uu_dexit;
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_pmcurs()
**      permute the cursor state
**
**  PARAMETERS   
**      INPUT  : 
**          invoke = command line used to invoke function
**      OUTPUT :  
**          strptr = pointer to character string of cursor action
**
**  RETURNS      :  type of program (always a string)
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

int ud_pmcurs(invoke, strptr)
char *invoke;					/* string that invoked SAF */
char **strptr;					/* string pointer return */
{

	uu_denter(UU_DTRC,(us, "entering ud_pmcurs, UD_cycle2ptr=%d",
							UD_cycle2ptr));

	if(UD_cycle2ptr < 0)
	{

/*	-- puck is requesting a choice (disable cycling) -- */

		*strptr = UD_chccycle;
	}
	else

/*	-- puck is not requesting a choice (enable cycling) -- */

	{
		UD_cycle2ptr++;
		if(UD_cycle2ptr >= UD_cycle2len)
			UD_cycle2ptr = 0;

		UD_chdev3.subchar[1] = UD_cycle2[UD_cycle2ptr];
		*strptr = (UD_chdev3.subchar[2 - UD_chdev3.min]);
	}
	uu_dprint(UU_DTRC,(us, "leaving ud_pmcurs, str=%s", *strptr));

	uu_dexit;
	return(STRINGDEF);
}

/**************************************************************************
**
**  I_FUNCTION  :ud_is_prompt_open
**		Check if the prompt window is opened.
**  PARAMETERS   
**      INPUT  : none
**      OUTPUT :  none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
ud_is_prompt_open()
{
	return Sprompt;
}
