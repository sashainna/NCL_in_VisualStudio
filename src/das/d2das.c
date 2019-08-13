/*********************************************************************
**
**    NAME         :  d2das.c
**
**       CONTAINS:
**				ud_ldas
**				ud_adas
**  			ud_chcadas
**  			ud_chcldas
**				ud_das
**				ud_ddas
**				ud_lyesno
**				ud_hakt
**				ud_cnmint
**				int ud_fyesno(parent, msg, nc1, title, nc2)
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d2das.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:04
**
*********************************************************************/

#include "usysdef.h"
#include "usysg.h"
#include "dtypes.h"
#include "dasnog.h"
#include "dinput.h"
#include "derror.h"
#include "dasg.h"
#include "dsubcom.h"
#include "dselect.h"
#include "ddef.h"
#include "udebug.h"
#include "uhep.h"
#include "ustdio.h"
#include "gtbl.h"
#include "gdidd.h"
#include "nclfc.h"

/********************************************************************* 
**
**  E_FUNCTION		:  ud_ldas(type, subnum, errno1, buffer, size, numint, flag)
**      generalized front end to the DAS subystem to input multiple events
**		  of the same type.
**
**  PARAMETERS   
**      INPUT:  type   = type of das call
**					 subnum = prompt subsystem number
**					 errno1 = prompt number
**					 buffer = input buffer
**					 size   = size of buffer in number of inputs of request
**								  type (e.g., 5 coordinates) except for choice
**								  as number of choices and string which is
**								  number of characters
**					 flag = if UD_DEFAULT then one exists
**      OUTPUT: numint = number of interactions actually input
**
**  RETURNS      :  status of operation
**							UU_TRUE if terminated with a "done"
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

ud_ldas(type, subnum, errno1, buffer, size, numint, flag)
int type;				/* type of das call */
int subnum;				/* subsystem number */
int errno1;				/* prompt number */
char buffer[];			/* data return buffer */
int size;				/* size of data buffer */
int *numint;			/* actual number of interactions */
int flag;				/* if UD_DEFAULT then default exists */
{

	char *uu_uprompt0(), msg[100];
	UU_LOGICAL status;			/* DAS status save cell */

	/*uu_denter(UU_DTRC,(us,"enter ud_ldas, type=%d, subsys=%d, number=%d", 
							type, subnum, errno1));*/

	strcpy(msg, uu_uprompt0(subnum, errno1));
	ud_rpwrcom(msg);
	status=ud_ddas(type, msg, buffer, size, numint, flag);
	UD_promptsys = 0;
	UD_promptnum = 0;
	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  E_FUNCTION		:  ud_adas(type, subnum, errno1, buffer, size, numint, flag)
**      generalized front end to the DAS subystem to input multiple events
**		  of the same type that uses application prompts.
**
**  PARAMETERS   
**      INPUT:  type   = type of das call
**					 subnum = prompt subsystem number
**					 errno1 = prompt number
**					 buffer = input buffer
**					 size   = size of buffer in number of inputs of request
**								  type (e.g., 5 coordinates) except for choice
**								  as number of choices and string which is
**								  number of characters
**					 flag = if UD_DEFAULT then one exists
**      OUTPUT: numint = number of interactions actually input
**
**  RETURNS      :  status of operation
**								UU_TRUE if terminated with a "done"
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_adas(type, subnum, errno1, buffer, size, numint, flag)
int type;				/* type of das call */
int subnum;				/* subsystem number */
int errno1;				/* prompt number */
char buffer[];			/* data return buffer */
int size;				/* size of data buffer */
int *numint;			/* actual number of interactions */
int flag;				/* if UD_DEFAULT then default exists */
{

	char * uu_aprompt0(), msg[100];
	UU_LOGICAL status;			/* DAS status save cell */

	strcpy(msg, uu_aprompt0(subnum, errno1));
	ud_rpwrcom(msg);
	status=ud_ddas(type, msg, buffer, size, numint, flag);
	UD_promptsys = 0;
	UD_promptnum = 0;
	return(status);
}

/********************************************************************* 
**
**  E_FUNCTION		:  ud_chcldas(subnum, errno1, buffer, size, numint, flag)
**      generalized front end to the DAS subystem to input choice events
**
**  PARAMETERS   
**      INPUT:  subnum = prompt subsystem number
**					 errno1 = prompt number
**					 buffer = input buffer
**					 size   = size of buffer in number of inputs of request
**								  type (e.g., 5 coordinates) except for choice
**								  as number of choices and string which is
**								  number of characters
**					 flag = if UD_DEFAULT then one exists
**      OUTPUT: numint = number of interactions actually input
**
**  RETURNS      :  status of operation
**								UU_TRUE if terminated with a "done"
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_chcldas(subnum, errno1, buffer, size, numint, flag)
int subnum;				/* subsystem number */
int errno1;				/* prompt number */
char buffer[];			/* data return buffer */
int size;				/* size of data buffer */
int *numint;			/* actual number of interactions */
int flag;				/* if UD_DEFAULT then default exists */
{

	char **uu_uchoicemsg();
	UU_LOGICAL status;			/* DAS status save cell */
	int locnumint;					/* number of interaction cell */
	char **msg;						/* menu text pointer */

	msg = uu_uchoicemsg(subnum, errno1, &locnumint);
	ud_rpwrcom(*msg);
	status=ud_ddas(UD_DASCHOICE, msg, buffer, locnumint, numint, flag);
	UD_promptsys = 0;
	UD_promptnum = 0;
	return(status);
}

/********************************************************************* 
**
**  E_FUNCTION		:  ud_chcadas(subnum, errno1, buffer, size, numint, flag)
**      generalized front end to the DAS subystem to input choice events
**
**  PARAMETERS   
**      INPUT:  subnum = prompt subsystem number
**					 errno1 = prompt number
**					 buffer = input buffer
**					 size   = size of buffer in number of inputs of request
**								  type (e.g., 5 coordinates) except for choice
**								  as number of choices and string which is
**								  number of characters
**					 flag = if UD_DEFAULT then one exists
**      OUTPUT: numint = number of interactions actually input
**
**  RETURNS      :  status of operation
**								UU_TRUE if terminated with a "done"
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_chcadas(subnum, errno1, buffer, size, numint, flag)
int subnum;				/* subsystem number */
int errno1;				/* prompt number */
char buffer[];			/* data return buffer */
int size;				/* size of data buffer */
int *numint;			/* actual number of interactions */
int flag;				/* if UD_DEFAULT then default exists */
{

	char **uu_achoicemsg();
	UU_LOGICAL status;			/* DAS status save cell */
	int locnumint;					/* number of interaction cell */
	char **msg;						/* menu text pointer */

	msg = uu_achoicemsg(subnum, errno1, &locnumint);
	ud_rpwrcom(*msg);
	status=ud_ddas(UD_DASCHOICE, msg, buffer, locnumint, numint, flag);
	UD_promptsys = 0;
	UD_promptnum = 0;
	return(status);
}

/********************************************************************* 
**
**  E_FUNCTION		:  ud_das(type, prompt, buffer, size, numint)
**      generalized front end to the DAS subystem to input multiple events
**		  of the same type.
**
**  PARAMETERS   
**      INPUT:  type   = type of das call
**					 prompt = semantic prompt for all input except choice,
**								  array of pointers to choice strings if choice
**					 buffer = input buffer
**					 size   = size of buffer in number of inputs of request
**								  type (e.g., 5 coordinates) except for choice
**								  as number of choices and string which is
**								  number of characters
**      OUTPUT: numint = number of interactions actually input
**
**  RETURNS      :  status of operation
**								UU_TRUE if terminated with a "done"
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_das(type, prompt, buffer, size, numint)
int type;				/* type of das call */
char *prompt;			/* prompt string */
char buffer[];			/* data return buffer */
int size;				/* size of data buffer */
int *numint;			/* actual number of interactions */
{

	ud_rpwrcom(prompt);
	return(ud_ddas(type, prompt, buffer, size, numint, UD_NODEFAULT));
}

/********************************************************************* 
**
**  I_FUNCTION:  ud_cnmint(number)
**      if interaction count DAS limited, verifies conformity
**
**  PARAMETERS   
**      INPUT:  number = count of interactions
**      OUTPUT: none
**
**  RETURNS      :  UU_TRUE if OK, UU_FALSE otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

static UU_LOGICAL ud_cnmint(number)
int number;			/* number of interations */
{

	uu_denter(UU_DTRC,(us,"in cnmint, ptr=%d, flag=%d", UD_limbfptr,
				UD_LIMIT.lnumin));
	uu_dexit;
	ud_prntlim("cnmint");

	if(UD_LIMIT.lnumin == UU_TRUE)
	{
		if(number<=UD_LIMIT.fnthi && number >= UD_LIMIT.fntlo)
			return(UU_TRUE);
		else
		{

/*			-- number of interactions constrained between %d and %d -- */

			uu_uerror2(UD_DASHEP, 71, UD_LIMIT.fntlo, UD_LIMIT.fnthi);
			return(UU_FALSE);
		}
	}
	else
		return(UU_TRUE);
}
/********************************************************************* 
**
**  E_FUNCTION		:  ud_ddas(type, prompt, buffer, size, numint, defflag)
**      generalized front end to the DAS subystem to input multiple events
**		  of the same type.
**
**  PARAMETERS   
**      INPUT:  type   = type of das call
**					 prompt = semantic prompt for all input except choice,
**								  array of pointers to choice strings if choice
**					 buffer = input buffer
**					 size   = size of buffer in number of inputs of request
**								  type (e.g., 5 coordinates) except for choice
**								  as number of choices and string which is
**								  number of characters
**					defflag = if UD_DEFAULT then default exists in first entry
**								  in data return buffer
**      OUTPUT: numint = number of interactions actually input
**
**  RETURNS      :  status of operation
**								UU_TRUE if terminated with a "done"
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

ud_ddas(type, prompt, buffer, size, numint, defflag)
int type;					/* type of das call */
char *prompt;				/* prompt string */
char buffer[];				/* data return buffer */
int size;					/* size of data buffer */
int *numint;				/* actual number of interactions */
UU_LOGICAL defflag;		/* default exists flag */
{

	int stat;
	UU_REAL value;
	char erms[80];
	int locsize;			/* local buffer size control */
	UU_LOGICAL xflag;		/* flag to control transformation of coordinates */
	UD_DASTAT dastat;		/* status returned from das */
	int increment;			/* byte count of each element in this interaction */
	int locinc;				/* character pointer into buffer */
	int definc;				/* character pointer into buffer for default */

	UD_DASTAT ud_cart(), ud_int(), ud_val(), ud_chc(), ud_str(), ud_pick(),
			ud_ploc(), ud_select(), ud_vec(), ud_pickdum(), ud_popup(),
			ud_strdef(), ud_plane(), ud_scavec(), ud_scaval(), ud_scaint(),
			ud_cart2(),ud_scavec2();

	uu_denter(UU_DTRC,(us,"enter ud_ddas, prompt=%s", prompt)); 

restart:

/*	-- This section initializes the interaction device, pet,
		and buffer sizes appropriately -- */

/*	-- set local size parameter --*/

	locsize = size;

/*		-- if DAS is limited to a specific interacetion, then set it up --*/

	switch(type)
	{
		case UD_SCACART:
		case UD_SCACART2:
		case UD_SCANDC:
		case UD_DASCART:
		case UD_DASNDC:

			if(UD_limbf[UD_limbfptr].lsint == UU_TRUE)
			{
				UD_locint = UD_limbf[UD_limbfptr].fsint;
				UD_locdev = UD_limbf[UD_limbfptr].fsdev;
				UD_locech = UD_limbf[UD_limbfptr].fsech;
			}

/*			-- transform all coordinates to world system except ndc input -- */
/*			-- generate character count of the data return element -- */

			if ((type == UD_DASNDC) || (type == UD_SCANDC))
			{
				xflag = UU_FALSE;
				increment = sizeof(UD_NDCLOCREC);
			}
			else
			{
				xflag = UU_TRUE;
				increment = sizeof(UD_DASCORD);
			}
			break;

		case UD_SCAVEC:
		case UD_SCAVEC2:
		case UD_DASVEC:

			if(UD_limbf[UD_limbfptr].lsint == UU_TRUE)
			{
				UD_vecint = UD_limbf[UD_limbfptr].fsint;
				UD_vecdev = UD_limbf[UD_limbfptr].fsdev;
				UD_vecech = UD_limbf[UD_limbfptr].fsech;
			}

/*			-- generate character count of the data return element -- */

			increment = sizeof(UD_DASCORD);
			break;
		case UD_SCAVAL:
		case UD_SCAINT:
		case UD_SCAANGLE:
		case UD_SCAUNITLESS:
		case UD_SCADISTANCE:
			if(UD_limbf[UD_limbfptr].lsint == UU_TRUE)
			{
				UD_valint = UD_limbf[UD_limbfptr].fsint;
				UD_valdev = UD_limbf[UD_limbfptr].fsdev;
				UD_valech = UD_limbf[UD_limbfptr].fsech;
			}

/*			-- generate character count of the data return element -- */

			if(type == UD_SCAINT)
				increment = sizeof(UD_SCA_IVAL);
			else
				increment = sizeof(UD_SCA_VALUE);
			break;

		case UD_DASVAL:
		case UD_DASINT:
		case UD_DASANGLE:
		case UD_DASUNITLESS:
		case UD_DASDISTANCE:

			if(UD_limbf[UD_limbfptr].lsint == UU_TRUE)
			{
				UD_valint = UD_limbf[UD_limbfptr].fsint;
				UD_valdev = UD_limbf[UD_limbfptr].fsdev;
				UD_valech = UD_limbf[UD_limbfptr].fsech;
			}

/*			-- generate character count of the data return element -- */

			if(type == UD_DASINT)
				increment = sizeof(int);
			else
				increment = sizeof(UU_REAL);

			break;

		case UD_DASSELECT:

			locsize = 1;

		case UD_DASPICK:
		case UD_DASPCKLOC:

			if(UD_limbf[UD_limbfptr].lsint == UU_TRUE)
			{
				UD_pckint = UD_limbf[UD_limbfptr].fsint;
				UD_pckdev = UD_limbf[UD_limbfptr].fsdev;
				UD_pckech = UD_limbf[UD_limbfptr].fsech;
			}

/*			-- generate character count of the data return element -- */
	
			if(type == UD_DASPICK)
				increment = sizeof(UD_PPICKREC);
			else
				increment = sizeof(UD_PLOCREC);

			break;

/*
.....added scalar value
.....Yurong
*/
		case UD_DASSCALAR:
		case UD_DASSTRING:
		case UD_DASSTRINGDEF:

			locsize = 1;
			if(UD_limbf[UD_limbfptr].lsint == UU_TRUE)
			{
				UD_strint = UD_limbf[UD_limbfptr].fsint;
				UD_strdev = UD_limbf[UD_limbfptr].fsdev;
				UD_strech = UD_limbf[UD_limbfptr].fsech;
			}

/*			-- generate character count of the data return element -- */

			increment = size;
			break;

		case UD_DASCHOICE:
		case UD_POPUP:

			locsize = 1;
			if(UD_limbf[UD_limbfptr].lsint == UU_TRUE)
			{
				UD_chcint = UD_limbf[UD_limbfptr].fsint;
				UD_chcdev = UD_limbf[UD_limbfptr].fsdev;
				UD_chcech = UD_limbf[UD_limbfptr].fsech;
			}

/*			-- generate character count of the data return element -- */

			increment = sizeof(int);
			break;

		case UD_DASPLANE:

			if(UD_limbf[UD_limbfptr].lsint == UU_TRUE)
			{
				UD_pckint = UD_limbf[UD_limbfptr].fsint;
				UD_pckdev = UD_limbf[UD_limbfptr].fsdev;
				UD_pckech = UD_limbf[UD_limbfptr].fsech;
			}

/*			-- generate character count of the data return element -- */
	
			increment = sizeof(UD_PLANE_REC);
			locsize = 1;

			break;

		default:

/*---		" invalid das request from ud_ddas"		---*/

			uu_uerror0(UD_DASHEP, 9);
			break;
	}

/*	-- set the proper help level -- */

	switch(type)
	{
		case UD_SCACART:
		case UD_SCACART2:
		case UD_DASCART:

			UD_dasnum = 18;
			break;

		case UD_DASNDC:
		case UD_SCANDC:

			UD_dasnum = 28;
			break;

		case UD_DASVEC:
		case UD_SCAVEC:
		case UD_SCAVEC2:

			UD_dasnum = 23;
			break;

		case UD_DASINT:
		case UD_SCAINT:

			UD_dasnum = 22;
			break;

		case UD_DASANGLE:
		case UD_SCAANGLE:

			UD_dasnum = 29;
			break;

		case UD_DASUNITLESS:
		case UD_SCAUNITLESS:

			UD_dasnum = 30;
			break;

		case UD_SCAVAL:
		case UD_SCADISTANCE:
		case UD_DASVAL:
		case UD_DASDISTANCE:

			UD_dasnum = 20;
			break;

		case UD_DASSELECT:

			UD_dasnum = 19;
			break;

		case UD_DASPICK:
		case UD_DASPCKLOC:

			UD_dasnum = 25;
			break;

		case UD_DASSTRING:
		case UD_DASSTRINGDEF:

			UD_dasnum = 26;
			break;

		case UD_DASCHOICE:
		case UD_POPUP:

			UD_dasnum = 21;
			break;

	}

/* --		initialize loop variables   -- */

	locinc = 0;
	definc = 0;
	*numint = 0;

/*		loop while still room in buffer and operator does not signal completion */

	while(*numint < locsize)
	{
		switch(type)
		{
			case UD_SCACART:
			case UD_SCANDC:
			case UD_DASCART:
			case UD_DASNDC:

				dastat = ud_cart(prompt, &buffer[locinc], defflag, &buffer[definc],
									xflag);
				break;
			case UD_SCACART2:
				dastat = ud_cart2(prompt, &buffer[locinc], defflag, &buffer[definc],
									xflag);
				break;
			case UD_SCAVEC:
				dastat = ud_scavec(prompt, &buffer[locinc], defflag, &buffer[definc]);
				break;
			case UD_SCAVEC2:
				dastat = ud_scavec2(prompt, &buffer[locinc], defflag, &buffer[definc]);
				break;
			case UD_DASVEC:

				dastat = ud_vec(prompt, &buffer[locinc], defflag, &buffer[definc]);
				break;
			case UD_SCAVAL:
			case UD_SCADISTANCE:
				dastat = ud_scaval(prompt, &buffer[locinc], defflag, &buffer[definc],
									UD_DISTANCE);
				break;
			case UD_DASVAL:
			case UD_DASDISTANCE:

				dastat = ud_val(prompt, &buffer[locinc], defflag, &buffer[definc],
									UD_DISTANCE);
				break;
			case UD_SCAANGLE:
				dastat = ud_scaval(prompt, &buffer[locinc], defflag, &buffer[definc], 
									UD_DANGLE);
				break;
			case UD_DASANGLE:

				dastat = ud_val(prompt, &buffer[locinc], defflag, &buffer[definc], 
									UD_DANGLE);
				break;

			case UD_SCAUNITLESS:
				dastat = ud_scaval(prompt, &buffer[locinc], defflag, &buffer[definc], 
									UD_UNITLESS);
				break;
			case UD_DASUNITLESS:

				dastat = ud_val(prompt, &buffer[locinc], defflag, &buffer[definc], 
									UD_UNITLESS);
				break;

			case UD_SCAINT:
				dastat = ud_scaint(prompt, &buffer[locinc], defflag, &buffer[definc]);
				break;
			case UD_DASINT:
				dastat = ud_int(prompt, &buffer[locinc], defflag, &buffer[definc]);
				break;

			case UD_DASPICK:

				dastat = ud_pickdum(prompt, &buffer[locinc]);
				break;

			case UD_DASPCKLOC:

				dastat = ud_ploc(prompt, &buffer[locinc]);
				break;

			case UD_DASSCALAR:
				dastat = ud_str(prompt, buffer, defflag, buffer, size, numint);
/*
.....check if the input is the valid scalar
*/
				stat = ncl_get_scalar_value(buffer, &value);
				if (stat==-1)
				{
					sprintf(erms,"%s is not a valid scalar value",buffer);
					ud_wrerr(erms);
					goto restart;
				}
				break;
			case UD_DASSTRING:

				dastat = ud_str(prompt, buffer, defflag, buffer, size, numint);
				break;

			case UD_DASSTRINGDEF:

				dastat = ud_strdef(prompt, buffer, defflag, buffer, size, numint);
				break;

			case UD_DASCHOICE:

				dastat = ud_chc(prompt, &buffer[locinc], size);
				break;

			case UD_POPUP:

				dastat = ud_popup(prompt, &buffer[locinc]);
				break;

			case UD_DASSELECT:

/*       check for operator control entered        */

/* NCL - added dastat= for altaction. kathy */
			dastat = ud_select(prompt, NULL) ;

			if (dastat == DE_ALTACT)
				break;

/*				-- see if number of interactions are DAS limited -- */

				if(ud_cnmint(UD_Select_cnt) == UU_FALSE)
					goto restart;
				dastat = DE_DONE;
				*numint = UD_Select_cnt;
				break;

		case UD_DASPLANE:
				dastat = ud_plane(prompt, buffer);
				break;

			default:

/*				-- "illegal das code " --       */

				break;
		}

/*			check for operator control entered        */

		switch(dastat)
		{

/*			-- normal input -- */

			case DE_TRUE:

				if(locinc > 0)
					definc = definc + increment;
				locinc = locinc + increment;
				if(((type!=UD_POPUP) && type!=UD_DASCHOICE) && (type!=UD_DASSTRING))
				{
					(*numint)++;

					if((*numint) >= locsize)
					{
						UD_dassys = 0;
						uu_dexit;
						return(UU_TRUE);
					}
				}
				else
				{
					UD_dassys = 0;
					uu_dexit;
					return(UU_TRUE);
				}
				break;


/*			-- operation complete -- */

			case DE_DONE:

/*				-- see if number of interactions are DAS limited -- */

				if(ud_cnmint(*numint) == UU_FALSE)
					goto restart;

				UD_dassys = 0;
				uu_dexit;
				return(UU_TRUE);
				break;

/*					entry reject              */

			case DE_RLAST:

				if(*numint > 0)
				{
					(*numint)--;
					locinc = locinc - increment;
				}
				break;

/*					return to parser    */

			case DE_RCOMMAND:

				UD_dassys = 0;
				uu_dexit;
				return(UU_FALSE);
				break;

/*					restart input sequence    */

			case DE_RESTART:

				goto restart;
				break;

/*			-- application alternate action          */

			case DE_ALTACT:

				UD_dassys = 0;
				uu_dexit;
				return(UU_ALTACTION);
				break;

/* NCL -             up or down arrows          */

         case DE_UPFIELD:
         case DE_DNFIELD:

            UD_dassys = 0;
            return(UU_TRUE) ;
            break ;
		}
	}
	return(UU_FALSE);
}

/*********************************************************************
**
**    E_FUNCTION     :  UU_LOGICAL ud_lyesno(subnum, prnum)
**			query the operator for a yes or no answer
**
**    PARAMETERS   
**       INPUT  : 
**				subnum = prompt subsystem number for UNIHEP
**				prnum  = prompt number
**       OUTPUT :  
**          none
**
**    RETURNS      : UU_TRUE is yes and UU_FALSE if no
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

UU_LOGICAL ud_lyesno(subnum, prnum)
int subnum;
int prnum;
{
	char *uu_uprompt0(), msg[100];
	UU_LOGICAL status, ud_yesno();

	uu_denter(UU_DTRC,(us,"enter ud_lyesno, subsys=%d, number=%d", 
						subnum, prnum));

	strcpy(msg, uu_uprompt0(subnum, prnum));
	status = ud_yesno(0, msg, "Question?");
	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  E_FUNCTION:  ud_hakt(subsys, prmnum)
**      prompt the operator to hit any key to continue
**
**  PARAMETERS   
**      INPUT:  subsys = HEP subsystem number
**					 prmnum = HEP prompt number
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

void ud_hakt(subsys, prmnum)
int subsys;					/* subsystem number */
int prmnum;					/* prompt number */
{

	char *uu_uprompt0();
	UD_DEVENT event;						/* event buffer */
	char pbuf[100];						/* prompt buffer */
	UU_LOGICAL saveim;					/* save input mapping flag */
	static UD_EVENTDEF inparm = {1, ""};	/* string input parameters */


/*	-- get the prompt to hit any key to continue -- */

	strcpy(pbuf, uu_uprompt0(subsys, prmnum));

/*	-- write out the prompt message -- */

	ud_rpwrcom(pbuf);
	ud_wrprm(pbuf);

/*	-- disable input mapping -- */

	saveim = UD_enableim;
	UD_enableim = UU_FALSE;

/*	-- get any input -- */

	ud_gevt(&event, UD_STRING, pbuf, 1, 1, 1, &inparm);
	UD_enableim = saveim;

/*	-- bring down the prompt -- */

	ud_killmsg(UD_PROMPTMSG);
}

/*********************************************************************
**
**    E_FUNCTION     :  ud_fyesno(parent, msg, title)
**       A FORTRAN callable routine to ask user answer for yes or no
**			for Motif interface only
**    PARAMETERS
**       INPUT  :
**			parent: parent window
**         msg:   Message display in question box 
**         title: Title for question box
**       OUTPUT :
**          none
**
**    RETURNS      : UU_TRUE is yes and UU_FALSE if no
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
int ud_fyesno(parent, msg, nc1, title, nc2)
char *msg, *title;
int *parent, *nc1, *nc2;
{
	int answer;
	char cmsg[500], ctitle[500];
	strncpy(cmsg, msg, *nc1);
	cmsg[*nc1] = '\0'; 
	strncpy(ctitle, title, *nc2);
	ctitle[*nc2] = '\0'; 

	answer = (*(ug_gksstli.wsopen[0].connid)[UW_YESNOCANCEL])(*parent, cmsg,  ctitle);
	if (answer==-1)
	{
/*
.....if canceled, act as Rejust Op
*/
		ud_jump(-1, UU_FALSE);
	}
	return answer;
}
