/*********************************************************************
**
**    NAME         :  d2das.c
**
**       CONTAINS:
**				ud_ddas
**				ud_lyesno
**				int ud_fyesno(parent, msg, nc1, title, nc2)
**				ud_strdef
**				ud_vec
**				ud_vec1
**				ud_str
**				ud_str1
**				ud_val
**				ud_val1
**				ud_cart
**				ud_cart1
**
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        ipvdas.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:12:57
**
*********************************************************************/

#include "usysdef.h"
#include "lipv.h"
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
#include "mdcpln.h"
#include "uims.h"
#include "calcom.h"
#include "gmat4.h"
#include "ginqxf.h"

int UD_textpre_curpos = 0;
int UD_string_add = 0;
static int UD_cart_part = 0;
static UU_LOGICAL vallimtst(value)
#define LOCREC (*event).indata.locdata

UU_REAL value;					/* value to constrain */
{

	if(UD_LIMIT.lpran == UU_TRUE)
	{
		if(value<=UD_LIMIT.lpranhi && value>=UD_LIMIT.lpranlo)
			return(UU_TRUE);
		else
		{

/*			-- value constrained between %g and %g -- */

			uu_uerror2(UD_DASHEP, 72, UD_LIMIT.lpranlo, UD_LIMIT.lpranhi);
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
			case UD_DASVEC:

				dastat = ud_vec(prompt, &buffer[locinc], defflag, &buffer[definc]);
				break;
			case UD_DASVAL:
			case UD_DASDISTANCE:

				dastat = ud_val(prompt, &buffer[locinc], defflag, &buffer[definc],
									UD_DISTANCE);
				break;
			case UD_DASANGLE:

				dastat = ud_val(prompt, &buffer[locinc], defflag, &buffer[definc], 
									UD_DANGLE);
				break;
			case UD_DASUNITLESS:

				dastat = ud_val(prompt, &buffer[locinc], defflag, &buffer[definc], 
									UD_UNITLESS);
				break;
			
			case UD_DASSTRING:

				dastat = ud_str(prompt, buffer, defflag, buffer, size, numint);
				break;

			case UD_DASSTRINGDEF:

				dastat = ud_strdef(prompt, buffer, defflag, buffer, size, numint);
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
/********************************************************************* 
**
**  I_FUNCTION		:  ud_strdef(prompt, ret_str, defflag, def_str, mxlen, len)
**      character string with default high level DAS
**
**  PARAMETERS   
**
**      input:  prompt = operator prompt string
**					 mxlen = input buffer length
**					 defflag = default exists flag
**					 def_str = default string
**
**      output: ret_str =  string to return
**					 len     =  length of string input
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_strdef(prompt, ret_str, defflag, def_str, mxlen, len)
char *prompt; 						/* operator prompt string  */
UD_STRREC *ret_str;					/* string to return  */
UU_LOGICAL defflag;					/* default exists flag */
UD_STRREC *def_str;					/* default string */
int mxlen;							/* buffer length */
int *len;							/* length of string input */
{

	UD_DEVENT event;				/* event buffer */
	UD_DASTAT status;				/* status return cell */
	UD_DASTAT ud_str1();			/* semantic interpreter */
	UD_EVENTDEF inparm;				/* string input parameters */
	static char locstr[80] = "\0";	/* local string buffer */
    char locprm[200];				/* copy of prompt while in do loop */
    char *errstr ="There is no space for continuation sign ($). Re-enter.";

	int evtype,evecho,evdev,pos;
	UD_DASTAT ud_pick1();
	UD_PPICKREC ret_pck;
	char tempstr[500];

	uu_denter(UU_DTRC,(us,"entering ud_strdef"));

	UD_string_add = 0;
	inparm.strbfsz = mxlen;
	inparm.defstr = (*def_str).instring;

	strcpy(locprm,prompt); 
	if(strlen(locstr) > 0)
	{
		strcat(inparm.defstr,locstr);
		strcpy(locstr,"\0");
	}
/*.....*/ 
 
	do
	{
        strcpy(locprm,prompt);

		ud_gevt(&event, UD_strint, locprm, 1, UD_strdev, UD_strech, &inparm);
		status = ud_str1(&event, (*ret_str).instring, defflag, 
			(*def_str).instring, mxlen, len);
        if(event.evclass == UD_LOCATOR) goto forse;

again:;
        if((status!=DE_AGAIN) && (inparm.termcon==UD_PARTERM))
		{
/*			-- if we get a partial string status and string input was OK, then
				the input was terminated by a non carriage return and we have to
				look on the Q for the terminating event -- */

			if(gqsize() > 0)
			{
/*
.....
..... Added by Paul for "command" input. 08/26/92
.....
*/
forse:;
                strcpy(locprm,prompt);
                strcat(locprm,"  ");
                strcat(locprm,def_str->instring);
/*.....*/

/*
.....Allow VERIFY mode to work when toggling
.....to PICK mode from within a form
.....Bobby  -  5/24/94
*/
				status = DE_AGAIN;
				evtype = UD_strint;
				evdev  = UD_strdev;
				evecho = UD_strech;
				while (status == DE_AGAIN)
				{
					ud_gevt(&event, evtype, locprm, 1, evdev, evecho, 
							&inparm);
force2:;
				}
				status = ud_str1(&event, locstr, defflag, def_str, mxlen, len);
                if (status == DE_TRUE)
				{
                    if(event.evclass == UD_PICK || event.evclass == UD_LOCATOR)
                    {
                        if(strlen(def_str->instring) >= mxlen && *len > 0)
                        {
                            ud_wrerr(errstr);
                            strcpy(locstr,"\0");
                            status = DE_AGAIN;
                            goto again;
                        }
                        if(strlen((*def_str).instring)+ (*len) >= mxlen)
                        {
                            strcat(def_str->instring,"$");
                            *len = strlen(def_str->instring);
                            status = DE_TRUE; 
                            goto again;
                        }
/*
.....should not put in the end of string but insert to the previous insert cursor position.
*/
						pos = UD_textpre_curpos;
                        if (pos>=strlen(def_str->instring))
                			strcat(def_str->instring,locstr);
						else
						{
							strncpy(tempstr, def_str->instring, pos);
							tempstr[pos] = '\0';
							strcat(tempstr, locstr);
							UD_textpre_curpos = strlen(tempstr);
							strcat(tempstr, &(def_str->instring[pos]));
							strcpy(def_str->instring, tempstr);
							UD_string_add = 1;
						}
                        *len = strlen(def_str->instring);
                        strcpy(locstr,"\0");
                	    status = DE_AGAIN;
                    }
                    else if (event.evclass == UD_STRING)
                    {
                        strcpy(def_str->instring,locstr);
                        strcpy(locstr,"\0");
                        goto again;
                    }
				}
/*.....*/
			}
		}
		(*ret_str).termcon = status;
	}
	while(status == DE_AGAIN);
	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION		:  ud_val(prompt, ret_val, defflag, def_val, typeflag)
**      valuator high level DAS
**
**  PARAMETERS   
**
**      input:  prompt = operator prompt string
**					 defflag = default exists flag
**					 def_val = default value
**
**      output: ret_val = valuator to return
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_val(prompt, ret_val, defflag, def_val, typeflag)
char *prompt; 				/*   operator prompt string  */
UU_REAL *ret_val;			/*   value to return  */
UU_LOGICAL defflag;		/*   default exists flag */
UU_REAL *def_val;			/*   default value */
int typeflag;				/*  type of value requested (UD_DISTANCE, UD_DANGLE, or
												UD_UNITLESS */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_val1();				/* semantic interpreter */
	UD_DEVENT event;					/* input event buffer */
	char *ud_undist(), *ud_unang();
	char pbuf[100];					/* prompt buffer */
	int prec;							/* field length/precision */
	int len;								/* min field length */

	uu_denter(UU_DTRC,(us,"entering ud_val"));

/*	-- build the prompt message -- */

	if(defflag == UU_TRUE)
	{
		prec = 3;
		len = 7;
		switch(typeflag)
		{
			case UD_DISTANCE:
				sprintf(pbuf, "%s %s [%s]", prompt, UD_syndis, 
					ud_undist(prec, len, *def_val));
				break;
			case UD_DANGLE:
				sprintf(pbuf, "%s %s [%s]", prompt, UD_synang, 
					ud_unang(prec, len, *def_val));
				break;
			case UD_UNITLESS:
				sprintf(pbuf, "%s %s [%g]", prompt, UD_synul, *def_val);
				break;
		}
	}
	else
	{
		switch(typeflag)
		{
			case UD_DISTANCE:
				sprintf(pbuf, "%s %s", prompt, UD_syndis);
				break;
			case UD_DANGLE:
				sprintf(pbuf, "%s %s", prompt, UD_synang);
				break;
			case UD_UNITLESS:
				sprintf(pbuf, "%s %s", prompt, UD_synul);
				break;
		}
	}

	do
	{
		ud_gevt(&event, UD_valint, pbuf, 1, UD_valdev, UD_valech, NULL);
		status = ud_val1(&event, ret_val, defflag, def_val, typeflag);
	}
	while(status == DE_AGAIN);

	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION			:  ud_vec(prompt, ret_vec, defflag, def_vec)
**      vector high level DAS
**
**  PARAMETERS   
**
**      input:  prompt = operator prompt string
*8					 defflag = default exists flag
**					 def_vec = default coordinate
**
**      output: ret_vec =  coordinate to return
**
**  RETURNS      : status of operation
**  SIdE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_vec(prompt, ret_vec, defflag, def_vec)
char *prompt ; 					/* operator prompt string  */
UU_REAL ret_vec[3] ;					/* vector to return  */
UU_LOGICAL defflag;					/* default exists flag */
UU_REAL def_vec[3];					/* default vector */
{

	UD_DASTAT status;							/* status return cell */
	UD_DASTAT ud_vec1();						/* semantic interpreter */
	UD_DEVENT event;							/* event buffer */
	char pbuf[100];
	char *ud_unvec();

	uu_denter(UU_DTRC,(us,"entering ud_vec"));

/*	-- set up the next prompt -- */

	if(defflag == UU_TRUE)
	{
		sprintf(pbuf, "%s %s [%s]",
			prompt, UD_synwc, ud_unvec(3, 8, def_vec));
	}
	else
		sprintf(pbuf, "%s %s", prompt, UD_synvec);

/*
.....Initialize verify counter
*/
	do
	{
		ud_gevt(&event, UD_vecint, pbuf, 1, UD_vecdev, UD_vecech, NULL);
/*
.........Test for VERIFY is made in ud_vec1() since we have the 'event'
.........data parsed into the format expected by ncl_verify_pick(). - RAZ
*/
		status = ud_vec1(&event, ret_vec, defflag, def_vec);
	}
	while(status == DE_AGAIN);

	uu_dexit;
	return(status);
}
/********************************************************************* 
**
**  I_FUNCTION		:  ud_str1(event, ret_str, defflag, def_str, mxlen, len)
**      character string high level DAS
**
**  PARAMETERS   
**
**      input:  event = event structure
**					 mxlen = input buffer length
**					 defflag = default exists flag
**					 def_str = default string
**
**      output: ret_str =  string to return
**					 len     =  length of string input
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_str1(event, ret_str, defflag, def_str, mxlen, len)
UD_DEVENT *event; 				/*   event structure  */
char *ret_str;						/*   string to return  */
UU_LOGICAL defflag;				/*   default exists flag */
char *def_str;						/*   default string */
int mxlen;							/*   buffer length */
int *len;							/*   length of string input */
{

	int loclen;						/* string length */
	UD_DASTAT status;				/* status return cell */
	UD_DASTAT ud_auxm();
    char str_tmp[81];
    UD_NDCLOCREC coord;
	int save_active = 1;

	uu_denter(UU_DTRC,(us,"entering ud_str1"));

	status = DE_TRUE;
	if((*event).evclass == UD_STRING)
	{
		loclen = strlen((*event).indata.stringdata);
		if((loclen <= mxlen) ||
           (loclen == mxlen+1 && event->indata.stringdata[loclen-1] == '\\'))
		{
			strcpy(ret_str, (*event).indata.stringdata);

/*			-- remove "\" if partial string -- */

			if(loclen > 0)
				if(ret_str[loclen-1] == '\\')
				{
					ret_str[loclen-1] = '\0';
					loclen--;
				}

			*len = loclen;
			status = DE_TRUE;
		}
		else
		{

			strncpy(ret_str, (*event).indata.stringdata,mxlen-1);
			ret_str[mxlen-1] = '\0';
			uu_uerror0(UD_DASHEP, 36);
			status = DE_AGAIN;
		}
	}
	else if((*event).evclass == UD_CHOICE && (*event).evdev == UD_AUXMENU)
	{
		status = ud_auxm(event);
		if(status == DE_DEFAULT)
		{
			if(defflag == UU_TRUE)
			{
				status = DE_TRUE;
				*len = strlen(def_str);
				strcpy(ret_str, def_str);
			}
			else
			{
				uu_uerror0(UD_DASHEP, 3);
				status = DE_AGAIN;
			}
		}
	}
    else
	{
/*---		" wrong event type"		---*/

    uu_uerror0(UD_DASHEP, 38);
    status = DE_AGAIN;
	}
/*
	-- return status --
*/
	if(status == DE_TRUE)
		uu_dprint(UU_DTRC,(us,"leave ud_str1, len=%d, str=%s", *len, ret_str));

	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION		:  ud_val1(event, ret_val, defflag, def_val, typeflag)
**      valuator high level DAS
**
**  PARAMETERS   
**
**      input:  event = event structure
**					 defflag = default exists flag
**					 def_val = default value
**					 typeflag = type of value requested
**
**      output: ret_val = valuator to return
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_val1(event, ret_val, defflag, def_val, typeflag)
UD_DEVENT *event;						/* event structure */
UU_REAL *ret_val;						/*   value to return  */
UU_LOGICAL defflag;					/*   default exists flag */
UU_REAL *def_val;						/*   default value */
int typeflag;							/*  type of value requested (UD_DISTANCE, 
												UD_DANGLE, or UD_UNITLESS */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_auxm();
	UD_DASDATA dsda;					/* return buffer from kb coordinate parser */
	UU_REAL value;
	char stringdata[100];
	int stat = 0;
	
	uu_denter(UU_DTRC,(us,"entering ud_val1"));

	status = DE_TRUE;
	if((*event).evclass == UD_VALUATOR)
	{
		status = DE_TRUE;

		if(typeflag == UD_DISTANCE)
		{
			 UM_len_exttoint((*event).indata.valdata, *ret_val);
		}
		else if(typeflag == UD_DANGLE)
		{
			 UM_ang_exttoint((*event).indata.valdata, *ret_val);
		}
		else
			*ret_val = (*event).indata.valdata;

/*			-- set up default interactions -- */

		UD_valint = UD_VALUATOR;

	}
	else if((*event).evclass == UD_STRING)
	{
		strcpy(stringdata, (*event).indata.stringdata);

		if(ud_dasin(stringdata, &dsda, typeflag) == UU_FALSE)
			goto retagain;
		else
		{
			switch(dsda.dtype)
			{
/*					-- incremental form input; error -- */

				case UD_DNINCREMENT:
				case UD_DQINCREMENT:
		
/*---				"incremental form not allowed in real number input");		---*/

					uu_uerror0(UD_DASHEP, 42);
					goto retagain;
					break;

/*					-- reference point; move to reference modal location -- */

				case UD_DREFERENCE:

/*---				"reference coordinate definition not allowed");		---*/

					uu_uerror0(UD_DASHEP, 43);
					goto retagain;
					break;

/*					-- regular coordinate input; error -- */

				case UD_DCOORDINATE:

/*---				"coordinate form not allowed in real number input");		---*/

					uu_uerror0(UD_DASHEP, 44);
					goto retagain;
					break;

/*					-- scalar input -- */

				case UD_DSCALAR:

					status = DE_TRUE;
					*ret_val = dsda.stval.dval;
					break;

/*					-- garbage input -- */

				default:

/*---				"unrecognizable input in real number input"		---*/

					uu_uerror0(UD_DASHEP, 45);
					goto retagain;

			}
		}

/*			-- set up default interactions -- */

		UD_valint = UD_STRING;
	}
	else if((*event).evclass == UD_CHOICE)
	{
		if((*event).evdev == UD_AUXMENU)
		{

			status = ud_auxm(event);
			if(status == DE_DEFAULT)
			{
				if(defflag == UU_TRUE)
				{
					status = DE_TRUE;
					*ret_val = *def_val;
				}
				else
				{

/*---				"no default in effect"		---*/

					uu_uerror0(UD_DASHEP, 3);
					goto retagain;
				}
			}
		}
		else
		{

/*---		"invalid choice event in UD_DASVAL"		---*/

			uu_uerror0(UD_DASHEP, 47);
			goto retagain;
		}
	}

	else
	{

/*---	" invalid event type"		---*/

		uu_uerror0(UD_DASHEP, 47);
		goto retagain;
	}

	if(status == DE_TRUE)
	{
		if(vallimtst(*ret_val) == UU_FALSE)
			goto retagain;
	}
/* -- return value -- */

	uu_dexit;
	return(status);

retagain:
	uu_dexit;
	return(DE_AGAIN);
}

/********************************************************************* 
**
**  I_FUNCTION			:  ud_vec1(event, ret_vec, defflag, def_vec)
**      vector high level DAS
**
**  PARAMETERS   
**
**      input:  event = event structure
**					 defflag = default exists flag
**					 def_vec = default coordinate
**
**      output: ret_vec =  coordinate to return
**
**  RETURNS      : status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_vec1(event, ret_vec, defflag, def_vec)
UD_DEVENT *event;						/* event structure */
UU_REAL ret_vec[3];					/* vector to return  */
UU_LOGICAL defflag;					/* default exists flag */
UU_REAL def_vec[3];					/* default vector */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_auxm();
	UU_REAL x, y, z ;					/* coordinate storage   */
	UD_DASDATA dsda;					/* return buffer from kb coordinate parser */
	UU_REAL cord[3];
	static UU_REAL origin[3] = {0.,0.,0.};	/* origin coordinate */
	int stat;
	UD_PPICKREC picker;
	UD_NDCLOCREC pndc;
	int i;
	char strbuf[100], erms[256];
#define PICKPTR (*event).indata.pickdata

	uu_denter(UU_DTRC,(us,"entering ud_vec1"));

	status = DE_TRUE ;
	switch( (*event).evclass )
	{
		case UD_LOCATOR :

/*---		"screen position input not allowed in vector input"		---*/

			uu_uerror0(UD_DASHEP, 49);
			goto retagain;
			break ;
		case UD_VALUATOR :
/*---		"valuator input not allowed in vector input"		---*/

			uu_uerror0(UD_DASHEP, 77);
			goto retagain;

			break ;
		case UD_CHOICE :

			if((*event).evdev == UD_AUXMENU)
			{
				status = ud_auxm(event);
				if(status == DE_DEFAULT)
				{
					status = DE_TRUE;
					if(defflag == UU_TRUE)
					{
						status = DE_TRUE;
						x = def_vec[0];
						y = def_vec[1];
						z = def_vec[2];
					}
					else
					{

/*---					"no default in effect"		---*/

						uu_uerror0(UD_DASHEP, 3);
						goto retagain;
					}
				}
			}
			else
			{

/*---			"choice device inconsistent in UD_DASVEC"		---*/

				uu_uerror0(UD_DASHEP, 53);
				goto retagain;
			}
			break ;
		case UD_STRING :
			if(ud_dasin((*event).indata.stringdata, &dsda, UD_DISTANCE) == UU_FALSE)
				goto retagain;
			else
			{
				switch(dsda.dtype)
				{

/*					-- incremental form input; initialize and go input -- */

					case UD_DNINCREMENT:
					case UD_DQINCREMENT:
			
/*---				"incremental form not allowed in vector input"		---*/

						uu_uerror0(UD_DASHEP, 54);
						goto retagain;
						break;

/*					-- reference point; move to reference modal location -- */

					case UD_DREFERENCE:

/*---					"reference coordinate not allowed in vector input"		---*/

						uu_uerror0(UD_DASHEP, 55);
						goto retagain;
						break;

/*					-- regular coordinate input; convert to cartesian and 
						return normally -- */

					case UD_DCOORDINATE:

						um_cotovc(dsda.stval.stcord.coord, dsda.stval.stcord.cordtype,
							cord);
						um_ccstomcs(1, cord, cord);
						x = cord[0];
						y = cord[1];
						z = cord[2];
						break;

/*					-- scalar input; error -- */

					case UD_DSCALAR:

/*---					"scalar form not allowed in vector input"		---*/

						uu_uerror0(UD_DASHEP, 56);
						goto retagain;
						break;

/*					-- garbage input -- */

					default:

/*---					"unrecognizable form in vector input"		---*/

						uu_uerror0(UD_DASHEP, 57);
						goto retagain;

				}
			}

/*			-- set up default interactions -- */

			UD_vecint = UD_STRING;

			break ;

		case UD_VECTOR :

			UM_cc_exttoint((*event).indata.vecdata, cord);

			x = cord[0];
			y = cord[1];
			z = cord[2];

/*			-- set up default interactions -- */

			UD_vecint = UD_PICK;

			break ;

		default :
			
/*---		" invalid event in UD_DASVEC"		---*/

			uu_uerror0(UD_DASHEP, 58);
			goto retagain;
			break ;
	}

/* -- return vector -- */

	ret_vec[0] = x;
	ret_vec[1] = y;
	ret_vec[2] = z;

/*	-- check if vector is 0,0,0 -- */

	if(status == DE_TRUE)
	{
		if(um_cceqcc(ret_vec, origin) == UU_TRUE)
		{

/*---		"invalid vector of <0,0,0>"		---*/

			uu_uerror0(UD_DASHEP, 59);
			goto retagain;
		}
	}

	uu_dexit;
	return(status);

retagain:
	uu_dexit;
	return(DE_AGAIN);
}

/********************************************************************* 
**
**  I_FUNCTION		:  ud_str(prompt, ret_str, defflag, def_str, mxlen, len)
**      character string high level DAS
**
**  PARAMETERS   
**
**      input:  prompt = operator prompt string
**					 mxlen = input buffer length
**					 defflag = default exists flag
**					 def_str = default string
**
**      output: ret_str =  string to return
**					 len     =  length of string input
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_str(prompt, ret_str, defflag, def_str, mxlen, len)
char *prompt ; 					/*   operator prompt string  */
char *ret_str ;					/*   string to return  */
UU_LOGICAL defflag;				/*   default exists flag */
char *def_str ;					/*   default string */
int mxlen;							/*   buffer length */
int *len;							/*   length of string input */
{

	UD_DEVENT event;						/* event buffer */
	UD_DASTAT status;						/* status return cell */
	UD_DASTAT ud_str1();					/* semantic interpreter */
	UD_EVENTDEF inparm;					/* string input parameters */
	char pbuf[100];						/* prompt character buffer */

	uu_denter(UU_DTRC,(us,"entering ud_str"));

/*	-- build the prompt message -- */

	if(defflag == UU_TRUE)
		sprintf(pbuf, "%s%s [%s]", prompt, UD_synstr, def_str);
	else
		sprintf(pbuf, "%s%s", prompt, UD_synstr);

	inparm.strbfsz = mxlen;
	inparm.defstr = "";

	do
	{
		ud_gevt(&event, UD_strint, pbuf, 1, UD_strdev, UD_strech, &inparm);
		status = ud_str1(&event, ret_str, defflag, def_str, mxlen, len);
	}
	while(status == DE_AGAIN);

	uu_dexit;
	return(status);
}
/********************************************************************* 
**
**  I_FUNCTION		:  ud_cart(prompt, ret_cord, defflag, def_cord, xflag)
**      coordinate high level DAS routine
**
**  PARAMETERS   
**      INPUT:  prompt = operator prompt string
**					 defflag = default exists flag
**					 def_cord = default coordinate
**					 xflag = NDC space flag if UU_FALSE
**      OUTPUT: ret_cord =  coordinate to return
**
**  RETURNS      : status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_cart(prompt, ret_cord, defflag, def_cord, xflag)
char *prompt;	 					/* operator prompt string  */
UD_NDCLOCREC *ret_cord;			/* coordinate to return  */
UU_LOGICAL defflag;				/* default exists flag */
UD_NDCLOCREC *def_cord;			/* default coordinate */
UU_LOGICAL xflag;					/* NDC flag (UU_TRUE=W.C, UU_FALSE=NDC) */
{

	UD_DASTAT status;						/* status return cell */
	UD_DASTAT ud_cart1();
	UD_DEVENT event;						/* event buffer */
	UU_REAL cord[3];						/* coordinate buffer */
	char pbuf[100];						/* character buffer */
	UD_EVENTDEF inparm;					/* string input parameters */
	char instring[256];

/* --- start of executable code --- */

	uu_denter(UU_DTRC,(us,"entering ud_cart, UD_locint=%d", UD_locint));

/*	-- set up the next prompt -- */

	if(defflag == UU_TRUE)
	{

/*		-- convert to construction coordinate system */

		cord[0] = (*def_cord).cord[0];
		cord[1] = (*def_cord).cord[1];
		cord[2] = (*def_cord).cord[2];

		if(xflag == UU_TRUE)
		{
			sprintf(pbuf, "%s %s [%s]",
				prompt, UD_synwc, ud_uncord(3, 8, cord));
		}
		else
		{
			sprintf(pbuf, "%s %s [<%g,%g,%g>]", 
				prompt, UD_synndc, cord[0], cord[1], cord[2]);
		}
	}
	else
	{
		if(xflag == UU_TRUE)
			sprintf(pbuf, "%s %s", prompt, UD_synwc);
		else
			sprintf(pbuf, "%s %s", prompt, UD_synndc);
	}

	do
	{
		if ((UD_cart_part) && (UD_locint==UD_STRING) && (event.indata.stringdata!=NULL))
		{
			strcpy(instring, event.indata.stringdata);
			inparm.defstr = instring;
			inparm.strbfsz = 256;
			ud_gevt(&event, UD_locint, pbuf, 1, UD_locdev, UD_locech, &inparm);
		}
		else
			ud_gevt(&event, UD_locint, pbuf, 1, UD_locdev, UD_locech, NULL);
		UD_cart_part = 0;
		status = ud_cart1(&event, ret_cord, defflag, def_cord, xflag);
	}
	while(status == DE_AGAIN);

	UD_cart_part = 0;
	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION		:  ud_cart1(event, ret_cord, defflag, def_cord, xflag)
**      coordinate high level DAS routine
**
**  PARAMETERS   
**      INPUT:  event = event structure
**					 defflag = default exists flag
**					 def_cord = default coordinate
**					 xflag = NDC space flag if UU_TRUE
**      OUTPUT: ret_cord =  coordinate to return
**
**  RETURNS      : status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_cart1(event, ret_cord, defflag, def_cord, xflag)
UD_DEVENT *event;					/* event structure */
UD_NDCLOCREC *ret_cord;		/* coordinate to return  */
UU_LOGICAL defflag;				/* default exists flag */
UD_NDCLOCREC *def_cord;		/* default coordinate */
UU_LOGICAL xflag;					/* NDC flag (UU_TRUE=W.C., UU_FALSE=NDC) */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_auxm();
	int i;								/* temp variable */
	int ix, iy;							/* temp integers */
	UU_REAL dxx, dyy;					/* temp gridding */
	UU_REAL x, y, z;					/* coordinate storage */
	Gwpoint3 markpnt;					/* GKS structure for point echo on screen */
	UU_REAL cord[3], cord1[3];		/* Model coordinate structures */
	UD_NDCLOCREC ploc;				/* pickloc record for nearest endpoint */
	UD_PPICKREC picker;				/* pickloc record for nearest endpoint */
	UD_DASDATA dsda;					/* return buffer from kb coordinate parser */
	int stat;							/* status */
	UD_AREA *grafarea;				/* UIMS layout pointer */
	CRSLT stbptr;						/* symbol table entry pointer */
	UD_DASTAT ncl_verify_pick();
	char tempstr[80];	
	int assoc_buf,strln, len;
	UU_KEY_ID j, key; 
	char strbuf[100], erms[256];

	int xy[2];
	LtDoublePoint orig;
	LtDoubleVector norm;

	strcpy(ret_cord->label,"      ");

	uu_denter(UU_DTRC,(us,"entering ud_cart1 xflag=%d, UD_locint=%d", 
								xflag, UD_locint));

	status = DE_TRUE ;
	(*ret_cord).choice = (*event).evclass;
	switch((*event).evclass)
	{
		case UD_LOCATOR:
			(*(ug_gksstli.wsopen[*UD_ksws].connid)[UG_DNDCDEV]) 
				(LOCREC.position, xy, *UD_ksws);
			LiDrawableProjectImageToWorld(LW_drawable,LW_viewport, xy, orig, norm);

			x = cord1[0] = orig[0];
			y = cord1[1] = orig[1];
			z = cord1[2] = orig[2] = 0;

			if((*event).evclass != UD_locint)
			{
				UD_locint = UD_LOCATOR;
				UD_locech = 1;
				UD_locdev = 1;
			}
			break ;

		case UD_VALUATOR:

/*---		"valuator input not allowed in coordinate DAS"  ----*/

			uu_uerror0(UD_DASHEP, 1);
			goto retagain;
			break ;

		case UD_STRING:
			len = strlen((*event).indata.stringdata);
			if ((len>0)&&((*event).indata.stringdata[len-1] == '\\'))
			{
/*
......remove "\" if partial string and goto do again to get next
*/
				(*event).indata.stringdata[len-1] = '\0';
				UD_cart_part = 1;
/*	
......set up default interactions
*/
				if((*event).evclass != UD_locint)
				{
					UD_locint = UD_STRING;
					UD_locech = 1;
					UD_locdev = 1;
				}
				goto retagain;
			}
			strcpy(ret_cord->label, (*event).indata.stringdata);
 			if(xflag == UU_TRUE)
			{
 				if(ud_dasin((*event).indata.stringdata, &dsda, UD_DISTANCE)
 								==UU_FALSE)
 								goto retagain;
			}
			else
			{
 				if(ud_dasin((*event).indata.stringdata, &dsda, UD_UNITLESS)
 								==UU_FALSE)
 								goto retagain;
			}

			switch(dsda.dtype)
			{

/*				-- regular coordinate input; 
					convert to cartesian and return normally -- */

				case UD_DCOORDINATE:

					um_cotovc(dsda.stval.stcord.coord, dsda.stval.stcord.cordtype,
							cord);
					if(xflag == UU_TRUE)
					{
						um_ccstomcs(0, cord, cord1);
						x = cord1[0];
						y = cord1[1];
						z = cord1[2];
					}
					else
					{
						x = cord[0];
						y = cord[1];
						z = cord[2];

/*						-- check for unormalized coordinates if getting NDC -- */

						if( 	(x<0. || x>1.0) ||
								(y<0. || y>1.0) ||
								(z<0. || z>1.0) )
						{
							uu_uerror0(UD_DASHEP, 99);
							goto retagain;
						}
					}
					break;

/*				-- scalar input; error -- */

				case UD_DSCALAR:

/*					--	"scalar input not allowed in coordinate form" --*/

					uu_uerror0(UD_DASHEP, 5);
					goto retagain;
					break;


/*				-- garbage input -- */

				default:

/*---				"unrecognizable input in coordinate form"  ----*/

					uu_uerror0(UD_DASHEP, 6);
					goto retagain;
			}

/*			-- set up default interactions -- */

			if((*event).evclass != UD_locint)
			{
				UD_locint = UD_STRING;
				UD_locech = 1;
				UD_locdev = 1;
			}
			break ;

		case UD_VECTOR:

			UM_cc_exttoint((*event).indata.vecdata, cord);
			um_ccstomcs(0, cord, cord);
			x = cord[0];
			y = cord[1];
			z = cord[2];
			break ;

		default:
/*---		" invalid event in UD_DASCART"  ----*/
			uu_uerror0(UD_DASHEP, 7);
			goto retagain;
			break ;
	}

/* -- return coordinate -- */

	if(status == DE_TRUE)
	{
		cord[0] = x;
		cord[1] = y;
		cord[2] = z;
		um_mcstoccs(0, cord, cord);
		UM_cc_inttoext(cord, cord1);
		uu_dprint(UU_DTRC,(us,"in ud_cart1 at rp, mcs = %g %g %g", x, y, z));
		uu_dprint(UU_DTRC,(us,"in ud_cart1 at rp, ccs = %g %g %g", 
					cord[0], cord[1], cord[2]));
		uu_dprint(UU_DTRC,(us,"in ud_cart1 at rp, rp = %g %g %g", 
					cord1[0], cord1[1], cord1[2]));

/*		-- set current point -- */

		if(xflag == UU_TRUE)
		{
			(*ret_cord).cord[0] = x;
			(*ret_cord).cord[1] = y;
			(*ret_cord).cord[2] = z;
		}
		else
		{
/*			-- if the NDC coordinate was entered as a locator then go back to
				the original event record to get the coordinate, otherwise assume
				it was input as a string and converted to the wcs using
				normtran 0 -- */

			if((*event).evclass == UD_LOCATOR)
			{
				(*ret_cord).transform = LOCREC.transform;
				(*ret_cord).choice = LOCREC.choice;
				(*ret_cord).cord[0] = LOCREC.position[0];
				(*ret_cord).cord[1] = LOCREC.position[1];
				givref3(LOCREC.transform, cord);
				gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
				(*ret_cord).cord[2] = z;
				ug_mcopy((*ret_cord).wndc3_mat, gqnormmat((*ret_cord).transform));
				ug_invrt((*ret_cord).ndcw3_mat, (*ret_cord).wndc3_mat);
			}
			else
			{
				(*ret_cord).transform = 1;
				(*ret_cord).choice = -1;
				(*ret_cord).cord[0] = x;
				(*ret_cord).cord[1] = y;
				(*ret_cord).cord[2] = 0.0;
				ug_mcopy((*ret_cord).wndc3_mat, gqnormmat((*ret_cord).transform));
				ug_invrt((*ret_cord).ndcw3_mat, (*ret_cord).wndc3_mat);
			}
		}
	}

	uu_dexit;
	return(status);

retagain:
	uu_dexit;
	return(DE_AGAIN);
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
int type;
char *prompt;
char buffer[];
int size;
int *numint;
{
	return(ud_ddas(type, prompt, buffer, size, numint, UD_NODEFAULT));
}

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

	strcpy(msg, uu_uprompt0(subnum, errno1));
	status=ud_ddas(type, msg, buffer, size, numint, flag);
	UD_promptsys = 0;
	UD_promptnum = 0;
	return(status);
}
