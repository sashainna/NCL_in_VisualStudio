/*********************************************************************
**
**    NAME         :  d2hldas.c
**
**       CONTAINS:
**  			ud_chc
**  			ud_int
**  			ud_str
**  			ud_strdef
**  			ud_val
**  			ud_vec
**  			ud_chc1
**  			ud_int1
**  			ud_str1
**  			ud_val1
**  			ud_vec1
**  			ud_vec2
**  			vallimtst
**				ud_scaint
**  			ud_scaval
**  			ud_scavec
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d2hldas.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:04
**
*********************************************************************/
#include "usysdef.h"
#include "dasnog.h"
#include "dinput.h"
#include "ddef.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "mdrel.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "ustdio.h"
#include "view.h"
#include "driver.h"
#include "dselmask.h"
/*
.....For resolving pick to vector.
*/
#include "gmat4.h"
#include "ginqxf.h"
#include "gtblvar6.h"
#include "dselect.h"

#define FUZZ (UU_REAL) 1.0e-4
#define ZILCH(number) (fabs(number)<FUZZ)

#include "class.h"
#define SELREC (*event).indata.pickdata

/* MILLS- To set the RETURN key like use default. */
UU_LOGICAL     dflag;

/* MILLS- Logical flag indicating who the calling routine is. For adding name thru 
   toggle to pick. */
extern UU_LOGICAL ncl_where;

/*
.....For verify mode, reset to zero first time through picking routines
.....In case of previous reject op. - RAZ
*/
extern int NCL_nopick_cnt;
extern int UD_select_key;

UD_DASTAT ncl_verify_pick();
UD_DASTAT ud_cart1();
int UD_textpre_curpos = 0;
int UD_string_add = 0;
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3
extern int NCL_mark_method;
extern int  UR_active;
char UD_prompt[1224];
/********************************************************************* 
**
**  I_FUNCTION:  vallimtst(value)
**      verify value is between DAS limits
**
**  PARAMETERS   
**
**      input:  value = value to constrain
**
**      output: none
**
**  RETURNS      :  UU_TRUE if properly constrained, UU_FALSE otherwise
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

static UU_LOGICAL vallimtst(value)
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
**  I_FUNCTION			:  ud_chc(prompt, ret_chc, num_chc)
**      choice high level DAS
**
**  PARAMETERS   
**
**      input:  prompt = prompt buffer
**					 num_chc =  number of choices (or lines) in prompt
**
**      output: ret_chc = choice number returned
**
**  RETURNS      :  status of the operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_chc(prompt, ret_chc, num_chc)
char *prompt; 						/*   operator prompt string */
int *ret_chc;						/*   button number */
int num_chc;						/*	  number of choices */
{

	UD_DASTAT status;						/* temp status variable */
	UD_DASTAT ud_chc1();					/* semantic interpreter */
	UD_DEVENT event;						/* event buffer */

	uu_denter(UU_DTRC,(us,"entering ud_chc"));

	do
	{
		ud_gevt(&event, UD_chcint, prompt, num_chc-1, UD_chcdev, UD_chcech, NULL);
		status = ud_chc1(&event, ret_chc);
	}
	while(status == DE_AGAIN);

	uu_dexit;
	return(status);
}


/********************************************************************* 
**
**  I_FUNCTION			:  ud_int(prompt, ret_int, defflag, def_int)
**      integer high level DAS
**
**  PARAMETERS   
**
**      input:  prompt = operator prompt string
**					 defflag = default exist flag
**					 def_int = default integer
**
**      output: ret_int = coordinate to return
**
**  RETURNS      :  status  
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_int(prompt, ret_int, defflag, def_int)
char *prompt ; 					/*  operator prompt string  */
int *ret_int ;						/*  integer to return  */
UU_LOGICAL defflag;				/*  default flag */
int *def_int ;						/*  default integer */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_int1();				/* semantic interpreter */
	UD_DEVENT event;					/* input event buffer */
	char pbuf[100];					/* prompt buffer */

	uu_denter(UU_DTRC,(us,"entering ud_int"));

	/* MILLS- To set up RETURN like use default key. */
	dflag = defflag;

/*	-- set up the next prompt -- */

	if(defflag == UU_TRUE)
		sprintf(pbuf, "%s %s [%d]", prompt, UD_synint, *def_int);
	else
		sprintf(pbuf, "%s %s", prompt, UD_synint);

	do
	{
		ud_gevt(&event, UD_valint, pbuf, 1, UD_valdev, UD_valech, NULL);
		status = ud_int1(&event, ret_int, defflag, def_int);
	}
	while(status == DE_AGAIN);

	uu_dexit;
	return(status);
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

	/* MILLS- To setup RETURN like use default key. */
	dflag = defflag;

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
	static char locstr[1024] = "\0";	/* local string buffer */
    char locprm[1224];				/* copy of prompt while in do loop */
    char *errstr ="There is no space for continuation sign ($). Re-enter.";

	int evtype,evecho,evdev,pos;
	UD_DASTAT ud_pick1();
	UD_PPICKREC ret_pck;
	char tempstr[500];

	uu_denter(UU_DTRC,(us,"entering ud_strdef"));

	UD_string_add = 0;
	inparm.strbfsz = mxlen;
	inparm.defstr = (*def_str).instring;
/*
.....Reset verify list if have any
*/
	ud_reset_verify_list();
/*
..... 
..... Added by Paul for "command" input. 08/26/92 
..... 
*/ 
	strcpy(locprm,prompt); 
	if(strlen(locstr) > 0)
	{
		strcat(inparm.defstr,locstr);
		strcpy(locstr,"\0");
	}
/*.....*/ 
 
	do
	{
/*
.....
..... Added by Paul for "command" input. 08/26/92
.....
*/
        strcpy(locprm,prompt);
		strcpy(UD_prompt, prompt);
/*.....*/

		ud_gevt(&event, UD_strint, locprm, 1, UD_strdev, UD_strech, &inparm);
/*
.....if picking, we need do the verify first
*/
		if(event.evclass == UD_PICK) 
		{
			evtype = UD_strint;
			evdev  = UD_strdev;
			evecho = UD_strech;
			goto force2;
		}
		status = ud_str1(&event, (*ret_str).instring, defflag, 
			(*def_str).instring, mxlen, len);
        if(event.evclass == UD_PICK || event.evclass == UD_LOCATOR) goto forse;

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
                strcpy(locprm,UD_prompt);
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
/*
.....Reset verify list if have any
*/
				ud_reset_verify_list();
				while (status == DE_AGAIN)
				{
					ud_gevt(&event, evtype, locprm, 1, evdev, evecho, 
							&inparm);
force2:;
                    if(event.evclass == UD_PICK)
					{
						status = ud_pick1(&event,&ret_pck);
						if (status == DE_TRUE)
						{
/*
.....restore picking area and ready to pick again or accept
*/
							ud_restore_pickarea();
/*
.....this changed to use other key function to deal with it
*/
							if (NCL_mark_method==DYNAMIC_MARK)
							{
								if (UD_select_key)
								{
									ncl_verify_pick2(&ret_pck);
									ud_post_msg(2);
									UD_select_key = 0;
									status = DE_AGAIN;
								}
							}
							else
							{
								status = ncl_verify_pick(&ret_pck);
							}
						}							
						evtype = UD_PICK;
						evdev  = UD_pckdev;
						evecho = UD_pckech;
					}
					else status = DE_TRUE;
				}
				status = ud_str1(&event, locstr, defflag, def_str, mxlen, len);
/*
.....restore picking area after all done.
*/
				if(event.evclass == UD_PICK)
				{
					ud_restore_pickarea();
					UD_select_key = 0;
				}
/*
.....
..... Added by Paul for "command" input. 08/26/92
.....
*/
                if (status == DE_TRUE)
				{
                    if(event.evclass == UD_PICK || event.evclass == UD_LOCATOR)
                    {
                        if(strlen(def_str->instring) >= mxlen && *len > 0)
                        {
                            /*ERROR* There is no space for cont. sign($) */
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
/************************   status = DE_DNFIELD; *******************/
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

	/* MILLS- To set up RETURN like use default key. */
	dflag = defflag;

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

	/*MILLS-  To set up RETURN like use default key. */
	dflag = defflag;

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
/*
.....Reset verify list if have any
*/
	ud_reset_verify_list();
	do
	{
		ud_gevt(&event, UD_vecint, pbuf, 1, UD_vecdev, UD_vecech, NULL);
/*
.........Test for VERIFY is made in ud_vec1() since we have the 'event'
.........data parsed into the format expected by ncl_verify_pick(). - RAZ
*/
		status = ud_vec1(&event, ret_vec, defflag, def_vec);
		if(UM_2d3d_mode == UM_2D &&  status == DE_TRUE)
		{
			if(!ZILCH(ret_vec[2]))
				if(ret_vec[0] != 0. || ret_vec[1] != 0.)
					uu_uerror0(UD_DASHEP, 88);
		}
	}
	while(status == DE_AGAIN);

	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION			:  ud_chc1(event, ret_chc)
**      choice high level DAS
**
**  PARAMETERS   
**
**      input:  event = event buffer
**
**      output: ret_chc = choice number returned
**
**  RETURNS      :  status of the operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_chc1(event, ret_chc)
UD_DEVENT *event;					/* event structure */
int *ret_chc;						/* button number */
{

	UD_DASTAT status;						/* temp status variable */
	UD_DASTAT ud_auxm();

	uu_denter(UU_DTRC,(us,"entering ud_chc1"));

	status = DE_TRUE;
	if((*event).evclass == UD_CHOICE)
	{
		if((*event).evdev == UD_AUXMENU)
		{
			status = ud_auxm(event);
			if(status == DE_DONE)
				*ret_chc = 0;
		}
		else if((*event).evdev < UD_st_menu_num)
		{
			status = DE_TRUE;
			*ret_chc = (*event).indata.choicedata;
		}
		else
		{

/*--		Menu in use	--*/

			uu_uerror0(UD_DASHEP, 20);
			status = DE_AGAIN;
		}
	}
	else
	{

/*--	invalid event in UD_DASCHOICE	--*/
/*
...changed for avoid uncorrect error message
...Yurong
*/
		if(!((UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)||
				(UD_Rpstate[UD_Rpstate_ptr].flag==PLAYSUSP)))
			uu_uerror0(UD_DASHEP, 8);
		status = DE_AGAIN;
	}

	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION			:  ud_int1(event, ret_int, defflag, def_int)
**      integer high level DAS
**
**  PARAMETERS   
**
**      input:  event = event structure
**					 defflag = default exist flag
**					 def_int = default integer
**
**      output: ret_int = coordinate to return
**
**  RETURNS      :  status  
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_int1(event, ret_int, defflag, def_int)
UD_DEVENT *event;					/*  event structure  */
int *ret_int;						/*  integer to return  */
UU_LOGICAL defflag;				/*  default flag */
int *def_int;						/*  default integer */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_auxm();
	UD_DASDATA dsda;					/* return buffer from kb coordinate parser */
	UU_REAL rtemp;						/* temp real variable */
	int stat;
	UU_REAL value;
	char strbuf[100];

	uu_denter(UU_DTRC,(us,"entering ud_int1"));

	status = DE_TRUE;
	if((*event).evclass == UD_VALUATOR)
	{
		*ret_int = (int)(*event).indata.valdata;
		status = DE_TRUE;

/*			-- set up default interactions -- */

		UD_valint = UD_VALUATOR;
	}
	else if((*event).evclass == UD_STRING)
	{
/*
.....consider the scalar value first
*/
/*
.....Get the scalar and convert to value
*/
		stat = ncl_get_scalar_value((*event).indata.stringdata, &value);
		if (stat==-1)
		{
			ud_winerror("A scalar value expected");
			goto retagain;
		}
		else if (stat==1)
			sprintf(strbuf, "%d", (int)value);
		else
			strcpy(strbuf, (*event).indata.stringdata);

		if(ud_dasin(strbuf, &dsda, UD_UNITLESS) == UU_FALSE)
		{
			goto retagain;
		}
		else
		{
			switch(dsda.dtype)
			{

/*					-- regular coordinate input; error -- */

				case UD_DCOORDINATE:

/*---				"coordinate form not allowed in real number input"		---*/

					uu_uerror0(UD_DASHEP, 17);
					goto retagain;
					break;

/*					-- scalar input -- */

				case UD_DSCALAR:

					status = DE_TRUE;
					*ret_int = (int)dsda.stval.dval;
					break;

/*					-- garbage input -- */

				default:

/*---				"unrecognizable input in real number input"		---*/

					uu_uerror0(UD_DASHEP, 6);
					goto retagain;

			}
		}
		status = DE_TRUE;

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
					*ret_int = *def_int;
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

/*---		"invalid choice event in UD_DASINT"		---*/

			uu_uerror0(UD_DASHEP, 20);
			goto retagain;
		}
	}
	else
	{

/*---	" invalid event type"		---*/

		uu_uerror0(UD_DASHEP, 20);
		goto retagain;
	}

	if(status == DE_TRUE)
	{
		rtemp = *ret_int;
		if(vallimtst(rtemp) != UU_TRUE)
			goto retagain;
	}
	uu_dexit;
	return(status);

retagain:
	uu_dexit;
	return(DE_AGAIN);
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
    uv_segbuff(buf);          /* user defined segment data */
    struct UC_entitydatabag e;
    char str_tmp[1024];
    UD_NDCLOCREC coord;
	int save_active = 1;

	uu_denter(UU_DTRC,(us,"entering ud_str1"));

	status = DE_TRUE;
	if((*event).evclass == UD_STRING)
	{
		loclen = strlen((*event).indata.stringdata);
/* NCL
		if(loclen == 0)
		{
			if(defflag == UU_TRUE)
			{
				status = DE_TRUE;
				*len = strlen(def_str);
				strcpy(ret_str, def_str);
			}
			else
			{

*/
/* ---			"no default in effect"		---*/
/*

				uu_uerror0(UD_DASHEP, 3);
				status = DE_AGAIN;
			}
		}
		else if(loclen <= mxlen)
*/
/*
...
...Changed by Paul to allow to enter full string (72 characters) and
...to press any control key instead of <RETURN> only (as it was before).
...Old version:          if(loclen <= mxlen)
...05/07/93
...
*/
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

/*---		" string buffer too small - reenter"		---*/

/*
.....Return allowable portion of line
.....so user can re-edit it
.....Bobby  -  5/12/99
*/
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

/*---			"no default in effect"		---*/

				uu_uerror0(UD_DASHEP, 3);
				status = DE_AGAIN;
			}
		}
	}
/* 
..... 
.....Changed to be able to pick something while in the "command" 
.....input mode. Paul. 08/26/92.
.....Old variant:
.....       MILLS- If the routine is called from ncl_add_name and
.....       toggle to pick is chosen just exit. 
.....    else if((*event).evclass == UD_PICK && (*event).evdev == UD_CRCOMMAND)
.....    {
.....    if (ncl_where)
.....        status = DE_ALTACT;
.....    }
.....
*/
	else if((*event).evclass == UD_PICK)
    {
		save_active =  UR_active;
		gsegrud (SELREC.pickpath[0],buf);
		e.key = uv_getkey (buf);
/*
......if pick is for Secondary unibase, we need use Secondary unibase
......to retrieve data
*/
		if (ud_getpick_type()==UD_PICK_SECONDARY)
			ur_getu_second();
		ur_retrieve_data_fixed(&e);
		ncl_get_label(&e, ret_str);
		*len = strlen(ret_str);
		if (ud_getpick_type()==UD_PICK_SECONDARY)
		{
			if (save_active==1)
				ur_getu_work();
			else
				ur_getu_second();
		}
    }
/*
.....
..... Added to be able to pick point "by location" while in the "command"
..... input mode. Paul 08/26/92.
.....
*/
	else if((*event).evclass == UD_LOCATOR)
    {
/*         
.....
..... Create the "Nested point" definition.
.....
*/
            status = ud_cart1(event, &coord, UU_FALSE, &coord, UU_TRUE);
            ncl_cctostr(2,coord.cord,str_tmp);
            sprintf(ret_str,"(PT/%s)",str_tmp);
            *len = strlen(ret_str);
    }
	else if ((*event).evclass == UD_PICKMENU)
	{
/*
......set the status to DE_ALTACT instead of DE_TRUE because this is not a normal
......input for accept a string, set the return string to empty
*/
		*len = 0;
		ret_str[0] = '\0';
		status = DE_ALTACT;
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
/*
.....consider the scalar value first
*/
/*
.....Get the scalar and convert to value
*/
		stat = ncl_get_scalar_value((*event).indata.stringdata, &value);
		if (stat==-1)
		{
			ud_winerror("A scalar value expected");
			goto retagain;
		}
		else if (stat==1)
			sprintf(stringdata, "%f", value);
		else
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

		case UD_PICK :
/*
....Originally:
....		"pick input not allowed in vector input"		.....
....
....			uu_uerror0(UD_DASHEP, 50);
....			goto retagain;
....
....Now:
.............We allow the user to pick an entity and define a vector from it.
.............move pertinent stuff in to a UD_PPICKREC record,
*/
			picker.depth = PICKPTR.depth;
			for(i=0; i<picker.depth; i++)
				picker.pickpath[i] = PICKPTR.pickpath[i];
/*
.....restore picking area and ready to pick again or accept
*/
			ud_restore_pickarea();
/*
........VERIFY MODE:
*/
/*
.....this changed to use other key function to deal with it
*/
			if (NCL_mark_method==DYNAMIC_MARK)
			{
				if (UD_select_key)
				{
					ncl_verify_pick2(&picker);
					ud_post_msg(2);
					UD_select_key = 0;
					status = DE_AGAIN;
				}
			}
			else
			{
				status = ncl_verify_pick(&picker);
			}
			if (status == DE_AGAIN)
				goto retagain;
/*
.............move more pertinent stuff in to an UD_NDCLOCREC record,
*/
			pndc.transform = PICKPTR.transform;
			ug_mcopy(pndc.wndc3_mat, gqnormmat(pndc.transform));
			ug_invrt(pndc.ndcw3_mat, pndc.wndc3_mat);

			pndc.cord[0] = PICKPTR.position[0];
			pndc.cord[1] = PICKPTR.position[1];
			givref3(pndc.transform, cord);
			gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
			pndc.cord[2] = z;
/*
.............Get the vector from the picked entity
*/
			stat = uc_ploc_to_vector(2, &picker, &pndc, cord);
/*
.............Change the event record to vector type, and 
.............convert to user coordinates system and put vector into
.............input event.
*/
			if (stat == UU_SUCCESS)
				{
				(*event).evclass = UD_VECTOR;
				UM_cc_inttoext(cord, (*event).indata.vecdata);
/*
.............Now continue as though we were 'case UD_VECTOR'.
*/
				x = cord[0];
				y = cord[1];
				z = cord[2];

/*			-- set up default interactions -- */

				UD_vecint = UD_PICK;
				}
			else
				{
				/* should issue error like: */
				/* "Can't evaluate a vector from picked entity." */
				/* but this is "restricted entity ..." */
				uu_uerror0(UD_DASHEP, 76);
				goto retagain;
				}

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
/*
.....consider the scalar value first
*/
/*
.....Get the scalar and convert to value
*/
			stat = ncl_parse_scalar_values((*event).indata.stringdata, strbuf, 0);
			if (stat==-1)
			{
				sprintf(erms,"\"%s\" is not a valid scalar value",(*event).indata.stringdata);
				ud_wrerr(erms);
				goto retagain;
			}
			if(ud_dasin(strbuf, &dsda, UD_DISTANCE) == UU_FALSE)
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
**  I_FUNCTION			:  ud_vec2(event, ret_vec)
**      Vector high level DAS. The difference from ud_vec1 is there
**      are no error messages in the case of an empty input and in the
**      case when the user hits "Done". Also, we assume NODEFAULT
**
**  PARAMETERS   
**
**      input:  event = event structure
**
**      output: ret_vec =  coordinate to return
**
**  RETURNS      : status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_vec2(event, ret_vec)
UD_DEVENT *event;						/* event structure */
UU_REAL ret_vec[3];					/* vector to return  */
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
   char ibuf[100];
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

		case UD_PICK :
/*
.............We allow the user to pick an entity and define a vector from it.
*/
			picker.depth = PICKPTR.depth;
			for(i=0; i<picker.depth; i++)
				picker.pickpath[i] = PICKPTR.pickpath[i];
/*
.....restore picking area and ready to pick again or accept
*/
				ud_restore_pickarea();
/*
........VERIFY MODE:
*/
/*
.....this changed to use other key function to deal with it
*/
			if (NCL_mark_method==DYNAMIC_MARK)
			{
				if (UD_select_key)
				{
					ncl_verify_pick2(&picker);
					ud_post_msg(2);
					UD_select_key = 0;
					status = DE_AGAIN;
				}
			}
			else
			{
				status = ncl_verify_pick(&picker);
			}
			if (status == DE_AGAIN)
				goto retagain;
/*
.............move more pertinent stuff in to an UD_NDCLOCREC record,
*/
			pndc.transform = PICKPTR.transform;
			ug_mcopy(pndc.wndc3_mat, gqnormmat(pndc.transform));
			ug_invrt(pndc.ndcw3_mat, pndc.wndc3_mat);

			pndc.cord[0] = PICKPTR.position[0];
			pndc.cord[1] = PICKPTR.position[1];
			givref3(pndc.transform, cord);
			gwndc3(&x, &y, &z, cord[0], cord[1], cord[2]);
			pndc.cord[2] = z;
/*
.............Get the vector from the picked entity
*/
			stat = uc_ploc_to_vector(2, &picker, &pndc, cord);
/*
.............Change the event record to vector type, and 
.............convert to user coordinates system and put vector into
.............input event.
*/
			if (stat == UU_SUCCESS)
				{
				(*event).evclass = UD_VECTOR;
				UM_cc_inttoext(cord, (*event).indata.vecdata);
/*
.............Now continue as though we were 'case UD_VECTOR'.
*/
				x = cord[0];
				y = cord[1];
				z = cord[2];

/*			-- set up default interactions -- */

				UD_vecint = UD_PICK;
				}
			else
				{
				/* should issue error like: */
				/* "Can't evaluate a vector from picked entity." */
				/* but this is "restricted entity ..." */
				uu_uerror0(UD_DASHEP, 76);
				goto retagain;
				}

			break ;

		case UD_CHOICE :

			if((*event).evdev == UD_AUXMENU)
			{
				status = ud_auxm(event);
				if(status == DE_DEFAULT)
				{
					status = DE_TRUE;

/*---					"no default in effect"		---*/

						uu_uerror0(UD_DASHEP, 3);
						goto retagain;
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
/*
.....consider the scalar value first
*/
/*
.....Get the scalar and convert to value
*/
		stat = ncl_parse_scalar_values((*event).indata.stringdata, strbuf, 0);
		if (stat==-1)
		{
			sprintf(erms,"\"%s\" is not a valid scalar value",(*event).indata.stringdata);
			ud_wrerr(erms);
			goto retagain;
		}
		strcpy(ibuf, strbuf);
		i = strlen(ibuf);
		if ((i==0) || (ibuf[i-1]=='\\'))
				goto retagain;
			else if(ud_dasin(ibuf, &dsda, UD_DISTANCE) == UU_FALSE)
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
**  I_FUNCTION			:  ud_scavec(prompt, ret_vec, defflag, def_vec)
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

UD_DASTAT ud_scavec(prompt, ret_vec, defflag, def_vec)
char *prompt ; 					/* operator prompt string  */
UD_DASCORD *ret_vec;					/* vector to return  */
UU_LOGICAL defflag;					/* default exists flag */
UU_REAL def_vec[3];					/* default vector */
{

	UD_DASTAT status;							/* status return cell */
	UD_DASTAT ud_vec1();						/* semantic interpreter */
	UD_DEVENT event;							/* event buffer */
	UU_REAL vec[3];
	char pbuf[100];
	char *ud_unvec();

	uu_denter(UU_DTRC,(us,"entering ud_vec"));

	/*MILLS-  To set up RETURN like use default key. */
	dflag = defflag;

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
/*
.....Reset verify list if have any
*/
	ud_reset_verify_list();
	do
	{
		ud_gevt(&event, UD_vecint, pbuf, 1, UD_vecdev, UD_vecech, NULL);
/*
.........Test for VERIFY is made in ud_vec1() since we have the 'event'
.........data parsed into the format expected by ncl_verify_pick(). - RAZ
*/
		status = ud_vec1(&event, vec, defflag, def_vec);
		if(UM_2d3d_mode == UM_2D &&  status == DE_TRUE)
		{
			if(!ZILCH(vec[2]))
				if(vec[0] != 0. || vec[1] != 0.)
					uu_uerror0(UD_DASHEP, 88);
		}
	}
	while(status == DE_AGAIN);
/*
.....Do not copy values if mouse is middle clicked
*/
	if(status == DE_TRUE)
	{
		ret_vec->cord[0] = vec[0];
		ret_vec->cord[1] = vec[1];
		ret_vec->cord[2] = vec[2];
		strcpy(ret_vec->label, event.indata.stringdata);
	}
	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION		:  ud_scaval(prompt, ret_val, defflag, def_val, typeflag)
**      valuator high level DAS (this function similar to ud_val but
**				return the integer value and string value (UD_SCA_IVAL))
**
**  PARAMETERS   
**
**      input:  prompt = operator prompt string
**					 defflag = default exists flag
**					 def_val = default value
**
**      output: ret_val = valuator structure to return
**
**  RETURNS      :  status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_scaval(prompt, ret_val, defflag, def_val, typeflag)
char *prompt; 				/*   operator prompt string  */
UD_SCA_VALUE *ret_val;			/*   value to return  */
UU_LOGICAL defflag;		/*   default exists flag */
UU_REAL *def_val;			/*   default value */
int typeflag;				/*  type of value requested (UD_DISTANCE, UD_DANGLE, or
												UD_UNITLESS */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_val1();				/* semantic interpreter */
	UD_DEVENT event;	
	UU_REAL val;
	char *ud_undist(), *ud_unang();
	char pbuf[100];					/* prompt buffer */
	int prec;							/* field length/precision */
	int len;								/* min field length */

	uu_denter(UU_DTRC,(us,"entering ud_val"));

	/* MILLS- To set up RETURN like use default key. */
	dflag = defflag;

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
		status = ud_val1(&event, &val, defflag, def_val, typeflag);
	}
	while(status == DE_AGAIN);
/*
.....Do not copy values if mouse is middle clicked
*/
	if(status == DE_TRUE)
	{
		ret_val->value = val;
		strcpy(ret_val->label, event.indata.stringdata);
	}
	uu_dexit;
	return(status);
}
/********************************************************************* 
**
**  I_FUNCTION			:  ud_scaint(prompt, ret_int, defflag, def_int)
**      integer high level DAS (this function similar to ud_int but
**				return the integer value and string value (UD_SCA_IVAL))
**
**  PARAMETERS   
**
**      input:  prompt = operator prompt string
**					 defflag = default exist flag
**					 def_int = default integer
**
**      output: ret_int = integer structure to return
**
**  RETURNS      :  status  
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_scaint(prompt, ret_int, defflag, def_int)
char *prompt ; 					/*  operator prompt string  */
UD_SCA_IVAL *ret_int ;						/*  integer to return  */
UU_LOGICAL defflag;				/*  default flag */
int *def_int ;						/*  default integer */
{

	UD_DASTAT status;					/* status return cell */
	UD_DASTAT ud_int1();				/* semantic interpreter */
	UD_DEVENT event;					/* input event buffer */
	char pbuf[100];					/* prompt buffer */
	int val;

	uu_denter(UU_DTRC,(us,"entering ud_int"));

	/* MILLS- To set up RETURN like use default key. */
	dflag = defflag;

/*	-- set up the next prompt -- */

	if(defflag == UU_TRUE)
		sprintf(pbuf, "%s %s [%d]", prompt, UD_synint, *def_int);
	else
		sprintf(pbuf, "%s %s", prompt, UD_synint);

	do
	{
		ud_gevt(&event, UD_valint, pbuf, 1, UD_valdev, UD_valech, NULL);
		status = ud_int1(&event, &val, defflag, def_int);
	}
	while(status == DE_AGAIN);
/*
.....Do not copy values if mouse is middle clicked
*/
	if(status == DE_TRUE)
	{
		ret_int->value = val;
		strcpy(ret_int->label, event.indata.stringdata);
	}
	uu_dexit;
	return(status);
}

/********************************************************************* 
**
**  I_FUNCTION			:  ud_scavec2(prompt, ret_vec, defflag, def_vec)
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

UD_DASTAT ud_scavec2(prompt, ret_vec, defflag, def_vec)
char *prompt ; 					/* operator prompt string  */
UD_DASCORD *ret_vec;					/* vector to return  */
UU_LOGICAL defflag;					/* default exists flag */
UU_REAL def_vec[3];					/* default vector */
{

	UD_DASTAT status;							/* status return cell */
	UD_DASTAT ud_vec1();						/* semantic interpreter */
	UD_DEVENT event;							/* event buffer */
	UU_REAL vec[3];
	char pbuf[100];
	char *ud_unvec();
	UD_DASTAT ud_pick1();
	struct UC_entitydatabag e;
	UD_PPICKREC ret_pck;
    int dsegid;
    uv_segbuff(buffer);

	uu_denter(UU_DTRC,(us,"entering ud_vec"));

	/*MILLS-  To set up RETURN like use default key. */
	dflag = defflag;

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
/*
.....Reset verify list if have any
*/
	ud_reset_verify_list();
	do
	{
/*
.....only allow picking for PT, PV
*/
		ud_lgeo(UU_TRUE,UD_ncl_vepv);
		ud_gevt(&event, UD_vecint, pbuf, 1, UD_vecdev, UD_vecech, NULL);
		if (event.evclass==UD_PICK)
		{
			status = ud_pick1(&event, &ret_pck);
			dsegid = ret_pck.pickpath[0];
			if (ud_isassist_seg(dsegid)==0)
			{
				gsegrud(dsegid,buffer);
				e.key = uv_getkey(buffer);
    			ur_retrieve_data_fixed(&e);
    			ncl_get_label(&e, ret_vec->label);
			}
			else
				ret_vec->label[0] = '\0';
		}
/*
.........Test for VERIFY is made in ud_vec1() since we have the 'event'
.........data parsed into the format expected by ncl_verify_pick(). - RAZ
*/
		status = ud_vec1(&event, vec, defflag, def_vec);
		if(UM_2d3d_mode == UM_2D &&  status == DE_TRUE)
		{
			if(!ZILCH(vec[2]))
				if(vec[0] != 0. || vec[1] != 0.)
					uu_uerror0(UD_DASHEP, 88);
		}
		ud_lgeo(UU_FALSE,UD_ncl_vepv);
	}
	while(status == DE_AGAIN);
/*
.....Do not copy values if mouse is middle clicked
*/
	if(status == DE_TRUE)
	{
		ret_vec->cord[0] = vec[0];
		ret_vec->cord[1] = vec[1];
		ret_vec->cord[2] = vec[2];
	}
	uu_dexit;
	return(status);
}

