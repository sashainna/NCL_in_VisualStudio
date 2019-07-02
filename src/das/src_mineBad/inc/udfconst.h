/*************************************************************************
**		NAME	:		udfconst.h
**			Contains:	all manifest constants used in other forms include
**							or c source files.
**
**		COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**		MODULE NAME AND RELEASE LEVEL:
**       udfconst.h , 25.2
**		DATE AND TIME OF LAST MODIFICATION:
**       01/20/17 , 09:50:36
**************************************************************************/

#ifndef UDFCONSTH

/* --	forms constant definitions -- */

#define UD_MAXOP	1				/* maximum of 256 opcodes per form (later)*/
#define UD_MAXNAM	64				/* maximum of 64 characters in form name */
#define UD_MAXPS	64				/* maximum prompt/message size in characters */
#define UD_MAXMS	32				/* maximum mask string size in characters */
#define OFFSET	100				/* offset added to das data types to separate */
										/* them from UD_FSTATs */

typedef enum						/* status return type */
{
	UD_FLDOK,						/* data entered and checked ok */
	UD_FWD,							/* requested to go to next field */
	UD_BAK,							/* requested to go to previous field */
	UD_BADREQ,						/* data type requested was invalid or undefined */
	UD_ACTOK,						/* the form is successfully activated */
	UD_BADACT,						/* init or activate error - data no good */
	UD_VALOK,						/* data passed all checks */
	UD_BADRNG,						/* data out of range */
	UD_BADTYP,						/* data was not an allowed type */
	UD_FDSYNC,						/* no of form fields != no of data fields */
	UD_DFLTER,						/* the default display errored out */
	UD_FILLOK,						/* form filled, all data present, chk'd */
	UD_DONE,							/* "done" key hit with no oustanding mandatory */
	UD_ALT,							/* the "user alternate action" key was hit */
	UD_DFLTOK,						/* any default(s) properly displayed */
	UD_PRSTOK,						/* all went well, the form is now displayed. */
	UD_BADMSG,						/* there was a message diplay error */
	UD_BADPRMT,						/* there was a prompt display error */
	UD_BADMSK,						/* there was a mask display error */
	UD_FRMOK,						/* form traversed ok, good data returned */
	UD_BADFILL,						/* form data fill error, data no good */
	UD_BADPRST,						/* form presentation error, data no good */
	UD_XFROK,						/* data transfer was done successfully */
	UD_BADXFR,						/* bad data type in data transfer call */
	UD_OUTOK,						/* dummy output routine ok return */
	UD_DATAOK,						/* data returned was not funct. key */
	UD_PASSF,						/* no data entered to field */
	UD_TFWD,							/* toggle forward entered */
	UD_TBAK,							/* toggle backward entered */
	UD_ENTERF,						/**/
	UD_FRMCLOSE				/* form closed */
} UD_FSTAT;

#define UD_DISPLAYF 0
#define UD_INPUTF	  1

/* Defines for method calls.  Calls methods on following conditions
	(Set fstruct.method_returns to bitwise or of the following)	*/

#define UD_ENTER  (char)1		/* Call method when field is entered	*/
#define UD_EXIT   (char)2		/* Call method when field is exitted
														(i.e. user hits CR) */
#define UD_TOGGLE (char)4		/* Call method when user toggles field.  Only
											has an affect on toggle fields */

#define UDFCONSTH
#endif

