/*********************************************************************
**
**    NAME         :  d1gkssup.c
**
**       CONTAINS:
**       	ud_openarcf
**       	ud_createarcf
**				ud_qlocpet
**				ud_qchoicepet
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d1gkssup.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:02
**
*********************************************************************/

#include "usysdef.h"
#include "uerror.h"
#include "ustdio.h"
#include "usysg.h"
#include "udebug.h"
#include "dasnog.h"
#include "dinput.h"
#include "uims.h"
#include "g.h"
#include "xenv1.h"
#include "xfsys1.h"

/*********************************************************************
**    E_FUNCTION :  	ud_openarcf(fname)
**       front end to gks gopenarchf
**    PARAMETERS   
**       INPUT  : 
**          fname = archive file name
**       OUTPUT :  
**          none
**    RETURNS      : 0 ifopened, 1 if file created, -1 if can't create
**    SIDE EFFECTS : print error message
**    WARNINGS     : none
*********************************************************************/

int ud_gopenarcf(fname)
char *fname;					/* file name */
{
	int status;
	int open;					/* file opened flag */
	int dir_seq;				/* direct / sequential access flag */
	int reclen;					/* record length */
	int size;					/* file size */
	int mode;					/* For checking existance 	*/
	char fullname[100];		/* Temp For checking existance 	*/

 	uu_denter(UU_DTRC,(us, "enter ud_gopenarcf, files=%s", fname));

/*	-- inquire to see if file exists first -- */

	mode = UX_EXISTS| UX_CREATE;	/* Check for file existence */
	status = ux_mk_chk_syspath(UU_NULL, UU_NULL, fname, UU_NULL, 
					UU_NULL, &mode, fullname, UX_NPRTERRS | UX_QUOTES);

	if(!(mode & UX_NEXISTS))
	{
		status = gopenarcf(fname);
		if(status != 0)
		{
			uu_uerror1(DAS, 80, fname);
			if(status == 1)
				ux_delete(fname,UX_PRTERRS);
		}
	}
	else
	{
		uu_uerror1(DAS, 80, fname);
		status = 1;
	}

 	uu_dprint(UU_DTRC,(us, "leave ud_gopenarcf, status=%d", status));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  	ud_createarcf(fname)
**       front end to gks gopenarchf to create a new file
**    PARAMETERS   
**       INPUT  : 
**          fname = archive file name
**       OUTPUT :  
**          none
**    RETURNS      : 0 ifopened, 1 if file created, -1 if can't create
**    SIDE EFFECTS : print error message
**    WARNINGS     : none
*********************************************************************/

int ud_gcreatearcf(fname)
char *fname;					/* file name */
{
	int status;

 	uu_denter(UU_DTRC,(us, "enter ud_gcreatearcf, files=%s", fname));

	status = gopenarcf(fname);
	if(status != 1)
	{
		uu_uerror1(DAS, 81, fname);
	}

 	uu_dprint(UU_DTRC,(us, "leave ud_gcreatearcf, status=%d", status));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  	ud_qlocpet(device, pet)
**       inquire if workstation supports a locator pet
**    PARAMETERS   
**       INPUT  : 
**          device = locator device number
**				pet	 = p.e.t. to inquire
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if supported, UU_FALSE otherwise
**    SIDE EFFECTS : print error message
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL ud_qlocpet(device, pet)
int device;						/* device number */
int pet;							/* pet to inquire */
{
	UU_LOGICAL status;
	int i;
/*--	Gdefloc *gqdefloc(); -- */
	Gdefloc *deflocptr;		/* pointer to inquiry locator record */

 	uu_denter(UU_DTRC,(us, "enter ud_qlocpet, device=%d, pet=%d", device, pet));

	deflocptr = gqdefloc(UD_ksws, device);
	
	status = UU_FALSE;
	for (i=0; i<(*deflocptr).n_pets; i++)
	{
 		uu_dprint(UU_DTRC,(us, "in ud_qlocpet, i=%d, pet=%d",
				i, (*deflocptr).pets[i]));
		if((*deflocptr).pets[i] == pet)
		{
			status = UU_TRUE;
			goto done;
		}
	}

done:
 	uu_dprint(UU_DTRC,(us, "leave ud_qlocpet, status=%d", status));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  	ud_qchoicepet(device, pet)
**       inquire if workstation supports a choice pet
**    PARAMETERS   
**       INPUT  : 
**          device = choice device number
**				pet	 = p.e.t. to inquire
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if supported, UU_FALSE otherwise
**    SIDE EFFECTS : print error message
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL ud_qchoicepet(device, pet)
int device;						/* device number */
int pet;							/* pet to inquire */
{
	UU_LOGICAL status;
	int i;
/*--	Gdefloc *gqdefchoice(); -- */
	Gdefchoice *defchoiceptr;		/* pointer to inquiry choice record */

 	uu_denter(UU_DTRC,(us, "enter ud_qchoicepet, device=%d, pet=%d",
				device, pet));

	defchoiceptr = gqdefchoice(UD_ksws, device);
	
	status = UU_FALSE;
	for (i=0; i<(*defchoiceptr).n_pets; i++)
	{
 		uu_dprint(UU_DTRC,(us, "in ud_qchoicepet, i=%d, pet=%d",
				i, (*defchoiceptr).pets[i]));

		if((*defchoiceptr).pets[i] == pet)
		{
			status = UU_TRUE;
			goto done;
		}
	}

done:
 	uu_dprint(UU_DTRC,(us, "leave ud_qchoicepet, status=%d", status));
	uu_dexit;
	return(status);
}
