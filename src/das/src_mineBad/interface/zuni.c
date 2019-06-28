/*********************************************************************
**
**    NAME         :  zuni.c
**
**       CONTAINS:
**                              iuni()
**                              uz_tut_on()
**                              uz_tut_off()
**                              uz_record_on()
**                              uz_record_off()
**                              uz_playback()
**                              uz_qd_signon()
**          uz_cptrtog()
**          uz_cptroff()
**          uzu_cptron()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zuni.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:39
**
*********************************************************************/
#include "usysdef.h"
#include "dsubcom.h"
#include "dmenucom.h"
#include "dinput.h"
#include "ustdio.h"
#include "uhep.h"
#include "udebug.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "dasnog.h"
#include "dcapture.h"
#include "nclfc.h"
#include "lcom.h"
#include "mfort.h"
#include "driver.h"
#include "nccs.h"

int UZ_signed_on;                                       /* signed on flag */

#define ERROR uu_uerror0(UU_SIGNON, 1)
#define UNISYS UU_TRUE
#define MSG "Function not implemented. Use EXIT icon."

/* NCL: nisstr == UU_NULL if running non-nis */
extern char *nisstr;
extern UU_LOGICAL UR_changed;
UU_LIST NCL_ldr_list;

/*********************************************************************
**    E_FUNCTION :  iuni()
**       APPLICATION main menu entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
iuni()
	{
    UM_int2 idx,ival;


	UZ_signed_on = UU_TRUE;

	/* Assign default values to NIS, NCLCADD and NCLCAM common areas */
	/* do for NIS and NONNIS */
	znu_null_devs();
	ul_init();
	/* initialize the status area */
	uz_status_init();

	/* Initialize NCLCAM processor */
	/* MOVED FROM ul_get_part(); */
	nclini(); 
/*
.....Need to set these ifl values after calling nclini, otherwise
.....they will be reset to zero.  These are the values for the
.....SOURCE modals.  JLS 2/14/00
*/
	idx = 360;
	ival = UL_format_line;
	setifl(&idx,&ival);
	idx = 230;
	ival = UL_indent_all;
	setifl(&idx,&ival);
	idx = 231;
	ival = UL_indent_sep;
	setifl(&idx,&ival);
	idx = 355;
	ival = UL_major_case;
	setifl(&idx,&ival);
	idx = 356;
	ival = UL_vocab_case;
	setifl(&idx,&ival);
	idx = 357;
	ival = UL_label_case;
	setifl(&idx,&ival);
	idx = 358;
	ival = UL_alignment;
	setifl(&idx,&ival);
/*
.....Initialize the list used for labels and leader lines
*/
	uu_list_init (&NCL_ldr_list,sizeof(UU_KEY_ID),0,100);
	/* MILLS: intialize altered label location entity into unibase */
	ncl_init_labtbl(NULL);
	UR_changed = UU_FALSE;
/*
.....Save unadultered name table
*/
	um_save_labels();

	/* If NIS do NIS signon, else do UU_application signon */
/*
......for WinNT, signon when window created
......Yurong 1/16/02
*/
#if UU_COMP!=UU_WIN2K
	ud_signon(1);
#endif
	/* NCL: ud_iconif() is an infinite loop.  ud_iconif never returns, 
			but - a REJECT OP causes the application to return to the
			previous UD_MARK() point which is in unicad_().  */
	/* this is probably out of date and can be removed.
	nclu_wrapup();
	*/
	return(0);
	}

/*********************************************************************
**    E_FUNCTION :  uz_tut_on()
**       DDC main menu entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_tut_on()
{
	if(UZ_signed_on == UU_TRUE) 
	{
		UD_tut_mode = UD_TUT_ON; 
		uj_tutorial("oztuton0.tut");
	}
return 0;
}

/*********************************************************************
**    E_FUNCTION :  uz_tut_off()
**       DDC main menu entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_tut_off()
{
	UD_tut_mode = UD_TUT_OFF;
	return 0;
}

/*********************************************************************
**    E_FUNCTION :  uz_record_on()
**       DDC main menu entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_record_on()
{ 
   if(UZ_signed_on == UU_TRUE)
		uz_zrecon();
	else 
		ERROR;
	return 0;
}

/*********************************************************************
**    E_FUNCTION :  uz_record_off()
**       DDC main menu entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_record_off()
{ 
	if(UZ_signed_on == UU_TRUE)
		uz_zrecoff();
	else 
		ERROR;
	return 0;
}

/*********************************************************************
**    E_FUNCTION :  uz_playback()
**       DDC main menu entry point
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_playback()
{ 
	if(UZ_signed_on == UU_TRUE)
		uz_zplayback();
	else 
	ERROR;
	return 0;
}

/*********************************************************************
**    E_FUNCTION :  uz_cptrtog()
**       Toggle capture on and off
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_cptrtog()
{
	/*  If first time, initialize capture state to UU_FALSE for toggle. */
   if (UD_initialize_capture == UU_FALSE) 
	{
	UD_capture.state = UU_FALSE; 
      UD_initialize_capture = UU_TRUE;
   }

	if (UD_capture.state == UU_TRUE)
		uz_cptroff();
	else
		uzu_cptron();
return 0;
}

/*********************************************************************
**    E_FUNCTION :  uz_cptroff()
**       End capture of messages written to scrolling windows
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_cptroff()
{
	/*  If first time, initialize capture state to UU_FALSE */
   if (UD_initialize_capture == UU_FALSE) 
	{
	UD_capture.state = UU_FALSE; 
      UD_initialize_capture = UU_TRUE;
   }

	if (UD_capture.state == UU_TRUE)
	{
		uz_actcapture ("Capture OFF");
		ux_close(UD_capture.cplun, UX_PRTERRS);
		UD_capture.state = UU_FALSE;
	}
	else
		uu_outputerr("Capture not active.");
return 0 ;
}

/*********************************************************************
**    E_FUNCTION :  uzu_cptron()
**       Capture messages written to scrolling windows
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uzu_cptron()
{
	char fname[UX_MAX_PATH_LEN];        /* file name buffer */
	char cptrdir[UX_MAX_PATH_LEN];     /* path name buffer */
	char fullname[UX_MAX_PATH_LEN];    /* system name buffer */
	UX_pathname bname;                                              /* basename of user specified
														pathname -- not used */
	int  len, mode, status;
	char *p, *ux_getenv();

	/*  If first time, initialize capture state to UU_FALSE */
   if (UD_initialize_capture == UU_FALSE) 
	{
	UD_capture.state = UU_FALSE; 
      UD_initialize_capture = UU_TRUE;
   }

	if (UD_capture.state == UU_FALSE)
	{
		/*              --- enter capture file name: ---*/
		fname[0] = '\0';
		ud_ddas(UD_DASSTRING, "Enter Capture file name", fname, 
	   UX_MAX_PATH_LEN, &len, UD_DEFAULT);
		if (len <= 0)
	 goto done;
		while (ux_get_base_fname(fname, bname, (UX_NPRTERRS | UX_NCHK)) !=
		       UU_SUCCESS)
		{
			uu_uerror0(UX_UDOS,24);
			ud_ddas(UD_DASSTRING, "Enter Capture file name", fname, 
					UX_MAX_PATH_LEN, &len, UD_DEFAULT);
			if (len <= 0)
		goto done;
		}

		/*              -- convert to system dependent name -- */
		p = ux_getenv("U_TEXT_AREA", UX_PRTERRS);
		if(p != NULL)
			strcpy(cptrdir, "^U_TEXT_AREA");
		else
			strcpy(cptrdir, "^UX_HOMEDIR");

		mode = 0;       
		/* Check for file existence */
		ux_mk_chk_syspath(UU_NULL, cptrdir, fname, UU_NULL, 
					"UD_TEXT_SUFFIX", &mode, fullname, UX_PRTERRS);

		if(!(mode & UX_NEXISTS))
		{
			/*--            "file exists, overwrite?"         --*/
			if(ud_lyesno(UD_DASHEP, 8)==UU_TRUE)
				status = ux_delete(fullname, UX_PRTERRS);
			else
			{
				uzu_cptron();
	    goto done;
			}
		}

		/*--            Create new file.        --*/
		status = ux_create_file( fullname, 0666, UU_NULL, "STREAM", 
			"ASCII", "UX_NOEXTRA", &UD_capture.cplun, UX_PRTERRS);
		if (status != 0)
		{
			/*                      --- " cannot open file"         ---*/
			uu_uerror0(UD_DASHEP, 61);
			return(UU_FALSE);
		}
		uz_actcapture ("Capture ON");
		UD_capture.state = UU_TRUE;
	}
else
	uu_outputerr("Capture already active.");

done:;
return 0;
}
