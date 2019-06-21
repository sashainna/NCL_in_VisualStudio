/*********************************************************************
**
**    NAME         :  zappinit.c
**
**    CONTAINS:
**				uu_init_usr_app 1-19-89:rah
**				uu_reset_usr_app
**    			uu_init_mpe
**    			uu_reset_mpe
**				uu_app_done
**				uu_mpe_done
**				uz_init_cam()
**				uz_init_cad()
**				uz_init_nis()
**				uz_init_plus()
**				uz_init_signon()
**				(uu_sys_err_recovery)- leave unmodified for now ...1-19:rah
**
**    COPYRIGHT 1987 (c) Mills Data Systems Co. Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zappinit.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:36
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"		/* Unicad TRACE/DEBUGGING */
#include "dmenucom.h"	/* Defines vars for menu navigation */
#include "dsubcom.h"	/* Defines UD_chctable[], etc */
#include "dasg.h"		/* Defines POPUPREC, etc */
#include "calcom.h"		/* Defines the calculator */
#include "diconm.h"		/* Defines UD_ICMU_UP, etc */
#include "xenv1.h"		/* Defined ux_getenv(), etc */
#include "zpipe.h"		/* Defines UD_num_safs, etc. */
#include "nclfc.h"
#include "ribase.h"

/** NCL INCLUDES **/
#define NCLIPGM
#include "nclicons.h"	/* defines NCL icon arrays 				  **/
#undef NCLIPGM
extern char *nisstr;

/*	-- Define active Graphic devices in this executable -- */

/*#define DPGM 1*/
#include "driver.h"
/*#undef DPGM*/

int UU_application = UU_NCLNIS;				/* set NCL active */
static int counter = 0;						/* system error recorvery call count */

/*********************************************************************
**    E_FUNCTION     :  int uu_init_usr_app()
**       Initialization routine for NCL
**			code.
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_init_usr_app()
	{
	extern int (*UD_savefkb)(), (*UD_savekb)(), (*UD_savemouse)();
	extern UD_POPUPREC nmodfy[];
	char *ux_getenv(), *p;
	UU_LOGICAL ud_qchoicepet();
	extern int uz_user_key();

	uu_denter(UU_DTRC,(us,"enter uu_init_usr_app"));

/*  NCL: -- INITIALIZE RUN TIME VARIABLES -- */
	znu_init_runvars();

/*	-- bring down the alternate action menus -- */

	UD_ICMU_DN(MODEL_ICONS);
	UD_ICMU_DN(MOTION_ICONS);
	UD_ICMU_DN(ALTACT_ICONS);
	UD_ICMU_DN(SELECT_ICONS);
	UZ_DOWNCADD();
	UZ_DOWNCAM();
	UZ_DOWNNIS();

/*	-- set the puck active for menu picks if puck available -- */

	if(ud_qchoicepet(UD_start_menu_num, 24) == UU_TRUE)
 		ud_dtmenu(24);

	um_initpopupmenus();
	um_ag_initpopupmenus();
	ud_initpp(&nmodfy[0]);
 	ua_init_drafting();
	ua_init_text();	/* this routine should be called after ua_init_drafting*/

	/* Initialize drawing sizes from drwsize.init file values */
	nclu_init_drawing_size();

/* initialize the menus if indicated -- */

/*	p = ux_getenv("U_INITMENU",UX_NPRTERRS);
/*	if(p != UU_NULL)
/*	{
/*		if((ud_strcomp(p, "pre") == -1) || (ud_strcomp(p, "PRE") == -1))
/*		{
/*			UD_menu_init = 0;
/*			znu_init_menus();
/*			UD_menu_init = 1;
/*		}
/*	}*/

/*	-- set the choice device tables inactive -- */

	UD_savekb  = UD_chctable[0];
	UD_savefkb  = UD_chctable[1];
	UD_savemouse = UD_chctable[2];
	UD_chctable[0] = uz_user_key;
/*	UD_chctable[1] = znupro;*/
	UD_chctable[5] = uz_user_key;

/** devices are like this:  -------

	UD_chctable[0] = keyboard
	UD_chctable[1] = function keys
	UD_chctable[2] = mouse/puck
	UD_chctable[3] = tablet
	UD_chctable[4] = 
	UD_chctable[5] = icon arrays

**  ------------------------------- **/

/*-- set-up keyboard --*/
	znu_init_kb(&UD_chctable[0], nisstr);

/*-- set-up function keys --*/
	znu_init_fk(&UD_chctable[1], nisstr);

/*-- set-up mouse definitions --*/
	znu_init_ms(&UD_chctable[2]);

/*-- set-up tablet definitions --*/
	znu_init_tb(&UD_chctable[3]);

/*-- set-up icon arrays --*/
/*	znu_init_ic(&UD_chctable[5], nisstr);*/

/*-- end of input device initialization --*/

	uu_dprint(UU_DTRC,(us,"leave uu_init_usr_app()"));
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION     :  int uu_reset_usr_app()
**       Reset routine for NCL environmental state
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uu_reset_usr_app()
	{
	extern UU_LOGICAL	UR_changed;	/* Unibase change flag */
	CRSLT stbptr;						/* symbol table entry pointer */

	uu_denter(UU_DTRC,(us,"enter uu_reset_usr_app()"));

/*	-- clear out status area -- */
/*  -- uz_clear_status();    -- */

/*	-- set input units to inches -- */
/*  -- um_setunits(UM_INCH);     -- */

/* -- set reference point to 0,0,0 -- */

	uq_calc2("rp=<0,0,0>", &stbptr);

	UR_changed = UU_FALSE;	/* mark database clean after initial junk is in */

	uu_dprint(UU_DTRC,(us,"leave uu_reset_usr_app()"));
	uu_dexit;
	}

/*********************************************************************
**
**    E_FUNCTION     :  int uu_init_mpe()
**       This routine initializes the MPE system.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : 
**							The order that the routines are called in
**							is very important and should not be changed.
**
*********************************************************************/

uu_init_mpe()
{
	uu_denter(UU_DTRC,(us,"enter uu_init_mpe()"));

/* -- initialize the tool storage manager -- */

	uu_toolmalloc_init();

/* -- Check write access on the working diretory before continuing -- */
	if (ux_dirtest0()!=UU_SUCCESS)
		exit(1);

/*	-- Initialize the UNICAD internal debugging or trace system.
 		Note:  This may be removed if you are not running with the
 				 tracing system. -- */

	uu_init_debug();

/* -- Initialize udos system -- */

	ux_init_table(1);

/*	-- Initialize GKS and the DAS	-- */

	ud_init_das();

/*	-- Initialize the user interface	-- */

	ud_init_ui();

/*	-- Initialize the help, error, and prompt system -- */

	uu_initerr();
	uu_inithep();

	uu_dprint(UU_DTRC,(us,"leave uu_init_mpe()"));
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION     :  int uu_reset_mpe()
**       This routine resets the MPE system.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : 
**							The order that the routines are called in
**							is very important and should not be changed.
**
*********************************************************************/

uu_reset_mpe()
{
	extern UU_LOGICAL	UR_changed;	/* Unibase change flag */

	uu_denter(UU_DTRC,(us,"enter uu_reset_mpe()"));


/* --	Initialize UNIBASE -- */

	ur_init_unibase();

/* -- Initialize ALL relations in UNIBASE -- */

	if (uc_init_relations() != UU_SUCCESS)
		goto failed;

/* --	Initialize the modeling subsystem	-- */

	um_init_model();

/* -- Initialize Viewing -- */

	uv_init();

/* --	Initialize ROMULUS	-- */

	um_init_romulus();

/* -- Initialize symbol data; note, this MUST be after modelling 
		is initialized -- */

   ub_init_sym_data();  

/* -- Reset Calculator; note, this MUST be after relations are initialized -- */

	uqi_tbreset();

/* -- Initialize Unibase Statistics; note, After relations are initialized -- */

	ur_init_unibase_stat(UR_STAT_INIT);
	
	UR_changed = UU_FALSE;	/* mark database clean after initial junk is in */

	goto done;
failed:;
	uu_dprint(UU_DTRC,(us,"FAILURE in uu_reset_unicad"));
done:;
	uu_dprint(UU_DTRC,(us,"leave uu_reset_unicad()"));
	uu_dexit;
}

/*********************************************************************
**
**    E_FUNCTION     :  int uu_app_done()
**       This routine closes the application.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uu_app_done()
{

/*	-- save the part if necessary -- */

	if(ur_unibase_used() == UU_TRUE)
/*
.....added one more parameter becasue of ur_save_part changed
.....Yurong 10/8/98
*/
/*		ur_save_part(1);	/* tell save this is an exit with parameter TRUE*/
		ur_save_part(1, 0);
}

/*********************************************************************
**
**    E_FUNCTION     :  int uu_mpe_done()
**       This routine closes the MPE system.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uu_mpe_done()
{

/*	-- shut down DIGS -- */

	uu_unidone();
}

/*********************************************************************
**    E_FUNCTION     :  uz_init_cam
**       Initialize the NCLCAM application.
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uz_init_cam()
{
/*
.....Set application to NCLCAM
*/
	UU_application = UU_NCLCAM;
/*
.....Bring up CAM icons
*/
/*	znu_init_ic(&UD_chctable[5],UU_NULL);*/
/*	UZ_UPCAM();*/
/*
.....Activate the CAM function keys
*/
	uz_light_buttons();
/*
.....Update the status area
*/
	uz_status();
/*
.....Display the copyright notice
*/
	znu_copyright();
}

/*********************************************************************
**    E_FUNCTION     :  uz_init_cad
**       Initialize the NCLCADD application.
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uz_init_cad()
{
/*
.....Set application to NCLCADD
*/
	UU_application = UU_NCLCADD;
/*
.....Delete any visible motion
*/
	motdel();
/*
.....Bring up CADD icons
*/
/*	znu_init_ic(&UD_chctable[5],UU_NULL);*/
/*	UZ_UPCADD();*/
/*
.....Activate the CADD function keys
*/
	uz_light_buttons();
/*
.....Update the status area
*/
	uz_status();
	ud_killmsg(UD_ERRORMSG);
/*
.....Display the copyright notice
*/
	znu_copyright();
}

/*********************************************************************
**    E_FUNCTION     :  uz_init_nis
**       Initialize the NIS application.
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uz_init_nis()
{
/*
.....Set application to NIS
*/
	UU_application = UU_NCLNIS;
/*
.....Bring up NIS icons
*/
/*	znu_init_ic(&UD_chctable[5],nisstr);*/
/*	UZ_UPNIS();*/
/*
.....Activate the NIS function keys
*/
	uz_light_buttons();
/*
.....Update the status area
*/
	uz_status();
/*
.....Display the copyright notice
*/
	znu_copyright();
}

/*********************************************************************
**    E_FUNCTION     :  uz_init_plus
**       Initialize the NCLPLUS application.
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uz_init_plus()
{
/*
.....Bring up NCLPLUS icons
*/
	UU_application = UU_NCLPLUS;
/*	znu_init_ic(&UD_chctable[5],UU_NULL);*/
/*
.....Set application to NCLCAM
*/
	UU_application = UU_NCLCAM;
/*
.....Display the copyright notice
*/
	znu_copyright();
}

/*********************************************************************
**    E_FUNCTION     :  uz_init_signon
**       Initialize the NCL Signon application.
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uz_init_signon()
{
/*
.....Set application to NCL Signon
*/
	UU_application = UU_NCLSIGNON;
/*
.....Activate the Signon function keys
*/
	uz_light_buttons();
/*
.....Clear the status area
*/
	uz_clear_status();
}
