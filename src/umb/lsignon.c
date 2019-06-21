/*********************************************************************
**     FILENAME: lsignon.c
**     CONTAINS:   
**                 autcam
**                 ul_sysexit()
**     MODULE NAME AND RELEASE LEVEL 
**       lsignon.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:11:20
*********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "uctype.h"
#include "diconm.h"
#include "dsubcom.h"
#include "lcom.h"
#include "lumb.h"
#include "mfort.h"
#include "udebug.h"
#include "udforms.h"
#include "xenv1.h"
#include "xfsys0.h"
#include "xfsys1.h"
#include "nclicons.h"
#include "nclfc.h"

#define tabn 18
#define NODEFAULT 0
#define DEFAULT 1

extern int NCL_vx_flag;

extern char *nisstr;
extern UU_LOGICAL UR_changed;

/*********************************************************************
**	 E_FUNCTION : autcam
**			This function is a Fortran callable routine that informs
**			the caller whether this is an active CAM terminal.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  icam = 0 = CAM is not authorized.
**				1 = CAM is authorized.
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS:
*********************************************************************/

void autcam(icam)
	UM_int2 *icam;
{
	*icam = UL_cam;
	return;
}

/*********************************************************************
**	 E_FUNCTION : ul_sysexit()
**			Close NCLCAM and UNICAD, exit to system.
**	 PARAMETERS	
**		 INPUT  :  none
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: 
**	 WARNINGS:
*********************************************************************/
void ul_sysexit()
{
void ul_clear_exit();
#if UU_COMP == UU_WIN2K
	uw_ntapp_exit();
	return;
#endif
	uw_close_keycom();
	ul_clear_exit();
	exit(0);
}

/*********************************************************************
**	 E_FUNCTION : ul_clear_exit()
**			Close NCLCAM and UNICAD, and ready to exit.
**	 PARAMETERS	
**		 INPUT  :  none
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: 
**	 WARNINGS:
*********************************************************************/
void ul_clear_exit()
{
	nclfin();
#if UU_COMP == UU_VAXVMS
	ul_reset_control();
#endif
	UR_changed = UU_FALSE;
	uu_app_done();
	uu_mpe_done();
/*
......clear key/menu function parameter spaces
*/
	uz_clear_parmspc();
	if (NCL_vx_flag)
	{
	UD_markptr = 0;
	longjmp (UD_markstk[0].markenv,-1);
	return;
	}
}
