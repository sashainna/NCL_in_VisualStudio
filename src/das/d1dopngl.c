#include "usysdef.h"

/*********************************************************************
**
**    NAME         :  d1dopngl.c
**
**       CONTAINS:
**       	ud_ddopngl
**    MODULE NAME AND RELEASE LEVEL 
**       d1dopngl.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:02
**
*********************************************************************/

#include "dasg.h"
#include "dinput.h"
#include "driver.h"
#include "usysg.h"
#include "gentry.h"
#include "ginq.h"
#include "dsubcom.h"
#include "xenv1.h"
/*********************************************************************
**
**    E_FUNCTION         :  ud_ddopngl()
**       set up for openGL
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

ud_ddopngl()
{
#ifdef UU_OPENGL
	extern int wsgl;
	extern int uz_user_key(),uz_user_fkey(),imouse3(),uz_user_button();
	char *p, *ux_getenv();
	Gdspsize *dspsize;

	p = ux_getenv("gio",UX_PRTERRS);
	if (p == NULL) p = "tt";

/*	-- set up the substitution table -- */

	UD_chctable[0] = uz_user_key;
	UD_chctable[1] = uz_user_fkey;
	UD_chctable[2] = imouse3;
	UD_chctable[3] = uz_user_button; 

/*	-- open the workstation -- */

	UD_gksws[UD_wsptr] = gopenws(p,&wsgl);

/*	-- for historical reasons, move primary ws into UD_ksws -- */

	if(UD_wsptr == 0)
	{
		UD_ksws = UD_gksws[0];
		UD_kswsadr = &wsgl;
	}

/*	-- activate the workstation now that it is opened -- */

	gactivatews(UD_gksws[UD_wsptr]);

/*	-- init some info about the screen size -- */

	dspsize=gqdisplaysize(UD_gksws[UD_wsptr]);
	UD_aspect = (*dspsize).device.y;
	UD_rasteru= (*dspsize).raster.x;
	UD_rasterv= (*dspsize).raster.y;
	UD_wsptr++;
	return 0;
#endif
}
