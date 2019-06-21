/*********************************************************************
**
**    NAME         :  zsflmgmt.c
**
**       CONTAINS:
**				uz_load()
**				uz_save()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zsflmgmt.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:38
**
*********************************************************************/

#include "usysdef.h"
#include "uhep.h"
#include "mdunits.h"
#include "mfort.h"
#include "nclfc.h"

extern char UR_dpn[];
extern UU_LOGICAL UM_modax_disp;
extern int UR_restore_mtrl;
extern int UR_restore_lights;
extern int UR_restore_clr;
extern UU_LOGICAL UR_restore_units;

/*********************************************************************
**    E_FUNCTION :  uz_load()
**       entry into part load
**    PARAMETERS   
**       INPUT  : 
**          pfile: part file to loaded
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_load(pfile)
char *pfile;
{
	UU_LOGICAL ud_yesno(),stat;
	UM_real8 mx[12];
	UM_int2  i2flg;
	int user_opened;
	
	if ((pfile!=UU_NULL)||(pfile[0]!='\0'))
		user_opened = 0;
	else
		user_opened = 1;


	stat = ur_unibase_used();
	if (stat)
		stat = ud_yesno(0, "Current geometry will be overwritten. Continue?",
				"Load Unibase");
	else
		stat = UU_TRUE;
	if (stat)
	{
		if(UR_restore_lights)
			ul_save_lgtmod();
		if(UR_restore_mtrl)
			ul_save_mtrlmod();
		if(UR_restore_clr)
			ul_save_clrmod();
		if(ur_load_part(pfile, 0)==0)
		{
			mcsmx (&i2flg, mx);
			stmdmx (mx);
			um_drw_mod_axis (UM_modax_disp);
			uz_load_status();
			if (!UR_restore_units) unbuni();
			if (user_opened)
				nclc_save_recent_file(pfile, 1);
		}
		else
			return -1;
	}
	else
		return -1;
	return 0;
}

/*********************************************************************
**    E_FUNCTION :  uz_save()
**       entry into part save
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_save()
{
/*
.....added one more parameter becasue of ur_save_part changed
.....Yurong 10/8/98
*/
/*	ur_save_part(0);  */
	ur_save_part(0, 0);
	uz_actpart(UR_dpn);
	return 0;
}
