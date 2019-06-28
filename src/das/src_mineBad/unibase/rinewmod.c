 /*********************************************************************
**    NAME         :  rinewmod.c
**       CONTAINS:
**       ur_install_new_modals()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			rinewmod.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:11:45
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "rienvtab.h"
#include "umoveb.h"
#include "nclver.h"
#include "riallmod.h"
#include "rienvtab.h"
#include "rver9400.h"
#include "rver9700.h"
#include "mdrel.h"

extern int UR_restore_mtrl, UR_restore_clr;
extern UU_LOGICAL  UR_load_env;
extern int UR_readenv;
extern int UM_material_reset;
/*********************************************************************
**    E_FUNCTION     :  ur_install_new_modals()
**       move newly loaded Unibase modals into working copies.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : put modal variables
**    WARNINGS     : none
*********************************************************************/
int ur_install_new_modals()
{
	extern struct UR_env_table_rec	UR_environ_table[];

	int		i,k;

	uu_denter(UU_RTRC,(us,"ur_install_new_modals()"));

	if ((UR_load_env==0)&&(UR_readenv==0))
		return 0;
	UR_readenv = 0;
/*
.....Save name modals
.....in case external Unibase does not have them
.....such as when created by PUT/geo
*/
	um_save_labels();
	/* install modals from new(buffer) copy */
	for(k = 0; UR_environ_table[k].name[0] != '\0'; k++)
	{
      uu_move_byte(UR_environ_table[k].new_adrs,UR_environ_table[k].adrs,
					UR_environ_table[k].length);
	}
#ifdef UU_DEBUGON
	uu_dprint(UU_RITRC,(us,"printing modals:"));
	ur_dump_modal("attrmdl", "UM_", UU_FALSE);
	ur_dump_modal("dispattr", "UM_", UU_FALSE);
	ur_dump_modal("drwmdl", "UM_", UU_FALSE);
	ur_dump_modal("mtrlmdl", "UM_", UU_FALSE);
	ur_dump_modal("labelmdl", "UM_", UU_FALSE);
#endif
/*
......material modal struct changed
......Yurong
*/
	if (NCL_infile_version<9.350)
	{
		UM_mtrlmdl.index = UM_mtrlmdl_old.index;
		for( i=0; i<64; i++ ) 
		{
			UM_mtrlmdl.ka[i] = 1.0;
			UM_mtrlmdl.kd[i] = UM_mtrlmdl_old.kd[i];
			UM_mtrlmdl.ks[i] = UM_mtrlmdl_old.ks[i];
			UM_mtrlmdl.spec_exp[i] = UM_mtrlmdl_old.spec_exp[i];
			if ((i==0)||(i==1)||(i==4))
			{
				UM_mtrlmdl.ks_r[i] = 0.0;
				UM_mtrlmdl.ks_g[i] = 0.0;
				UM_mtrlmdl.ks_b[i] = 0.0;
			}
			else
			{
				UM_mtrlmdl.ks_r[i] = 1.0;
				UM_mtrlmdl.ks_g[i] = 1.0;
				UM_mtrlmdl.ks_b[i] = 1.0;
			}
			sprintf(UM_mtrlmdl.name[i], "Type %d", i+1);
		}
	}
	if (NCL_infile_version<9.651 && UM_labelmdl96.num != 0)
	{
		UM_labelmdl.key = UM_labelmdl96.key;
		UM_labelmdl.rel_num = UM_labelmdl96.rel_num;
		UM_labelmdl.use_count = UM_labelmdl96.use_count;
		UM_labelmdl.max = UM_labelmdl96.max;
		UM_labelmdl.num = UM_labelmdl96.num;
		UM_labelmdl.key = UM_labelmdl96.key;
		for (i=0; i<20;i++)
		{
			strcpy(UM_labelmdl.pf[i], UM_labelmdl96.pf[i]);
			strcpy(UM_labelmdl.pfs[i], UM_labelmdl96.pfs[i]);
			UM_labelmdl.next[i] = UM_labelmdl96.next[i];
			UM_labelmdl.subscr[i] = UM_labelmdl96.subscr[i];
			UM_labelmdl.issub[i] = UM_labelmdl96.issub[i];
		}
		for (i=20; i<40; i++) 
		{
			UM_labelmdl.subscr[i] = 0;
			UM_labelmdl.issub[i] = 0;
			strcpy(UM_labelmdl.pf[i], "UN");
			strcpy(UM_labelmdl.pfs[i], "");
		}
		for (i=1; i<256; i++) UM_labelmdl.rel[i] = UM_labelmdl96.rel[i];
/*
.....update new relaton label
*/
		umi_init_rel_label(UA_TEXT_REL,"AN");
		umi_init_rel_label(UB_SYMBOL_REL, "SY", 1);
		umi_init_rel_label(UM_SOLID_REL, "SO", 1);
	}
	if(UR_restore_mtrl)
		ul_unibase_mtrl();
	if(UR_restore_clr)
/*		ul_unibase_clr(); */
		ncl_post_load_color(1);
	UM_material_reset = 1;
	if (NCL_infile_version<9.350)
	{
		UR_environ_table[1].adrs = (char *)&UM_mtrlmdl;
		UR_environ_table[1].new_adrs = (char *)&UM_new_mtrlmdl; 
		UR_environ_table[1].length = sizeof(struct UM_mtrlmdl_rec);
	}
	if (NCL_infile_version<9.651)
	{
		UR_environ_table[4].adrs = (char *)&UM_labelmdl;
		UR_environ_table[4].new_adrs = (char *)&UM_new_labelmdl; 
		UR_environ_table[4].length = sizeof(struct UM_labelmdl_rec);
	}
/*
.....Restore name modals
*/
	if (UM_labelmdl.num == 0) um_reset_labels();
	uu_dexit;
	return(0);
}
