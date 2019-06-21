/*********************************************************************
**    NAME         : amenu.c
**       CONTAINS:
**				ua_menu_switch
**				ua_switch_standards
**    		ua_update_note_text
**    		ua_update_dimension
**		
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**			amenu.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:05:36
*********************************************************************/
#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "umoveb.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "dasnog.h"
#include "dasg.h"
#include "adraft.h"
#include "adrfcom.h"
#include "atext.h"

int UA_local_dir =0;
/*********************************************************************
**    E_FUNCTION     : ua_menu_switch(menu_num)
**       Put up the appropriate pop-up submenu.
**    PARAMETERS   
**       INPUT  : 
**				menu_num						menu number
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_menu_switch(menu_num)
int		menu_num;
	{
	int		choice;
	int		status;
	uu_denter(UU_STRC,(us,"ua_menu_switch(menu_num=%d)", menu_num));
	switch( menu_num )
		{
		case 1:
			{
			status = ua_popmenu(19,&(choice));
			if( ( status!=1 ) )
				{
				uu_dexit;
				return 0;
				}
			else
				{
				switch( choice )
					{
					case 1:
						{
						ua_radius(4);
						}
						break;
					case 2:
						{
						ua_radius(5);
						}
						break;
					case 3:
						{
						ua_radius(6);
						}
						break;
					case 4:
						{
						ua_diam(10);
						}
						break;
					}
				}
			}
			break;
		case 2:
			{
			status = ua_popmenu(20,&(choice));
			if( ( status!=1 ) )
				{
				uu_dexit;
				return 0;
				}
			else
				{
				switch( choice )
					{
					case 1:
						{
						ua_diam(1);
						}
						break;
					case 2:
						{
						ua_diam(2);
						}
						break;
					case 3:
						{
						ua_diam(3);
						}
						break;
					case 4:
						{
						ua_diam(7);
						}
						break;
					case 5:
						{
						ua_diam(8);
						}
						break;
					case 6:
						{
						ua_diam(9);
						}
						break;
					}
				}
			}
			break;
		case 3:
			{
			status = ua_popmenu(22,&(choice));
			if( ( status!=1 ) )
				{
				uu_dexit;
				return 0;
				}
			else
				{
				switch( choice )
					{
					case 1:
						{
						UA_local_dir = 1;
						ua_swap_standard("ANSISTD.DAT",UU_FALSE);
						}
						break;
					case 2:
						{
						UA_local_dir = 1;
						ua_swap_standard("ISOSTD.DAT",UU_FALSE);
						}
						break;
					case 3:
						{
						UA_local_dir = 1;
						ua_swap_standard("BSISTD.DAT",UU_FALSE);
						}
						break;
					case 4:
						{
						UA_local_dir = 1;
						ua_swap_standard("DINSTD.DAT",UU_FALSE);
						}
						break;
					case 5:
						{
						ua_swap_standard("",UU_TRUE);
						}
						break;
					}
				}
			}
			break;
		}
	uu_dexit;
	return 0;
	}
/*********************************************************************
**    E_FUNCTION     : ua_switch_standards()
**       Routine to re-generate drafting entities for a new standard.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_switch_standards()
	{
	char     fname[60];
	int		choice, status, char_cnt, rel_num;
	UU_KEY_ID  key;
	UU_LOGICAL init;
	struct UA_generic_draft	edrf;
	struct UA_txt_rec  note;
	struct UA_txtattr_rec    note_attr;

	uu_denter(UU_STRC,(us,"ua_switch_standards()"));
	status = ua_popmenu(22,&choice);
	if(  status != 1  ) goto fexit;
	else
		{
		switch( choice )
			{
			case 1:
				strcpy(fname, "ANSISTD.DAT");
				ua_init_standard(fname, UU_TRUE);
				break;
			case 2:
				strcpy(fname, "ISOSTD.DAT");
				ua_init_standard(fname, UU_TRUE);
				break;
			case 3:
				strcpy(fname, "BSISTD.DAT");
				ua_init_standard(fname, UU_TRUE);
				break;
			case 4:
				strcpy(fname, "DINSTD.DAT");
				ua_init_standard(fname, UU_TRUE);
				break;
			case 5:
				ud_ldas(UD_DASSTRING,UA_DRAFTING,112,fname,40,&char_cnt,
																	UD_NODEFAULT);
				/* check if user entered any text */
				if(char_cnt == 0)  goto fexit;
				ua_init_standard(fname, UU_TRUE);
				break;
			}
		}
	um_update_drwunits();

	/* now run through dimensions */
	init = UU_TRUE;
	rel_num = UA_LINEAR_DIM;
	while(uv_getalldispinrelation(init, rel_num, &key) == UU_SUCCESS)
		{
		edrf.key = key;
		uc_retrieve_data(&edrf,sizeof(struct UA_generic_draft	)) ;
		if(ua_asso_check(&edrf) == UU_SUCCESS)
			{
			/* update appropriate attributes */
			if(edrf.etype != UA_CROSSHATCH)
				{
				ua_update_dimension(&edrf);
				ua_regen_drafting(&edrf);
				}
			}
		init = UU_FALSE;
		}
	
	/* now do note text */
	init = UU_TRUE;
	rel_num = UA_TEXT_REL;
	while(uv_getalldispinrelation(init, rel_num, &key) == UU_SUCCESS)
		{
		note.key = key;
		if(ua_get_text(&note, sizeof(struct UA_txt_rec)) == UU_SUCCESS)
			{
			note_attr.key = note.key;
			if(ur_retrieve_attr(&note_attr) == UU_SUCCESS)
				{
				ua_update_note_text(&note, &note_attr);
				um_update_geom(&note,UM_DEFAULT_TF);
				ur_update_attr(&note_attr);
				uc_display(&note);
				}
			}
		init = UU_FALSE;
		}
fexit:;
	uu_dexit;
	return 0;
	}
/*********************************************************************
**    E_FUNCTION     : ua_update_note_text(note, out)
**		Update current entity attribute bundle. 
**    PARAMETERS   
**       INPUT  : 
**          note							note entity
**				out							new attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_update_note_text(note, out)
	struct UA_txt_rec *note;
	struct UA_txtattr_rec *out;
	{
	UU_REAL     scale;

	uu_denter(UU_STRC, (us, "ua_update_note_text"));

	um_get_drwscale(&scale);

	out->height = UA_char_size_note/scale;

fexit:
	uu_dexit;
	return 0;
	}
/*********************************************************************
**    E_FUNCTION     : ua_update_dimension(edrf)
**		Update appropriate entity attributes
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_update_dimension(edrf)
	struct UA_generic_draft *edrf;
	{
	int i;
	UU_REAL scale;

	uu_denter(UU_STRC, (us, "ua_update_dim_attr"));

	um_get_drwscale(&scale);

	edrf->draft_stand = UA_drafting_std;
	edrf->txt_orent = UA_txt_orient;
	edrf->char_size = (UA_char_size/scale);
	edrf->grid_dist = (UA_grid_dist/scale);
	for(i=0;i<10;i++)
		{
		edrf->txt_blk[i].txt_size = (UA_char_size/scale);
		edrf->txt_blk[i].tangle = 0.0;
		}
	edrf->linear_units = UA_linear_units;
	edrf->units_sym = UA_units_sym;
	edrf->ang_units = UA_ang_units;
	edrf->dim_zero_sup = UA_dim_zero_sup;
	edrf->dim_roundoff = UA_dim_roundoff;
	edrf->diam_symbol = UA_diam_symbol;
	edrf->dia_place = UA_dia_place;
	edrf->dual_l_units = UA_d_lin_units;
	edrf->dual_a_units = UA_d_ang_units;
	edrf->d_units_sym = UA_d_units_sym;
	edrf->d_dim_z_sup = UA_d_dim_z_sup;
	edrf->d_dim_roundoff = UA_d_dim_roundoff;
	edrf->rad_symb = UA_radius_sym;
	edrf->rad_place = UA_radius_place;
	edrf->dim_type = UA_dim_type	;
	edrf->dim_rnd_fact = UA_dim_rnd_fact;
	edrf->d_dim_rnd_fact = UA_d_dim_rnd_fact;
	edrf->arrow_size = UA_arrow_size/scale;
	for(i=0;i<10;i++)
		{
		edrf->arrow_blk[i].arrow_type = UA_arrow_symbol;
		edrf->arrow_blk[i].size = UA_arrow_size/scale;
		}
	edrf->txt_gap = UA_gap_dim_text/scale;
	edrf->gap_to_geo = UA_gap_geom_line/scale;
	edrf->ext_past_line = UA_ext_past_line/scale;
	edrf->lead_orient = UA_ldr_orient;
	edrf->leader_loc = UA_ldr_location;
	edrf->stub_length = UA_ldr_stub_len/scale;
	edrf->tol_places = UA_tol_dec_places;
	edrf->du_tol_pl = UA_d_tol_dec_plac;
	edrf->tol_zero_sup = UA_tol_zero_sup;
	edrf->dim_places = UA_dec_places;
	edrf->dual_place = UA_d_dec_places;
	edrf->d_tol_z_sup = UA_d_tol_z_sup;
	edrf ->tol_roundoff = UA_tol_roff_meth;
	edrf->d_tol_roff_meth = UA_d_tol_roff_meth;
	edrf->tol_rnd_fact = UA_tol_rnd_fact;
	edrf->d_tol_rnd_fact = UA_d_tol_rnd_fact;
	edrf->upper_tol = UA_lin_up_tol_val;
	edrf->d_lin_up_tol = UA_d_lin_up_tol;
	edrf->lower_tol = UA_lin_lo_tol_val;
	edrf->d_lin_lo_tol = UA_d_lin_lo_tol;
	if(edrf->etype == UA_ANGULAR_DIM)
		{
		edrf->upper_tol = UA_ang_up_tol_val;
		edrf->lower_tol = UA_ang_lo_tol_val;
		}
fexit:
	uu_dexit;
	return 0;
	}
