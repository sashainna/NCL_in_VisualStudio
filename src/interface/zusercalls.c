/*********************************************************************
**	FILENAME: zusercalls.c
**	CONTAINS:
**				uz_user_keydef(ktab,index,xflag)
**				uz_daskey1(sub, parms)
**				uz_user_dascalls(index)
**				uz_mouse_functions
**				uz_func_call
**    MODULE NAME AND RELEASE LEVEL
**       zusercalls.c , 25.15
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 10:21:41
*********************************************************************/
#include "usysdef.h"
#include "lcom.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "dmark.h"
#include "driver.h"
#include "dselect.h"
#include "bsym.h"
#include "ustdio.h"
#include "uhep.h"
#include "mdcpln.h"
#include "mattr.h"
#include "mdunits.h"
#include "nclicons.h"
#include "view.h"
#include "zkeysym.h"
#include "atext.h"
#include "dpipe.h"
#include "nclfc.h"
#include "dmotif.h"
#include "mpocket.h"

int UZ_nclipv_view;
extern ATXT_FRM UA_txtattr, UB_txtattr;
extern UX_libdata_bag UB_spl_libdata_rec;
extern UX_libdata_bag UB_libdata_rec;
extern int UV_current_sview;
int NCL_event_reject = 0;
extern int UW_dynamic_funkey;
extern  char UR_dpn[];
char UZ_mouse_function[20][80] = {
	"", "", "","","","","","","","",
	"", "", "","","","","","","",""};

#define NALPHAKEYS 18
#define SELST ud_lpop()
#define SELEND ud_lpsh(UU_FALSE)

static char *alpha[NALPHAKEYS]={"0","1","2","3","4","5","6","7","8","9",
   ".",",","+","-","/","*","<-","ENTER"};


#define RBUFF(call) if(UD_BUFTEST() == UU_FALSE) \
		{ncl_reset_select(0); call; ncl_reset_select(0);} \
		else uu_uerror0(UD_DASHEP, 77)

extern int NT_FuncEvent, NT_FuncEof;
extern int NCL_mouse_func;
int NCL_funkey = 0;
int UW_update_stat = 0;
extern UU_LOGICAL LW_active;
static int S_tmp_ipv = 1;
/*
.....those two function S_check_ipv, S_reset_ipv is for check the positions and
.....set ipv active in order for dynamic function to be applied accordingly (NCL or IPV window)
.....but it is not need now, we only need the mouse active dynamic functions depend on cursor position
.....which already handled in PKTWIN and NCLVIEW window
.....so this two function just simply return.
.....if we need them later, can just remove 'return' statement
*/
static void S_check_ipv()
{
	return;
/*
.....check if the mouse cursor is on the active ipv window
.....if yes, set the ipv active
*/
	if (LW_active)
	{
		if (uw_cursor_onipv())
		{
			UZ_nclipv_view = 1;
			um_set_screen_area(UM_IPV_WINDOW);
			S_tmp_ipv = 1;
		}
	}
}

static void S_reset_ipv()
{
	return;
/*
.....reset back to orginal state
*/
	if (S_tmp_ipv)
	{
		if (UZ_nclipv_view == 1)
		{
			UZ_nclipv_view = 0;
			um_reset_pocket_graphics(UM_IPV_WINDOW);
		}
	}
	S_tmp_ipv = 0;
}
/*********************************************************************
**	 I_FUNCTION : uz_daskey1(sub, parms)
**		This function accepts a daskey number as input and
**		optionally executes its user defined function.
**	 PARAMETERS	
**		 INPUT  :
**					sub: key sub number define in structure UZ_keyfuncs
**							in zkeysym.h
**					parms: paramter of the function if need
**		 OUTPUT :
**	 RETURNS: 0 = failed to call user define function
**	          1 = DAS function call
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_daskey1(sub, parms)
char *parms;
int sub;
{
	int ir;
	ir = 1;
	switch(sub)
/*
...sub number define in structure UZ_keyfuncs
...in zkeysym.h
*/
	{
		case 0:
			uj_tut_on(); break;
		case 1:
			uj_tut_off(); break;
		case 2:
			uc_deleteLast(); break;
		case 4 :
			uz_zrecon(); break;
		case 5:
			uz_zplayback(parms); break;
		case 6:
			uz_zrecoff(); break;
		case 7:
			ud_prompt(); break; 
		case 8:
			ud_echoline(); break;
/*
		case 9:
			uz_zpause(); break;
*/
		case 10:
			uj_help(); break;
		case 11:
			uz_zpanic(); break;
/*
.....removed, doing nothing
.....Yurong 10/14/98
*/
/*
		case 12:
			ud_delay(" ");  break;
*/
		case 13:
			uw_mfmenu_reset(UU_TRUE,UU_TRUE,UU_FALSE); break;
		case 27:
			uz_zpanic(); break;
		case 33:
			uz_zdrafting(); break;
		case 34:
			uq_calc(); break;
		case 35:
			uz_zpause(); break;
		case 36:
			uz_zresu(); break;
		default: 
			ir = 0;    break;
	}
	return ir;	
}		
/*********************************************************************
**	 I_FUNCTION : uz_user_dascalls(index)
**		This function accepts a daskey actually hit by user and 
**		executes its user defined function.
**	 PARAMETERS	
**		 INPUT  :
**				index: daskey name	
**		 OUTPUT :
**	 RETURNS: 
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_user_dascalls(index)
char* index;
{	int i, markval;
	if(index[0]=='\0')
		return 1;
	for(i=0; i<UD_subsyslen; i++)
	{
		if((ud_strcomp(&index[1], UD_subsys[i].name) < 0)||
				(ud_strcomp(&index[1], "prompt") < 0))
		{
			UD_MARK(markval, UU_TRUE);
			if(markval == 0)
			{	ud_rpwrmenu(index, "", "DASKEY");
				if(strcmp(&index[1],"prompt")==0)
				{
					ud_prompt();      
					break;
				}
				else
				{
/*
...execute define functions
*/
					if(UD_subsys[i].nump == 0)
						(*UD_subsys[i].ddentry)();
					else
						(*UD_subsys[i].ddentry)(index);
				}
			}

			UD_UNMARK(markval);
		}
	}
	if (i==UD_subsyslen)
		return 1;
	return 0;
}
/*********************************************************************
**	 I_FUNCTION : uz_user_keydef(ktab,index,xflag)
**		This function accepts a key definition structure as input and
**		optionally executes its user defined function.
**	 PARAMETERS	
**		 INPUT  :
**			ktab   = Input function key table.
**			xflag  = 1 = Call this function now.
**		 OUTPUT :
**			index  = Returns the DAS function call, if this key was
**			         programmed using a DAS function.
**     RETURNS: 0 = Normal function call.
**              1 = DAS function call (index contains DAS function).
**              2 = This key is not associated with a function.
**              3 = Alpha key.
**              5 = Normal function call but it will treat as text input
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_user_keydef(ktab,index,xflag)
int xflag;
char **index;
UZ_keytable ktab;
#define RESL if(UV_act_screen[0].nvports>1)ud_dtcord(UD_LOCATOR,1,1)
{
#if UU_COMP==UU_WIN2K
	int i;
#endif
	int i1,irtn, status;
	int pos[2];
	int size[2];
	char buf[80];
	char parms[500], text_str[256];
	UX_pathname fname; 
/*
.....Initialize routine
*/
/*
.....why?, comment out because when dynamic view, dynamic mouse, we allow
.....user to use key function to alloc dynamic center, and we need this 
.....UZ_nclipv_view value, can't reset here
.....Yurong 10/7/04
*/
/*	UZ_nclipv_view = 0; */
/*
.....pass in this parameter string into function if needed
*/
	if ((ktab.params==NULL)||(ktab.params[0]=='\0'))
		parms[0] = '\0';
	else
		strcpy(parms, ktab.params);
/*
.....No key definition
*/
	if (ktab.type == NOKEY)
	{
		return(2);
	}
/*
.....added menu key
.....ignore the flag now
.....Yurong 4/4/00
*/
	if (ktab.type == MENUKEY)
		goto doit;
/*
.....Make sure key is valid in this application
*/
	if (ktab.flag == SELFL && !SELECT_UP) return(2);
	if (UM_2d3d_mode == UM_2D)
	{
		if (!(ktab.flag & DRWFL) && ktab.flag != SELFL)
		{
			if (xflag == 1 && NCL_mouse_func == 0)
				uu_uerror1(UU_SIGNON,6,"This Menu");
			return(2);
		}
	}
	else
	{
		if (ktab.flag == DRWFL)
		{
			if (xflag == 1 && NCL_mouse_func == 0)
				uu_uerror1(UU_SIGNON,8,"This Menu");
			return(2);
		}
		if ((UU_application == UU_NCLNIS && !(ktab.flag & NISFL)) ||
			(UU_application == UU_NCLSIGNON && !(ktab.flag & SGNFL)))
		{
			return(2);
		}
		if (ktab.flag == IPVFL && UL_ipv == 0) return(2);

		if (UU_application == UU_NCLCAM)
		{
			if (UL_cad == 0 && ktab.flag & CADFL && !(ktab.flag & CAMFL))
			{
				if (xflag == 1 && NCL_mouse_func == 0)
					uu_uerror1(UU_SIGNON,15,"This Menu");
				return(2);
			}
			if (UL_cam == 0 && ktab.flag & CAMFL && !(ktab.flag & CADFL))
			{
				if (xflag == 1 && NCL_mouse_func == 0)
					uu_uerror1(UU_SIGNON,16,"This Menu");
				return(2);
			}
		}
		else if (UU_application == UU_NCLCADD)
		{
			if (UL_cam == 0 && ktab.flag & CAMFL && !(ktab.flag & CADFL))
			{
				if (xflag == 1 && NCL_mouse_func == 0)
					uu_uerror1(UU_SIGNON,16,"This Menu");
				return(2);
			}
		}
	}
doit:
/*
.....added auto save unibase 
.....Yurong 9/14/98
.......Added auto save part program - ASF 1/13/14.
*/
	ur_cond_auto_save();
	ncl_cond_auto_save();

/*
.....DAS defined keys
*/
	if (ktab.type == DASKEY)
	{
		*index = UZ_daskey[ktab.sub];
		return(1);
	}
/*
.....Alpha keys
*/
	if (ktab.type == ALPHAKEY)
	{
		*index = alpha[ktab.sub];
		return(3);
	}
/*
.....Function execution keys
*/
	irtn = 0;
	if (xflag || ktab.flag & EXECFL)
	{
		UD_MARK(i1,UU_FALSE);
		if (i1 == 0)
		{
			ud_lpsh(UU_FALSE);
/*
........NIS defined keys
*/
			if (ktab.type == NISKEY)
			{
				switch (ktab.sub)
				{
				case 0: 
						if (UW_dynamic_funkey==-1)
						{
							if (UD_pickmode==1)
								ud_select_nextpk();
							else
							{
								NCL_funkey = 1;
								ud_select_nextpk();
							}
						}
						else
						{
							if (UZ_nclipv_view == 1)
								ul_ipv_dyncenter();
							else
								uvu_dyncenter();
						}
					break;
				case 1: status = uz_load_keydef_file(parms); 
					break;
				case 2: break;
				case 3: break;
				case 4: break;
				case 5: break;
				case 6: break;
				case 7: break;
				case 8: uz_cptrtog(); break;
				case 9: ul_system(0); break;
/*
.....added parameter
.....Yurong 12/1/00
*/
				case 10: ul_system(1, parms); break;
				case 11: ul_system(2); break;
				case 12: ul_set_ncl_mod(); break;
				case 13: ul_que_ncl(); break;
				case 14: ul_que_util("",0); break;
				case 15: ul_display_batch(1); break;
				case 16: ul_display_batch(2); break;
				case 17: ul_submit_batch(1); break;
				case 18: ul_submit_batch(2); break;
				case 19: ul_submit_batch(3); break;
				case 20: ul_display_batch(3); break;
				case 21: ul_status_batch(); break;
				case 22: ulu_copy(); break;
				case 23: ulu_rename(); break;
				case 24: ulu_delete(); break;
				case 25: ulu_purge(); break;
				case 26: uz_iface_modals(); break;
				case 27: uz_pos_modals(); break;
				case 28: ul_spawn("toolib",2); break;
#if UU_COMP == UU_WIN2K
				case 29: uw_edit_menuarea(); break;
#endif
				case 30: ud_print_screen("",-1,0,0); break;
				case 31: ul_set_display_mod(); break;
				case 32: ulu_display(1); break;
				case 33: ulu_display(2); break;
				case 34: ulu_display(3); break;
				case 35: ulu_display(4); break;
				case 36: ulu_display(5); break;
				case 37: ulu_edit(1); break;
				case 38: ulu_edit(2); break;
				case 39: ulu_edit(3); break;
				case 40: ulu_edit(4); break;
				case 41: ulu_edit(5); break;
				case 42: ulu_print(1); break;
				case 43: ulu_print(2); break;
				case 44: ulu_print(3); break;
				case 45: ulu_print(4); break;
				case 46: ulu_print(5); break;
				case 47: ul_status_print(); break;
				case 48: ul_plot_it(1); break;
				case 49: ul_set_plot_mod(); break;
				case 50: ul_plot_it(2); break;
				case 51: ul_post_it(1,0); break;
				case 52: ul_post_it(2,0); break;
				case 53: ul_post_it(4,0); break;
				case 54: ul_util_it(2,2); break;
				case 55: ul_util_it(4,1); break;
				case 56: ul_set_punch_mod(); break;
				case 57: ul_set_dnc_mod(); break;
				case 58: ul_punch_it(1,0); break;
				case 59: ul_punch_it(1,1); break;
				case 60: ul_punch_it(2,0); break;
				case 61: ul_punch_it(2,1); break;
				case 62: ul_punch_it(3,0); break;
				case 63: ul_punch_it(3,1); break;
				case 64: ul_dnc_it(0); break;
				case 65: ul_dnc_it(1); break;
				case 66: ul_spawn("ncliges",2); break;
				case 67: ul_spawn("vdatool",2); break;
				case 68: ul_spawn("mcvtool",2); break;
				case 69: ul_spawn("mpttool",2); break;
				case 70: ul_spawn("qcvtool",2); break;
				case 71: ul_spawn("nmgtool",2); break;
				case 72: ul_spawn("dcnvtool",2); break;
				case 73: 
					fname[0] = '\0';
					status = ud_load_layout(fname);
					break;
				case 74: ud_save_layout("");
							break;
				case 75: ud_menu_design();
							break;
				case 76: 
					status = uz_load_mousedef(parms, 0);
					break;
				}
			}
/*
........CAD/CAM defined keys
*/
			else if (ktab.type == NCLKEY)
			{
				switch (ktab.sub)
				{
				case 0: ur_unibase_modals(); break;
				case 1: uz_repaint(0); break;
				case 2: uz_repaint(1); break;
				case 3: uz_view_from_axis(4); break;
				case 4: uz_view_from_axis(5); break;
				case 5: uz_view_from_axis(6); break;
				case 6: uz_view_from_axis(7); break;
				case 7: RESL; nclu_toggle_labels(1); break;
				case 8: umu_inactgrid(); uz_actgrid("Grid OFF"); break;
				case 9: umu_gridfrm(); uz_actgrid("Grid ON"); break;
				case 10: RESL; nclu_erase(0); break;
				case 11: RESL; nclu_color_change(); break;
				case 12: RESL; nclu_layer_change(); break;
				case 13: RESL; umu_get_def_attr();
					uz_actlayer(ur_get_attrmdl_layer()); break;
				case 14: break;
				case 15: RESL; nclu_line_style_change(); break;
				case 16: nclu_set_pick_aper(); break;
				case 17: nclu_disply(0); break;
				case 18: RESL; nclu_toggle_labels(2); break;
				case 19: nclu_set_mark(); break;
				case 20: nclu_limit_pick(); break;
				case 21: uvu_dyncenter(); break; 
				case 22: nclu_set_verify(); break;
				case 23: uz_load_view(parms); break;
				case 24: uz_trim_extend(2); break;
				case 25: uz_change_view(); break;
				case 26: S_check_ipv(); uz_extrema_zoom(); break;
				case 27: S_check_ipv(); uz_pan(); break;
				case 28: S_check_ipv(); nclu_reset_prev(UU_TRUE); break;
				case 29: S_check_ipv(); uz_window_zoom(); break;
				case 30: S_check_ipv(); uz_zoom(); break;
				case 31: S_check_ipv(); uz_reset_view(); break;
				case 32: uz_save_view(); break;
				case 33: uz_scale_xy(); break;
				case 34: uz_view_screen(parms); break;
				case 35:
					{
						ncl_calculator(text_str); 
						strcpy (*index, text_str);
						irtn = 5;
						break;
					}
				case 36: RESL; nclu_toggle_leader(1); break;
				case 37: RESL; nclu_toggle_leader(2); break;
				case 38: ncl_position_labels();break;
				case 39: ncl_reset_labloc();break;
				case 40: uvu_savev(); break;
				case 41: S_check_ipv(); uz_dyn_mouse(); break;
				case 42: uvu_deletev(); break;
				case 43: S_check_ipv(); uz_view_from_axis(0); break;
				case 44: S_check_ipv(); uz_view_from_axis(1); break;
				case 45: S_check_ipv(); uz_view_from_axis(2); break;
				case 46: uz_tracut_view(); break;
				case 47: uz_refsys_view(); break;
				case 48: uz_matrix_view(); break;
				case 49: S_check_ipv(); uz_dyn_zoom(); break;
				case 50: S_check_ipv(); uz_dyn_pan(); break;
				case 51: S_check_ipv(); uz_dyn_unzoom(); break;
				case 52: S_check_ipv(); uv_dynstep_view(UV_current_sview,1,-1); break;
				case 53: S_check_ipv(); uv_dynstep_view(UV_current_sview,1,1); break;
				case 54: S_check_ipv(); uv_dynstep_view(UV_current_sview,2,-1); break;
				case 55: S_check_ipv(); uv_dynstep_view(UV_current_sview,2,1); break;
				case 56: S_check_ipv(); uv_dynstep_view(UV_current_sview,3,-1); break;
				case 57: S_check_ipv(); uv_dynstep_view(UV_current_sview,3,1); break;
				case 58: S_check_ipv(); uv_dynstep_view(UV_current_sview,4,-1); break;
				case 59: S_check_ipv(); uv_dynstep_view(UV_current_sview,4,1); break;
				case 60: S_check_ipv(); uv_dynstep_view(UV_current_sview,5,-1); break;
				case 61: S_check_ipv(); uv_dynstep_view(UV_current_sview,5,1); break;
				case 62: S_check_ipv(); uv_dynstep_view(UV_current_sview,6,-1); break;
				case 63: S_check_ipv(); uv_dynstep_view(UV_current_sview,6,1); break;
				case 64: uv_select_sview(-1); break;
				case 65: uv_select_sview(1); break;
				case 66: S_check_ipv(); uz_rotate_x_y(); break;
				case 67: S_check_ipv(); uz_tumble(); break;
				case 68: S_check_ipv(); uj_dynzrot(); break;
				case 69: uz_rotate_vec(); break;
				case 70: S_check_ipv(); uz_dyn_modals(); break;
				case 71: udm_signoff(UU_FALSE); break;
				case 72: nclu_redisp(); break;
				case 73: umu_get_def_attr(); uz_actlayer(ur_get_attrmdl_layer());
					break;
				case 74: nclu_name_modals(); break;
				case 75: umu_sda1_layer_num(); uz_actlayer(ur_get_attrmdl_layer());
					break;
				case 76: umu_layer_manage(); break;
				case 77: ncl_display_buffer(); break;
				case 78: nclu_modals(); break;
				case 79: nclu_pick_modals(); break;
				case 80: nclu_units(0); uz_actunits(UM_cpln.length_unit); break;
				case 81: nclu_units(1); uz_actunits(UM_cpln.length_unit); break;
				case 82: ucu_query(1); break;
				case 83: udm_signoff(UU_TRUE); 
#if UU_COMP==UU_WIN2K
/*
......sent reject event all the way to the top then jump before restart
*/
					for (i=0; i<UD_markptr;i++)
						uw_ntuser_event(3, 3);
#endif
					break;
				case 84: uz_invisible(); break;
				case 85: uz_invis_all(); break;
				case 86: uz_visible(); break;
				case 87: uz_visible_all(); break;
				case 88: umu_get_ent_attr(); break;
				case 89: uz_trim_extend(0); break;
				case 90: umu_trim1_curve(1); break;
				case 91: break;
				case 92: nclu_alter_label(); break;
				case 93: um_drw_mod_axis(UU_TRUE); break;
				case 94: um_drw_mod_axis(UU_FALSE); break;
				case 95: um_drw_cpl_axis(UU_TRUE,UU_FALSE); break;
				case 96: um_drw_cpl_axis(UU_FALSE,UU_FALSE); break;
				case 97: nclu_modsys(1); break;
				case 98: uz_setcpln(0,2); break;
				case 99: uz_act_grid(); break;
				case 100: uz_inact_grid(); break;
				case 101: nclu_chg_modaxis(0); break;
				case 102: nclu_chg_modaxis(1); break;
				case 103: nclu_chg_modaxis(2); break;
				case 104: nclu_snap_modaxis(); break;
				case 105: nclu_rot_modaxis(); break;
				case 106: uz_setcpln(1,1); break;
				case 107: uz_setcpln(2,1); break;
				case 108: uz_chg_wp_axis(); break;
				case 109: uz_swap_wp_axis(); break;
				case 110: uz_wp_to_plane(); break;
				case 111: uz_rot_wp_axis(); break;
				case 112: SELST; uz_single(); SELEND; break;
				case 113: SELST; ud_chain(UU_FALSE); SELEND; break;
				case 114: SELST; uz_all_display(); SELEND; break;
				case 115: SELST; uz_nclu_allentity_type(); SELEND; break;
				case 116: SELST; ud_chain_modals(UU_FALSE); SELEND; break;
				case 117: SELST; uz_region_in(); SELEND; break;
				case 118: SELST; uz_region_out(); SELEND; break;
				case 119: SELST; uz_reg_in_x(); SELEND; break;
				case 120: SELST; uz_reg_out_x(); SELEND; break;
				case 121: SELST; ud_inevt("STRING device = 1 string = \\\\3"); SELEND; break;
				case 122: SELST; ud_reject(UD_Selrej_ptr, UD_Selrej_cnt); SELEND; break;
				case 123: SELST; ud_reject(0, 0); SELEND; break;
				case 124: SELST; uz_set_sel_fil(); SELEND; break;
				case 125: SELST; uz_filtered_sgl(); SELEND; break;
				case 126: SELST; uz_filtered_chn(); SELEND; break;
				case 127: SELST; uz_fltr_all_dsp(); SELEND; break;
				case 128: SELST; uz_freg_in(); SELEND; break;
				case 129: SELST; uz_freg_out(); SELEND; break;
				case 130: SELST; uz_freg_in_x(); SELEND; break;
				case 131: SELST; uz_freg_out_x(); SELEND; break;
				case 132: 
#if UU_COMP!=UU_WIN2K
					uw_mfmenu_reset(UU_FALSE,UU_TRUE,UU_TRUE); 
#endif
					break;
				case 133: 
#if UU_COMP!=UU_WIN2K
					uw_mfmenu_reset(UU_FALSE,UU_FALSE,UU_TRUE); 
#endif
					break;
				case 134: ncl_label_modals(); break;
/*
.....added for display part of 
.....entity date
.....Yurong
*/
				case 135: ucu_query(2); break;
/*
.....Added for shading and unshading surfaces and shapes,
.....merged 4 functions into these two.  JLS 9/30/99
*/
				case 136: uz_render_surf(); break;
				case 137: nclu_unshade_surf(); break;
				case 138: nclu_analyze_surf(0); break;
				case 139: nclu_analyze_surf(1); break;
/*
..... View down tool.
*/
				case 140: uz_tool_view(); break;
				case 141: nclc_selectsystem(); break;
				case 142: nclc_selectunits(); break;
				case 143: nclu_geo_measure(); break;
				case 144: ncl_pan_toggle(); break;
				case 145: ncl_rotate_toggle(); break;
				case 146: ncl_domi_toggle(); break;
				case 147: ncl_gain_down(); break;
				case 148: ncl_gain_up(); break;
				case 149: ncl_gain_def(); break;
				case 150: ncl_toggle_spacemouse(); break;
				case 151: uz_vport_shaded(1); break;
				case 152: uz_vport_shaded(2); break;
				case 153: uz_vport_shaded(3); break;
				case 154: uz_vport_shaded(4); break;
				case 155: ul_session_save(UU_NULL); break;
				case 156: 
					fname[0] = '\0';
					ul_session_load(fname,UU_TRUE);
					break;
				case 157: nclu_sf_chain();  break;
				case 158: ncl_color_modals();  break;
				case 159: uvu_background_form();  break;
				case 160: ncl_open_file(parms);  break;
				case 161: break;
				case 162: ul_new_session(); break;
				case 163: umu_set_surf_edges(UU_FALSE); break;
				case 164: umu_set_surf_edges(UU_TRUE); break;
				case 165: SELST; ncl_sel_filter(); SELEND; break;
				case 166: uz_swap_view(); break;
				case 167: uz_swap_vis(); break;
				case 168: ncl_load_color(parms, 0); break;
				case 169: ncl_load_color(parms, 1); break;
				case 170: uz_key_pick_loc();break;
				case 171: uw_ntreset_spacemouse(); break;
				case 172: uw_form_desgn(0); break;
				case 173: ur_unibase_stat(parms); break;
				case 174: nclu_auto_save(); break;
				}
			}
/*
........CAM defined keys
*/
			else if (ktab.type == CAMKEY)
			{
				switch (ktab.sub)
				{
				case 0: nclu_rename_geom(UU_TRUE); break;
				case 1: nclc_input_mode(); break;
				case 2: break;
				case 3: break;
				case 4: nclu_arcslp_fillet(); break;
				case 5: nclu_fmill();break;
				case 6: nclu_create_shape(); break;
				case 7: nclu_shape_mod_display(); break;
				case 8: nclu_cv_sf_edge(1); break;
				case 9: nclu_cv_io_sf_sf(1); break;
				case 10: nclu_create_test_case(); break;
				case 11: uz_zncl_cmd(); break;
				case 12: break;
				case 13: break;
				case 14: umu_shape_modals(); break;
				case 15: ncl_playback_current(); break;
				case 16: umu_cv_offset_comp(); break;
				case 17: nclu_cmd_show(6); break;
				case 18: ncl_edit_option(); break;
				case 19: ncl_toggle_label(); break;
				case 20: nclu_disp_cut(); break;
				case 21: break;
				case 22: nclu_erase_motion(); break;
				case 23: ncl_step_displayed(-1,0,UU_NULL); break;
				case 24: ncl_step_displayed(1,0,UU_NULL); break;
				case 25: nclu_playback(); break;
				case 26: nclu_playfile(); break;
				case 27: nclu_pt_xy(0); break;
				case 28: nclu_pt_xy(1); break;
				case 29: nclu_pt_intof(); break;
				case 30: nclu_pt_intof3(); break;
				case 31: nclu_pv_pv_xxx_vesca(2); break;
/*
.....Added for subtraction of vectors.  JLS 2/9/99
*/
				case 32: nclu_ve_ve_ve(0); break;
				case 33: nclu_pt_endpt(); break;
				case 34: nclu_pt_te(); break;
				case 35: nclu_pt_patern(); break;
				case 36: nclu_pt_ce_ci(); break;
				case 37: nclu_pt_on_sf(0); break;
				case 38: nclu_waterline_cut(); break;
				case 39: nclu_pt_ci_atangl(); break;
				case 40: nclu_pt_on_cv(0); break;
				case 41: nclu_pt_dist_cv(); break;
				case 42: nclu_pt_pt_ci_angle(); break;
				case 43: nclu_pt_offset(); break;
				case 44: nclu_genpts(1); break;
				case 45: nclu_genpts(0); break;
				case 46: nclu_ln_pt_pt(); break;
/*
.....Changed so the routine that creates lines from xy_xy
.....is the same as the xyz_xyz, passing in 0 means xy and 
.....passing in 1 indicates xyz. JLS 2/12/99
*/
				case 47: nclu_ln_xyz_xyz(0); break;
				case 48: nclu_ln_xyz_xyz(1); break;
				case 49: nclu_ln_tanto_ci_ci(); break;
				case 50: nclu_ln_parlel_ln(); break;
				case 51: nclu_ln_plane(); break;
				case 52: nclu_ln_pt_parlel_ln(); break;
				case 53: nclu_ln_pt_perpto_ln(); break;
				case 54: nclu_ln_xyz_xyz(2);break;
				case 55: nclu_ln_pt_atangl_ln(); break;
				case 56: nclu_sfparm_redef();  break;
				case 57: nclu_ln_pt_tanto_cv(); break;
				case 58: nclu_ln_intof_pl_pl(); break;
				case 59: nclu_ln_ci_atangl_ln(); break;
				case 60: nclu_ln_fwd(); break;
				case 61: nclu_ln_xy_axis(1); break;
				case 62: nclu_ln_xy_axis(2); break;
				case 63: nclu_ln_offset(); break;

            case 64: nclu_ci_xy_ra(); break;
            case 65: nclu_ci_xyz_ra(); break;
            case 66: nclu_ci_pt_pt_ve(); break;
            case 67: nclu_ci_pt_pt_pt(); break;
            case 68: nclu_cv_offset1(1); break;
            case 69: nclu_ci_tanto(); break;
            case 70: nclu_ci_tanto_ln_ln_ln(); break;
            case 71: nclu_tlaxis(); break;
            case 72: nclu_cv_offset1(2); break;
            case 73: nclu_ci_ce_tt(); break;
            case 74: nclu_ci_ce_pt_tanto_ln(); break;
            case 75: nclu_ci_ce_pt_tanto_ci(); break;
            case 76: nclu_cv_offset(2); break;
            case 77: nclu_ci_ce_pt_ra(); break;
            case 78: nclu_ci_ce_pt_pt(); break;
            case 79: nclu_set_command_src(); break;
            case 80: nclu_ci_canon_params(); break;
            case 81: nclu_ci_canon_geom(); break;
            case 82: nclu_ci_transl(); break;

				case 83: nclu_goto(1); break;
				case 84: break;
				case 85: break;
				case 86: break;
				case 87: break;
				case 88: break;
				case 89: break;
				case 90: break;
				case 91: break;
				case 92: break;
				case 93: nclu_godlta(); break;
				case 94: nclu_playfeed(); break;
				case 95: nclu_playinterp(); break;
				case 96: nclu_playclip(); break;
				case 97: ncl_tool_attr("motmodal.frm"); break;
				case 98: nclu_cmd_set(parms); break;
#if UU_COMP==UU_WIN2K
				case 99: ncl_macro_formdesgn(parms); break;
#endif
				case 100: nclu_cmd_run(parms); 
					ud_jump(-1,UU_FALSE);
					break;
				case 101: nclu_tool_calc(); break;
				case 102: ncl_macro_call(parms); break;
				case 103: nclu_cmd_edit(1); break;
				case 104: nclu_cmd_edit(2); break;
				case 105: nclu_cmd_edit(3); break;
				case 106: nclu_cmd_edit(4); break;
				case 107: nclu_cmd_edit(5); break;
				case 108: nclu_cmd_edit(6); break;
				case 109: nclu_cmd_edit(7); break;
				case 110: nclu_cmd_edit(8); break;
				case 111: nclu_cmd_show(4); break;
				case 112: nclu_cmd_show(5); break;
				case 113: nclu_cmd_show(6); break;
				case 114: nclu_cutter_def(); break;
				case 115: nclu_contour(); break;
				case 116: break;
				case 117: break;
				case 118: break;
				case 119: break;
				case 120: break;
				case 121: break;
				case 122: break;
				case 123: break;
				case 124: break;
				case 125: break;
				case 126: break;
				case 127: break;
				case 128: nclu_multax(1); break;
				case 129: nclu_multax(2); break;
				case 130: break;
				case 131: break;
				case 132: nclu_cut(1); break;
				case 133: nclu_cut(4); break;
				case 134: nclu_cut(5); break;
				case 135: nclu_cut(2); break;
				case 136: nclu_cut(3); break;
				case 137: nclu_cut_copy(1); break;
				case 138: nclu_cut_copy(2); break;
				case 139: nclu_cut_copy(3); break;
				case 140: nclu_cut_copy(4); break;
				case 141: nclu_cut_copy(5); break;
				case 142: nclu_cut_copy(6); break;
				case 143: nclu_cut(6); break;
				case 144: nclu_cut(7); break;
				case 145: nclu_fedrat(); break;
				case 146: break;
				case 147: break;
				case 148: break;
				case 149: break;
				case 150: nclu_go(); break;
				case 151: break;
				case 152: break;
				case 153: break;
				case 154: break;
				case 155: break;
				case 156: break;
				case 157: break;
				case 158: break;
				case 159: break;
				case 160: break;
				case 161: nclu_vmp_mod(); break;
				case 162: nclu_pocket(); break;
				case 163: nclu_pocket_pokmod(); break;
				case 164: nclu_pocket_advanced(); break;
				case 165: nclu_scrub(); break;
				case 166: nclu_rmill(); break;
				case 167: uz_lathe_rough(); break;
				case 168: uz_lathe_finish(); break;
				case 169: nclu_remove(); break;
				case 170: nclu_redef_fillet(); break;
				case 171: nclu_redef_close(); break;
				case 172: nclu_revers_geom(); break;
				case 173: nclu_clone_scale(); break;
				case 174: nclu_clone_translate(0); break;
				case 175: nclu_clone_translate(1); break;
				case 176: nclu_clone_rotate(0); break;
				case 177: nclu_clone_mirror(); break;
				case 178: nclu_clone_matrix(); break;
				case 179: nclu_move_scale(); break;
				case 180: nclu_move_translate(0); break;
				case 181: nclu_move_translate(1); break;
				case 182: nclu_move_rotate(0); break;
				case 183: nclu_move_mirror(); break;
				case 184: nclu_move_matrix(); break;
				case 185: nclu_pl_ijkd(); break;
				case 186: nclu_pl_pt_pt_pt(); break;
				case 187: nclu_pl_pt_parlel_pl(); break;
				case 188: nclu_pl_parlel_pl(); break;
				case 189: nclu_pl_pt_perpto_ve(); break;
				case 190: nclu_pl_pt_pt_perpto_pl(); break;
				case 191: nclu_pl_pt_perpto_pl_pl(); break;
				case 192: nclu_pl_ln_perpto_pl(); break;
				case 193: nclu_pl_pv(); break;
				case 194: nclu_cv_pt(1); break;
/*
.....I do believe that nclu_cv_pt_ve is mixed up
.....with nclu_cv_fit_pt_ve. So switched 205 and 195 JLS 2/23/99
.....Additionally the cases 197 and 209 were merged into
.....196 and 208.
*/
				case 195: nclu_cv_pt_ve(1); break;
				case 196: nclu_cv_pt_thru_pt(1); break;
				case 197: nclu_cv_io_sf_sf(2); break;
				case 198: nclu_cv_fit_pt_thru_pt(1,1); break;
				case 199: nclu_cv_fit_pt_thru_pt(2,1); break;
				case 200: nclu_cv_sf_edge(0); break;
				case 201: nclu_cv_io_sf_sf(0); break;
				case 202: nclu_cv_offset(1); break;
				case 203: nclu_cv_conic(); break;
				case 204: nclu_cv_composite(); break;
				case 205: nclu_cv_fit_pt_ve(1); break;
				case 206: nclu_cv_pt(2); break;
				case 207: nclu_cv_fit_pt_ve(2); break;
				case 208: nclu_cv_pt_thru_pt(2); break;
				case 209: nclu_cv_sf_edge(2); break;
				case 210: nclu_cv_fit_pt_thru_pt(1,2); break;
				case 211: nclu_cv_fit_pt_thru_pt(2,2); break;
				case 212: nclu_cv_pt_ve(2); break;
				case 213: nclu_ve_ijk(); break;
				case 214: nclu_ve_fwd(); break;
				case 215: nclu_ve_tlaxis(); break;
				case 216: nclu_ve_pt_pt(); break;
				case 217: nclu_ve_pv(); break;
				case 218: nclu_ve_perpto_pl(); break;
				case 219: nclu_ve_unit_ve(); break;
				case 220: nclu_ve_ve_ve(1); break;
				case 221: nclu_ve_ve_cross_ve(); break;
				case 222: nclu_ve_ve_times(); break;
				case 223: nclu_ve_intof_pl_pl(); break;
				case 224: nclu_ve_pt_sf(); break;
				case 225: nclu_ve_tt_cv(); break;
				case 226: nclu_sf_net(1); break;
				case 227: nclu_sf_out(); break;
				case 228: nclu_sf_ruled(1); break;
				case 229: nclu_sf_bnd_slp(1); break;
				case 230: nclu_sf_bnd_0(1); break;
				case 231: nclu_sf_fit_cv(1,1); break;
				case 232: nclu_sf_fit_cv(2,1); break;
				case 233: nclu_sf_offset(); break;
				case 234: nclu_sf_pl_pl_ra(); break;
				case 235: nclu_sf_fillet(); break;
				case 236: nclu_sf_mesh(); break;
				case 237: nclu_sf_quilt(); break;
				case 238: nclu_sf_net(0); break;
				case 239: umu_srf_modals(); break;
				case 240: umu_set_surf_uv_paths(); break;
				case 241: nclu_sf_ruled(2); break;
				case 242: nclu_sf_bnd_slp(2); break;
				case 243: nclu_sf_bnd_0(2); break;
				case 244: nclu_sf_fit_cv(1,2); break;
				case 245: nclu_sf_fit_cv(2,2); break;
				case 246: nclu_nsf_multi_cv(); break;
				case 247: nclu_nsf_revolv(2); break;
				case 248: nclu_pn_pt_ptve(); break;
				case 249: nclu_pn_pt_ve_incr(); break;
				case 250: nclu_pn_ci(); break;
				case 251: nclu_pn_ci_incr(); break;
				case 252: nclu_pn_pn_ve(); break;
				case 253: nclu_pn_pn_ve_incr(); break;
				case 254: nclu_pn_ptpn_ptpn(); break;
				case 255: nclu_mx_pt_ve_ve(); break;
				case 256: nclu_mx_invers(); break;
				case 257: nclu_mx_mx_mx(); break;
				case 258: nclu_mx_mirror(); break;
				case 259: nclu_mx_scale(); break;
				case 260: nclu_mx_rotate(); break;
				case 261: nclu_mx_transl(); break;
				case 262: nclu_mx_params(); break;
				case 263: nclu_post_word_add(); break;
				case 264: nclu_cmd_show(1); break;
				case 265: nclu_cmd_show(2); break;
				case 266: nclu_cmd_show(3); break;
				case 267: nclu_cmd_show(7); break;
				case 268: nclu_cmd_show(8); break;
				case 269: nclu_cmd_show(9); break;
				case 270: nclu_cmd_show(10); break;
				case 271: 
					status = nclu_read_part_prog(NULL, 2); 
/*
					if ((UL_program[0]!='\0')&&(status==0))
					{
						strcpy(fname, UL_program);
						strcat(fname,".");
						strcat(fname,UL_program_suffix);
						nclc_save_recent_file(fname, 5);
					}
*/
					break;
				case 272: nclu_read_part_prog(NULL, 1); break;
				case 273: nclu_edt(); break;
				case 274: break;
				case 275: nclu_pv_perpto_geo();break;
				case 276: nclu_nsf_revolv(1); break;
				case 277: nclu_revers_clfile(0); break;
				case 278: nclu_revers_clfile(1); break;
				case 279: nclu_get_allgeo();  break;
				case 280: RBUFF(nclu_get_geo());  break;
				case 281: RBUFF(nclu_get_geo_rename()); break;
				case 282: RBUFF(nclu_get_geo_thru());   break;
				case 283: RBUFF(nclu_get_geo_thru_re());   break;
				case 284: nclu_get_layer();   break;
				case 285: nclu_get_type(); break;
				case 286: nclu_put_allgeo();  break;
				case 287: RBUFF(nclu_put_geo());  break;
				case 288: RBUFF(nclu_put_geo_thru());   break;
				case 289: nclu_get_clfile(); break;
				case 290: nclu_get_asfile(); break;
				case 291: nclu_edit_clfile(); break;
				case 292: nclu_erase_clfile(); break;
				case 293: nclu_sequnc(0); break;
				case 294: nclu_sequnc(1); break;
				case 295: nclu_sequnc(2); break;
				case 296: nclu_sequnc(3); break;
				case 297: nclu_pv_xy(0); break;
				case 298: nclu_pv_xy(1); break;
				case 299: nclu_pv_pt_pt(); break;
				case 300: nclu_pv_pt_ve(); break;
				case 301: nclu_pv_ln(); break;
				case 302: nclu_pv_te_tlaxis(); break;
				case 303: nclu_pv_te_fwd(); break;
				case 304: nclu_pv_ce_ci(); break;
				case 305: nclu_pv_io_pl_pl_pl(); break;
				case 306: nclu_pv_cv_dist(); break;
				case 307: nclu_pv_pv_xxx_vesca(1); break;
				case 308: nclu_pv_pv_xxx_vesca(3); break;
				case 309: nclu_pv_unit_pv(); break;
				case 310: nclu_pv_perpto_pl(0); break;
				case 311: nclu_put_geo_type(); break;
				case 312: nclu_pv_pv_xxx_vesca(4); break;
				case 313: nclu_pv_pv_xxx_vesca(5); break;
				case 314: nclu_put_layer(); break;
				case 315: nclu_pv_patern(); break;
				case 316: nclu_genpts(2); break;
				case 317: break;
				case 318: break;
				case 319: nclu_loopst(); break;
				case 320: nclu_doloop(); break;
				case 321: nclu_one_cmd(); break;
				case 322: nclu_if(); break;
				case 323: nclu_jumpto(); break;
				case 324: nclu_undo(); break;
				case 325: nclu_loopnd(); break;
				case 326: nclu_termac(); break;
				case 327: nclu_fini(); break;
				case 328: nclu_modsys(0); break;
				case 329: nclu_modsys(1); break;
				case 330: nclu_refsys(0,0,0); break;
				case 331: nclu_refsys(2,0,0); break;
				case 332: nclu_zsurf(); break;
				case 333: nclu_can(); break;
				case 334: nclu_pv_revsf();break;
				case 335: nclu_cv_revsf(0);break;
				case 336: nclu_cv_revsf(1);break;
				case 337: nclu_sym_table();break;
				case 338: nclu_get_data(); break;
				case 339: nclu_put_data(); break;

				case 340: nclu_pt_on_cv(1); break;
				case 341: nclu_pt_on_sf(1); break;

				case 342: uz_ubfn_invisible();  break;
				case 343: uz_ubfn_invis_all();  break;
				case 344: uz_ubfn_visible();  break;
				case 345: uz_ubfn_vis_all();  break;
				case 346: nclu_rem_sufint(); break;
				case 347: nclu_num(0); break;
				case 348: nclu_num(1); break;
				case 349: nclu_cv_out(2); break;
				case 350: nclu_cv_out(3); break;
				case 351: nclu_cv_out(4); break;
				case 352: nclu_cv_out(5); break;
				case 353: nclu_pt_proj_sf(); break;
				case 354: nclu_pv_proj_sf(); break;
				case 355: nclu_project_sf(0); break;

				case 356: nclu_sf_trim(); break;
				case 357: nclu_sf_redef(); break;

				case 358: nclu_project_sf(1); break;
				case 359: nclu_project_sf(2); break;
				case 360: nclu_pl_pt_ve_ve(); break;
				case 361: nclu_open_usecond(parms); break;
				case 362: nclu_close_usecond(); break;
				case 363: nclu_ci_offset(); break;
				case 364: nclu_comp_cv_out(); break;
				case 365: nclu_cv_sfs(1); break;
				case 366: nclu_cv_sfs(0); break;
				case 367: nclu_trim_remove(); break;
				case 368: nclu_profile(); break;
				case 369: 
					{
						ncl_scalar_form(text_str); 
						strcpy (*index, text_str);
						irtn = 5;
						break;
					}
				case 370: ncl_scalar_define(UU_NULL); break;
				case 371: nclu_get_allgeo_rename();  break;
				case 372: nclu_pl_fit();  break;
				case 373: nclu_pv_fit();  break;
				case 374: nclu_pt_fit();  break;
				case 375: nclu_notes();  break;
				case 376: nclu_notes_attrib(0);  break;
				case 377: nclu_notes_attrib(1);  break;
				case 378: nclu_engrave();  break;
				case 379: ubu_explode_syminstance(2); break;
				case 380: nclu_solid_box(1); break;
				case 381: nclu_solid_box(2); break;
				case 382: nclu_solid_cone(1); break;
				case 383: nclu_solid_cone(2); break;
				case 384: nclu_solid_cyl(1); break;
				case 385: nclu_solid_cyl(2); break;
				case 386: nclu_solid_cyl(3); break;
				case 387: nclu_solid_sweep(); break;
				case 388: nclu_solid_contour(); break;
				case 389: nclu_solid_revolve(); break;
				case 390: nclu_solid_sphere(1); break;
				case 391: nclu_solid_sphere(2); break;
				case 392: nclu_solid_torus(1); break;
				case 393: nclu_solid_torus(2); break;
				case 394: nclu_solid_torus(3); break;
				case 395: nclu_solid_load(); break;
				case 396: nclu_solid_save(); break;
				case 397: nclu_solid_bound(); break;
				case 398: nclu_solid_load_stl(); break;
				case 399: nclu_stl_save(); break;
				case 400: uju_qry_motion(); break;					
				case 401: nclu_cv_offset1(3); break;
				case 402: nclu_cv_out(6); break;
				case 403: nclu_cv_out(7); break;
				case 404: nclu_pmill(); break;
				case 405: nclu_smill(); break;
				case 406: nclu_data_stmt(); break;
				case 407: nclu_mx_coordsys();break;
				case 408: ncl_step_displayed(-2,0,UU_NULL); break;
				case 409: ncl_step_displayed(2,0,UU_NULL); break;
				case 410: ncl_step_displayed(-3,0,UU_NULL); break;
				case 411: nclu_vmill(); break;
				case 412: nclu_data_file_stmt(); break;
				case 413: ncl_data_form(); break;
				case 414: nclu_reset_call(parms); break;
				case 415: nclu_solid_compos(); break;
				case 416: nclu_solid_sf_out(); break;
				case 417: nclu_ssplin_compos(); break;
				}
			}
/*
........CAD defined keys
*/
			else if (ktab.type == CADKEY)
			{
				switch (ktab.sub)
				{
				case 0: nclu_rename_geom(UU_FALSE); break;
				case 4: uz_place_symbol(); break;
				case 5: ubu_explode_syminstance(1); break;
				case 6: ubu_instance_display_form("binstdisp.frm"); break;
				case 7: ua_get_txt_attr(&UB_txtattr,"atxtsym.frm"); break;
				case 8: ubu_change_instance_visibility(); break;
				case 9: ubu_create_sym(); break;
				case 10: ubu_delete_lib_symmaster(); break;
				case 11: ubu_rename_lib_symmaster(); break;
				case 12: ubu_archive_symmaster(); break;
				case 16: ubu_load_symmaster(UU_NULL); break;
				case 17: ubu_load_symmaster_lib(); break;
				case 18: ubu_delete_symmaster(); break;
				case 19: ubu_rename_symmaster(); break;
				case 20: uxu_create_lib(&UB_libdata_rec); break;
				case 21: uxu_delete_lib(&UB_libdata_rec); break;
				case 22: uxu_rename_lib(&UB_libdata_rec); break;
				case 23: um_feamain(); break;
				case 24: um_feareset(); break;
				case 25: umu_2d_analysis(); break;
				case 26: umu_2da_modals(); break;
				case 27: umu_c1_pt(); break;
				case 28: umu_c1_deltapt(); break;
				case 29: umu_c1_ipcpc(); break;
				case 30: umu_c1_pt_on_crv_at_pal(); break;
				case 31: umu_c1_nearpt(); break;
				case 32: umu_c1_pt_on_srf_at_pal(); break;
				case 33: umu_c1_lpt(); break;
				case 34: umu_c1_cpt(); break;
				case 35: umu_c2_pp(); break;
				case 36: umu_c2_connected(); break;
				case 37: umu_c2_pt(); break;
				case 38: umu_c2_tt(); break;
				case 39: umu_c2_tancrv_at_pt(); break;
				case 40: umu_c2_parto(); break;
				case 41: umu_c2_parpt(); break;
				case 42: umu_c2_angle(UU_TRUE); break;
				case 43: umu_c2_normcrv_at_pt(); break;
				case 44: umu_c2_angle(UU_FALSE); break;
				case 45: umu_c2_ptveclen(); break;
				case 46: umu_m2_chamfer(); break;
				case 47: umu_m2_multi_chamfer(); break;
				case 48: umu_c3_3pt(UU_TRUE); break;
				case 49: umu_c3_arccraa(); break;
				case 50: umu_c3_arc2pt(); break;
				case 51: umu_c3_ttr(2); break;
				case 52: umu_c3_3tan(1); break;
				case 53: umu_crv_fillet(); break;
				case 54: umu_multi_fillet(); break;
				case 55: umu_c3_3pt(UU_FALSE); break;
				case 56: umu_c3_2diampts(); break;
				case 57: umu_c3_cr(); break;
				case 58: umu_c3_cp(); break;
				case 59: umu_c3_ct(); break;
				case 60: umu_c3_ttr(1); break;
				case 61: umu_c3_3tan(0); break;
				case 62: umu_c3_rc(); break;
				case 63: nclu_cv_rbsp(0); break;
				case 64: nclu_cv_rbsp(1); break;
				case 65: umu_c4_elpart(); break;
				case 66: umu_c4_elbox(); break;
				case 67: umu_c4_pbvxfc(); break;
				case 68: umu_c4_pvpt(); break;
				case 69: umu_c4_hyptax(); break;
				case 70: umu_c4_5conds(0,0); break;
				case 71: umu_c4_iges(); break;
				case 72: umu_c4_p4pts(); break;
				case 73: umu_c5_mergecrv(); break;
				case 74: umu_c5_string(); break;
				case 75: umu_spl5_compcrv(); break;
				case 76: umu_dissolve(); break;
				case 77: umu_linear_curve_sweep(); break;
				case 78: umu_circular_curve_sweep(); break;
				case 79: umu_project_to_plane(); break;
				case 80: nclu_plane_rect(); break;
				case 81: nclu_cylinder(); break;
				case 82: nclu_cone(); break;
				case 83: nclu_sphere(); break;
				case 84: nclu_torus(); break;
				case 85: nclu_revsrf(); break;
				case 86: nclu_tabcyl(); break;
				case 87: nclu_ruledbsp(); break;
				case 88: nclu_rbsf_4bndycrvs(0); break;
				case 89: nclu_rbsf_4bndycrvs(1); break;
				case 90: nclu_rbsf_fitcurves(); break;
				case 91: umu_srf_modals(); break;
				case 92: umu_set_surf_uv_paths(); break;
				case 93: ncl_rev_normal(); break;
				case 94: um_lights(); break;
				case 95: ul_load_lgtmod(0); break;
				case 96: ul_load_lgtmod(1); break;
				case 97: uz_material_modals(); break;
				case 98: uz_lights(); break;
				case 99: uz_delete(); break;
				case 100: uz_mod_entity(); break;
				case 101: uz_del_all_disp(); break;
				case 102: uz_scale(); break;
				case 103: umu_split_curve(); break;
				case 104: umu_reverse_curve(); break;
				case 105: uz_move_pt_pt(); break;
				case 106: uz_move_vector(); break;
				case 107: uz_move_drag(); break;
				case 108: uz_move_rotate(); break;
				case 109: uz_move_mirror(); break;
				case 110: uz_copy_pt_pt(); break;
				case 111: uz_copy_vector(); break;
				case 112: uz_copy_drag(); break;
				case 113: uz_copy_rotate(); break;
				case 114: uz_copy_mirror(); break;
				case 115: uz_copy_scale(); break;
				case 116: 
					fname[0] = '\0';
					status = uz_load(fname); 
					break;
				case 117: uz_save(); break;
				case 118: 
					fname[0] = '\0';
					ur_load_part(fname,1); break;
				case 119: break;
				case 120: break;
				case 121: break;
				case 122: break;
				case 123: ucu_doSelSav(); break;
				case 124: break;
				case 128: uz_zrecon(); break;
				case 129: uz_zrecoff(); break;
				case 130: status = uz_zplayback(parms); 
					break;
/*
.....update status display
.....added by Yurong
.....8/20/97
*/
				case 162: udm_enter_drawing(); uz_status(); break;
				case 163: udm_exit_drawing(); uz_status(); break;
				case 164: umu_c46_create_drawing(); uz_actscale(); break;
				case 165: umu_m46_place_view(); break;
				case 166: umu_46_load_drawing(); uz_actscale(); break;
				case 167: umu_46_delete_drawing(); uz_actscale(); break;
				case 168: umu_46_rename_drawing(); break;
				case 170: umu_46_archive_drawing(); break;
				case 171: umu_46_retrieve_drawing(); break;
/*
.....remove uj_plotit and uj_plot_offline
.....combined in uj_create_plot
.....Yurong 9/3/97
*/
				case 173: uj_create_plot(); break;
				case 174: uj_plotfrm(); break;
				case 175: uj_pentbfrm(); break;
				case 178: umu_set_drwscale(); uz_actscale(); break;
				case 179: ua_set_drafting_view(UU_FALSE); break;
				case 180: ua_save_new_standard(); break;
				case 181: ua_menu_switch(3); break;
				case 182: ua_linear_disp(1); break;
				case 183: ua_linear_disp(2); break;
				case 184: ua_linear_disp(3); break;
				case 185: ua_linear_disp(4); break;
				case 186: ua_linear_disp(5); break;
				case 187: ua_angular(); break;
				case 188: ua_arclen(); break;
				case 189: ua_menu_switch(1); break;
				case 190: ua_menu_switch(2); break;
				case 191: ua_center_line(); break;
				case 192: ua_polar(); break;
				case 193: ua_notes(); break;
				case 194: ua_label(); break;
				case 195: ua_txt_on_arc(); break;
				case 196: ua_xhatch(); break;
				case 197: ua_arrow(); break;
				case 198: ua_fptol(); break;
				case 199: ua_balloon(); break;
				case 200: ua_mod_attributes(9); break;
				case 201: ua_user_regen(); break;
				case 202: ua_edit_text(); break;
				case 203: ua_mod_attributes(8); break;
				case 204: ua_mod_attributes(1); break;
				case 205: ua_mod_attributes(2); break;
				case 206: ua_mod_attributes(3); break;
				case 207: ua_mod_attributes(4); break;
				case 208: ua_mod_attributes(5); break;
				case 209: ua_mod_attributes(6); break;
				case 210: ua_mod_attributes(7); break;
				case 211: ua_mod_attributes(10); break;
				case 212: ua_mod_attributes(11); break;
				case 213: ua_get_txt_attr(&UA_txtattr,"atxtinfo.frm"); break;
				case 214: ua_set_txt_attr("atxtattr.frm"); break;
				case 215: ua_set_dim_attr("adimattr.frm"); break;
				case 216: ua_set_tol_attr("atolattr.frm"); break;
				case 217: ua_set_dl_attr("adlattr.frm"); break;
				case 218: ua_set_ext_attr("aextattr.frm"); break;
				case 219: ua_set_ll_attr("allattr.frm"); break;
				case 220: ua_set_aro_attr("aaroattr.frm"); break;
				case 221: ua_set_sp_attr("aspattr.frm"); break;
				case 222: uz_zdrafting(); break;
				case 223: uz_zdrafting_exit(); break;
				case 224: uq_calc(); break;
				}
			}
			else if (ktab.type == MENUKEY)
			{
				pos[0] = -1;
				pos[1] = -1;
				size[0] = -1;
				size[1] = -1;
				if (udm_read_menu(ktab.name,pos,size, 1, 1, -1) != UU_SUCCESS)
				{
					sprintf(buf,"Could not load menu: %s",ktab.name);
					ud_winerror(buf);
				}
			}
/*
........NCLIPV defined keys
*/
			else if (ktab.type == IPVKEY)
			{
				switch (ktab.sub)
				{
				case 0: ul_verify_box(0,1); break;
				case 1: ul_verify_box(0,2); break;
				case 2: ul_verify_cyl(0,1); break;
				case 3: ul_verify_cyl(0,2); break;
				case 4: ul_verify_sweep(0); break;
				case 5: ul_verify_revsf(0); break;
				case 6: ul_verify_box(1,1); break;
				case 7: ul_verify_box(1,2); break;
				case 8: ul_verify_cyl(1,1); break;
				case 9: ul_verify_cyl(1,2); break;
				case 10: ul_verify_sweep(1); break;
				case 11: ul_verify_revsf(1); break;
				case 12: ul_verify(1); break;
				case 13: ul_verify(2); break;
				case 14: ul_verify(3); break;
				case 15: ul_verify(4); break;
				case 16: ul_verify_modals(); break;
				case 17: ul_ipv_reset_view(UV_current_sview); break;
				case 18: ul_ipv_tool_modals(); break;
				case 19: ul_ipv_compare(); break;
				case 20: ul_ipv_remove_chips(UU_FALSE,UU_NULL,0); break;
				case 21: ul_ipv_section() ; break;
				case 22: ul_verify_contour(0); break;
				case 23: ul_ipv_archive_session(""); break;
				case 24: 
					fname[0] = '\0';
					status = ul_ipv_load_session(fname,UU_TRUE); 
					break;
				case 25: ul_ipv_stock_mod(0); break;
				case 26: ul_ipv_stock_mod(1); break;
				case 27: ul_ipv_stock_attr(0); break;
				case 28: ul_ipv_stock_attr(1); break;
				case 29: ul_ipv_save_stock(); break;
				case 30: ul_ipv_load_stock(); break;
				case 31: ul_ipv_save_stl(0,"",0,0); break;
				case 32: ul_ipv_save_stl(1,"",0,0); break;
				case 33: ul_ipv_load_stl(0); break;
				case 34: ul_ipv_load_stl(1); break;
				case 35: ul_verify_box(0,3); break;
				case 36: ul_ipv_tool_list(); break;
				case 37: ul_ipv_tool_color(); break;
				case 38: ul_ipv_measure(); break;
				case 39: ul_ipv_save_session(); break;
				case 40: ul_ipv_restore_session(); break;
				case 41: ul_ipv_reset_session(UU_TRUE); break;
				case 42: if (ul_ipv_view_active()) uz_dyn_mouse(UU_TRUE); break;
				case 43: if (ul_ipv_view_active()) uz_extrema_zoom(); break;
				case 44: if (ul_ipv_view_active()) uz_view_from_axis(0); break;
				case 45: if (ul_ipv_view_active()) uz_view_from_axis(1); break;
				case 46: if (ul_ipv_view_active()) uz_view_from_axis(2); break;
				case 47: if (ul_ipv_view_active()) uz_view_from_axis(4); break;
				case 48: if (ul_ipv_view_active()) uz_view_from_axis(5); break;
				case 49: if (ul_ipv_view_active()) uz_view_from_axis(6); break;
				case 50: if (ul_ipv_view_active()) uz_view_from_axis(7); break;
				case 51: if (ul_ipv_view_active()) uz_repaint(1); break;
				case 52: if (ul_ipv_view_active()) uz_change_view(1); break;
				case 53: if (ul_ipv_view_active()) nclu_reset_prev(UU_TRUE); break;
				case 54: if (ul_ipv_view_active()) uz_window_zoom(); break;
				case 55: if (ul_ipv_view_active()) uz_zoom(); break;
				case 56: if (ul_ipv_view_active()) uz_pan(); break;
				case 57: if (ul_ipv_view_active()) uz_tracut_view(); break;
				case 58: if (ul_ipv_view_active()) uz_refsys_view(); break;
				case 59: if (ul_ipv_view_active()) uz_matrix_view(); break;
				case 60: if (ul_ipv_view_active()) uz_tool_view(); break;
				case 61: ul_ipv_dyncenter(); break;
				case 62: ul_verify_contour(1); break;
				case 63: ud_print_ipvscreen("",-1,0,0); break;
				case 64: ul_ipv_diag_form(); break;
				case 65: ul_ipv_swap_screen(); break;
				case 66: ul_ipv_mach_form(); break;
				case 67: ul_ipv_playback(); break;
				case 68: break;
				case 69: ul_ipv_region_form(); break;
				case 70: ul_ipv_display_mod(); break;
				case 71: ul_ipv_light_form(); break;
				case 72: ul_verify_cone(0,2); break;
				case 73: ul_verify_cone(0,3); break;
				case 74: ul_verify_cyl(0,3); break;
				case 75: ul_verify_sphere(0,1); break;
				case 76: ul_verify_sphere(0,2); break;
				case 77: ul_verify_torus(0,1); break;
				case 78: ul_verify_torus(0,2); break;
				case 79: ul_verify_torus(0,3); break;
				case 80: ul_verify_solid(0); break;
				case 81: ul_verify_cone(1,2); break;
				case 82: ul_verify_cone(1,3); break;
				case 83: ul_verify_cyl(1,3); break;
				case 84: ul_verify_sphere(1,1); break;
				case 85: ul_verify_sphere(1,2); break;
				case 86: ul_verify_torus(1,1); break;
				case 87: ul_verify_torus(1,2); break;
				case 88: ul_verify_torus(1,3); break;
				case 89: ul_verify_solid(1); break;
				case 94: ul_ipv_mot_stack_step(-1,parms); break;
				case 95: ul_ipv_mot_stack_step(1,parms); break;
				case 96: ul_ipv_mot_stack_step(-2,parms); break;
				case 97: ul_ipv_mot_stack_step(2,parms); break;
				case 98: ul_ipv_mot_stack_reset(); break;
				case 99: ul_ipv_mot_stack_modals(); break;
				case 100: ul_ipv_mot_stack_step(-3,parms); break;
				}
				UZ_nclipv_view = 0;
			}
/*
........Undefined keys
*/
			else
			{
				irtn = 0;
			}
/*
........End of Function execution keys
*/
/*
......update time
*/
			if (UW_stat_mode)
			{
				uw_set_update_stat(1);
				uz_status();
				uw_set_update_stat(0);
			}
			else
				uz_acttime();
		}
/*
.....Immediate execution flag
.....Return undefined key
*/
		if (ktab.flag & EXECFL) irtn = -1;
/*
.....End of routine
*/
		ud_lpop();
		S_reset_ipv();
		UD_UNMARK(i1);
	}
	return(irtn);
}
/*********************************************************************
**	 I_FUNCTION :uz_mouse_functions(func, type)
**		This function accepts a mouse input and execute its function
**	 PARAMETERS	
**		 INPUT  :
**					func: mouse input name
**							
**					type: mouse input type
**						0: choice mode
**						1: locate mode
**						2: pick mode
**						3: text
**		 OUTPUT :
**	 RETURNS: -1 = failed 
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
uz_mouse_functions(func, type)
char *func;
int type;
{
	char func_str[80], buf[256], parms[500], *index;
	UZ_keytable ktab;
	short app;
	int sub, type1, stat, irtn;
	int size[2], pos[2];
	if (strcmp(func, "mouse_left")==0)
	{
		if (type==0)
			strcpy(func_str, UZ_mouse_function[0]);
		else if (type==1)
			strcpy(func_str, UZ_mouse_function[5]);
		else if (type==2)
			strcpy(func_str, UZ_mouse_function[10]);
		else if (type==3)
			strcpy(func_str, UZ_mouse_function[15]);
	}
	else if (strcmp(func, "mouse_middle")==0)
	{
		if (type==0)
			strcpy(func_str, UZ_mouse_function[1]);
		else if (type==1)
			strcpy(func_str, UZ_mouse_function[6]);
		else if (type==2)
			strcpy(func_str, UZ_mouse_function[11]);
		else if (type==3)
			strcpy(func_str, UZ_mouse_function[16]);
	}
	else if (strcmp(func, "mouse_right")==0)
	{
		if (type==0)
			strcpy(func_str, UZ_mouse_function[2]);
		else if (type==1)
			strcpy(func_str, UZ_mouse_function[7]);
		else if (type==2)
			strcpy(func_str, UZ_mouse_function[12]);
		else if (type==3)
			strcpy(func_str, UZ_mouse_function[17]);
	}
	else if (strcmp(func, "wheel_down")==0)
	{
		if (type==0)
			strcpy(func_str, UZ_mouse_function[3]);
		else if (type==1)
			strcpy(func_str, UZ_mouse_function[8]);
		else if (type==2)
			strcpy(func_str, UZ_mouse_function[13]);
		else if (type==3)
			strcpy(func_str, UZ_mouse_function[18]);
	}
	else if (strcmp(func, "wheel_up")==0)
	{
		if (type==0)
			strcpy(func_str, UZ_mouse_function[4]);
		else if (type==1)
			strcpy(func_str, UZ_mouse_function[9]);
		else if (type==2)
			strcpy(func_str, UZ_mouse_function[14]);
		else if (type==3)
			strcpy(func_str, UZ_mouse_function[19]);
	}
	else
		return -1;
	if (func_str[0]=='\0')
		return 0;
	if (uz_which_keydef(func_str,&type1,&sub,&app) == UU_SUCCESS)
	{
/*
.....set mouse function on flag
*/
		NCL_mouse_func = 1;
		ktab.type = type1;
		ktab.sub = sub;
		ktab.flag = app;
		parms[0] = '\0';
		ktab.params = parms;
		index = buf;

		irtn = uz_user_keydef(ktab,&index,1);
		if (irtn == 1)
		{
			NT_FuncEvent = 10000 + sub;
			NT_FuncEof = 2;
			uw_ntuser_event(NT_FuncEvent, NT_FuncEof);
		}
		else if (irtn == 3)
		{
			uw_ntinsert_cmd(sub);
		}
		if (irtn == 5)
		{
			if (index[0]!='\0')
				uw_ntinsert_cmdstr(index);
		}
		return irtn;
	}
	else
/*
......menu, only display popup menu
*/
	{
		ud_rpwrmenu(func_str, parms, func_str);
		pos[0] = -1;
		pos[1] = -1;
		size[0] = -1;
		size[1] = -1;

		if (udm_read_menu(func_str,pos,size, 1, 1, UDM_MTYPE_POPUP) != UU_SUCCESS)
		{
			sprintf(buf,"Could not load menu: %s",func_str);
			uw_nterror(buf);
		}
		return 0;
	}
	return  -1;
}
		
/*********************************************************************
**	 I_FUNCTION :uz_func_call(func, params)
**		This function call the function with 'func' name and 'params' parameter
**	 PARAMETERS	
**		 INPUT  :
**					func: function name
**					params: parameter
**		 OUTPUT :
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
void uz_func_call(func, params)
char *func, *params;
{
	UZ_keytable ktab;
	short app;
	int sub, type, irtn;
	char *index, buf[256];

	if (uz_which_keydef(func,&type,&sub,&app) == UU_SUCCESS)
	{
		ktab.type = type;
		ktab.sub = sub;
		ktab.flag = app;
		ktab.params = params;
		index = buf;

		irtn = uz_user_keydef(ktab,&index,1);
		if (irtn == 1)
		{
			NT_FuncEvent = 10000 + sub;
			NT_FuncEof = 2;
			uw_ntuser_event(NT_FuncEvent, NT_FuncEof);
		}
		else if (irtn == 3)
		{
			uw_ntinsert_cmd(sub);
		}
		if (irtn == 5)
		{
			if (index[0]!='\0')
				uw_ntinsert_cmdstr(index);
		}
		return;
	}
}

