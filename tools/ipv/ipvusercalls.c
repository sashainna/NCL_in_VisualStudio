/*********************************************************************
**	FILENAME: ipvusercalls.c
**	CONTAINS:
**		uz_user_keydef(ktab,index,xflag)
**    MODULE NAME AND RELEASE LEVEL
**       ipvusercalls.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       02/08/16 , 09:12:33
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
#include "nclmplay.h"
#include "dmotif.h"

int UZ_nclipv_view;
char UZ_mouse_function[20][80] = {
	"", "", "","","","","","","","",
	"", "", "","","","","","","",""};

extern ATXT_FRM UA_txtattr, UB_txtattr;
extern UX_libdata_bag UB_spl_libdata_rec;
extern UX_libdata_bag UB_libdata_rec;
extern int UV_current_sview;
int NCL_event_reject = 0;
extern int UW_dynamic_funkey;
extern int NT_FuncEvent, NT_FuncEof;
extern int NCL_mouse_func;

#define NALPHAKEYS 18

static char *alpha[NALPHAKEYS]={"0","1","2","3","4","5","6","7","8","9",
   ".",",","+","-","/","*","<-","ENTER"};
static UN_motseg_isn Sisn={0,UU_NULL};
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
{
	int i;
	int i1,irtn;
	int pos[2];
	int size[2];
	char buf[80];
	char parms[40], text_str[256];
	UX_pathname fname; 

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
	if (!(ktab.flag & NCLIPVFL))
		return (2);
/*
.....Function execution keys
*/
	irtn = 0;
	if (xflag || ktab.flag & EXECFL)
	{
		UD_MARK(i1,UU_FALSE);
		if (i1 == 0)
		{
			if (ktab.type == DASKEY)
			{
				switch (ktab.sub)
				{
				case 13: 
					uw_mfmenu_reset(UU_TRUE,UU_TRUE,UU_FALSE); 
					break;
				}
			}
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
				case 14: ul_verify(3); break;
				case 15: ul_verify(4); break;
				case 16: ul_verify_modals(); break;
				case 17: ul_ipv_reset_view(-1); break;
				case 18: ul_ipv_tool_modals(); break;
				case 19: ul_ipv_compare(); break;
				case 20: ul_ipv_remove_chips(UU_FALSE,UU_NULL,0); break;
				case 21: ul_ipv_section() ; break;
				case 22: ul_verify_contour(0); break;
				case 23: ul_ipv_archive_session(""); break;
				case 24: 
					fname[0] = '\0';
					ul_ipv_load_session(fname,UU_TRUE); 
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
				case 41: ul_ipv_reset_session(); break;
				case 42: if (ul_ipv_view_active()) nclipv_dynmouse(UU_TRUE); break;
				case 43: if (ul_ipv_view_active()) nclipv_vpzoomextrema(); break;
				case 44: if (ul_ipv_view_active()) nclipv_view_from_axis(0); break;
				case 45: if (ul_ipv_view_active()) nclipv_view_from_axis(1); break;
				case 46: if (ul_ipv_view_active()) nclipv_view_from_axis(2); break;
				case 47: if (ul_ipv_view_active()) nclipv_view_from_axis(4); break;
				case 48: if (ul_ipv_view_active()) nclipv_view_from_axis(5); break;
				case 49: if (ul_ipv_view_active()) nclipv_view_from_axis(6); break;
				case 50: if (ul_ipv_view_active()) nclipv_view_from_axis(7); break;
				case 51: if (ul_ipv_view_active()) nclipv_repaint(); break;
				case 52: if (ul_ipv_view_active()) nclipv_change_view(1); break;
				case 53: if (ul_ipv_view_active()) nclipv_reset_prev(); break;
				case 54: if (ul_ipv_view_active()) nclipv_vpzoomin(); break;
				case 55: if (ul_ipv_view_active()) nclipv_vpzoomout(); break;
				case 56: if (ul_ipv_view_active()) nclipv_pan(); break;
				case 57: if (ul_ipv_view_active()) nclipv_tracut_view(); break;
				case 58: if (ul_ipv_view_active()) nclipv_refsys_view(); break;
				case 59: if (ul_ipv_view_active()) nclipv_get_mx_view(); break;
				case 60: if (ul_ipv_view_active()) nclipv_tool_view(); break;
				case 61: ul_ipv_dyncenter(); break;
				case 62: ul_verify_contour(1); break;
				case 63: ud_print_ipvscreen("",-1,0,0); break;
				case 64: ul_ipv_diag_form(); break;
				case 65: break;
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
				case 90: nclipv_exit(); break;
				case 91: nclipv_restart(); break;
				case 92: nclipv_setin(); break;
				case 93: nclipv_setmm(); break;
				case 94: ul_ipv_mot_stack_step(-1); break;
				case 95: ul_ipv_mot_stack_step(1); break;
				case 96: ul_ipv_mot_stack_step(-2); break;
				case 97: ul_ipv_mot_stack_step(2); break;
				case 98: ul_ipv_mot_stack_reset(); break;
				case 99: ul_ipv_mot_stack_modals(); break;
				case 100: ul_ipv_mot_stack_step(-3); break;
				}
			}
/*
........CAM defined keys
*/
			else if (ktab.type == CAMKEY)
			{
				switch (ktab.sub)
				{
					case 26: nclu_playfile(); break;
					case 94: nclu_playfeed(); break;
					case 95: nclu_playinterp(); break;
				}
			}
			else if (ktab.type == NISKEY)
			{
				switch (ktab.sub)
				{
					case 0: 
						ul_ipv_dyncenter();
						break;
					case 9: ul_system(0); break;
					case 10: ul_system(1, parms); break;
					case 26: uz_iface_modals();
						break;
					case 75: uw_ntmenu_desgn(0, 0);
						break;
				}
			}
			else if (ktab.type == NCLKEY)
			{
				switch (ktab.sub)
				{
//
				case 49: uvu_dynzoom(); break;
				case 50: uvu_dynpan(); break;
				case 51: uvu_restorev(0); break;
//
				case 52: uv_dynstep_view(UV_current_sview,1,-1); break;
				case 53: uv_dynstep_view(UV_current_sview,1,1); break;
				case 54: uv_dynstep_view(UV_current_sview,2,-1); break;
				case 55: uv_dynstep_view(UV_current_sview,2,1); break;
				case 56: uv_dynstep_view(UV_current_sview,3,-1); break;
				case 57: uv_dynstep_view(UV_current_sview,3,1); break;
				case 58: uv_dynstep_view(UV_current_sview,4,-1); break;
				case 59: uv_dynstep_view(UV_current_sview,4,1); break;
				case 60: uv_dynstep_view(UV_current_sview,5,-1); break;
				case 61: uv_dynstep_view(UV_current_sview,5,1); break;
				case 62: uv_dynstep_view(UV_current_sview,6,-1); break;
				case 63: uv_dynstep_view(UV_current_sview,6,1); break;
//		
				case 66: uvu_dynxyrot(); break;
				case 67: uvu_dyntumble(); break;
				case 68: uvu_dynzrot(); break;
//
				case 70: uvu_dynmodals(); break;
				case 144: ncl_pan_toggle(); break;
				case 145: ncl_rotate_toggle(); break;
				case 146: ncl_domi_toggle(); break;
				case 147: ncl_gain_down(); break;
				case 148: ncl_gain_up(); break;
				case 149: ncl_gain_def(); break;
				}
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
		}
/*
.....Immediate execution flag
.....Return undefined key
*/
		if (ktab.flag & EXECFL) irtn = -1;
		UD_UNMARK(i1);
	}
	return(irtn);
}
motisn_set()
{
	int isn;
/*
.....Store isn stack
*/
	if (Sisn.nent > 0)
		isn = ncl_motisn_set(&Sisn,UU_TRUE);
	else
		isn = -1;
	return(isn);
}
motisn(jbuf)
int *jbuf;
{
	int i;
/*
.....Allocate memory for isn stack
*/
	if (jbuf[0] > Sisn.nent)
	{
		if (Sisn.nent != 0) uu_free(Sisn.line);
		Sisn.line = (int *)uu_malloc(jbuf[0]*sizeof(int));
	}
/*
.....Store isn stack
*/
	Sisn.nent = jbuf[0];
	for (i=1;i<=Sisn.nent;i++)
		Sisn.line[i-1] = jbuf[i];
}
motdel()
{
	if (Sisn.nent != 0) uu_free(Sisn.line);
	Sisn.nent = 0;
	ncl_motisn_reset();
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

uz_mouse_functions_old(func, type)
char *func;
int type;
{
	char func_str[80], buf[256], parms[500], *index;
	UZ_keytable ktab;
	short app;
	int sub, type1, stat;
	int FuncEvent, FuncEof;

	if (strcmp(func, "mouse_left")==0)
	{
		return 0;
	}
	else if (strcmp(func, "mouse_middle")==0)
	{
		if (type==0)
			return 0;
		else
			strcpy(func_str, "KEY_DONE");
	}
	else if (strcmp(func, "mouse_right")==0)
	{
		if (type==0)
			return 0;
		else
			strcpy(func_str, "KEY_REJECT");
	}
	else 
		return 0;
	if (uz_which_keydef(func_str,&type1,&sub,&app) == UU_SUCCESS)
	{
/*
.....set mouse function on flag
*/
		FuncEvent = 10000 + sub;
		FuncEof = 2;
		uw_ntuser_event(FuncEvent, FuncEof);
		return 1;
	}
	return  -1;
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
