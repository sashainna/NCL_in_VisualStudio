/*********************************************************************
**  FILENAME: lipvmsl.c
**  CONTAINS:
**       ul_ipv_initialize
**			ul_ipv_init_lights
**			ul_ipv_set_lights
**			ul_ipv_destroy_lights
**			ul_ipv_set_background
**			ul_ipv_set_colors
**
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        lipvmsl.c , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**        06/30/15 , 11:38:07
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "xfsys0.h"
#include "driver.h"
#include "lcom.h"
#include "mdrel.h"
#include "mfort.h"
#include "mxxx.h"
#include "mpocket.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include <ctype.h>
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "gtbl.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"
#include "lipvstack.h"
#include "usysg.h"
#include "uims.h"

extern int UL_ipv;
static FILE *Sjou_fd;
static UU_LOGICAL LW_used=UU_FALSE;
static LtSref Slight_list=0;

void ul_ipv_set_background();
void ul_ipv_init_lights();
void ul_ipv_set_lights();
extern int uw_color_table[64][3];
/*********************************************************************
**	 E_FUNCTION:int ul_ipv_initialize()
**		Initializes interactive NCLIPV.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_initialize()
{
	int i,j,nc;
	char *p,*ux_getenv();
	char fname[1024];
	LtData value;
	LtColour colour_value;
	LtData stuff;
	void ul_ipv_update_colors();
	LW_mach_toolpin_struc tpin;
/*
.....System is already initialized
.....so just return
*/
	if (LW_initialized || UL_ipv == 0)
	{
		LW_stock_default[0].color = 1; /* White */
		LW_stock_default[1].color = 2; /* Blue */
		if (UL_ipv == 0) LW_version = LW_NCVERIFY;
		goto done;
	}
/*
.....Determine Version of NCLIPV to support
*/
	p = ux_getenv("NCLIPV_VERSION",UX_NPRTERRS);
	LW_version = LW_MWORKS;
	if (p != NULL && strcmp(p,"OLD") == 0)
	{
		LW_display = LW_NCVERIFY;
		return(UU_SUCCESS);
	}
/*
.....Determine ACTIVE or PASSIVE mode
*/
	p = ux_getenv("NCLIPV_MODE",UX_NPRTERRS);
	LW_display = LW_PASSIVE;
	if (p != NULL && strcmp(p,"ACTIVE") == 0)
	{
		LW_display = LW_ACTIVE;
		LW_mach_mode = LW_VISICUT;
	}
/*
.....Initialize LightWorks 
*/
	LiInitialiseLocal();
	uw_glset_context(UM_NCL_WINDOW,UU_TRUE);
/*
.....Open journaling file
*/
/*
	Sjou_fd = UU_NULL;
	p = ux_getenv("NCLIPV_JOURNAL",UX_NPRTERRS);
	if (p != UU_NULL)
	{
		strcpy(fname,p);
		nc = strlen(fname);
*/
/*
.....for WinNT, we allow filename with spaces,
.....only remove trailling spaces for WinNT
.....Yurong 1/17/02
*/
/*
#if UU_COMP!=UU_WIN2K
		ul_strip_blanks(fname,&nc);
#else
*/
/*
.....we also need to remove preceding spaces
.....Yurong 3/1/02
*/
/*
		for (i=0; i<(int)strlen(fname); i++)
		{
			if (fname[i]!=' ') break;
		}
		strcpy(fname, &(fname[i]));
		for (nc=strlen(fname); nc>0; nc--)
		{
			if (fname[nc-1]==' ')
			fname[nc-1] = '\0';
			else
			break;
		}
		nc = strlen(fname);
#endif
		if (nc != 0)
		{
			Sjou_fd = LiFileOpen(fname,LI_FILE_WRITE);
			LiDataSetGenericPtr(&value,Sjou_fd);
			LiControlSet(LI_CONTROL_JOURNAL_FILE,&value);
			LiDataSetBitfield(&value,LI_JOURNAL_TEXT);
			LiControlSet(LI_CONTROL_JOURNALING,&value);
		}
	}
*/
/*
.....Setup light
*/
	ul_ipv_init_lights();
/*
.....Initialize the LightWorks system
*/
	LW_step = 0;
	for (i=0;i<LW_MAX_SPINDLE;i++)
	{
		LW_tool[i] = (LtSessionPrim)0;
		LW_num_holder[i] = 0;
		LW_shank[i] = (LtSessionPrim)0;
		LW_holder[i][0] = (LtSessionPrim)0;
	}
	LW_rv_accy = 500;
	LW_toler = .005;
	LW_maxang = 5.;
	LW_translucency[0] = LW_translucency[1] = LW_translucency[2] = 100;
	LW_box_expansion = 0.;
	LW_stl_format = 1;
	LW_stl_units = 0;
	LW_stl_toler = .005;
	LW_stl_flag[0] = UU_TRUE;
	LW_stl_flag[1] = UU_TRUE;
	LW_stl_flag[2] = UU_FALSE;
	LW_ntool = 0;
	LW_ntool_sess = 0;
	LW_tool_sess_act = 0;
	LW_active = UU_FALSE;
	LW_session_save = 0;
	LW_env = 0;
	LW_auto_reset = UU_TRUE;
/*
.....Initialize display properties
*/
	LW_display_prop.mode = 0;
	LW_display_prop.axes = UU_FALSE;
	LW_display_prop.buf = 1;
	LW_display_prop.swap = UU_FALSE;
	LW_display_prop.minimize = UU_FALSE;
	LW_display_prop.hide_auto = UU_FALSE;
	LW_display_prop.hide_lucency = 20;
	LW_display_prop.hide_edge = UU_FALSE;
	LW_display_prop.shader = 1;
	LW_display_prop.bgcolor = 1;
	LW_display_prop.bgrgb[0] = LW_display_prop.bgrgb[1] =
		LW_display_prop.bgrgb[2] = 0.;
	LW_display_prop.grad[0] = 2;
	LW_display_prop.grad[1] = 1;
	LW_display_prop.grgb[0][0] = LW_display_prop.grgb[0][1] =
		LW_display_prop.grgb[0][2] = 1.;
	LW_display_prop.grgb[1][0] = LW_display_prop.grgb[1][1] =
		LW_display_prop.grgb[1][2] = 0.;
	for (i=0;i<4;i++)
	{
		LW_display_prop.fcolor[i] = 1;
		LW_display_prop.frgb[i][0] = LW_display_prop.frgb[i][1] =
			LW_display_prop.frgb[i][2] = 0.;
	}
	LW_display_prop.bgfile[0] = '\0';
	LW_display_prop.rotate = 0;
	LW_display_prop.stretch = UU_TRUE;
/*
.....Initialize clashes
*/
	strcpy(LW_diag_file,"nclipv.log");
	for (i=0;i<16;i++)
	{
		LW_clash_log[i] = UU_TRUE;
		LW_clash_stop[i] = UU_TRUE;
	}
	LW_clash_stop[2] = UU_FALSE;
	LW_clash_stop[6] = UU_FALSE;
	LW_clash_stop[10] = UU_FALSE;
	LW_clash_stop[12] = UU_FALSE;
	LW_clash_stop[13] = UU_FALSE;
	LW_clash_stop[14] = UU_FALSE;
	LW_clash_log[2] = UU_FALSE;
	LW_clash_log[6] = UU_FALSE;
	LW_clash_log[10] = UU_FALSE;
	LW_clash_log[12] = UU_FALSE;
	LW_clash_log[13] = UU_FALSE;
	LW_clash_log[14] = UU_FALSE;
	LW_clash_flag = UU_FALSE;
	LW_reset_log = UU_FALSE;
	LW_clash_color = 3; /* Red */
	for (i=0;i<16;i++)
	{
		LW_default_log[i] = LW_clash_log[i];
		LW_default_stop[i] = LW_clash_stop[i];
	}
/*
.....Initialize Visicut regions
*/
	LW_region_defined = UU_FALSE;
	for (i=0;i<6;i++) LW_region[i] = 0.;
/*
.....Initialize Machine simulation
*/
	LW_mach_simul = UU_FALSE;
	LW_mach_name[0] = '\0';
	LW_mach_nmodel = LW_mach_nsolid = 0;
	LW_mach_stock.stock = 0;
	for (i=0;i<LW_MAX_SPINDLE;i++)
	{
		LW_spindle[i] = -1;
		LW_spindle_ix[i] = 0;
		LW_spindle_load[i] = 0;
		LW_spindle_vis[i] = UU_TRUE;
	}
	LW_spindle_num = 1;
	LW_spindle_nload = 1;
	uu_list_init(&LW_mach_toolpin,sizeof(LW_mach_toolpin_struc),10,5);
	tpin.mpin[0][0] = tpin.mpin[0][1] = tpin.mpin[0][2] = 0.;
	tpin.mpin[1][0] = tpin.mpin[1][1] = tpin.mpin[1][2] = 0.;
	tpin.mpin[2][0] = tpin.mpin[2][1] = tpin.mpin[2][2] = 0.;
	tpin.mpin[1][2] = tpin.mpin[2][0] = 1.;
	tpin.ppin[0][0] = tpin.ppin[0][1] = tpin.ppin[0][2] = 0.;
	tpin.ppin[1][0] = tpin.ppin[1][1] = tpin.ppin[1][2] = 0.;
	tpin.ppin[2][0] = tpin.ppin[2][1] = tpin.ppin[2][2] = 0.;
	tpin.ppin[1][2] = tpin.ppin[2][0] = 1.;
	strcpy(tpin.label,"X");
	tpin.axis = 0;
	um_identtf(tpin.matrix);
	um_identtf(tpin.invmx);
	uu_list_push(&LW_mach_toolpin,&tpin);
	LW_mach_num_tpin = 1;
	LW_mach_tpin_ix = 0;
	LW_mach_defined = UU_FALSE;
	LW_mach_desc.type = -1;
/*
.....we support multi-path for UL_NCLIPV_MACHINES value,
.....so just copy UL_NCLIPV_MACHINES string to LW_mach_dir
*/
/*	ul_getvalid_fulldir("UL_NCLIPV_MACHINES",LW_mach_dir); */
	strcpy(LW_mach_dir, "UL_NCLIPV_MACHINES");

	uu_list_init0(&LW_register_offsets);
/*
.....Set up materials (colors)
*/
	for (i=0;i<UM_POCKET_COLORS;i++)
	{
		LiColourInitialise (colour_value,
			UM_pkcolors[i].red/255.,
			UM_pkcolors[i].green/255.,
			UM_pkcolors[i].blue/255.);
		LW_material[i] = ul_ipv_create_material (colour_value);
	}
	LW_stock_material = 15; /* Grey */
	LW_fixture_material = 2; /* Blue */
	LW_clash_material.fixture = 3; /* Red */
	LW_clash_material.holder = 3; /* Red */
	LW_clash_material.rapid = 3; /* Red */
	LW_highlight_color = 13; /* Pink */
/*
.....Define metric conversion matrix
*/
	LW_xform_mminch = LiTransformCreate();
	LiTransformScale(LW_xform_mminch,1./25.4,1./25.4,1./25.4);
	LW_xform_inchmm = LiTransformCreate();
	LiTransformScale(LW_xform_inchmm,25.4,25.4,25.4);
/*
.....Set up default tool attributes
*/
	LW_default_tool.color = 4; /* Green */
	LW_default_tool.cut_color = -1; /* Auto */
	LW_default_tool.hold_color = 5; /* Magenta */
	LW_default_tool.initial = 2; /* Blue */
	LW_cutcolor_index = -1;
	LW_default_tool.use_stock[0] = 0;
	LW_default_tool.use_stock[1] = 0;
	LW_default_tool.toler = .005;
	LW_default_tool.maxang = 5.;
	LW_default_tool.edge = UU_FALSE;
	LW_default_tool.edge_color = -1;
	LW_default_tool.translucency = 100;
	LW_default_tool.rapid = 0.;
	LW_default_tool.shank_color = 12; /* Orange */
	LW_default_tool.shank_clash = 1; /* Holder */
	LW_tool_limit[0] = .001;
	LW_tool_limit[1] = 1000.;
	LW_tool_limit[2] = .001;
	LW_tool_from = UU_TRUE;
/*
.....Initialize tool position
*/
	for (i=0;i<5;i++)
	{
		LW_tool_pos[i] = 0.;
		LW_last_tool_pos[i] = 0.;
	}
	LW_tool_pos[5] = 1.;
	LW_last_tool_pos[5] = 1.;

/*
.....Initialize motion attributes
*/
	LW_mot_ndata = 0;
	LW_mot_data = (UN_mot_data *)UU_NULL;
	LW_mot_data_first = (UN_mot_data *)UU_NULL;
	LW_mot_nattr = 0;
	LW_mot_attr = (UN_mot_attr *)UU_NULL;
	LW_mot_attr_first = (UN_mot_attr *)UU_NULL;
/*
.....Set up default Stock & Fixtures
*/
	LW_stock_default[0].color = 4; /* Green */
	LW_stock_default[1].color = 6; /* Yellow */
	LW_delete_stocks = UU_FALSE;
	for (j=0;j<2;j++)
	{
		LW_stock_data[j] = (LW_stock_struc *)uu_lsnew();
		LW_stock_data[j] = (LW_stock_struc *)uu_lsinsrt((char *)LW_stock_data[j],
			sizeof(LW_stock_struc));
		LW_stock_first[j] = LW_stock_data[j];
		LW_stock_default[j].id = 0;
		LW_stock_default[j].translucency = 100;
		LW_stock_default[j].visible = UU_TRUE;
		LW_stock_default[j].active = UU_TRUE;
		LW_stock_default[j].important = UU_TRUE;
		LW_stock_default[j].toler = .005;
		LW_stock_default[j].edge = UU_FALSE;
		LW_stock_default[j].edge_color = -1;
		LW_stock_default[j].toler = .005;
		LW_stock_default[j].mxflag = UU_FALSE;
		LW_stock_default[j].mxchg = UU_FALSE;
		LW_stock_default[j].axes = UU_FALSE;
		LW_stock_default[j].axes_color = LW_stock_default[j].color;
		LW_stock_default[j].axis_seg = -1;
		LW_stock_default[j].mxname[0] = '\0';
		um_identtf(LW_stock_default[j].matrix);
		LW_stock_idn[j] = 1;
		LW_reset_attr[j] = UU_FALSE;
	}
/*
.....Setup NCLIPV window
*/
	strcpy(LW_ipv_title,"NCLIPV");
	LW_ipv_pos[0] = 600;
	LW_ipv_pos[1] = 400;
	LW_ipv_size[0] = 400;
	LW_ipv_size[1] = 300;
/*
.....Setup Playback form
*/
	LW_play_modal[0] = 0;
	LW_play_modal[1] = 100;
	LW_play_modal[2] = 0;
	LW_play_modal[3] = 1;
	LW_play_modal[4] = 1;
	LW_play_modal[5] = 1;
	LW_play_modal[6] = 0;
	LW_play_modal[7] = 0;
	LW_play_modal[8] = 0;
	LW_play_modal[9] = 0;
	LW_play_modal[10] = 0;
	LW_play_modal[11] = 0;
	LW_play_modal[12] = 0;
	LW_play_modal[13] = 0;
	LW_play_modal[14] = 1;
	LW_play_modal[15] = 0;
	LW_play_modal[16] = 0;
	LW_play_modal[17] = 0;
	LW_play_modal[18] = 0;
	LW_play_modal[19] = 0;
/*
.....Setup NCLIPV Monitor form
*/
	LW_monitor = UU_TRUE;
	for (i=0;i<N_IPVMON_FLD;i++) LW_monitor_field[i] = 1;
	LW_monitor_field[IPVMON_LINAXS] = 0;
	LW_monitor_field[IPVMON_ROTAXS] = 0;
	LW_monitor_field[IPVMON_HEAD2] = 0;
	LW_monitor_field[IPVMON_HEAD3] = 0;
	LW_monitor_field[IPVMON_HEAD4] = 0;
	LW_monitor_field[IPVMON_MOVTIM] = 0;
	LW_monitor_field[IPVMON_CUTCOM] = 0;
	LW_monitor_field[IPVMON_COOLNT] = 0;
	LW_monitor_field[IPVMON_DOCK] = 2;
/*
.....Setup RapidCut
*/
	LiDataSetDouble(&stuff,.0005);
	LiControlSet(LI_CONTROL_RV_MESH_FACET_TOL,&stuff);
	LiDataSetEnum(&stuff,LI_MW_FACET_OUTSIDE);
	LiControlSet(LI_CONTROL_RV_MESH_FACET_TYPE,&stuff);
	LiDataSetBoolean(&stuff,TRUE);
	LiControlSet(LI_CONTROL_RV_EXTRA_GL_POLYS,&stuff);
	LiDataSetEnum(&stuff,LI_RV_VERIFY_SMOOTHED);
	LiControlSet(LI_CONTROL_RV_GRAPHICS_MODE,&stuff);
	LiDataSetBoolean(&stuff,FALSE);
	LiControlSet(LI_CONTROL_SO_CONSISTENT_3_AXIS,&stuff);
/*
.....The following variable sets up
.....Number of polygons to generate
.....to smooth out pixels
.....5 means every 5th pixel, etc.
.....The higher the number, the lower the
.....resolution and faster the animation
*/
/*	LiDataSetNat32(&stuff,2);
	LiControlSet(LI_CONTROL_RV_NUMBER_OF_MESHES,&stuff);*/
/*	LiDataSetNat32(&stuff,5);
	LiControlSet(LI_CONTROL_RV_MESH_INTERVAL,&stuff);*/
/*
.....Setup Visicut
*/
	LiDataSetBoolean(&stuff,TRUE);
	LiControlSet(LI_CONTROL_VI_LATEST_MATERIAL,&stuff);
	LiControlSet(LI_CONTROL_VI_LATEST_CUT_NUMBER,&stuff);
	LiDataSetBoolean(&stuff,FALSE);
	LiControlSet(LI_CONTROL_VI_XFER_DIFF_ATTRIBS,&stuff);
	LiDataSetBoolean(&stuff,FALSE);
	LiControlSet(LI_CONTROL_MW_TOUCH_IS_CLASH,&stuff);
/*	LiDataSetBoolean(&stuff,TRUE);
	LiControlSet(LI_CONTROL_MW_ADJUST_VIEW_FROM,&stuff);*/
	LiDataSetEnum(&stuff,LI_VI_MERGE_TYPE_SAME_MATERIAL);
	LiControlSet(LI_CONTROL_VI_INIT_MERGE_TYPE,&stuff);
	LiDataSetEnum(&stuff,LI_VI_CUT_DATA_STORE_FULL);
	LiControlSet(LI_CONTROL_VI_CUT_DATA_STORAGE,&stuff);
	LiDataSetDouble(&stuff,LW_toler);
	LiControlSet(LI_CONTROL_VI_PATH_FIT_TOL,&stuff);
	LiDataSetBoolean(&stuff,FALSE);
	LiControlSet(LI_CONTROL_VI_STOP_AT_ILLEGAL,&stuff);
/*
....Setup Compare structure
*/
	LW_compare.type = 0;
	LW_compare.hilite = 11; /* Sea Green */
	LW_compare.units = 0; /* Inch */
	LW_compare.file[0] = '\0';
	LW_compare.auto_toler = .001;
	LW_compare.toler[0] = -.004; LW_compare.toler[1] = -.003;
	LW_compare.toler[2] = -.002; LW_compare.toler[3] = -.001;
	LW_compare.toler[4] = .001; LW_compare.toler[5] = .002;
	LW_compare.toler[6] = .003; LW_compare.toler[7] = .004;
	LW_compare.color[0] = 3; LW_compare.color[1] = 12;
	LW_compare.color[2] = 6; LW_compare.color[3] = 13;
	LW_compare.color[4] = 4; LW_compare.color[5] = 11;
	LW_compare.color[6] = 7; LW_compare.color[7] = 10;
	LW_compare.color[8] = 2;
	LW_compare.sftol = .002;
	LW_compare.maxdis = .04;
	LW_compare.maxang = 30.;
	LW_compare.vis = UU_FALSE;
	LW_compare.grid_flag = UU_FALSE;
	LW_compare.grid = 20;
	LW_compare.translucency = 80;
	LW_compare.minvol = .01;
	LW_compare.analyze = UU_FALSE;
	uu_move_byte(&LW_compare,&LW_default_compare,sizeof(LW_compare_struc));
	LW_diff_solid_view = UU_FALSE;
/*
.....Initialize motion stack variables
*/
	LW_mot_stack_active = UU_TRUE;
	LW_mot_stack_size = 1000;
	LW_mot_stack_fixture = UU_FALSE;
/*
.....Load modals file
*/
	ul_ipv_load_mod();
/*
.....Setup highlight color, Pink
*/
	LiColourInitialise (LW_highlight_value, 1.0, 0.761, 0.8);
/*
.....Set background
........Must be done after Drawable is created
*/
/*	ul_ipv_set_background();*/
/*
.....Initialize motion attribute
*/
	LW_attrib = LiAttributeClassCreate();
	LW_initialized = UU_TRUE;
	LW_used = UU_FALSE;
	LW_dummy = 0;
	ul_ipv_update_colors();
done:;
	return(UU_SUCCESS);
}
/*********************************************************************
**	 E_FUNCTION : ul_ipv_init_lights()
**			This function initializes the NCLIPV light structures and
**       activates the lights.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void ul_ipv_init_lights()
{
	int i;
/*
.....Initialize light structures
*/
	for (i=0;i<4;i++)
	{
		if (i == 0)
		{
			LW_lights[i].active = UU_TRUE;
			LW_lights[i].type = LW_LIGHT_EYE;
			LW_lights[i].intensity = 70;
		}
		else if (i == 1)
		{
			LW_lights[i].active = UU_TRUE;
			LW_lights[i].type = LW_LIGHT_AMBIENT;
			LW_lights[i].intensity = 30;
		}
		else if (i == 2)
		{
			LW_lights[i].active = UU_FALSE;
			LW_lights[i].type = LW_LIGHT_DISTANT;
			LW_lights[i].intensity = 100;
		}
		else
		{
			LW_lights[i].active = UU_FALSE;
			LW_lights[i].type = LW_LIGHT_POINT;
			LW_lights[i].intensity = 100;
		}
		LW_lights[i].color = 1;
		LW_lights[i].rgb[0] = LW_lights[i].rgb[1] = LW_lights[i].rgb[2] = 1.;
		LW_lights[i].position[0] = LW_lights[i].position[1] = 0.;
		LW_lights[i].position[2] = 0.;
		LW_lights[i].direction[0] = LW_lights[i].direction[1] = 0.;
		LW_lights[i].direction[2] = 1.;
	}
/*
.....Activate the lights
.....Done after viewport is created now
*/
/*	ul_ipv_set_lights(LW_lights);*/
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_set_lights(lights)
**			This function activates the NCLIPV lights.
**	 PARAMETERS	
**		 INPUT  :
**        lights  = Array of lights to activate.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void ul_ipv_set_lights(lights)
LW_light_struc *lights;
{
	int i,nshader;
	LtFloat rval;
	LtColour colour_value;
	LtData data;
	LtPoint tpos;
	LtShader shader[LW_MAX_LIGHT];
/*
.....Destroy previously defined shaders
.....This routine has been obsoleted by LiShaderReferenceDestroy()
*/
/*
	for (i=0;i<Snshader;i++)
	{
		if (Sshader[i] != 0) LiShaderDestroy(Sshader[i]);
		Sshader[i] = 0;
	}
*/
/*
.....Loop through the lights
*/
	nshader = 0;
	for (i=0;i<LW_MAX_LIGHT;i++)
	{
		if (lights[i].active)
		{
			switch (lights[i].type)
			{
/*
.....Ambient light
*/
			case LW_LIGHT_AMBIENT:
				shader[nshader] = LiShaderCreate(LI_SHADER_CLASS_LIGHT,"ambient");
				break;
/*
.....Distant light
*/
			case LW_LIGHT_DISTANT:
				shader[nshader] = LiShaderCreate(LI_SHADER_CLASS_LIGHT,"distant");
				LiDataSetPoint(&data,lights[i].position);
				LiShaderSetArg(shader[nshader],"location",&data);
				um_vcplvc(lights[i].position,lights[i].direction,tpos);
				LiDataSetPoint(&data,tpos);
				LiShaderSetArg(shader[nshader],"to",&data);
				LiDataSetBoolean(&data,TRUE);
/*//				LiShaderSetArg(shader[nshader],"shadows",&data);*/
				break;
/*
.....Eye light
*/
			case LW_LIGHT_EYE:
				shader[nshader] = LiShaderCreate(LI_SHADER_CLASS_LIGHT,"eye");
				break;
/*
.....Point light
*/
			case LW_LIGHT_POINT:
				shader[nshader] = LiShaderCreate(LI_SHADER_CLASS_LIGHT,"point");
				LiDataSetPoint(&data,lights[i].position);
				LiShaderSetArg(shader[nshader],"location",&data);
				LiDataSetBoolean(&data,TRUE);
/*//				LiShaderSetArg(shader[nshader],"shadows",&data);*/
				break;
			}
/*
.....Set color
*/
			if (lights[i].color == -1)
			{
				LiColourInitialise(colour_value,lights[i].rgb[0],
					lights[i].rgb[1],lights[i].rgb[2]);
			}
			else
			{
				ul_ipv_color_set(colour_value,lights[i].color);
			}
			LiDataSetColour(&data,colour_value);
			LiShaderSetArg(shader[nshader],"colour",&data);
/*
.....Set intensity
*/
			rval = (UU_REAL)lights[i].intensity / 100.;
			LiDataSetFloat(&data,rval);
			LiShaderSetArg(shader[nshader],"intensity",&data);
			nshader++;
		}
	}
/*
.....Define the lights
*/
	Slight_list = LiShaderListCreate(nshader,shader);
/*	LiLightListSet(Slight_list);*/
	LiMWViewportSetLightList(LW_viewport,Slight_list);
	if (LW_active) ul_ipv_view_same(LW_vport.xform);
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_destroy_lights()
**			This function destroys any active lights in NCLIPV.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void ul_ipv_destroy_lights()
{
/*
.....Destory the lights
*/
	if (Slight_list)
	{
		LiShaderReferenceDestroy(Slight_list,TRUE);
		Slight_list = 0;
	}
}
/*********************************************************************
**	 E_FUNCTION : ul_ipv_set_background()
**			This function activates the user requested NCLIPV background
**       shader.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS:    none.
**	 SIDE EFFECTS:  none.
**	 WARNINGS: none.
*********************************************************************/
void ul_ipv_set_background()
{
	int i;
	LtColour colour_value[4];
	LtData data;
	LtShader shader;
/*
....Setup the requested shader
*/
	switch (LW_display_prop.shader)
	{
/*
.....Solid background
*/
	case 0:
		shader = LiShaderCreate(LI_SHADER_CLASS_BACKGROUND,"plain");
		if (LW_display_prop.bgcolor == -1)
		{
			LiColourInitialise(colour_value[0],LW_display_prop.bgrgb[0],
				LW_display_prop.bgrgb[1],LW_display_prop.bgrgb[2]);
		}
		else
		{
			LiColourInitialise(colour_value[0],
				UM_pkcolors[LW_display_prop.bgcolor].red/255,
				UM_pkcolors[LW_display_prop.bgcolor].green/255,
				UM_pkcolors[LW_display_prop.bgcolor].blue/255);
		}
		LiDataSetColour(&data,colour_value[0]);
		LiShaderSetArg(shader,"colour",&data);
		break;
/*
.....Graduated background
*/
	case 1:
		shader = LiShaderCreate(LI_SHADER_CLASS_BACKGROUND,"graduated");
      for (i=0;i<2;i++)
		{
			if (LW_display_prop.grad[i] == -1)
			{
				LiColourInitialise(colour_value[i],LW_display_prop.grgb[i][0],
					LW_display_prop.grgb[i][1],LW_display_prop.grgb[i][2]);
			}
			else
			{
				LiColourInitialise(colour_value[i],
					UM_pkcolors[LW_display_prop.grad[i]].red/255,
					UM_pkcolors[LW_display_prop.grad[i]].green/255,
					UM_pkcolors[LW_display_prop.grad[i]].blue/255);
			}
		}
		LiDataSetColour(&data,colour_value[0]);
		LiShaderSetArg(shader,"top colour",&data);
		LiDataSetColour(&data,colour_value[1]);
		LiShaderSetArg(shader,"bottom colour",&data);
		break;
/*
.....4 Corner background
*/
	case 2:
		shader = LiShaderCreate(LI_SHADER_CLASS_BACKGROUND,"four corner");
      for (i=0;i<4;i++)
		{
			if (LW_display_prop.fcolor[i] == -1)
			{
				LiColourInitialise(colour_value[i],LW_display_prop.frgb[i][0],
					LW_display_prop.frgb[i][1],LW_display_prop.frgb[i][2]);
			}
			else
			{
				LiColourInitialise(colour_value[i],
					UM_pkcolors[LW_display_prop.fcolor[i]-1].red/255,
					UM_pkcolors[LW_display_prop.fcolor[i]-1].green/255,
					UM_pkcolors[LW_display_prop.fcolor[i]-1].blue/255);
			}
		}
		LiDataSetColour(&data,colour_value[0]);
		LiShaderSetArg(shader,"top left colour",&data);
		LiDataSetColour(&data,colour_value[1]);
		LiShaderSetArg(shader,"top right colour",&data);
		LiDataSetColour(&data,colour_value[2]);
		LiShaderSetArg(shader,"bottom left colour",&data);
		LiDataSetColour(&data,colour_value[3]);
		LiShaderSetArg(shader,"bottom right colour",&data);
		break;
/*
.....Image Background
*/
	case 3:
		shader = LiShaderCreate(LI_SHADER_CLASS_BACKGROUND,"image");
		LiDataSetString(&data,LW_display_prop.bgfile);
		LiShaderSetArg(shader,"file name",&data);
		switch (LW_display_prop.rotate)
		{
		case 0:
			LiDataSetEnum(&data,LI_ANGLE_DEG_0);
			break;
		case 1:
			LiDataSetEnum(&data,LI_ANGLE_DEG_90);
			break;
		case 2:
			LiDataSetEnum(&data,LI_ANGLE_DEG_180);
			break;
		case 3:
			LiDataSetEnum(&data,LI_ANGLE_DEG_MINUS_90);
			break;
		}
		LiShaderSetArg(shader,"rotation",&data);
		LiDataSetBoolean(&data,1-LW_display_prop.stretch);
		LiShaderSetArg(shader,"keep aspect",&data);
		break;
	}
/*
.....Set the background
*/
		LiDataSetGenericPtr(&data,shader);
		LiDrawableSetProperty(LW_drawable,LI_DRAWABLE_PROP_BACKGROUND,&data);
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_set_colors()
**			This function sets up the automatic cut colors for NCLIPV.
**	 PARAMETERS	
**		 INPUT  :  none
**			cmsg = Modal ctyp's parameter.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
void ul_ipv_set_colors()
{
	int i,j,k,ist,ifl;
	UU_LOGICAL iuse;
	LW_stock_struc *sd,*sdtmp;
/*
.....Only set up automatic colormap
.....if automatic color change is enabled
*/
	if (LW_default_tool.cut_color != -1)
	{
		LW_cut_colormap[0] = LW_default_tool.cut_color;
		LW_n_cut_colormap = 1;
		LW_cutcolor_index = 0;
		goto done;
	}
/*
.....Set up colormap
*/
	LW_n_cut_colormap = 0;
	ist = 1;
	for (i=ist;i<UM_POCKET_COLORS;i++)
	{
		iuse = UU_TRUE;
/*
........Ignore clash colors
*/
		if (i == LW_clash_material.fixture || i == LW_clash_material.holder ||
			i == LW_clash_material.rapid || i == LW_highlight_color)
				iuse = UU_FALSE;
/*
........Ignore Stock and Fixture colors
*/
		for (j=0;j<2;j++)
		{
			if (!LW_default_tool.use_stock[j])
			{
				sd = LW_stock_first[j];
				for (k=0;k<LW_nstock[j];k++)
				{
					ifl = 0;
					do
					{
						ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
						if (ifl == -2) break;
						if (sdtmp->color == i) 
						{
							iuse = UU_FALSE;
							break;
						}
					} while (ifl != -1);
					sd = (LW_stock_struc *)uu_lsnext(sd);
				}
			}
		}
/*
.....Store color
*/
		if (iuse)
		{
			LW_cut_colormap[LW_n_cut_colormap] = i;
			LW_n_cut_colormap++;
		}
	}
/*
.....No colors assigned
.....So assign single color 
*/
	if (LW_n_cut_colormap == 0)
	{
		LW_cut_colormap[0] = 5;
		LW_n_cut_colormap = 1;
		LW_cutcolor_index = 0;
	}
/*
.....End of routine
.....Determine initial color index
*/
done:;
	if (LW_cutcolor_index == -1)
	{
		LW_cutcolor_index = 0;
		for (i=0;i<LW_n_cut_colormap;i++)
		{
			if (LW_cut_colormap[i] >= LW_default_tool.initial)
			{
				LW_cutcolor_index = i;
				break;
			}
		}
	}
	return;
}
void ul_ipv_update_colors()
{
	int i;
	LtColour colour_value;
	if (LW_initialized==0)
		return;
	for (i=0;i<UM_POCKET_COLORS;i++)
	{
		if (LW_material[i]!=0)
			LiMaterialDestroy(LW_material[i]);

		UM_pkcolors[i].red = uw_color_table[i][0];
		UM_pkcolors[i].green = uw_color_table[i][1];
		UM_pkcolors[i].blue = uw_color_table[i][2];

		LiColourInitialise (colour_value,
			UM_pkcolors[i].red/255.,
			UM_pkcolors[i].green/255.,
			UM_pkcolors[i].blue/255.);
		LW_material[i] = ul_ipv_create_material (colour_value);
	}
}
