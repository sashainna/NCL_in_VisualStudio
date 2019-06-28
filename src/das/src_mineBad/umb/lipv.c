/*********************************************************************
**  FILENAME: lipv.c
**  CONTAINS:   
**              ul_ipv_terminate
**              ul_ipv_start
**              ul_ipv_end
**              ul_ipv_open_window
**              ul_ipv_close_window
**              ipv_resize_window()
**              ul_ipv_load_dummy()
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipv.c , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**       02/08/16 , 09:13:24
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
#include "usysg.h"
#include "uims.h"
#include "spmouse.h"

extern int UV_current_sview;
int IPV_resize=0;
void ul_ipv_close_window();
void ipv_resize_window();
char *LW_window;

int PKx,PKy, GP_PKx, GP_PKy;
static FILE *Sjou_fd;
static UU_LOGICAL LW_used=UU_FALSE;
extern char *UM_pocket_hwnd;
int UM_swap_ipv = 0;
int UW_resize_window = 0;
static int IPV_first = 1;
extern int NCL_swap_changed;

extern int NAUTMACH;
/*********************************************************************
**	 E_FUNCTION:int ul_ipv_terminate()
**		Terminates an interactive NCLIPV.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_terminate()
{
	int i,j,ifl;
	LW_stock_struc *sptr;
	if (UL_ipv == 1)
	{
		if (LW_initialized && LW_used)
		{
/*
.....Get rid of window
*/
			ul_ipv_end();
/*
.....Get rid of stocks and fixtures
*/
			for (j=0;j<2;j++)
			{
				LW_stock_data[j] = LW_stock_first[j];
				for (i=0;i<LW_nstock[j];i++)
				{
					ifl = 0;
					do
					{
						ul_ipv_get_next_stock(LW_stock_data[j],&sptr,&ifl,UU_FALSE);
						if (ifl == -2) break;
						ul_ipv_remove_prim(sptr);
						uu_free(sptr->data);
					} while (ifl != -1);
					if (LW_stock_data[j]->type == LW_STOCK_COMPOS)
						uu_free(LW_stock_data[j]->data);
					LW_stock_data[j] = (LW_stock_struc *)uu_lsnext(LW_stock_data[j]);
				}
				uu_lsdel(LW_stock_first[j]);
				LW_nstock[j] = 0;
				LW_stock_first[j] = 0;
				LW_stock_data[j] = 0;
				LW_stock_idn[j] = 1;
			}
/*
.....Destroy lights
*/
			ul_ipv_destroy_lights();
/*
.....Destroy tool
*/
			ul_ipv_deselect_tool();
/*
.....Destroy the machining session
*/
			if (LW_viewport != 0) LiMWViewportDestroy(LW_viewport);
			if (LW_view != 0) LiViewDestroy(LW_view);
			ul_ipv_mot_stack_delete();
			ul_ipv_delete_sessions();
			if (LW_session[0] != 0) LiSessionDestroy(LW_session[0]);
			if (LW_session[1] != 0) LiSessionDestroy(LW_session[1]);
			LW_viewport = 0;
			LW_view = 0;
			LW_session[0] = 0;
			LW_session[1] = 0;
			LW_active = UU_FALSE;
/*
.....Destroy the materials
*/
			for (i=0;i<UM_POCKET_COLORS;i++)
			{
				LiMaterialDestroy(LW_material[i]);
				LW_material[i] = 0;
			}
/*
.....Destroy the transformations
*/
			LiTransformDestroy(LW_xform_mminch);
			LiTransformDestroy(LW_xform_inchmm);
/*
.....Close the journal file
*/
			if (Sjou_fd != UU_NULL) LiFileClose(Sjou_fd);
			Sjou_fd = UU_NULL;
/*
.....Terminate the NCLIPV session
*/
			LiTerminateLocal();
			LW_initialized = UU_FALSE;
		}
	}
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_start(flag)
**		This function initiates an NCLIPV session.
**	 PARAMETERS	
**		 INPUT  :
**        flag    = UU_TRUE = Starts NCLIPV from the interface with no
**                  limitations.  UU_FALSE = Starting NCLIPV from a
**                  motion form Preview button.
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_start(flag)
UU_LOGICAL flag;
{
	int status,ierr,wid,hgt, stat;
	char lmsg[80];
	LtSession session;
	UX_pathname fullname;
	FILE *fptr;

	status = UU_SUCCESS;
	if (LW_display_prop.swap && flag)
	{
		UM_swap_ipv = 1;
		NCL_swap_changed = 1;
	}
/*
.....Start MachineWorks session
*/
	if ((LW_version == LW_MWORKS && !LW_active)||(LW_nclipv==LW_STANDALONE))
	{
/*
.....Allocate NCLIPV terminal
*/
		pwdall("NCLIPV",lmsg,&ierr);
		if (ierr != 0)
		{
			ud_wrerr(lmsg);
			return(UU_FAILURE);
		}
		if (NAUTMACH && LW_mach_simul)
		{
			pwdall("SIMULATE",lmsg,&ierr);
			if (ierr != 0)
			{
				if (LW_mach_defined) ul_ipv_destroy_assembly();
				LW_mach_simul = UU_FALSE;
			}
		}
/*
.....Open log file
*/
		ul_ipv_open_diag(LW_diag_file,UU_TRUE,LI_FILE_APPEND);
/*
.....Start a new machining session
*/
		session = LW_session[LW_mach_mode];
		status = ul_ipv_start_session(UU_TRUE,UU_TRUE);
		if (status != UU_SUCCESS) return(status);
/*
.....Open the NCLIPV Monitor Panel
*/
		if (LW_monitor) ul_ipv_monitor_form();
/*
...Open the window
*/
		ul_ipv_open_window();
		if (UM_swap_ipv && LW_display_prop.minimize) um_minimize_pocket_window();
		LW_active = UU_TRUE;
		LW_used = UU_TRUE;
		if (LW_display_prop.swap && flag)
		{
/*
......put those call after assign	PKx = wid;
									PKy = hgt;
......because the graphic window haven't assign PKx and PKy yet
......PKx=Pky=0 will cause a fatal error in my DELL ccomputer
......Yurong 10/13/05
*/
/*			ul_ipv_view_same (UV_current_sview);*/
			IPV_first = 0;
#if UU_COMP == UU_WIN2K
			uw_ntget_gwsize(&wid, &hgt);
#else
#if UU_COMP == UU_IRIS4D
#ifndef UU_RS6000
			uw_glget_gwsize(&wid, &hgt);
#endif
#endif
#endif
			PKx = wid;
			PKy = hgt;
			ul_ipv_view_same (UV_current_sview);
			ipv_resize_window();
			um_reset_pocket_graphics(UM_IPV_WINDOW);
			uw_glresize_graphics(GP_PKx, GP_PKy, 1);
			uw_glinit_shading();
			uw_gllight_define();
		}
	}
	NCL_swap_changed = 0;
	UV_SM_NCL = 0;
/*
.....Predefine stocks by
.....Processing up to 1st motion
*/
	if (session == UU_NULL) ul_ipv_prescan_stocks();
/*
.....load UZ_IPV_KEYDEFS key defines
*/
	stat = uz_open_keydef("UZ_IPV_KEYDEFS",fullname,&fptr, 0);
	if (stat == UU_SUCCESS)
	{
		ux_fclose0(fptr);
		uz_load_keydef_file(fullname); 
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_end()
**		This function ends an NCLIPV session.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_end()
{
	int status, stat;
	UX_pathname fullname;
	FILE *fptr;
/*
.....Cancel MachinWorks
*/
	if (LW_version == LW_MWORKS && LW_active)
	{
/*
.....swap back the window before end the IPV section
*/
		if (UM_swap_ipv)
			ul_ipv_swap_screen();
/*
........Deallocate the NCLIPV terminal
*/
		pwddea("NCLIPV");
		if (LW_mach_simul) pwddea("SIMULATE");
/*
.....Close the NCLIPV Monitor Panel
*/
/*		if (LW_monitor == 1) ul_ipv_monitor_close();*/
/*
.....Close the Measurement form
*/
/*		ul_ipv_measure_close();*/
/*
........End the simulation
*/
		status = UU_SUCCESS;
		if (LW_auto_reset) ul_ipv_end_session();
		LW_active = UU_FALSE;
		ul_ipv_close_window();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
		UW_resize_window = 1;
		ul_ipv_close_diag();
/*
.....Close the NCLIPV Monitor Panel
*/
		if (LW_monitor == 1) ul_ipv_monitor_close();
/*
.....Close the Measurement form
*/
		ul_ipv_measure_close();
/*
.....load UZ_IPV_KEYDEFS key defines
*/
		stat = uz_open_keydef("UZ_CAM_KEYDEFS",fullname,&fptr,0);
		if (stat == UU_SUCCESS)
		{
			ux_fclose0(fptr);
			uz_load_keydef_file(fullname); 
		}
	}
/*
.....End of routine
*/
	UV_SM_NCL = 1;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_open_window()
**		Opens an interactive NCLIPV window.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_open_window()
{
	int status,i;
	char *win;
	LtData data;
/*	LtDriver driver;*/
	char *um_get_ncl_win();
/*
.....Delete the previous drawable
*/
	if (LW_drawable!=0)
		LiDrawableDestroy(LW_drawable);
	LW_drawable = 0;
/*
.....Open the window
*/
	status = um_pocket_window("NCLIPV",UU_NULL,UM_IPV_WINDOW);
	if (status == 0)
	{
		if (UM_swap_ipv==0)
			win = um_get_pocket_window(UM_IPV_WINDOW);
		else
			win = um_get_ncl_win();
/*		LiDataSetEnum(&data,LI_OGLDRV_WINDOW_MODE_APP);
		LiControlSet(LI_CONTROL_OGLDRV_WINDOW_MODE,&data);*/
		LiDataSetEnum(&data,LI_RN_RENDERER_OPENGL);
		LiControlSet(LI_CONTROL_RN_RENDERER_TYPE,&data);
/*
.....Setup animation mode
*/
#ifdef UU_IPV
/*		LiDataSetBoolean(&data,1-LW_display_prop.buf);
		LiControlSet(LI_CONTROL_MO_USE_SWAP_BUFFERS,&data);*/
#if UU_COMP == UU_WIN2K
/*		LiOGLDrvWindowInitialise((HWND)win,0);*/
		i = LW_display_prop.buf == 0 ? LI_RN_DRAWABLE_SWAP|LI_RN_DRAWABLE_THREAD_SAFE : LI_RN_DRAWABLE_THREAD_SAFE;
		LW_drawable = LiDrawableCreate((HWND)win,NULL,i);
#else
/*		LiOGLDrvWindowInitialise((Window)win);*/
		i = LW_display_prop.buf == 0 ? LI_RN_DRAWABLE_SWAP : 0;
		LW_drawable = LiDrawableCreate(/*display*/0,(Window)win,NULL,i);
#endif
#endif
		LW_window = win;
/*
		driver = LiDriverFindFromName("lidrvwin");
		LiDriverSet(driver);

		LiDataSetGenericPtr(&data,(LtGenericPtr)win);
		LiDriverSetControl(driver, LI_CONTROL_DRIVER_WINDOW, &data);
		LiControlSet(LI_CONTROL_DRIVER_WINDOW, &data);
*/
		LiViewSetResolution(LW_view, PKx, PKy, 1.0);
/*		LiDriverResizeNotify(PKx, PKy);*/
/*
........Set up the view
*/
		if (UM_swap_ipv==0)
		{
			if (IPV_first)
				ul_ipv_view_same (UV_current_sview);
			else
				ul_ipv_view_same (LW_vport.xform);
			IPV_first = 0;
			um_reset_pocket_graphics(UM_IPV_WINDOW);
 		}
	}
/*
........Set background
*/
	ul_ipv_set_background();
	return (status);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_close_window()
**		Opens an interactive NCLIPV window.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_close_window()
{
/*
.....don't swap back here, instead put those code in ul_ipv_end()
.....because the ul_ipv_end() already end the IPV section before goes here
.....so here does nothing
*/
/*
	if (UM_swap_ipv)
		ul_ipv_swap_screen();
*/
	if (LW_drawable!=0)
		LiDrawableDestroy(LW_drawable);
	LW_drawable = 0;
	um_close_pocket_window(UM_IPV_WINDOW);
	IPV_first = 1;
/*	if (UM_swap_ipv)*/
		uz_repaint(0);
}

/*********************************************************************
**	 E_FUNCTION:int ipv_resize_window()
**		resize NCLIPV window.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ipv_resize_window()
{
	um_set_screen_area(UM_IPV_WINDOW);
	IPV_resize = 1;
	gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));
	uv_setxform(&LW_vport);
	LiViewSetResolution(LW_view, PKx, PKy, 1.0);
/*
.....Setting the background forces
.....MachineWorks to redraw the entire window
.....Required with the MachineWorks 6.6. libraries
*/
	ul_ipv_set_background();
	ul_ipv_view_same(LW_vport.xform);
	IPV_resize = 0;
	um_reset_pocket_graphics(UM_IPV_WINDOW);
}
/*********************************************************************
**	 E_FUNCTION:int ul_ipv_load_dummy()
**		Create a dummy stock in order to show NCLIPV initial window.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_load_dummy()
{
	int i,stat;
	void ul_ipv_view_stock();
	LW_stock_struc *sd;

	LW_dummy = 1;
	sd = LW_stock_data[0];
	sd->id = 1;
	LW_stock_idn[0] = 2;
	sd->color = -1;
	sd->translucency = 100;
	sd->visible = UU_FALSE;
	sd->active = UU_FALSE;
	sd->important = LW_stock_default[0].important;
	sd->toler = LW_stock_default[0].toler;
	sd->edge = 0;
	sd->edge_color = 64;
	sd->mxflag = UU_FALSE;
	sd->mxchg = UU_FALSE;
	sd->axes = LW_stock_default[0].axes;
	sd->axes_color = -1;
	sd->stock = 0;
	sd->axis_seg = -1;
	strcpy(sd->mxname,LW_stock_default[0].mxname);
	um_tftotf(LW_stock_default[0].matrix,sd->matrix);
/*
........Box Solid
*/
	sd->type = LW_STOCK_BOX;
	sd->data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*6);
	if (sd->data == UU_NULL) goto failed;
	sd->data[0] = -100;
	sd->data[1] = -100;
	sd->data[2] = -100;
	sd->data[3] = 100;
	sd->data[4] = 100;
	sd->data[5] = 100;
	LW_stock_data[0] = (LW_stock_struc *)uu_lsinsrt((char *)LW_stock_data[0],
				sizeof(LW_stock_struc));
	if (LW_stock_data[0] == UU_NULL) goto failed;
	LW_nstock[0]++;
/*
.....Create stock
*/
	stat = UU_SUCCESS;
	if (LW_active || LW_session[LW_mach_mode] != 0)
		stat = ul_ipv_create_stock(sd,0,1);
/*
.....View the stock
*/
	ul_ipv_view_stock(0,sd);
/*
.....delete this dummy stock 
*/
	ul_ipv_remove_stock(0, sd);
	goto done;
/*
.....Failed to allocate memory
*/
failed:;
	ud_wrerr("Could not allocate memory for stock.");
	stat = UU_FAILURE;
/*
.....End of routine
*/
done:;
	LW_dummy = 0;
	return(stat);

}
