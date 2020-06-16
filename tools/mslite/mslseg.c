/********************************************************************* 
**  NAME:  mslseg.c
**
**		CONTAINS: 
**			msl_start ()
**			msl_open_window()
**			msl_terminate()
**
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       mslseg.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       07/20/15 , 08:45:17
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"

#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"
#include "mdcpln.h"

char *LW_window;
extern int MS_current_view;
extern int PKx,PKy;
extern int UL_ipv;
/*********************************************************************
**	 E_FUNCTION:int msl_start()
**		This function initiates an mls session.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int msl_start()
{	
	UL_ipv = 1;
	ms_create_vport();
	ms_create_view();
	ul_ipv_initialize();
/*
.....End of routine
*/
	return(UU_SUCCESS);
}
/*********************************************************************
**	 E_FUNCTION:int msl_open_window()
**		Opens an interactive MSLITE window. This function is replace
**			to NCL ul_ipv_open_window function because there is little 
**			difference
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int msl_open_window()
{
	int status,i;
	char *win;
	char *p,*ux_getenv();
	LtData data;

	status = 0;
/*	LiDataSetEnum(&data,LI_OGLDRV_WINDOW_MODE_APP);
	LiControlSet(LI_CONTROL_OGLDRV_WINDOW_MODE,&data);*/
	LiDataSetEnum(&data,LI_RN_RENDERER_OPENGL);
	LiControlSet(LI_CONTROL_RN_RENDERER_TYPE,&data);
	p = ux_getenv("NCLIPV_SWAP",UX_NPRTERRS);
	i = LI_RN_DRAWABLE_SWAP;
	if (p != UU_NULL)
	{
		if (ul_to_number(p,&status))
		{
			i = status == 0 ? LI_RN_DRAWABLE_SWAP : 0;
/*			LiDataSetBoolean(&data,status);
			LiControlSet(LI_CONTROL_MO_USE_SWAP_BUFFERS,&data);*/
		}
	}
/*	LiOGLDrvWindowInitialise((HWND)LW_window,0);*/
	LW_drawable = LiDrawableCreate((HWND)LW_window,NULL,i);
	LiViewSetResolution(LW_view, PKx, PKy, 1.0);
/*
........Set up the view
*/
	LW_active = 1;
	msl_setsize(PKx, PKy);
	LW_vport.cur_view = MS_current_view;
	uv_setxform();
	LiViewSetResolution(LW_view, PKx, PKy, 1.0);
	msl_vfit(1);
/*
.....Set the background
*/
	ul_ipv_set_background();
	return (status);
}

/*********************************************************************
**	 E_FUNCTION:int msl_terminate()
**		Terminates an interactive MSLITE.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void msl_terminate()
{
	int i,j;

	if (LW_initialized)
	{
		ul_ipv_end_session();
/*
.....Get rid of stocks and fixtures
*/
/*
		for (j=0;j<2;j++)
		{
			LW_stock_data[j] = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				if (LW_stock_data[j]->stock != 0)
					LiPrimitiveDestroy(LW_stock_data[j]->stock);
				uu_free(LW_stock_data[j]->data);
				LW_stock_data[j] = (LW_stock_struc *)uu_lsnext(LW_stock_data[j]);
			}
			uu_lsdel(LW_stock_first[j]);
			LW_nstock[j] = 0;
			LW_stock_first[j] = 0;
			LW_stock_data[j] = 0;
			LW_stock_idn[j] = 1;
		}
*/
/*
.....Destroy lights
*/
		ul_ipv_destroy_lights();
/*
.....Destroy tool
*/
/*
		if (LW_tool != 0)
		{
			LiPrimitiveDestroy(LW_tool);
			LW_tool = 0;
		}
*/
/*
.....Destroy the machining session
*/
		if (LW_session[0] != 0) LiSessionDestroy(LW_session[0]);
		if (LW_session[1] != 0) LiSessionDestroy(LW_session[1]);
		LW_session[0] = 0;
		LW_session[1] = 0;
		LW_active = UU_FALSE;
/*
.....Destroy the materials
*/
		for (i=0;i<UM_POCKET_COLORS;i++)
		{
			LiMaterialDestroy(LW_material[i]);
		}
/*
.....Destroy the transformations
*/
		LiTransformDestroy(LW_xform_mminch);
		LiTransformDestroy(LW_xform_inchmm);
/*
.....Terminate the NCLIPV session
*/
		LiTerminateLocal();
		LW_initialized = UU_FALSE;
	}
}

