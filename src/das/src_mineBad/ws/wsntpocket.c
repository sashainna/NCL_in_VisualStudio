/********************************************************************* 
**  NAME:  wsntpocket.cpp
**
**		CONTAINS:
**			uw_ntpocket_window(title)
**			uw_ntpaint_pocket(cx, cy)
**			uw_ntclose_pocket()
**			uw_ntset_maingw(cur_screen)
**			uw_ntsave_prect()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       wsntpocket.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:13
**
*********************************************************************/
#include "usysdef.h"
#if UU_COMP == UU_WIN2K
#include "dmotif.h"
#include "mxxx.h"
#include "uims.h"
#include "view.h"
#include "mpocket.h"
#include "lipv.h"
#include "lcom.h"
#include "wsgl.h"
/*
.....Global variable definitions
*/
extern int *UD_ksws;
/*
.....Static variables
*/
static int pocket_size[2]={0,0},pocket_pos[2]={0,0};
static struct UM_drawing_rec pocket_drawing;

int UW_pocket_mode=UU_FALSE;
static UM_current_type =  UM_DRAWING_WINDOW;
extern int UM_swap_ipv;
/**********************************************************************
**    I_FUNCTION :  uw_ntpocket_window(drawing,type)
**       Creates a Pocket Graphics Window.
**    PARAMETERS   
**       INPUT  : 
**          drawing     Drawing to display in pocket window.
**          type        Type of pocket window to open.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_ntpocket_window(title, draw_rec,type)
char *title;
int *draw_rec;
UM_pkwin_type type;
{
	int retstat;
	char *temp, pic_file[256];
	struct UM_drawing_rec *drawing;

	pic_file[0] = '\0';
	if (type==UM_DRAWING_WINDOW)
		drawing = (struct UM_drawing_rec *)draw_rec;
	else
	{
		if (draw_rec == UU_NULL)
			pic_file[0] = '\0';
		else
		{
			temp = (char *)draw_rec;
			strcpy(pic_file, temp);
		}
	}
/*
.....Initalize routine
*/
/*
......why?
*/
/*	UM_current_type = UM_DRAWING_WINDOW; */
	UM_current_type = type;
	if (type == UM_DRAWING_WINDOW)
		uu_move_byte(drawing,&pocket_drawing,sizeof(struct UM_drawing_rec));
/*
.....Take down previous pocket window
*/
	uw_ntclose_pocket(type);
/*
.....Make the pocket window active
*/
	if ((type == UM_DRAWING_WINDOW) || ((type == UM_IPV_WINDOW) && (UM_swap_ipv)))
		UW_pocket_mode = UU_TRUE;
/*
.....Open pocket window
*/
	uw_ntopen_pocket(title, pic_file, type);
/*
.....Return success
*/
	retstat = UU_SUCCESS;
/*
....End of routine
*/
	return(retstat);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntclose_pocket(type)
**       Closes a Pocket Graphics Window.
**    PARAMETERS   
**       INPUT  : 
**          type        Type of pocket window to close.
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_ntclose_pocket(type)
UM_pkwin_type type;
{
	int status,i;
	UV_vport vport;
/*
.....Close the pocket window
*/
	uw_ntclose_pocketwin(type);
/*
.....Do not redraw main graphics
*/
	if (type == UM_DRAWING_WINDOW)
	{
		ug_setredrwflag(0);
/*
.....Redraw all viewports (not contents though)
.....Because the segments were altered when the
.....screen changed
*/
		if (UW_pocket_mode)
		{
			for (i=0;i<UV_act_screen[0].nvports; i++)
			{
				status = uv_getvpid(UV_act_screen[0].vports[i],&vport);
				uv_drawvp(&vport);
			}
		}
	}
	UW_pocket_mode = UU_FALSE;
	status = UU_SUCCESS;
	return(status);
}

/**********************************************************************
**    I_FUNCTION : uw_ntset_maingw(cur_screen)
**       Set current main graphic view to cur_screen
**    PARAMETERS   
**       INPUT  : 
**          cur_screen: current screen to set
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_ntset_maingw(cur_screen)
char *cur_screen;
{
	int px[2];
	UU_REAL ndc[2];
	int width, height;
	if (UM_current_type != UM_DRAWING_WINDOW)
		return;
	uw_ntget_gwsize(&width, &height);

	udm_resize_graphics(&UD_duimsdeflt,width,height,0);
	gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));
/*
.....Set screen NDC coordinates
*/
	px[0] = uw_gl.xpixels;
	px[1] = uw_gl.ypixels;
	uw_gldevtondc(px,ndc);
	uw_gl.xndc = ndc[0];
	uw_gl.yndc = ndc[1];	
	uv_chgsc(cur_screen);
	um_reset_pocket_graphics(UM_DRAWING_WINDOW);
}

/**********************************************************************
**    I_FUNCTION : uw_ntpaint_pocket(cx, cy)
**       draw in pocket window
**    PARAMETERS   
**       INPUT  : 
**          cx, cy: pocket window size
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_ntpaint_pocket(cx, cy)
int cx, cy;
{
	int px[2];
	UU_REAL ndc[2];

	if (UM_current_type != UM_DRAWING_WINDOW)
		return;
	
	udm_resize_graphics(&UD_duimsdeflt,cx, cy,0);
	gswswindow(UD_ksws,&(UD_duimsdeflt.screen[0].wswind));

/*
.....Set screen NDC coordinates
*/
	px[0] = cx;
	px[1] = cy;
	uw_gldevtondc(px,ndc);
	uw_gl.xndc = ndc[0];
	uw_gl.yndc = ndc[1];
/*
.....Display the drawing
*/
	um_set_pocket_graphics(UM_DRAWING_WINDOW);
	gclearws(UD_ksws,1);
	um_view_pocket_drawing(&pocket_drawing);
}
/**********************************************************************
**    I_FUNCTION : uw_ntsave_prect (type, left, top, right, bottom)
**      save pocket window position ans size
**    PARAMETERS   
**       INPUT  : 
**          type: window type
**			left, top, right, bottom: window rect to be saved
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_ntsave_prect(type, left, top, right, bottom)
UM_pkwin_type type;
int left, top, right, bottom;
{
	if (type == UM_DRAWING_WINDOW)
	{
		UW_pocket_pos[0] = left;
		UW_pocket_pos[1] = top;
		UW_pocket_size[0] = right - left;
		UW_pocket_size[1] = bottom - top;
	}
	else if (type == UM_GRAPHIC_WINDOW)
	{
		UW_pic_size[0] = right - left;
		UW_pic_size[1] = bottom - top;
	}
	else
	{
		LW_ipv_pos[0] = left;
		LW_ipv_pos[1] = top;
		LW_ipv_size[0] = right - left;
		LW_ipv_size[1] = bottom - top;
	}

}

#endif
