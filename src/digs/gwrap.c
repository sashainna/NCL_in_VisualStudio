/*********************************************************************
**    NAME         :  gwrap.c
**       CONTAINS: wrap function for jump table functions
**		
**				ug_view_seg(n)
**				ug_delete_cutseg(vp, nseg)
**				ug_close_cutseg()
**				ug_get_clip(clip)
**				ug_set_clip(clip)
**				ug_dyndraw(n,ll,ur,erase)
**				ug_erase_cutseg(vp,sll,sur)
**				ug_reset_cutseg(vp)
**				ug_postn_cutseg(vp,nseg,pos)
**				ug_open_cutseg(vp,nseg,mfl,ll,ur)
**				ug_getsurf()
**				ug_setsurf(mask)
**				ug_graphsurf()
**				ug_popsurf()
**				ug_getdepth_mask()
**				ug_setdepth_mask()
**				ug_get_wsshade()
**				ug_get_lucency_mask(lucency, bytearray)
**				ug_wflush()
**				ug_clearvp(n)
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
** 
**    MODULE NAME AND RELEASE LEVEL 
**			gwrap.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:05:27
*********************************************************************/
#include "zsysdep.h"
#include <stdio.h>
#include "gtbl.h"
#include "gerror.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gviw.h"
#include <math.h>
#include "gsegac.h"
#include "udebug.h"

/*********************************************************************
**
**    E_FUNCTION:ug_view_seg(n)
**		View a segment
**
**    PARAMETERS   
**       INPUT  : 
**          n 					-- segment to view.
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ug_view_seg(n)
int n;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_VIEW_SEG])(n);
}

/*********************************************************************
**
**    E_FUNCTION:ug_close_cutseg(vp)
**		Close a cutter segment
**
**    PARAMETERS   
**       INPUT  : 
**			vp: viewport number 
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ug_close_cutseg(vp)
int vp;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_CLOSE_CUTSEG])(vp);
}


/*********************************************************************
**
**    E_FUNCTION:ug_delete_cutseg(vp, nseg)
**		delete a cutter object
**
**    PARAMETERS   
**       INPUT  :
**			vp: viewport number 
**          nseg   = cutter object number to delete.
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ug_delete_cutseg(vp, nseg)
int vp, nseg;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_DEL_CUTSEG])(vp, nseg);
}


/*********************************************************************
**
**    E_FUNCTION:ug_get_clip(clip)
**		Get current clip area
**
**    PARAMETERS   
**       INPUT  :
**			clip: clip area
**          
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ug_get_clip(clip)
int clip[4];
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_GET_CLIP])(clip);
}

/*********************************************************************
**
**    E_FUNCTION:ug_set_clip(clip)
**		Set current clip area
**
**    PARAMETERS   
**       INPUT  :
**			clip: clip area
**          
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ug_set_clip(clip)
int clip[4];
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_SET_CLIP])(clip);
}

/*********************************************************************
**
**    E_FUNCTION:ug_dyndraw(n,ll,ur,erase)
**		Redraws the current viewport when during dynamic viewing.
**	
**
**    PARAMETERS   
**       INPUT  :
**			n     = Active transformation.
**			ll    = Lower left of viewport in DC.
**			ur    = Upper right of viewport in DC.
**			erase = Erase segment flag.
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ug_dyndraw(n,ll,ur,erase)
int n,ll[2],ur[2],erase;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_DYNDRAW])(n,ll,ur,erase);
}

/*********************************************************************
**
**    E_FUNCTION:ug_erase_cutseg(vp,sll,sur)
**		    Erases the moving cutter symbol currently displayed in a
**          specific view port.
**
**    PARAMETERS   
**       INPUT  :
**          vp     = View port to erase the cutter symbol from.
**          lls    = Lower left corner of the cutter symbol's
**                   bounding 3-D box.
**          urs    = Upper right corner of bounding box.
**				
**       OUTPUT :  
**				None
**
**    RETURNS      : None
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
void ug_erase_cutseg(vp,sll,sur)
UU_REAL sll[],sur[];
int vp;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_ERASE_CUTSEG])(vp,sll,sur);
}
/*********************************************************************
**    E_FUNCTION     : ug_reset_cutseg(vp)
**          Marks the moving cutter symbol as not displayed.  Used when
**          we do not have an overlay plane.
**    PARAMETERS
**       INPUT  :
**          vp     = View port to erase the cutter symbol from.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_reset_cutseg(vp)
int vp;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_RESET_CUTSEG])(vp);
}

/*********************************************************************
**    E_FUNCTION     : ug_open_cutseg(vp,nseg,ll,ur)
**          Opens a graphics device segment for the purpose of storing
**          a moving cutter in.
**    PARAMETERS
**       INPUT  :
**			vp     = Viewport to open device segment for.
**       nseg   = 0 or graphics device segment to close.
**			ll     = Lower left of bounding box.
**			ur     = Upper right of bounding box.
**       OUTPUT :
**       nseg   = Graphics device segment number opened.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_open_cutseg(vp, nseg, bll, bur,vpcn)
int vp, *nseg;
UU_REAL bll[],bur[],vpcn[];
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_OPEN_CUTSEG])(vp,nseg,bll,bur,vpcn);
}

/*********************************************************************
**    E_FUNCTION     : ug_postn_cutseg(vp,nseg,pos)
**          Positions the graphics device segment which contains a
**			moving cutter.
**    PARAMETERS
**       INPUT  :
**			vp     = View port number to position cutter in.
**          nseg   = Graphics device segment of moving cutter to
**			         position.
**			pos    = XYZ position of moving cutter.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_postn_cutseg(vp,nseg,pos)
int nseg,vp;
UU_REAL pos[3];
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_POSTN_CUTSEG])(vp,nseg,pos);
}

/*********************************************************************
**    I_FUNCTION : ug_getsurf()
**      
**    DESCRIPTION:
**        Returns the plane mask for the current Surface.
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          uw_nt.cur_surf
**    RETURNS      : Current Surface mask.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_getsurf()
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		return (*(ug_gksstli.wsopen[0].connid)[UW_GETSURF])();
	return 1;
}

/*********************************************************************
**    I_FUNCTION : ug_setsurf(mask)
**      
**    DESCRIPTION:
**        Sets the plane mask for the current Surface.
**
**    PARAMETERS   
**       INPUT  : 
**          mask = Surface mask to set.
**       OUTPUT :  
**          none
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_setsurf(mask)
int mask;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_SETSURF])(mask);
}

/*********************************************************************
**    I_FUNCTION : ug_graphsurf()
**      
**    DESCRIPTION:
**        Sets the plane mask for the Graphics Surface.  
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_graphsurf()
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_GRAPHSURF])();
}

/*********************************************************************
**    I_FUNCTION : ug_popsurf()
**      
**    DESCRIPTION:
**        Sets the plane mask for the Menu Surface.  
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_popsurf()
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_POPSURF])();
}

/*********************************************************************
**    I_FUNCTION : ug_getdepth_mask()
**      
**    DESCRIPTION:
**        Gets the depth mask for graphic interface 
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : depth mask
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_getdepth_mask()
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		return (*(ug_gksstli.wsopen[0].connid)[UW_GET_DEPMASK])();
	return 0;
}

/*********************************************************************
**    I_FUNCTION : ug_setdepth_mask()
**      
**    DESCRIPTION:
**        Sets the depth mask for graphic interface 
**
**    PARAMETERS   
**       INPUT  : 
**          flag: depth mask flag
**       OUTPUT :  
**          none
**    RETURNS      : depth mask
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_setdepth_mask(flag)
int flag;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_SET_DEPMASK])(flag);
}
/*********************************************************************
**    I_FUNCTION : ug_get_wsshade()
**      
**    DESCRIPTION:
**        Get the shading mode
**
**    PARAMETERS   
**       INPUT  : 
**          None
**       OUTPUT :  
**          none
**    RETURNS      : depth mask
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_get_wsshade()
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		return (*(ug_gksstli.wsopen[0].connid)[UW_GET_WSSHADE])();
	return 0;
}

/******************************************************************
**    FUNCTION     : ug_get_lucency_mask(lucency, bytearray)
**       Get stipple for a translucency
**    PARAMETERS
**       INPUT  :
**					lucency: translucency(1-100)
**       OUTPUT :
**					bytearray: stipple 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*temp yurong void ug_get_lucency_mask(lucency, bytearray)
int lucency;
unsigned char *bytearray;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_GET_LUCENCY])(lucency, bytearray);
}
*/

/******************************************************************
**    FUNCTION     : ug_wflush()
**       flush the buffer
**    PARAMETERS
**       INPUT  :
**					None
**       OUTPUT :
**					None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_wflush()
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_FLUSH])();	
}
/*********************************************************************
**    I_FUNCTION : ug_clearvp(n)
**       Clears a single viewport.
**    PARAMETERS
**       INPUT  :
**          n: viewport number
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_clearvp(n)
int n;
{
	if (ug_gksstli.wsopen[0].connid!=NULL)
		(*(ug_gksstli.wsopen[0].connid)[UW_CLEAR_VP])(n);
}
