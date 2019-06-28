#include "usysdef.h"
#ifdef UU_OPENGL

/*********************************************************************
**  NAME:  wsglseg.c
**
**      GKS workstation: seg functions section.
**
**    CONTAINS:
**
**					uw_glcreseg
**					uw_glopnseg
**					uw_gldelseg
**					uw_glinvseg
**					uw_glcloseg
**					uw_glviewsg
**             uw_glhilite
**					uw_glredrawsegrect
**
**    MODULE NAME AND RELEASE LEVEL 
**			wsglseg.c , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**			02/08/17 , 11:49:12
**    
*********************************************************************/

#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#else
#if UU_COMP != UU_WIN2K 
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#endif

#include "wsgl.h"
#include "ginqatt3.h"
#include "udebug.h"
#include "zsysdep.h"
#include "driver.h"
#include "gviw.h"
#include "gsegop.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "gsegac.h"
#if UU_COMP != UU_WIN2K 
#include "wsxw.h"
#else
#include "wsntglfunc.h"
#endif
#include "ginqatt.h"
#include "ginqatt2.h"
#include "ginqatti.h"
#include "gmat4.h"
#include "mdattr.h"
#include "mpocket.h"
#include "view.h"
#include "dmotif.h"
#include "nccs.h"
#include "uims.h"
#include "lcom.h"
#include "wsglfun.h"

#define DBG UU_FALSE

/* single precision matrix copy */
typedef struct { float a[16]; } uw_tmp_mcopy_st;
#define uw_mcopy(y,x) *(uw_tmp_mcopy_st *)(y) = *(uw_tmp_mcopy_st *)(x)

extern int uw_glhicolor,uw_glvrfcolor;
#define HILITE uw_glhicolor           /* Color index for hilited segments */
#define VHILITE uw_glvrfcolor           /* Color index for verify hilited segments */
#define V2HILITE 14
#define INTSIZEOF(x) ((sizeof(x)+sizeof(int)-1)/sizeof(int))
#define INTSIZ(x) ((x+sizeof(int)-1)/sizeof(int))
#define TRUE 1
#define FALSE 0

/*
.....TEMP
*/
#define MSEG 10000
static UU_LOGICAL Screate_seg = UU_FALSE;
extern GLuint pick_matrix;
#if UU_COMP == UU_WIN2K
extern int *UD_ksws;
#endif

void uw_glinvseg();
void uw_glviewsg (int n);
extern UM_pkwin_type uw_glget_context();
extern int UM_swap_ipv, NCL_swap_changed;
extern int NCL_pick_verify, UD_picking_active;
#define HILITE_MARK 0
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3
extern int NCL_mark_method;
extern int UM_material_reset;
extern int UV_dynview_active;
extern int UW_current_pickid;
/*********************************************************************
**    I_FUNCTION     :  uw_glcreseg(prms,reply)
**       Create a graphics segment prms[2].  Push onto name stack 
**			the segment number. Begin storing subsequent output in the segment.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glcreseg(prms,reply)		 
int prms[];				/* number of the segment to create */
int reply[];			/* not used now */
{
}

/*********************************************************************
**    FUNCTION     :  uw_glopnseg(reply)
**       Open the segment.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glopnseg(prms,reply)		
int reply[];
int prms[];
{
	ug_gksos.sysstate = UG_SGOP;
	uu_denter(UU_GITRC,(us,"uw_glopnseg()"));

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  uw_gldelseg(prms)
**       Delete a specified segment.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldelseg(prms)					 /* delete a specified segment */
int prms[3];
{
	UG_segstli *sp;			/* pointer to segment's header */
/*
.....Delete the GL segment
*/
	glDeleteLists(MSEG+prms[2],1);
/*
.....Delete the segment from the view
*/
/*
........If segment is visible
........don't erase it
*/
	sp = ug_segac(prms[2]);
	if (sp->segatts.gvis == UG_VISIBLE) uw_glinvseg(prms[2]);
}

/*********************************************************************
**    I_FUNCTION     :  uw_glinvseg(seg)
**       Removes (invisibles) a specified segment from the screen.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glinvseg(seg)
int seg;
{
	UG_segstli *sp;			/* pointer to segment's header */
	GLboolean savemask;
	Gnrect rect;
	UM_coord zpt;
/*
.....In viewing defered mode
.....Redraw at next interaction
*/
   if ((*ug_gksstli.wsopen[uw_gl.wid].outptr).modmode == UG_NIVE)
   {
      ug_setredrwflag(1);
      ug_resetredrwrect();
   }
/*
.....Delete segment from view
.....By redrawing around it
*/
	else
	{
		sp = ug_segac(seg);
/*
........Calculate bounding box
*/
		if (sp->wcboxok != 1) ug_altersegbox(seg,zpt);
/*
........Redraw segment area to erase
*/
		if (sp->wcboxok == 1 && UW_erase_method == 0)
		{
			ug_segndcbox(seg,&rect);
			ug_setredrwrect2(&rect);
			ug_setredrwflag(1);
		}
/*
........WC box cannot be calculated
........Redraw deleted segment in black
*/
		else
		{
			uw_gl.erase = 1;
/*
.........When deleting a segment
.........Never write in depth buffer
*/
			savemask = uw_glget_depthmask();
			uw_gldepth_mask(0); 
/*
.........Erase segment
*/
			uw_glviewsg(seg);
			uw_gl.erase = 0;
/*
........Restore depth buffer
*/
			uw_gldepth_mask(savemask);  
		}
	}
}

/*********************************************************************
**    I_FUNCTION     :  uw_glcloseg(prms)
**       Close the open segment.  Traverse with uw_glviewsg.
**    PARAMETERS   
**       INPUT  : 
**          prms[2]: segment number
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glcloseg(prms)		
int prms[];
{
	UG_segstli *sp;
	uu_denter(UU_GITRC,(us,"uw_glcloseg(%d)", prms[2]));
/*
.....Close the digs segment
*/
	sp = ug_segac(prms[2]);
	UG_RESET_WCBOX(sp);
	
	Screate_seg = UU_TRUE;
	uw_glviewsg (prms[2]);
	uu_dexit; 
}


/*********************************************************************
**    FUNCTION     :  uw_glviewsg(n)
**			Traverse segment n, display on iris workstation using high
**			performance drawing subroutines.
**    PARAMETERS   
**       INPUT  : n 					-- segment to view.
**       OUTPUT : none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glviewsg (n)                 
int n;
{
	int i, sub, loop;						/* loop counters */
	UG_plylna3op *cmd;		/* pointer to each cmd */
	int ncmd;					/* number of commands in a segment */
	int icmd;					/* ith command */
	int cmdlen;					/* length of this command */
	int opcode;					/* digs opcode of this segment element */
	char *ug_lsielt();		/* function to return list element */
	UG_segstli *segptr;		/* pointer to segment n's header */
	UG_LSI *listp;				/* graphics command list for this seg */
	int hilite = FALSE;		/* TRUE = We're hilighting this segment */
	int histyle = FALSE;
	int hitrans = FALSE;
	int pick_hilite = FALSE;	
	int ln, mk, tx, fl;		/* Saved version of digs colors */
	Gstate state;				/* storage for xforms, atts, used on seg call */
	GLfloat modsave[16];			/* Save modelling matrix here */
	UU_LOGICAL first,list_execute;
	int color, ltype_chg, lwid_chg, gvis;
	int list_begin;
	int cur_vp;
	UV_vport vport;
	Gfloat pri;
	int wireframe_disp = 1;
	int hid_line = 0;
	int shaded_surf = 0;
	UU_LOGICAL ledgecolor,dirty_flag;

	int savinx, savmat,savluc;
	int list_line_first = 1;
	int list_wid_first = 1;
	int mask_saved, savemask,depth_saved,override[3];
	int pick_level, save_pick_level;
	UU_KEY_ID key,*keys;
	double save_wid1 = -1.0, wid2 = -1.0;
	int current_pick=-1, save_style = 1;
	long buffer[UG_UDATA_SIZE];
/*
.....Non-existent segment
*/
	if((segptr = ug_segac(n)) == NULL) return;
/*
.....Initialize routine
*/
	ledgecolor = UU_FALSE;
	ltype_chg = 0;
	lwid_chg = 0;
	mask_saved = 0;
	depth_saved = 0;
	pri = 1.0;
	list_begin = 0;
	savemask = uw_glget_depthmask();
	uw_glget_material(&savinx, &savmat, &savluc);
	dirty_flag = uw_glget_dirty_flag();
/*
.....Determine if this segment should use
.....its display attributes when
.....overriding attributes and this segment
.....is on the Do Not Override list
*/
	override[0] = UN_override_geo_attr[0];
	override[1] = UN_override_geo_attr[1];
	override[2] = UN_override_geo_attr[2];
	if (UN_override_geo_nkeys != 0)
	{
		gsegrud(n,buffer);
		key = uv_getkey(buffer);
		if (key != 0)
		{
			keys = (UU_KEY_ID *)UU_LIST_ARRAY(&UN_override_geo_keys);
			for (i=0;i<UU_LIST_LENGTH(&UN_override_geo_keys);i++)
			{
				if (key == keys[i])
				{
					override[0] = override[1] = override[2] = -1;
					break;
				}
			}
		}
	}
/*
.....Geometry is invisible
*/
	if (override[0] == -2) return;
/*
.....Create new segment if visual has changed
*/
	if (UD_cmap_changed == 1 || NCL_swap_changed == 1) Screate_seg = 1;
	if (Screate_seg)
	{
		override[0] = override[1] = override[2] = -1;
	}
/*
.....Define any XFORMS in segment
.....Prior to displaying segment
........Save current modeling matrix
*/
	if (segptr->xforms & (UG_SEGMODXF))
	{
		ug_savestate(&state);
		uw_mcopy(modsave,uw_gl.modxf);
	}

	listp = (UG_LSI *) segptr->seglist; 
	ncmd  = ug_lsinelt((*listp));
/*
.........Look for XFORMS
*/
	for( icmd=0; icmd < ncmd; ++icmd )
	{
		cmd    = (UG_plylna3op *)ug_lsielt(listp,icmd);
		cmdlen = ug_lsilen(listp,icmd)/sizeof(int);
		opcode = cmd->elttype;
		switch (opcode)
		{
		case UG_SNTRANOP:
			#if DBG == UU_TRUE
				printf("    UG_SNTRANOP\n");
			#endif
			uw_glload_matrix ( ((UG_sntranop *)cmd)->xform, UU_TRUE, uw_gl.modxf);
			ug_gksstli.curvwindex = ((UG_sntranop *) cmd)->xform;
			cur_vp = ((UG_sntranop *)cmd)->xform;
			if (cur_vp<=0)
			{
				goto endsw;
			}
			uv_getvpid(UV_act_screen[0].vports[cur_vp-1],&vport);
			if ((vport.disp_mode>3)||(vport.disp_mode<=0))
				vport.disp_mode = 2; 
			if (vport.wireframe)
				wireframe_disp = 1;
			else
				wireframe_disp = 0;
/*			uw_gllighting(UU_TRUE);*/
			hid_line = 0;
			if (vport.disp_mode==3) hid_line = 1;
			goto endsw;
		}
	}
endsw:;
	first = UU_TRUE;
	list_execute = UU_FALSE;
/*
.....Create new segment
*/
	if (Screate_seg == UU_TRUE)
	{
		if (uw_gl.erase || segptr->segatts.gvis != UG_VISIBLE)
			glNewList_d(n+MSEG,GL_COMPILE);
		else 
		{
			list_execute = UU_TRUE;
			glNewList_d(n+MSEG,GL_COMPILE_AND_EXECUTE);
		}
		uw_gllighting_reset();
		list_begin = 1;
	}
#if DBG == UU_TRUE
	printf("SEGMENT = %d\n",n);
#endif
/*
.....If we're hiliting this segment, save current colors
.....and set all colors to hilite color
*/
	if (segptr->segatts.ghilit == UG_HIGHLIGHTED &&
		(NCL_mark_method == HILITE_MARK || NCL_mark_method == DYNAMIC_MARK))
	{
		hilite = TRUE;
		ug_get_bundle_colors (&ln, &mk, &tx, &fl);
		color = HILITE;
		ug_set_bundle_colors (HILITE,HILITE,HILITE,HILITE);
/*
.....should not disable depth buffer when selected
*/
/*		uw_gldepth_mask(0);
		mask_saved = 1;  
*/
	}
	else if (segptr->segatts.ghilit == UG_PICKLIGHTED &&
		NCL_mark_method == DYNAMIC_MARK)
	{
		hilite = TRUE;
		pick_hilite = TRUE;
		save_pick_level = 1;
		ug_get_bundle_colors (&ln, &mk, &tx, &fl);
		color = VHILITE;
		ug_set_bundle_colors (VHILITE,VHILITE,VHILITE,VHILITE);
		uw_gldepth_func(0);
		uw_gldepth_mask(0);
		mask_saved = 1;
		depth_saved = 1;
	}
/*
.....If we're erasing this segment, set all colors to background
*/
	if (uw_gl.erase)
	{
		hilite = TRUE;
		ug_get_bundle_colors (&ln, &mk, &tx, &fl);
		color = ug_segerasecolor;
		ug_set_bundle_colors (color,color, color,color);
		uw_gldepth_mask(0);
		mask_saved = 1;  
	}
/*
.....If we're in the middle of Analyzation Playback
.....set all colors to requested geo color
*/
	if (UN_override_geo_mask && segptr->xforms)
	{
		if (override[0] != -1)
		{
			hilite = TRUE;
			ug_get_bundle_colors (&ln, &mk, &tx, &fl);
			color = override[0];
			ug_set_bundle_colors (color,color, color,color);
//			uw_gldepth_mask(0);
//			mask_saved = 1;  
		}
/*
........And line styles
*/

		if (override[1] != -1)
		{
			histyle = TRUE;
			gqlinetype()->typeno = override[1];
/*
.....the first attribute call inside the list must forced, because the list
.....can be called in different condition
*/
			if (list_begin && list_line_first)
			{
				uw_gllinetype(override[1], 1);
				list_line_first = 0;
			}
			else
				uw_gllinetype(override[1], 0);
			if (override[1] != 1) ltype_chg = 1;
		}
/*
........And translucency
*/
		if (override[2] != -1)
		{
			hitrans = TRUE;
			ug_set_lucency(override[2]);
			pri = override[2] / 100.00;
		}
	}
/*
.....Segment is already created
.....let's view it
*/
	if (!Screate_seg && !hilite && !histyle && segptr->segatts.gvis == UG_VISIBLE)
	{
		glCallList_d(n+MSEG);
		uw_gllighting_reset();
		UM_material_reset = 1;
		goto done;
	}

	if (!Screate_seg && (segptr->segatts.gvis != UG_VISIBLE && !uw_gl.erase))
		goto done;
/*
......If display assist segment, disable depth buffer
*/
	if (ud_isassist_seg(n))
	{
		uw_gldepth_func(0);
		uw_gldepth_mask(0);
		mask_saved = 1;
		depth_saved = 1;
	}
/*
.....Don't update dirty flag if writing to the front buffer
*/
	if (uw_glgetbuffer() != UG_BACK_BUFFER) uw_glset_dirty_flag(UU_FALSE);
/*
.....Loop through commands
*/
	for( icmd=0; icmd < ncmd; ++icmd ) 
	{
		cmd    = (UG_plylna3op *) ug_lsielt (listp,icmd);
		cmdlen = ug_lsilen(listp,icmd)/sizeof(int);
		opcode = cmd->elttype;

#if DBG == UU_TRUE
		uw_gldebug_opcode (opcode);
#endif

		switch (opcode)
		{

		case UG_NOOP: break;

		case UG_PAGEOP:
		case UG_PLYLNA3OP: 
			if ((wireframe_disp)||(pick_hilite) || ledgecolor)
			{
				if ((pick_hilite || hid_line || ledgecolor) &&
					(gqlinetype()->typeno == 9))
				{
					gqlinetype()->typeno = 1;
					uw_gllinetype(1, 1);
				}
				uw_glpolyln3_cmd (n,cmd);
			}
			break;

		case UG_PLYLNA2OP: 
			uw_glpolyln2_cmd (n,cmd);
			break;

		case UG_PLYLNRASOP: 
			if (wireframe_disp)
				uw_glpolylnras_cmd (n,cmd);
			break;

    	case UG_PLYMKA3OP: 
			if (wireframe_disp)
			{
				uw_glmarker3op (n,cmd);
			}
			break;
    	case UG_PLYMKA2OP: 
			if (wireframe_disp)
			{
				uw_glmarker2op (n,cmd);
			}
         break;

	 	case UG_PLYMKRASOP: 
			if (wireframe_disp)
			{
				uw_glmarkerrasop (n,cmd);
			}
			break;

    	case UG_TEXTOP: 
			if ((wireframe_disp)||(shaded_surf))
			{
				uw_gltextop (n,cmd);
			}
			break;

	 	case UG_TEXTRASOP: 
			if (wireframe_disp)
			{
				ug_textras(&((UG_textrasop *)cmd)->position,
					((UG_textrasop *)cmd)->string, segptr->userdata[0]);
			}
			break;

	 	case UG_FLAREA3OP: 
			if (wireframe_disp)
			{
				uw_glfill3_cmd (n,cmd);
			}
			break;

	 	case UG_FLAREAOP: 
			if (wireframe_disp)
			{
				uw_glfill_cmd (n,cmd);
			}
			break;

	 	case UG_FLAREARASOP: 
			if (wireframe_disp)
			{
				uw_glfillras_cmd (n,cmd);
			}
			break;

	 	case UG_SHADEAREAOP:
			if (hid_line)
			{
				uw_glrmhid_cmd(n, cmd);
			}
			else if (pick_hilite==FALSE)			
			{
				shaded_surf = 1;
				uw_glshade_cmd (n, cmd, ledgecolor);
			}
			break;

		case UG_CALLOP: 

			i = (*(UG_callop *)cmd).segno;  	/* call seg i */
			uw_glviewsg (i);						/* recursive call */
			break;

		case UG_SNTRANOP:
			if (first)
			{
				first = UU_FALSE;
			}
			else
			{
				uw_glload_matrix((((UG_sntranop *)cmd)->xform),UU_TRUE,uw_gl.modxf);
				ug_gksstli.curvwindex = ((UG_sntranop *)cmd)->xform;
			}
			break;

		case UG_MTRANOP:
			ug_smodxf(((UG_mtranop *)cmd)->xf,((UG_mtranop *)cmd)->type); 
			break;
		case UG_LMTRANOP:
			ug_slmodxf ( ((UG_lmtranop *)cmd)->xf,((UG_lmtranop *)cmd)->type ); 
			break;
/*
... All the attribute functions below have macros defined in wsi4dim.h
... the function versions are similar, but all call workstation.
*/
		case UG_LSTYLOP:
			if (!histyle)
			{
/*
.....the first attribute call inside the list must forced, because the list
.....can be called in different condition
*/
				gqlinetype()->typeno = (*(UG_lstylop *)cmd).ls.typeno;
				if ((list_begin)&&(list_line_first))
				{
					uw_gllinetype((*(UG_lstylop *)cmd).ls.typeno, 1);
					list_line_first = 0;
				}
				else
					uw_gllinetype((*(UG_lstylop *)cmd).ls.typeno, 0);

				if ((*(UG_lstylop *)cmd).ls.typeno!=1)
				{
					ltype_chg = 1;
				}
			}
			else
			{
				if ((*(UG_lstylop *)cmd).ls.typeno == 9)
				{
					gqlinetype()->typeno = 9;
					uw_gllinetype(gqlinetype()->typeno, 1);
				}
				else if (gqlinetype()->typeno == 9)
				{
					gqlinetype()->typeno = override[1];
					uw_gllinetype(gqlinetype()->typeno, 1);
				}
			}
			break;
		case UG_LWIDOP:
			if (!pick_hilite)
			{
/*
.....the first attribute call inside the list must be forced, because the list
.....can be called in different condition
*/
				gqlinewidth() = (*(UG_lwidop *)cmd).width;
				if ((list_begin)&&(list_wid_first))
				{
					uw_gllinewidth((*(UG_lwidop *)cmd).width, 1);
					list_wid_first = 0;
				}
				else
					uw_gllinewidth((*(UG_lwidop *)cmd).width, 0);
				if ((*(UG_lwidop *)cmd).width!=1.0)
					lwid_chg = 1;
				save_wid1 = (*(UG_lwidop *)cmd).width;
			}
			else
			{
				if (current_pick==2)
				{
					if ((*(UG_lwidop *)cmd).width<1.0)
						wid2 = 4.0;
					else
						wid2 = 4*(*(UG_lwidop *)cmd).width;
					if (gqlinewidth() != wid2)
					{
						uw_gllinewidth(wid2, 1);
						gqlinewidth() = wid2;
					}
				}
				if ((current_pick==1)||(current_pick==-1))
				{
					if ((*(UG_lwidop *)cmd).width<=1)
					{
						wid2 = 2.0;
					}
					else
					{
						wid2 = (*(UG_lwidop *)cmd).width * 2.0;
					}
					if (gqlinewidth() != wid2)
					{
						uw_gllinewidth(wid2, 1);
						gqlinewidth() = wid2;
					}
				}
				save_wid1 = (*(UG_lwidop *)cmd).width;
				lwid_chg = 1;
			}
			break;
		case UG_FONTOP:
			ug_textfp (&((UG_fontop *)cmd)->p);
			break;
		case UG_CHEXPOP:
			ug_charexp((*(UG_chexpop *)cmd).expn);
			break;
		case UG_CHPLANEOP:
			ug_txplane(&(*(UG_chplaneop *)cmd).txpvc);
			break;
		case UG_CHUP3OP:
			ug_charup3(&(*(UG_chup3op *)cmd).upvec);
			break;
		case UG_CHUP2OP:
			ug_charup(&(*(UG_chup2op *)cmd).upvec);
			break;
		case UG_CHPATHOP:
			ug_textpath((*(UG_chpathop *)cmd).path);
			break;
		case UG_CHSPACEOP:
			ug_charspace((*(UG_chspaceop *)cmd).spacing);
			break;
		case UG_CHJUSTOP:
			ug_textalign(&(*(UG_chjustop *)cmd).align);
	 		break;
		case UG_SYMBOLOP:
			ug_marktype((*(UG_symbolop *)cmd).type);
 			break;
		case UG_PICKIDOP:
			ug_spickid((*(UG_pickidop *)cmd).pid);
			if (pick_hilite)
			{
				if ((*(UG_pickidop *)cmd).pid!=UW_current_pickid)
				{
					current_pick = 1;
				}
				else
				{
					pick_level = ncl_getpick_level(n, (*(UG_pickidop *)cmd).pid, &sub, &loop);
					if (pick_level==2)
						current_pick = 2;
					else
						current_pick = 1;
				}
				if (save_wid1==-1)
					break;
				if (current_pick==1)
				{
					if (gqlinewidth() != save_wid1*2)
					{
						uw_gllinewidth(save_wid1*2, 1);
						gqlinewidth() = save_wid1*2;
					}
				}
				if (current_pick==2)
				{
					if (save_wid1<1.0)
						wid2 = 4.0;
					else
						wid2 = 4*save_wid1;
					if (gqlinewidth() != wid2)
					{
						uw_gllinewidth(wid2, 1);
						gqlinewidth() = wid2;
					}
				}
				lwid_chg = 1;
			}
 			break;
		case UG_EDGEFLAGOP:
			ug_sedgeflag((*(UG_edgeflagop *)cmd).f);
			ledgecolor = (*(UG_edgeflagop *)cmd).f;
 			break;
		case UG_DFATSOP:
 			ug_dfats();
			break;
/*
... Color commands, only set if not hiliting 
*/
		case UG_LNCOLROP:
			if( !hilite ) ug_linecolor((*(UG_lncolrop *)cmd).color);
			break;
		case UG_MKCOLROP:
			if( !hilite ) ug_markcolor((*(UG_mkcolorop *)cmd).color);
	 		break;
		case UG_TXCOLROP:
			if( !hilite ) ug_textcolor((*(UG_txcolorop *)cmd).color); 
			break;
		case UG_FACOLROP:
			if( !hilite ) ug_fillcolor((*(UG_facolorop *)cmd).color);
	 		break;
/*
.....added shading color
*/
		case UG_SHDCOLROP:
			if( !hilite ) 
				ug_linecolor((*(UG_lncolrop *)cmd).color);
			break;
/*
.....added shading translucency
.....Yurong 2/1/99
*/
		case UG_LUCENCYOP:
			if (!hitrans)
			{
				ug_set_lucency((*(UG_lucencyop *)cmd).lucency);
				pri = ((*(UG_lucencyop *)cmd).lucency ) / 100.00;
			}
			break;
/*
.....added shading translucency stipple
.....Yurong 2/1/99
*/
/*temp yurong 		case UG_STIPPLEOP:
			ug_set_stipple((*(UG_stippleop *)cmd).stipple);
			break;
*/		case UG_MATERIALOP:
			ug_set_material((*(UG_materialop *)cmd).material);
			break;
/*
...These attribute functions don't have macros yet... 
*/
   	case UG_CHHGTOP:
			ug_charheight((*(UG_chhgtop *)cmd).height);
	 		break;

		default: 
/*			fprintf(ug_gksos.erfile,"uw_glviewsg. illegal opcode=%d\n",opcode);*/
			break;
		}		/* case */
	}			/* for each command in seg */
	gssegpri(n,pri);
/*
... Restore the colors
*/
	if ( hilite ) ug_set_bundle_colors (ln,mk,tx,fl);
/*
...Restore modeling matrix
*/
	if( segptr->xforms & (UG_SEGMODXF) )
	{
		ug_resstate (&state);
		uw_mcopy (uw_gl.modxf,modsave);
	}
/*
.....all those reset should be place into list
*/
	if (ltype_chg)
		uw_gllinetype(1, 0);
	if (lwid_chg)
		uw_gllinewidth(1.0, 0);
	if (mask_saved)
		uw_gldepth_mask(savemask);
	if (depth_saved)
		uw_gldepth_func(1);
	if (list_begin)
	{
		glEndList_d();
		uw_gllighting_reset();
/*
.....if the call list just compile and not execute
.....reset the current save line attribute value, because they 
.....have reset to other value when call uw_gllinetype and uw_gllinewidth
.....when draw using openGL list but not executed (just compile)
*/
		if (!list_execute)
		{
			if (ltype_chg)
				uw_setsave_linetype(1);
			if (lwid_chg)
				uw_setsave_linewid (1.0);
/*
.....we need to do the same for current color index and material too
*/
			uw_glrestore_material(savinx, savmat, savluc);
		}
	}
/*
.....Calculate world coordinate box
*/
	ug_segwcbox(n);
/*
.....Mark the area covered by the segment
.....as dirty if writing to the back buffer
*/
done:;
	uw_glset_dirty_flag(dirty_flag);
	if (uw_glgetbuffer() == UG_BACK_BUFFER) uw_glmark_dirty_seg(n);
/*
.....we have to reset material call because the glCallList may change the
.....material which we don't know which color/material is, so we have to
.....reset the color/material
*/
	UM_material_reset = 1;
	Screate_seg = UU_FALSE;
	return;
}

/********************************************************************* 
**  I_FUNCTION:  uw_glhilite(prms) -- segment highlight.
**  PARAMETERS   
**      INPUT:  int prms[4] -- prms[2] contains segment number to hilite.
**										 prms[3] contains either HIGHLIGHTED or NORMAL.
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void uw_glhilite(prms)	
int prms[];
{
	Gseghi h;							/* Hilite mode */
	int segno;							/* Segment to hilite */
	UG_segstli *segptr;				/* Pointer to segno's header */

	segno = prms[2];
	h     = (Gseghi)prms[3];

	uu_denter2(UU_GITRC,(us,"uw_4dseghilite(seg=%d, hilit=%d)",segno,h));
/*
... Redraw the segment in the hilite color
*/
	segptr = ug_segac(segno);

	if( h == UG_HIGHLIGHTED )
		segptr->segatts.ghilit = UG_HIGHLIGHTED;
	else
		segptr->segatts.ghilit = UG_NORMAL;

	uw_gldrawbuffer(UG_BACK_BUFFER); 
	uw_glviewsg(segno);        /* Redraw the segment */
   uu_dexit;
}

/*********************************************************************
**    FUNCTION : uw_glredrawsegrect(ws,segrect,segno)
** 
**       Redraws a segment in the specified rectangle.
**
**    PARAMETERS   
**       INPUT  : 
**          ws      = Workstation id.
**          segrect = Rectangle region of screen to redraw.
**          segno   = Segment not to redraw within rectangle.
**							-1: redraw every
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glredrawsegrect(ws,segrect,segno)
Gws ws;
Gnrect *segrect;
Gint segno;
{
/*
.....Redraw the requested rectangle
*/
	uw_glupdrec(segrect,UU_TRUE);
}
#endif

