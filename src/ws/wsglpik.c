#include "usysdef.h"
#ifdef UU_OPENGL

/*********************************************************************
**    NAME         :  wsglpik.c
**
**    CONTAINS:
**			uw_glpik2()
**			uw_glpksg()
**			uw_glget_piksurf()
**			uw_gldesel_vpik()
**			uw_glrestore_pickarea()
**			uw_gldraw_pickfm()
**			uw_glput_pikgeom
**			ncl_reset_pikgeom
**			uw_get_pick_prio
**			uw_glchk_zseg
**
**     MODULE NAME AND RELEASE LEVEL
**       wsglpik.c , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**       05/01/17 , 15:42:34
*********************************************************************/

#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#include <decw$include:keysym.h>
#include <decw$include:cursorfont.h>
#else
#if UU_COMP != UU_WIN2K 
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#endif
#endif

#include "usysdef.h"
#include "wsgl.h"
#include "udebug.h"
#include "mdrel.h"
#include "modef.h"
#include "msol.h"
#include "zsysdep.h"
#include "dasnog.h"
#include "uims.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ginqatti.h"
#include "gsegac.h"
#include "class.h"
#include "nclicons.h"
#include "dselect.h"
#include "wsglfun.h"
#include "gviw.h"
#include "view.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "ulist.h"
#include "dinput.h"
#include "mpocket.h"
#include "ncl.h"
#include "nccs.h"
extern UG_wdt glwdt;
extern int uw_glvrfcolor;

static GLuint picking_mx = 0;
static int xf =-1, Sxform = 1;
static UU_LOGICAL Sinit_mem = UU_FALSE;
static UU_LOGICAL Sinit_motion = UU_FALSE;
static UU_LOGICAL Spick_type,Spick_motion;
static UU_LIST Slist_motion;

/* single precision matrix copy */
/*
.....changed to GLdouble because the uw_gl.modxf define as GLdouble a[16]
.....in wsgl.h
.....Yurong 3/30/01
*/
typedef struct { GLdouble a[16]; } uw_tmp_mcopy_st;
/*typedef struct { float a[16]; } uw_tmp_mcopy_st; */
#define uw_glmcopy(y,x) *(uw_tmp_mcopy_st *)(y) = *(uw_tmp_mcopy_st *)(x)

extern UU_REAL NCL_pick_aper;
static Gfloat clip_pos[2];
static int old_pik_vseg = -1, old_pik_pid = -1;
static Gnrect pk_box={10000.,10000.,-10000.,-10000.};
static Gnrect pk2_box={10000.,10000.,-10000.,-10000.};
static void S_restore_pickarea();
static void Matrix2D();
static void Matrix3D();
extern int NCL_cut_erasing;
extern int NCL_nopick_cnt;
extern UU_KEY_ID NCL_verify_list[256];
extern int NCL_verify_segno[256];
extern int WS_update_tip;
#define HILITE_MARK 0
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3
#define VHILITE uw_glvrfcolor
extern int NCL_mark_method;
static int Pick_test = 0;

#define MXINC 2
extern UN_motseg *mlist_ptr;
extern UN_motseg *mlist_first_ptr;

static void S_pick_geo();
static void S_pick_motion();
static void S_display_motion();
static void S_hilite_motion();
static void S_flush_motion();
static void S_restore_pickarea2();
extern int NCL_funkey;
extern int NCL_pik_hierarchy;
int UW_current_pickid = -1;
/********************************************************************
**    I_FUNCTION     :  uw_get_pick_prio(rel_num, prio)
**		Get the pick Priority number from the Geometry relation number
**
**    PARAMETERS
**       INPUT  :
**                rel_num:              Geometry relation number
**       OUTPUT :
**                prio: Priority number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_get_pick_prio(rel_num, prio)
int rel_num, *prio;
{
	switch (rel_num)
	{
		case UM_POINT_REL:
		case NCL_POINT_REL:
			*prio = 0;
			break;
		case NCL_POINTVEC_REL:
		case NCL_VECTOR_REL:
			*prio = 1;
			break;
		case UM_LINE_REL:
		case NCL_LINE_REL:
		case UM_CIRCLE_REL:
		case NCL_CIRCLE_REL:
			*prio = 2;
			break;
		case UM_CONIC_REL:
		case UM_RBSPLCRV_REL:
		case UM_AGCRV_REL:
		case UM_UVCVONSF_REL:
		case NCL_CURVE_REL:
		case NCL_EVALCV_REL:
		case UM_POLYLINE_REL:
			*prio = 3;
			break;
		case UM_COMPCRV_REL:
			*prio = 4;
			break;
		case NCL_MATRIX_REL:
			*prio = 5;
			break;
		case NCL_TEXTVAR_REL:
		case UA_TEXT_REL:
			*prio = 6;
			break;
		case UB_SYMBOL_REL:
			*prio = 7;
			break;
		case UA_LINEAR_DIMS_REL:
			*prio = 8;
			break;
		case NCL_PLN_REL:
			*prio = 9;
			break;
		case UM_SOLID_REL:
		case NCL_SURF_REL:
		case NCL_EVALSF_REL:
		case UM_RBSPLSRF_REL:
		case NCL_MESHSURF_REL:
		case NCL_QUILTSURF_REL:
		case NCL_TRIMSF_REL:
		case NCL_REVSURF_REL:
			*prio = 10;
			break;
		case NCL_NETSF_REL:
			*prio = 11;
			break;
		default:
			*prio = 20;
			break;
	}
}

/********************************************************************
**    I_FUNCTION     :  uw_glchk_zseg(zval, segno, id, tol, flag)
**		Get the pick Priority number from the Geometry relation number
**		when compare the segment type, always exclude motion and assit segment
**
**    PARAMETERS
**       INPUT  :
**				zval: value of the z-level to be checked
**				segno: segment to be checked
**				tol: tolerance of the Z-level
**               flag: 1: reset the check Z-level and segment
**       OUTPUT :
**                none
**    RETURNS      : -1: the segment can't be added
**					0: the value is OK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glchk_zseg(zval, segno, id, tol, flag)
GLfloat zval, tol;
int segno, id, flag;
{
	static GLfloat orig_z = 100000;
	static int old_seg = -1;
	static int old_key = -1;
	struct UC_entitydatabag e;
	UM_int2 old_type, etype;
	long buffer[UG_UDATA_SIZE];
	
	if (NCL_pik_hierarchy==0)
		return 0;
	if (flag==1)
	{
		orig_z = 100000;
		old_seg = -1;
		old_key = -1;
		return -1;
	}
	if (zval-tol>orig_z)
		return -1;
/*
......if first time, return success and the segno and id are valid
*/
	if (old_seg == -1)
	{
/*
......if it is motion, or assist segment, don't save the old segment
*/
		if (ud_isassist_seg(segno)==0 && !Spick_motion)
		{
			old_seg = segno;
			orig_z = zval;
		}
		return 0;
	}
	if (zval+tol<orig_z)
	{
/*
......if it is motion, or assist segment, don't save the old segment
*/
		if (ud_isassist_seg(segno)==0 && !Spick_motion)
		{
			orig_z = zval;
			old_seg = segno;
		}
		return 0;
	}
	gsegrud(old_seg, buffer);
	e.key = uv_getkey(buffer);
	if (e.key==0)
	{
/*
......if it is motion, or assist segment, don't save the old segment
*/
		if (ud_isassist_seg(segno)==0 && !Spick_motion)
		{
			orig_z = zval;
			old_seg = segno;
		}
		return 0;
	}
	ur_retrieve_data_fixed(&e);
	uw_get_pick_prio(e.rel_num, &old_type);

	if (ud_isassist_seg(segno)==0 && !Spick_motion)
	{
		gsegrud(segno,buffer);
		e.key = uv_getkey(buffer);
		ur_retrieve_data_fixed(&e);
		uw_get_pick_prio(e.rel_num, &etype);
	
		if (etype<old_type)
		{
			orig_z = zval;
			old_seg = segno;
			return 0;
		}
	}
	return -1;
}
/********************************************************************
**    I_FUNCTION     :  uw_glpik2(path, depth, xy, aper)
**		Traverse all segments, sending all information necessary for
**       picking to the geometry engines. 
**
**    PARAMETERS
**       INPUT  :
**                int xy[2];              xy position of pick
**       OUTPUT :
**                int path[];             pick path
**                int *depth;             length of seg
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpik2(path, depth, xy, aper)
int path[];             /* pick path, currently pkid, pkid, ..., segno */
int *depth;             /* length of seg */
int xy[2];              /* xy position of pick */
int aper[2];            /* pick aperture */
{
#define LEN 500000
	GLfloat z_value;
	GLfloat token;
	int i, temp, status; 
	int num, count; 
	Gnpoint pt;
	int closept[2];
	Gfloat closendc[2];
	int tempdepth;
	int nvdepth;
	UU_LOGICAL lfl;
	UV_vport vport;
	int hit=0;
	UN_motseg **mpt;
	static GLfloat *buffer;
	static int *nvpath,*temppath;
	static int picking_start = 0;
/*
.....'tol' is used to assist in the picking
.....of trimmed surface boundaries, whose
.....pick ids are always higher than that of the
..... corresponding surface
.....Bobby - 2/5/03
*/
	Gfloat tol;
	UM_pkwin_type sarea;
/*
.....we have to do this function one at a time
.....since some global value is general and delete such as picking_mx
*/
	if (picking_start==1)
		return;
	picking_start = 1;

	if (NCL_pik_hierarchy==1)
		tol = 0.000005;
	else
		tol = 0.000000;
/*
.....Initialize routine
*/
	Spick_type = ud_getpick_type();
	Spick_motion = ud_motion_pick_type();
/*
......we need make sure the context is set for NCL view
*/
	sarea = um_get_screen_area();	
	um_set_screen_area(UM_NCL_WINDOW);
	uw_glset_context(UM_NCL_WINDOW,UU_FALSE);
/*
.....Allocate memory for picking
*/
	if (!Sinit_mem)
	{
		buffer = (GLfloat *)uu_malloc(sizeof(GLfloat)*LEN);
		temppath = (int *)uu_malloc(sizeof(int)*LEN);
		nvpath = (int *)uu_malloc(sizeof(int)*LEN);
		Sinit_mem = UU_TRUE;
	}

	uw_glchk_zseg((GLfloat)0.0, (int)(-1), (int)(-1), (GLfloat)0.0, (int)1);
	uw_gldevtondc(xy, &pt);
	Sxform = ug_locvp(&pt);
	if (Sxform>=1)
	{
		uv_getvpid(UV_act_screen[0].vports[Sxform-1],&vport);
		lfl = Pick_test == 0;
		if (!ud_verifypick_type(&vport,lfl))
		{
			*depth = 0;
			picking_start = 0;
			ug_setredrwflag(0);
			return;
		}
	}

	glFeedbackBuffer_d(LEN, GL_3D, buffer);
	(void) glRenderMode_d(GL_FEEDBACK);
	
	closendc[0] = NCL_pick_aper;
	closendc[1] = NCL_pick_aper;
	uw_glndctodev(closendc, closept);

	picking_mx = glGenLists_d(1);
	glNewList_d( picking_mx, GL_COMPILE );
		gluPickMatrix_d((GLdouble)(xy[0]), (GLdouble)(xy[1]),
			(GLdouble)closept[0], (GLdouble)closept[1], uw_gl_matrix[Sxform].vpt);
	glEndList_d();

	uw_gldevtondc(xy, clip_pos);
/*
.....Pick geometry
*/
	if (!Spick_motion)
		S_pick_geo(clip_pos);
/*
.....Pick motion
*/
	else if (Sxform >= 1)
		S_pick_motion(clip_pos);
	
	uw_glflush();

	num = glRenderMode_d(GL_RENDER);
	if (num<=0) 
	{
		*depth = 0;
		picking_start = 0;
		ug_setredrwflag(0);
		return;
	}
	
	glDeleteLists_d(picking_mx, 1);
	picking_mx = 0;
	count = num + 1;
	*depth = 0;
	nvdepth = 0;
	tempdepth = 0;
	z_value = 100000;
	nvpath[0] = -1;
	while(count>1)
	{
		count--;
		token = buffer[num-count];
		if (token==GL_PASS_THROUGH_TOKEN)
		{
			count--;
			temp = buffer[num-count];
			switch(temp)
			{
				case 2:
					tempdepth += 2;
					count--;
					count--;
					if (tempdepth>LEN)
					{
#if UU_COMP != UU_WIN2K 
						uw_mferror ("Internal Error -  Buffer too small!");
#else	
						uw_nterror ("Internal Error -  Buffer too small!");
#endif
						hit = 0;
						goto done;
					}
					temppath[tempdepth-2] = buffer[num-count];
					temppath[tempdepth-1] = -1;
					break;
				case 4:
					tempdepth -= 2;
					break;
				case 6:
					count--;
					count--;
					temppath[tempdepth-1] = buffer[num-count];
					break;
			}
		}
		else if ((token==GL_POINT_TOKEN)
			|| (token==GL_BITMAP_TOKEN))
		{
			count = count - 3;
			if (buffer[num-count]-tol<=z_value)
			{
/*
.....Check selection filter for valid types and verify mode.
.....Yurong
*/
/*
.....always pass in depth = 0, which mean always allow one item to be picked
*/
				*depth = 0;
				status = ud_glpick_filter(depth, path, temppath[tempdepth-2],
								temppath[tempdepth-1]); 
				if (status)
				{
/*
.....save z value
*/
					hit = 1;
					status = uw_glchk_zseg(buffer[num-count], temppath[tempdepth-2], temppath[tempdepth-1], tol, 0);
					if (status==0)
					{
						z_value = buffer[num-count];
						nvdepth = nvdepth + 2;
						if (nvdepth>LEN)
						{
#if UU_COMP != UU_WIN2K 
						uw_mferror ("Internal Error -  Buffer too small!");
#else	
							uw_nterror ("Internal Error -  Buffer too small!");
#endif
							hit = 0;
							goto done;
						}
						nvpath[nvdepth-1] = temppath[tempdepth-1];
						nvpath[nvdepth-2] = temppath[tempdepth-2];
						*depth = 2;
						path[0] = temppath[tempdepth-2];
						path[1] = temppath[tempdepth-1];
					}
					else
					{
						*depth = 0;
						status = ud_glpick_filter(depth, path, nvpath[nvdepth-2],
									nvpath[nvdepth-1]); 
					}
				}
/*
......reset the previous select because *depth = 0 & ud_glpick_filter 
......will reset path value
*/
				else if (hit)
				{
					*depth = 0;
					status = ud_glpick_filter(depth, path, nvpath[nvdepth-2],
									nvpath[nvdepth-1]); 
				}
			}
		}
		else if ((token==GL_LINE_TOKEN)
			||(token==GL_LINE_RESET_TOKEN))
		{
			for (i=0; i<2; i++)
			{
				count = count - 3;
				if (buffer[num-count]-tol<=z_value)
				{
/*
.....Check selection filter for valid types and verify mode.
.....Yurong
*/
/*
.....always pass in depth = 0, which mean always allow one item to be picked
*/
					*depth = 0;
					status = ud_glpick_filter(depth, path, temppath[tempdepth-2],
									temppath[tempdepth-1]); 
					if (status)
					{
/*
.....save z value
*/
						hit =1;
						status = uw_glchk_zseg(buffer[num-count], temppath[tempdepth-2], temppath[tempdepth-1], tol, 0);
						if (status==0)
						{
							z_value = buffer[num-count];
							nvdepth = nvdepth + 2;
							nvpath[nvdepth-1] = temppath[tempdepth-1];
							nvpath[nvdepth-2] = temppath[tempdepth-2];
							*depth = 2;
							path[0] = temppath[tempdepth-2];
							path[1] = temppath[tempdepth-1];
						}
						else
						{
							*depth = 0;
							status = ud_glpick_filter(depth, path, nvpath[nvdepth-2],
										nvpath[nvdepth-1]); 
						}
					}
/*
......reset the previous select because *depth = 0 & ud_glpick_filter 
......will reset path value
*/
					else if (hit)
					{
						*depth = 0;
						status = ud_glpick_filter(depth, path, nvpath[nvdepth-2],
									nvpath[nvdepth-1]); 
					}
				}
			}			
		}
		else if (token==GL_POLYGON_TOKEN)
		{
			count--;
			temp = buffer[num-count];
			for (i=0; i<temp; i++)
			{
				count = count - 3;
				if (buffer[num-count]-tol<=z_value)
				{
/*
.....Check selection filter for valid types and verify mode.
.....Yurong
*/
/*
.....always pass in depth = 0, which mean always allow one item to be picked
*/
					*depth = 0;
					status = ud_glpick_filter(depth, path, temppath[tempdepth-2],
									temppath[tempdepth-1]); 
					if (status)
					{
						hit = 1;
						status = uw_glchk_zseg(buffer[num-count], temppath[tempdepth-2], temppath[tempdepth-1], tol, 0);
						if (status==0)
						{
							z_value = buffer[num-count];
							nvdepth = nvdepth + 2;
							nvpath[nvdepth-1] = temppath[tempdepth-1];
							nvpath[nvdepth-2] = temppath[tempdepth-2];
							*depth = 2;
							path[0] = temppath[tempdepth-2];
							path[1] = temppath[tempdepth-1];
						}
						else
						{
							*depth = 0;
							status = ud_glpick_filter(depth, path, nvpath[nvdepth-2],
										nvpath[nvdepth-1]); 
						}
					}
/*
......reset the previous select because *depth = 0 & ud_glpick_filter 
......will reset path value
*/
					else if (hit)
					{
						*depth = 0;
						status = ud_glpick_filter(depth, path, nvpath[nvdepth-2],
									nvpath[nvdepth-1]); 
					}
				}
			}
		}
	}
done:;
	if (!hit) *depth = 0;
/*
.....Only allow for one item to be picked
.....Each item has a depth of 2.
*/
	if (*depth > 2) *depth = 2;
	if (Spick_motion && *depth > 0)
	{
		mpt = (UN_motseg **)UU_LIST_ARRAY(&Slist_motion);
		path[0] = (int)mpt[path[0]];
		if (*depth == 2) path[1] = (int)mpt[path[1]];
	}
	picking_start = 0;
	ug_setredrwflag(0);
}
	
/*********************************************************************
**    FUNCTION     :  uw_glpksg(xform, n)
**		Traverse segment n, sending all information necessary for
**       picking to the geometry engines.
**    PARAMETERS   
**       INPUT  : n 			-- segment to traverse/pick..
**       OUTPUT : none 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_glpksg (xform,n)                 
int xform;
int n;
{
	int i,k;					
	UG_plylna3op *cmd;
	int ncmd;	
	int icmd;
	int cmdlen;		
	int opcode;
	char *ug_lsielt();	
	UG_segstli *segptr;
	UG_LSI *listp;	
	GLdouble modsave[16];			/* Save modelling matrix here */
   UV_vport vport;
	Gfloat x0, y0, z0;
	Gnrect3 nrect;
	int wireframe_disp = 1;
	int hid_line = 0;
	int shaded_surf = 0;
	Gtxfont prec;
	UG_shadearea *hidcmd;
	Gwpoint3 *p;

	if( (segptr = ug_segac(n)) == NULL )
	{
		uu_dprint(UU_GITRC,(us,"seg %d does not exist", n));
		uu_dexit;
		return(0);
	}
	uw_glmcopy(modsave,uw_gl.modxf);

	listp = (UG_LSI *) segptr->seglist; 
	ncmd  = ug_lsinelt((*listp));
	prec = UG_STRING;
/* 
.....Passthrough segment start code and seg number 
*/
	glPassThrough_d((GLfloat)2);
	glPassThrough_d((GLfloat)n);

	for( icmd=0; icmd < ncmd; ++icmd ) 
	{
		cmd    = (UG_plylna3op *) ug_lsielt (listp,icmd);
		cmdlen = ug_lsilen(listp,icmd)/sizeof(int);
		opcode = cmd->elttype;

		if( xf != xform && opcode != UG_SNTRANOP ) continue;
		switch (opcode)
		{
		case UG_PAGEOP:
		case UG_PLYLNA3OP: 
			if (wireframe_disp) uw_glpolyln3_cmd (n,cmd);
			break;

		case UG_PLYLNA2OP: 
			uw_glpolyln2_cmd (n,cmd);
			break;

		case UG_PLYLNRASOP: 
			uw_glpolylnras_cmd (n,cmd);
			break;

		case UG_PLYMKA3OP: 
/*
.....Bitmap works now
*/
			uw_glmarker3op (n,cmd);
			break;

    	case UG_PLYMKA2OP: 
			uw_glmarker2op (n,cmd);
         	break;

	 	case UG_PLYMKRASOP: 
			uw_glmarkerrasop (n,cmd);
			break;
		case UG_FONTOP:
			prec = ((UG_fontop *)cmd)->p.prec;
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
		case UG_CHHGTOP:
			ug_charheight((*(UG_chhgtop *)cmd).height);
			break;
	  	case UG_TEXTOP: 
/*
.....Picking matrix not working for bitmap
.....see if cursor in text area
.....and if yes, draw polygon in text area 
.....currect depth.
.....to let feedback buffer remember
*/
			if (prec == UG_STROKE)
			{
				UG_dtext prms;
				Gwpoint3 cpt;
				Gwrect3 wrect;
				prms.pos.x = ((UG_textop *)cmd)->position.x;
				prms.pos.y = ((UG_textop *)cmd)->position.y;
				prms.pos.z = ((UG_textop *)cmd)->position.z;
				prms.slen = ((UG_textop *)cmd)->len;
				strcpy(prms.s,((UG_textop *)cmd)->string);
				ug_fchext(&prms,&cpt,&wrect);
				glBegin_d(GL_POLYGON);
					glVertex3f_d(wrect.llf.x,wrect.llf.y,wrect.llf.z);
					glVertex3f_d(wrect.llf.x, wrect.urb.y, wrect.llf.z);
					glVertex3f_d(wrect.urb.x, wrect.urb.y, wrect.urb.z);
					glVertex3f_d(wrect.urb.x, wrect.llf.y, wrect.urb.z);
					glVertex3f_d(wrect.llf.x, wrect.llf.y, wrect.llf.z);
				glEnd_d();
			}
			else
			{
				ug_txrect(&((UG_textop *)cmd)->position, 
						((UG_textop *)cmd)->string, &nrect);
				if ( (nrect.llf.x > clip_pos[0] + NCL_pick_aper)
					||(nrect.llf.y > clip_pos[1] + NCL_pick_aper)
					||(nrect.urb.x<clip_pos[0]-NCL_pick_aper)
					||(nrect.urb.y<clip_pos[1]-NCL_pick_aper));
				else
				{
					glBegin_d(GL_POLYGON);

						gndcw3(&x0, &y0, &z0, nrect.llf.x, nrect.llf.y, nrect.llf.z);
						glVertex3f_d(x0, y0, z0);

						gndcw3(&x0, &y0, &z0, nrect.llf.x, nrect.urb.y, nrect.llf.z);
						glVertex3f_d(x0, y0, z0);

						gndcw3(&x0, &y0, &z0, nrect.urb.x, nrect.urb.y, nrect.urb.z);
						glVertex3f_d(x0, y0, z0);

						gndcw3(&x0, &y0, &z0, nrect.urb.x, nrect.llf.y, nrect.urb.z);
						glVertex3f_d(x0, y0, z0);

						gndcw3(&x0, &y0, &z0, nrect.llf.x, nrect.llf.y, nrect.llf.z);
						glVertex3f_d(x0, y0, z0);

					glEnd_d();
				}
			}
			break;

	 	case UG_TEXTRASOP: 
/*temp
			ug_textras(&((UG_textrasop *)cmd)->position,
				((UG_textrasop *)cmd)->string, segptr->userdata[0]);
*/
			break;

	 	case UG_FLAREA3OP: 
			uw_glfill3_cmd (n,cmd);
			break;

	 	case UG_FLAREAOP: 
			uw_glfill_cmd (n,cmd);
			break;

	 	case UG_FLAREARASOP: 
			uw_glfillras_cmd (n,cmd);
			break;

	 	case UG_SHADEAREAOP:
			if (!hid_line)
			{
				uw_glshade_cmd (n, cmd, UU_FALSE);
				shaded_surf = 1;
			}
			else
			{
				hidcmd = (UG_shadearea *)cmd;
				p = hidcmd->pts;
				uw_glcolor (0);
				for (i = 0; i < hidcmd->len; )
				{
					glBegin_d(GL_POLYGON);
					for (k=0;k<3;k++, i++) glVertex3f (p[i].x,p[i].y, p[i].z);
					glEnd_d();
				}
			}
			break;

		case UG_CALLOP: 

			i = (*(UG_callop *)cmd).segno;  	/* call seg i */
			uw_glpksg (i);						/* recursive call */
			break;

		case UG_SNTRANOP:
		{
			uw_glpkload_matrix ( ((UG_sntranop *)cmd)->xform, UU_TRUE,
				uw_gl.modxf);
			xf = (*(UG_sntranop *)cmd).xform;
			glMatrixMode_d(GL_PROJECTION);
			glLoadIdentity_d();
			glCallList_d(picking_mx);
			glOrtho_d(uw_gl_matrix[xform].ortho[0],
				uw_gl_matrix[xform].ortho[1],
				uw_gl_matrix[xform].ortho[2],
				uw_gl_matrix[xform].ortho[3],
				uw_gl_matrix[xform].ortho[4],
				uw_gl_matrix[xform].ortho[5]);
			glMatrixMode_d(GL_MODELVIEW);

			ug_gksstli.curvwindex = xf;
			uv_getvpid(UV_act_screen[0].vports[xf-1],&vport);
			if (vport.wireframe)
				wireframe_disp = 1;
			else
				wireframe_disp = 0;
			if (vport.disp_mode==3)
				hid_line = 1;
			else
				hid_line = 0;
			break;
		}
   	case UG_PICKIDOP:
		{
			int pkid;
			pkid = (*(UG_pickidop *)cmd).pid;
			glPassThrough_d((GLfloat)6);
			glPassThrough_d((GLfloat)pkid); 
			ug_spickid(pkid);
 			break;
		}
   	case UG_SYMBOLOP:
			ug_marktype((*(UG_symbolop *)cmd).type);
 			break;
	case UG_LSTYLOP:
	case UG_LWIDOP:
	case UG_EDGEFLAGOP:
   	case UG_DFATSOP:
	case UG_LNCOLROP: break;
	case UG_MKCOLROP: break;
	case UG_TXCOLROP: break;
	case UG_FACOLROP: break;
	case UG_LUCENCYOP: break;
	case UG_SHDCOLROP: break;
	case UG_MATERIALOP: break;
		default: 
/*			fprintf(ug_gksos.erfile,"uw_glpksg. illegal opcode=%d\n",opcode);*/
			break;
	}			/* case */

	}			/* for each command in seg */
/*
...Restore modeling matrix
*/
	uw_glmcopy (uw_gl.modxf,modsave);
	glPassThrough_d((GLfloat)4);
	return (0);
}

/********************************************************************
**    I_FUNCTION     :  uw_glget_piksurf(x, y, surftext)
**		find the surface in point (x,y) and draw the uw_glget_piksurfof the
**		surface in highlight then return the surface label 
**
**    PARAMETERS
**       INPUT  :
**                int x,y;              position of pick
**       OUTPUT :
**                surftext;             pick surface label
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glget_piksurf(x, y, surftext, flag)
int x, y,flag;
char *surftext;
{
	int path[50];       
	int depth;          
	int xy[2];   
	int aper[2];
	int segno;			
	int nline,type,nc,indx;
	UU_REAL dval;
	char sbuf[256],*p;
	UG_segstli *segptr;	
	int save_highlight;
	long buffer[UG_UDATA_SIZE];
	int ll[2], stat,i;
	UU_REAL nur[2];
	Gbuffer dw_buf,uw_glgetbuffer();
	UN_motseg *mptr;
	int path0, path1, level;
	UD_DPICKS pikseg;
	Gnpoint nxy;
	int pickid;
	static int save_x = -1, save_y = -1;

	if ((NCL_mark_method != DYNAMIC_MARK)&&(UD_pickmode==1))
		return -1;

	dw_buf = uw_glgetbuffer();
	xy[0] = x;
	xy[1] = y;
	aper[0] = aper[1] = 16;      
	uw_glpushx();
	Pick_test = 1;
	uw_glpik2(path, &depth, xy, aper);
	uw_glpopx();   
	uw_glreset_xform();
	if (depth==0)
	{
		if ((old_pik_vseg!=-1) && (pk_box.ll.x<=pk_box.ur.x))
		{
			S_restore_pickarea();
			old_pik_vseg = -1;
		}
/*
.....reset verify list
*/
		for (i = 0; i < NCL_nopick_cnt; i++)
		{
			if (!Spick_motion)
				gssegdet(NCL_verify_segno[i], UG_DETECTABLE);
			NCL_verify_list[i] = 0;
			NCL_verify_segno[i] = 0;
		}
		NCL_nopick_cnt = 0;
		old_pik_vseg = -1;
/*
.....reset and pick again
*/
		uw_glpushx();
		uw_glpik2(path, &depth, xy, aper);		
		uw_glpopx();   
		uw_glreset_xform();
/*
.....after reset, if it still no pick, return 
.....otherwise, pick the top one again
*/
		if (depth==0)
		{
			Pick_test = 0;
			uw_gldrawbuffer(dw_buf); 
			return -1;
		}
	}

	segno = path[0];
	if (Spick_motion)
	{
		segno = path[0];
		if (Spick_type == UD_PICK_MOTSEG) segno = path[1];
		mptr = (UN_motseg *)segno;
	}
/*
.....remove old segment highlight
*/
	uw_gldrawbuffer(UG_FRONT_BUFFER);
	level = 1;
	if ((ud_isassist_seg(segno)==0 && !Spick_motion))
	{
/*
.....check if this segment is surface with edge
.....or curve with 2 level of picking
*/
		pickid = path[1];
		level = ncl_get_seglabel(segno, pickid, surftext);
	}

	if (old_pik_vseg==segno)
	{
		if ((old_pik_pid!=-1)&&(old_pik_pid!=path[1]))
		{
			S_restore_pickarea2();
		}
		goto draw;
	}
	if ((old_pik_vseg!=-1) && (pk_box.ll.x<=pk_box.ur.x))
	{
		S_restore_pickarea();
/*
....reset the NCL_verify_list[i] segment detectable
....then reset NCL_verify_list IF the point's actually moved.
*/
		if ((!((x==save_x)&&(y==save_y)))&&(WS_update_tip==0))
		{
			for (i = 0; i < NCL_nopick_cnt; i++)
			{
				if (!Spick_motion)
					gssegdet(NCL_verify_segno[i], UG_DETECTABLE);
				NCL_verify_list[i] = 0;
				NCL_verify_segno[i] = 0;
			}
			NCL_nopick_cnt = 0;
		}
		old_pik_vseg = -1;
	}
/*
... Redraw the segment in the hilite color
*/
draw:;
	save_x = x; save_y = y;
	if (!Spick_motion)
	{
		segptr = ug_segac(segno);
		if (segptr==NULL)
		{
			stat = -1;
			goto done;
		}
		if (segptr->segatts.gdtect==UG_UNDETECTABLE)
		{
			stat = -1;
			goto done;
		}
		save_highlight = segptr->segatts.ghilit;
		segptr->segatts.ghilit = UG_PICKLIGHTED;
/*
.....calculate the high light box
*/
		if (segptr->wcboxok==1)
		{
			ug_segndcbox(segno,&pk_box);
/*
.....consider the bitmap marker position is in the center of the marker
.....so extra 10 pixel
*/
			ll[0] = 12; ll[1] = 12;
			uw_gldevtondc(ll,nur);
			pk_box.ll.x = pk_box.ll.x - nur[0]; 
			pk_box.ur.x = pk_box.ur.x + nur[0];
			pk_box.ll.y = pk_box.ll.y - nur[1]; 
			pk_box.ur.y = pk_box.ur.y + nur[1];
			if (level==2)
			{
				ug_segndcboxl2(segno, path[1], &pk2_box);
				pk2_box.ll.x = pk2_box.ll.x - nur[0]; 
				pk2_box.ur.x = pk2_box.ur.x + nur[0];
				pk2_box.ll.y = pk2_box.ll.y - nur[1]; 
				pk2_box.ur.y = pk2_box.ur.y + nur[1];
				old_pik_pid = path[1];
			}
			else
			{
				old_pik_pid = old_pik_pid;
			}
		}
		else
		{
			pk_box.ll.x = 0;
			pk_box.ll.y = 0;
			pk_box.ur.x = uw_gl.xndc;
			pk_box.ur.y = uw_gl.yndc;
			if (level==2)
			{
				old_pik_pid = path[1];
				pk2_box.ll.x = 0;
				pk2_box.ll.y = 0;
				pk2_box.ur.x = uw_gl.xndc;
				pk2_box.ur.y = uw_gl.yndc;
			}
		}
		UW_current_pickid = path[1];
		uw_glviewsg(segno);
		UW_current_pickid = -1;
	}
/*
.....Highlight motion
*/
	else 
	{ 
		S_hilite_motion(mptr);
	}
	stat = 0;
done:;
	uw_gldrawbuffer(dw_buf); 
	if (!Spick_motion) segptr->segatts.ghilit = save_highlight;
	old_pik_vseg = segno;
	uw_glflush();
	
	uw_glreset_xform();
/*
.....Return name of geometry
*/
	segno = path[0];
	pickid = path[1];
	if ((ud_isassist_seg(segno)==0 && !Spick_motion) && (segptr!=NULL) && (segptr->segatts.gdtect!=UG_UNDETECTABLE))
	{
		gsegrud(segno, buffer);
		path1 = uv_getkey(buffer);
		path0 = path[0];
		if (((NCL_funkey)||(UD_pickmode==0))&&(flag==1))
			ncl_verify_pick3(path0, path1);
/*
......if it is in choice mode, add this pick into pick segment stock
*/
		if ((UD_pickmode==0)&&(flag==1))
		{
			uw_gldevtondc(xy, &nxy);
			pikseg.choice = 1;
			pikseg.transform = ug_locvp(&nxy);
			pikseg.position[0] = nxy.x;
			pikseg.position[1] = nxy.y;
			pikseg.depth = depth;
			if (depth>0)
				pikseg.pickpath = (int*)uu_malloc (depth*sizeof(int));
			for (i=0; i<depth;i++)
			{
				pikseg.pickpath[i] = path[i];
			}	
			if (depth>0)
				ud_addpik_seg(pikseg);
		}
	}
/*
.....Return command that generated motion
*/
	else if (Spick_motion && mptr != UU_NULL)
	{
		indx = 0;
		ncl_motisn_get(mptr->isn,&nline,&type,&dval,sbuf,&nc,&indx);
		sbuf[71] = '\0';
		p = sbuf;
		while ((*p == ' ' || *p == '\t') && *p != '\0') p++;
		sprintf(surftext,"%d: %s",nline,p);
		path0 = path[0];
		path1 = path[1];
		if (((NCL_funkey)||(UD_pickmode==0))&&(flag==1))
			ncl_verify_pick3(path0, path1);
	}
	else
		surftext[0] = '\0';
	Pick_test = 0;
	
	return stat;
}

/********************************************************************
**    I_FUNCTION     :  uw_gldesel_vpik()
**		remove picking segment wireframe highlight
**
**    PARAMETERS
**       INPUT  :
**                none
**       OUTPUT :
**                none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldesel_vpik()
{
	int i;
/*
.....remove old segment highlight
*/
	if (old_pik_vseg!=-1)
	{
		gssegdet(old_pik_vseg, UG_DETECTABLE);
		S_restore_pickarea();
		for (i = 0; i < NCL_nopick_cnt; i++)
		{
			gssegdet(NCL_verify_segno[i], UG_DETECTABLE);
			NCL_verify_list[i] = 0;
			NCL_verify_segno[i] = 0;
		}
		NCL_nopick_cnt = 0;
		old_pik_vseg = -1;
	}
}

/********************************************************************
**    I_FUNCTION     :  uw_glrestore_pickarea()
**		Restores the picking highlight area and set highlight picking segment
**			UG_DETECTABLE
**		for DYNAMIC_MARK picking only
**    PARAMETERS
**       INPUT  :
**                none
**       OUTPUT :
**                none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glrestore_pickarea()
{
	Gbuffer dw_buf,uw_glgetbuffer();
	if (NCL_mark_method!=DYNAMIC_MARK)
		return;
	if (old_pik_vseg!=-1)
	{
		dw_buf = uw_glgetbuffer();
		gssegdet(old_pik_vseg, UG_DETECTABLE);
		S_restore_pickarea();
		old_pik_vseg = -1;
		if (dw_buf != UG_BOTH_BUFFER)
		{
			uw_gldrawbuffer(dw_buf); 
		}		
	}
}
/*********************************************************************
**    I_FUNCTION     : S_restore_pickarea()
**         Restores the picking highlight area 
**         Depending on the setting, the area is either copied from the
**         back buffer or the entire back buffer is copied to the front
**         buffer.
**
**    PARAMETERS
**       INPUT  :
**          box    = Area to restore.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_restore_pickarea()
{
	GLint b;
/*
.....Restore the highlighted pick area
*/
	if (pk_box.ll.x <= pk_box.ur.x)
	{
		glGetIntegerv_d(GL_BLEND,&b);
		uw_glmark_dirty_rect(&pk_box);
		ud_updatews(UG_SUPPRESS);
		glEnable_d(GL_DEPTH_TEST);
		glDepthMask_d(GL_TRUE);
		if (b) glEnable_d(GL_BLEND);
	}
	pk_box.ll.x = pk_box.ll.y = 10000.;
	pk_box.ur.x = pk_box.ur.y = -10000.;
	old_pik_vseg = -1;
	old_pik_pid = -1;
}

/*********************************************************************
**    I_FUNCTION     : S_restore_pickarea2()
**         Restores the picking highlight area (for level 2 picking)
**         Depending on the setting, the area is either copied from the
**         back buffer or the entire back buffer is copied to the front
**         buffer.
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
static void S_restore_pickarea2()
{
	GLint b;
/*
.....Restore the highlighted pick area
*/
	if (pk2_box.ll.x <= pk2_box.ur.x)
	{
		glGetIntegerv_d(GL_BLEND,&b);
		uw_glmark_dirty_rect(&pk2_box);
		ud_updatews(UG_SUPPRESS);
		glEnable_d(GL_DEPTH_TEST);
		glDepthMask_d(GL_TRUE);
		if (b) glEnable_d(GL_BLEND);
	}
	pk2_box.ll.x = pk2_box.ll.y = 10000.;
	pk2_box.ur.x = pk2_box.ur.y = -10000.;
	old_pik_pid = -1;
}

/******************************************************************* 
**   E_FUNCTION : Matrix2D()
**          This function set 2D matrix
**
**   PARAMETERS
**       INPUT  :  none
**
**       OUTPUT :  none
**   RETURNS:    none
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void Matrix2D(flag)
int flag;
{
	GLdouble orth[6];

	glPushAttrib_d(GL_VIEWPORT_BIT );
	glMatrixMode_d(GL_PROJECTION);
	if (flag) glPushMatrix_d();	
	glLoadIdentity_d();
	orth[0] = 0.; orth[1] = glwdt.dspsize.raster.x;
	orth[2] = 0.; orth[3] = glwdt.dspsize.raster.y;
	orth[4] = -1000.; orth[5] = 1000.;
	glOrtho_d(orth[0],orth[1],orth[2],orth[3],orth[4],orth[5]);
	glViewport_d(0,0,glwdt.dspsize.raster.x,glwdt.dspsize.raster.y);
	glMatrixMode_d(GL_MODELVIEW);
	if (flag) glPushMatrix_d();
	glLoadIdentity_d();
}

/******************************************************************* 
**   E_FUNCTION : Matrix3D()
**          This function set 3D matrix
**
**   PARAMETERS
**       INPUT  :  none
**
**       OUTPUT :  none
**   RETURNS:    none
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void Matrix3D()
{
	glMatrixMode_d(GL_PROJECTION);
	glPopMatrix_d();
	glMatrixMode_d(GL_MODELVIEW);
	glPopMatrix_d();
	glPopAttrib_d();
}

/******************************************************************* 
**   E_FUNCTION : uw_gldraw_pickfm()
**          Redraw the picking segment wireframe in highlight color
**			for DYNAMIC_MARK picking only
**
**   PARAMETERS
**       INPUT  :  none
**
**       OUTPUT :  none
**   RETURNS:    none
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
void uw_gldraw_pickfm()
{
	UG_segstli *segptr;	
	int save_highlight;
	Gbuffer dw_buf,uw_glgetbuffer();
	UN_motseg *mptr;

	if (NCL_mark_method != DYNAMIC_MARK)
		return;
/*
.....Draw using the front buffer
*/
	dw_buf = uw_glgetbuffer();
	uw_gldrawbuffer(UG_FRONT_BUFFER); 
/*
.....Redraw highlighted geometry
*/
	if (!Spick_motion)
	{
		if (old_pik_vseg>0)
		{
			segptr = ug_segac(old_pik_vseg);
/*
........Redraw the segment in the hilite color
*/
			if (segptr != NULL)
			{
				save_highlight = segptr->segatts.ghilit;
				segptr->segatts.ghilit = UG_PICKLIGHTED;
				uw_glviewsg(old_pik_vseg);
				segptr->segatts.ghilit = save_highlight;
			}
		}
	}
/*
.....Redraw highlighted motion
*/
	else
	{
		if (old_pik_vseg > 0)
		{
			mptr = (UN_motseg *)old_pik_vseg;
			S_hilite_motion(mptr);
		}
	}
/*
.....Flush the graphics
*/
	uw_gldrawbuffer(dw_buf); 
	uw_glflush();
done:;
}

/******************************************************************* 
**   I_FUNCTION : S_pick_geo(clip_pos)
**       Loops through the geometry segments and adds them to the
**			picking buffer.
**
**   PARAMETERS
**       INPUT  :
**          clip_pos    NDC coordinage of pick location.
**       OUTPUT :  none
**   RETURNS:    none
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
static void S_pick_geo(clip_pos)
Gfloat clip_pos[2];
{
	int draw;
	UU_KEY_ID viewkey;
	UG_segstli *p;
	UV_vport vport;
	Gnrect rect;
	uv_segbuff(sbuffer);

	ug_seginitscan();
	while( (p=ug_segscan()) != NULL ) 
	{
		if (Spick_type==UD_PICK_ASSIST)
		{
			if (ud_isassist_seg(p->segid)==0)
				continue;
		}
/*
.....limit draw only part inside pick area.
.....Yurong 2/9/99
*/
		draw = 0;
		if ((p->segatts.gvis   == UG_VISIBLE) &&
			(p->segatts.gdtect == UG_DETECTABLE) )
		{	
			if (p->wcboxok==1)
			{
				ug_segndcbox(p->segid,&rect);
/*
...boxes don't overlap or outside the box, do not draw
*/
				if ( (rect.ll.x > clip_pos[0] + NCL_pick_aper)
					||(rect.ll.y > clip_pos[1] + NCL_pick_aper)
					||(rect.ur.x<clip_pos[0]-NCL_pick_aper)
					||(rect.ur.y<clip_pos[1]-NCL_pick_aper));
/*
... boxes overlap
*/
				else
				{
					draw=1;
				}
			}
/*
... box not defined
*/
			else
			{
				draw=1;
			}
		}
		if( draw)
		{
			if (Spick_type==UD_PICK_ASSIST)
			{
				if (ud_isassist_seg(p->segid))
					draw = 1;
				else
					draw = 0;
			}
			else
			{
				if (ud_isassist_seg(p->segid))
					draw = 0;
				else
					draw = 1;
			}
			if ((Sxform>=1) && (draw))
			{
				gsegrud(p->segid, sbuffer);
				viewkey = uv_getviewkey(sbuffer);
				uv_getvpid(UV_act_screen[0].vports[Sxform-1],&vport);
				if (((viewkey!=0) && (vport.cur_view!=viewkey)||(viewkey==0)) && (Spick_type!=UD_PICK_ASSIST))
					draw = 0;
			}
			if(draw)
			{
				uw_glpksg(Sxform, p->segid);
			}
		}
	}
}

/******************************************************************* 
**   I_FUNCTION : S_pick_motion(clip_pos,xform)
**       Loops through the displayed motion and adds it to the
**			picking buffer.
**
**   PARAMETERS
**       INPUT  :
**          clip_pos    NDC coordinage of pick location.
**       OUTPUT :  none
**   RETURNS:    none
**   SIDE EFFECTS: none.
**   WARNINGS:
*********************************************************************/
static void S_pick_motion(clip_pos)
Gfloat clip_pos[2];
{
	Gnrect nrect;
/*
.....Define picking area
*/
	nrect.ll.x = clip_pos[0] - NCL_pick_aper;
	nrect.ll.y = clip_pos[1] - NCL_pick_aper;
	nrect.ur.x = clip_pos[0] + NCL_pick_aper;
	nrect.ur.y = clip_pos[1] + NCL_pick_aper;
/*
.....Display motion in picking buffer
*/
	S_display_motion(&nrect);
}

/*********************************************************************
**    E_FUNCTION     : S_display_motion(vp,cliprect)
**			Outputs the motion display to the pick buffer.
**    PARAMETERS   
**       INPUT  : 
**			   cliprect = NDC rectangle to display motion within.
**       OUTPUT : 
**			   none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_display_motion(cliprect)
Gnrect *cliprect;

{
	int inc,vfl,i,j,last_isn,nopick_isn,pkfl[2];
	UU_LOGICAL dirty;
	UU_REAL um_dist_2d();
	UN_motseg *mpt,*last_mpt;
	UV_vport vport;
	UN_motseg_view motview;
	Gwpoint3 gpt[2];
	Gnrect localrect;
/*
.....Initialize routine
*/
	dirty = uw_glget_dirty_flag();
	uw_glset_dirty_flag(UU_FALSE);
/*
.....Allocate memory for motion display pointers
*/
	if (!Sinit_motion)
	{
		uu_list_init(&Slist_motion,sizeof(UN_motseg *),1000,500);
		Sinit_motion = UU_TRUE;
	}
	else
		UU_LIST_EMPTY(&Slist_motion);
/*
.....No motion displayed
*/
	if (mlist_first_ptr == (UN_motseg *)-1) goto done;
/*
.....Expand clipping rectangle by a bit
*/
	if (um_dist_2d(&cliprect->ll,&cliprect->ur) == 0. ||
		cliprect->ll.x > cliprect->ur.x) goto done;
	localrect.ll.x = cliprect->ll.x; /* - .005; */
	localrect.ll.y = cliprect->ll.y; /* - .005; */
	localrect.ur.x = cliprect->ur.x; /* + .005; */
	localrect.ur.y = cliprect->ur.y; /* + .005; */
/*
.....Set this view port's transormation matrix
*/
	ug_sntran(Sxform);
	uv_getvpid(UV_act_screen[0].vports[Sxform-1],&vport);
/*
........Loop through motion list array
*/
	mpt = mlist_first_ptr;
/*
.....Loop through motion display
*/

	inc = 0;
	last_isn = -1;
	nopick_isn = -1;
	pkfl[0] = pkfl[1] = -1;
	do
	{
		mpt = (UN_motseg *)uu_lsnext(mpt);
		if (mpt == 0) goto done;
		last_mpt = mpt;
/*
........Only display motion which is
........visible in this view
*/
		vfl = 0;
		ncl_motview_get(&motview,mpt->view);
		for (j=0;j<motview.nview;j++)
		{
			if (vport.cur_view == motview.view[j]) vfl = 1;
		}
		if (vfl == 1)
		{
/*
........Only display motion that is
........not on Verify stack
*/
			for (i=0;i<NCL_nopick_cnt;i++)
				if (mpt == (UN_motseg *)NCL_verify_segno[i]) nopick_isn = mpt->isn;
			if (mpt->isn == nopick_isn) inc = 0;
/*
........Save motion point
*/
			gpt[inc].x = mpt->tend.x;
			gpt[inc].y = mpt->tend.y;
			gpt[inc].z = mpt->tend.z;
			inc++;
/*
...........Output current motion buffer
...........if at buffer limit
*/
			if (inc == 2)
			{
				S_flush_motion(inc,gpt,last_mpt,&last_isn,&localrect,pkfl,1);
				gpt[0].x = gpt[inc-1].x;
				gpt[0].y = gpt[inc-1].y;
				gpt[0].z = gpt[inc-1].z;
				inc = 1;
			}
		}
	} while (mpt != mlist_ptr && mpt != UN_step_ptr);
/*
.....Display last points in buffer
*/
	if (inc > 1)
		S_flush_motion(inc,gpt,last_mpt,&last_isn,&localrect,pkfl,1);
	if (last_isn != -1) glPassThrough_d((GLfloat)4);
/*
.....End of routine
*/
done:;
	uw_glset_dirty_flag(dirty);
	return;
}

/*********************************************************************
**    I_FUNCTION     : S_hilite_motion(mptr)
**			Highlights the currently selected motion display.
**    PARAMETERS   
**       INPUT  : 
**			   mptr     = Start of motion to display.  The motion up till
**                     the ISN changes will be displayed.
**       OUTPUT : 
**			   none.
**    RETURNS      : none
**    SIDE EFFECTS : Updates the highlight rectangle.
**    WARNINGS     : none
*********************************************************************/
static void S_hilite_motion(mptr)
UN_motseg *mptr;
{
	int inc,last_isn,savemask,ln,mk,tx,fl,pkfl[2];
	UU_LOGICAL dirty;
	UN_motseg *mpt;
	Gwpoint3 gpt[2];
	Gwrect3 wcbox;
/*
.....Initialize routine
*/
	pkfl[0] = pkfl[1] = -1;
	ug_sntran(Sxform);
/*
.....Save colors and depth mask
*/
	ug_get_bundle_colors(&ln,&mk,&tx,&fl);
	savemask = uw_glget_depthmask();
	dirty = uw_glget_dirty_flag();
/*
.....Set hiliting color and depth mask
*/
	ug_set_bundle_colors(VHILITE,VHILITE,VHILITE,VHILITE);
	uw_gldepth_func(0);
	uw_gldepth_mask(0);
	uw_glset_dirty_flag(UU_FALSE);
/*
.....Set hilite color
*/
	mpt = (UN_motseg *)uu_lsprev(mptr);
	if (mpt == UU_NULL) mpt = mptr;
	ug_linecolor(uw_glvrfcolor);
	uw_gllinetype(1);
	ug_linewidth(2.0);
/*
.....Initialize bounding box of hilited motion
*/
	wcbox.llf.x = wcbox.llf.y = wcbox.llf.z = 10000.;
	wcbox.urb.x = wcbox.urb.y = wcbox.urb.z = -10000.;
/*
.....Loop through motion display
*/
	inc = 0;
	last_isn = mptr->isn;
	do
	{
/*
........Save motion point
*/
		gpt[inc].x = mpt->tend.x;
		gpt[inc].y = mpt->tend.y;
		gpt[inc].z = mpt->tend.z;
/*
........Update WC box
*/
		if (gpt[inc].x < wcbox.llf.x) wcbox.llf.x = gpt[inc].x;
		if (gpt[inc].y < wcbox.llf.y) wcbox.llf.y = gpt[inc].y;
		if (gpt[inc].z < wcbox.llf.z) wcbox.llf.z = gpt[inc].z;
		if (gpt[inc].x > wcbox.urb.x) wcbox.urb.x = gpt[inc].x;
		if (gpt[inc].y > wcbox.urb.y) wcbox.urb.y = gpt[inc].y;
		if (gpt[inc].z > wcbox.urb.z) wcbox.urb.z = gpt[inc].z;
		inc++;
/*
...........Output current motion buffer
...........if at buffer limit or if
...........cutter is being displayed
*/
		if (inc == 2)
		{
			S_flush_motion(inc,gpt,mpt,&last_isn,UU_NULL,pkfl,2);
			gpt[0].x = gpt[inc-1].x;
			gpt[0].y = gpt[inc-1].y;
			gpt[0].z = gpt[inc-1].z;
			inc = 1;
			if (Spick_type == UD_PICK_MOTSEG) break;
		}
		if (mpt == mlist_ptr) break;
		mpt = (UN_motseg *)uu_lsnext(mpt);
	} while (mpt->isn == last_isn);
/*
.....Display last points in buffer
*/
	if (inc > 1)
		S_flush_motion(inc,gpt,mpt,&last_isn,UU_NULL,pkfl,2);
/*
.....End of routine
*/
done:;
/*
........Calculate NDC box
*/
	ug_ndcbox(&wcbox,&pk_box,Sxform);
/*
........Reset hiliting color and depth mask
.....Changed ug_set_bundle_colors() call so the input is passed by
.....value and not by reference - Andrew 9/26/12
*/
	ug_set_bundle_colors(ln,mk,tx,fl);
	uw_gldepth_func(1);
	uw_gldepth_mask(savemask);
	uw_glset_dirty_flag(dirty);
	return;
}

/*********************************************************************
**    I_FUNCTION     : S_flush_motion(np,gpt,last_mpt,cliprect,flag)
**			Outputs the displayed motion lines if they are within the
**			clipping rectangle or all motion if flag = 1.
**    PARAMETERS   
**       INPUT  : 
**				np       = Number of points in output array.
**				gpt      = Array of points to output.
**				mpt      = Motion block pointer to store as pick id.
**				isn      = Last ISN block processed.  The main pick id will
** 	                 change whenever the ISN block changes.  The pick id
** 	                 of each individual move will be stored in the Level-2
** 	                 pick id.
**				cliprect = NDC rectangle to display motion in.
**          pkfl     = Flags for outputting pick ids.  Should be set to
**                     -1,-1 on the first call and then will be modified
**                     by this routine.
**          flag     = 1 = Output motion for picking, 2 = for hiliting.
**       OUTPUT : 
**				isn      = Updated ISN block when it changes.  Only returned
**                     when 'flag' = 1.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_flush_motion(np,gpt,mpt,isn,cliprect,pkfl,flag)
int np;
Gwpoint3 gpt[];
UN_motseg *mpt;
int *isn;
Gnrect *cliprect;
int pkfl[];
int flag;
{
	int i,isp,inc,mxc;
	UU_LOGICAL inrect;
	UU_REAL lseg[2][2],z;
	Gwpoint3 opt[MXINC+1];
/*
.....Only draw motion segments that
.....are within the rectangle
*/
	isp = 0;
	inc = 0;
	for (i=0;i<np;i++)
	{
/*
.....Mark start of motion
*/
		if (mpt->isn != *isn && flag == 1)
		{
/*
........Output end of segment
*/
			if (*isn != -1) pkfl[1] = 1;
/*
........Output Level-1 Pick ID
*/
			mxc = UU_LIST_LENGTH(&Slist_motion);
			uu_list_push(&Slist_motion,&mpt);
			pkfl[0] = mxc;
			*isn = mpt->isn;
		}
		gwndc3(&lseg[isp][0],&lseg[isp][1],&z,gpt[i].x,gpt[i].y,gpt[i].z);
		if (isp == 0) isp = 1;
/*
........See if motion segment
........is within rectangle
*/
		else
		{
			if (flag == 1) inrect = ug_lineinrect2(lseg[0],lseg[1],cliprect);
			else inrect = UU_TRUE;
			if (inrect)
			{
				if (inc == 0)
				{
					um_vctovc(&gpt[i-1],&opt[inc]); inc++;
				}
				um_vctovc(&gpt[i],&opt[inc]); inc++;
				if (inc >= MXINC)
				{
/*
...........Output Level-2 Pick ID
*/
					if (flag == 1)
					{
						if (pkfl[1] != -1) glPassThrough_d((GLfloat)4);
						if (pkfl[0] != -1)
						{
							glPassThrough_d((GLfloat)2);
							glPassThrough_d((GLfloat)pkfl[0]);
						}
						mxc = UU_LIST_LENGTH(&Slist_motion);
						uu_list_push(&Slist_motion,&mpt);
						glPassThrough_d((GLfloat)6);
						glPassThrough_d((GLfloat)mxc);
						pkfl[0] = pkfl[1] = -1;
					}
/*
...........Output motion
*/
					gpolyline3(inc,opt);
					inc = 0;
				}
			}
/*
........Motion segment is outside of rectangle
........Output any buffered motion
*/
			else
			{
				if (inc > 1) gpolyline3(inc,opt);
				inc = 0;
			}
			lseg[0][0] = lseg[1][0];
			lseg[0][1] = lseg[1][1];
		}
	}
/*
........Output any buffered motion
*/
	if (inc > 1) gpolyline3(inc,opt);
}
uw_glput_pikgeom2(type, event)
int type;
UD_DEVENT *event;
{
	UD_DPICKS pikseg;
	int i, xy[2];
	Gnpoint nxy;
	if ((type!=1)&&(type!=2))
		return -1;
	if (type==1)
	{
		nxy.x = (*event).indata.locdata.position[0];
		nxy.y = (*event).indata.locdata.position[1];
		uw_glndctodev(&nxy, xy);
		uw_glput_pikgeom(xy[0], xy[1]);
		return 0;
	}
		
	pikseg.choice = 1;
	pikseg.transform = (*event).indata.pickdata.transform;
	pikseg.position[0] = (*event).indata.pickdata.position[0];
	pikseg.position[1] = (*event).indata.pickdata.position[1];
	pikseg.depth = (*event).indata.pickdata.depth;
	if (pikseg.depth>0)
		pikseg.pickpath = (int*)uu_malloc (pikseg.depth*sizeof(int));
	for (i=0; i<pikseg.depth;i++)
	{
		pikseg.pickpath[i] = (*event).indata.pickdata.pickpath[i];
	}
	if (pikseg.depth>0)
		ud_addpik_seg(pikseg);	
	return 0;
}
/********************************************************************
**    I_FUNCTION     :  uw_glput_pikgeom(x, y)
**		find the geometry in point (x,y) and put into the picking stack 
**
**    PARAMETERS
**       INPUT  :
**                int x,y;              position of pick
**       OUTPUT :
**                none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glput_pikgeom(x, y)
int x, y;
{
	int path[50];       
	int depth;          
	int xy[2];   
	int aper[2];
	int segno;			
	int i;
	Gbuffer dw_buf,uw_glgetbuffer();
	UD_DPICKS pikseg;
	Gnpoint nxy;
	static int save_x = -1, save_y = -1;

	dw_buf = uw_glgetbuffer();
	xy[0] = x;
	xy[1] = y;
	aper[0] = aper[1] = 16;      
	uw_glpushx();
	Pick_test = 1;
	uw_glpik2(path, &depth, xy, aper);
	uw_glpopx();   
	uw_glreset_xform();
	if (depth==0)
	{
/*
.....reset and pick again
*/
		uw_glpushx();
		uw_glpik2(path, &depth, xy, aper);		
		uw_glpopx();   
		uw_glreset_xform();
/*
.....after reset, if it still no pick, return 
.....otherwise, pick the top one again
*/
		if (depth==0)
		{
			Pick_test = 0;
			uw_gldrawbuffer(dw_buf); 
			return -1;
		}
	}
	segno = path[0];
	if (Spick_motion)
	{
		Pick_test = 0;
		uw_gldrawbuffer(dw_buf); 
		return -1;
	}
done:;
	save_x = x; save_y = y;
	uw_gldrawbuffer(dw_buf); 
	uw_glreset_xform();
/*
.....store geometry into the pick stack
*/
	if (ud_isassist_seg(segno)==0 && !Spick_motion)
	{
		uw_gldevtondc(xy, &nxy);
		pikseg.choice = 1;
		pikseg.transform = ug_locvp(&nxy);
		pikseg.position[0] = nxy.x;
		pikseg.position[1] = nxy.y;
		pikseg.depth = depth;
		if (depth>0)
			pikseg.pickpath = (int*)uu_malloc (depth*sizeof(int));
		for (i=0; i<depth;i++)
		{
			pikseg.pickpath[i] = path[i];
		}
		if (depth>0)
			ud_addpik_seg(pikseg);
	}
	Pick_test = 0;
	return 0;
}
ncl_reset_pikgeom()
{
	ud_free_pick_segs();
}

#endif
