#include "usysdef.h"
#ifdef UU_OPENGL

/*********************************************************************
**    NAME         :  wsgltrk.c
**      	GKS workstation pick, locate, track, etc. type routines.
**
**    CONTAINS:
**			uw_gltrk
**			uw_glinitloc
**
**    MODULE NAME AND RELEASE LEVEL 
**			wsgltrk.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:09
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

#include "wsgl.h"
#include "udebug.h"
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
/*
.....we do need this file
.....Yurong 8/26/97
*/
#include "nclicons.h"
#if UU_COMP != UU_WIN2K 
#include "wsxw.h"
#else
#include "wsntglfunc.h"
#endif
#include "mpocket.h"
#include "lcom.h"
#include "wsglfun.h"

int UW_rubber_active = 0;
extern UG_wdt glwdt;
static GLuint uw_glusrcursor[NCHDEV] = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0};
static Gipoint uw_glusrattach[NCHDEV];
static int uw_glusrwid[NCHDEV], uw_glusrhgt[NCHDEV];
static int uw_glusrllx[NCHDEV], uw_glusrlly[NCHDEV];
/*static GLuint usrcur_tran = 0;*/
/*static GLuint usrcur_orig = 0;*/
/*
.....Removed, already declared in wsgl.h
.....Yurong 8/26/97
*/
extern int SELECT_UP;

#define PICKTRIGLEN 6
extern int ug_picktriglen;
extern struct { 
	int dev; 
	int min,max;
} ug_piktrig[PICKTRIGLEN];

void uw_clear_depth();
static void S_calc_box();

extern char *UM_pocket_hwnd;
extern int UM_swap_ipv;
extern int UZ_nclipv_view;
extern int PKx,PKy;
extern int MSLite;

typedef struct
{
	int ll[2];
	int ur[2];
} TRK_box;

extern int UG_cursor_window;

static int Ssave_mask;

#if UU_COMP != UU_WIN2K 
int min (v1, v2)
int v1, v2;
{
	if (v1>v2) return v2;
	else return v1;
}
#endif
/*********************************************************************
**    I_FUNCTION : uw_gltrk(wid,xy,k,cursorno,locno,devno,usrc,offset)
**					---------> UG_DTRK
**      
**		DESCRIPTION :
**	 		track a cursor
**
**			If possible, make the graphics picking cursor the size
**			of the current pick aperture. ug_find.epsx, ug_find.epsy
**			are the ndc half-widths of the current aperture.
**
**			In all cases, x,y contain final coordinates. 
**
**		for BACKSPACE or FORMS CLEAR, the function keys must return
**		*k = '\025'  or '\010' and return code 1 (normal keyboard key).
**
**    PARAMETERS   
**       INPUT  : 
**		Gws wid;	workstation id
**		int xy[2];	input: initial cursor posn, dev coords
**				output: final cursor posn, dev coords
**		int *k;		ending key value
**		int cursorno;	which style cursor to display:
**				0=graphics picking cursor (PICK_CURSOR_SEG).
**				1=menu picking cursor (MENU_CURSOR_SEG).
**				2=loc cursor (LOC_CURSOR_SEG).
**				4 = rubber band line, loc cursor.
**				5 = rubber band rect, loc cursor.
**				6 = digital, loc cursor. 
**				21 = cursor is user defined.
**		int locno;	0=both t-wheels and tablet,
**				1=t-wheels only, 2=tablet only 
**		int devno;	if cursorno==21, devno=device number 
**		int *usrc;	 not used 
**		Gipoint *offset;
**
**       OUTPUT :  
**		int xy[2];	output: final cursor posn, dev coords
**    RETURNS :
**		0 =a keypad-2 key was hit, k contains the key number. 
**		1= An ASCII kbd key was hit, k contains ascii value.
**		2=a keypad-1 key was hit, k contains key number. 
**		3=a tablet puck button was used, k contains number. 
**			For 41xx, tablet pen is number 1, n-button puck
**			is numbers 2 thru n+1. For our puck:
**			yellow=2, white=3, blue=4, green=5. 
**			Return 3 for any locator device trigger - pen, puck,
**			mouse, arrow keys, joystick.
**			If the locator device has no trigger button (joystick,
**			arrow keys, thumbwheels), let the space bar be the trigger 
**			and set *k = 1.
**		5=arrow key. k contains which one (up,down,left,right)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_gltrk(wid,xy,k,cursorno,locno,devno,usrc,offset)
Gws wid;					
int xy[2];	 		
int *k;		
int cursorno;
int locno;		
int devno;			
int *usrc;
Gipoint *offset;
{
	int event,irtn,cxy[2];
	Gipoint points[5];
	int indx;
	int iorg[2];
	int i,ic, erase;
	float xf, yf;
	float trx, try;
/*	GLint viewport[4],vpt[4]; */
	GLint vpt[4];
	GLdouble ortho[6];
	char pstr[200],lstr[200],estr[200];
	int ll[2],ur[2],n;
	int sav_cur, old_pocket;
	TRK_box sbox;
/*
.....For dragging segments
*/
	int iprev[2];
	int first=UU_TRUE;
	Gbuffer savbuf,uw_glgetbuffer();
/*
.....This function only get a point, every thing drawed only
.....for temperately, so don't write in depth buffer
.....Yurong 11/11/98
*/
	Ssave_mask = uw_glget_depthmask();
/*
.....do not put set depth off here because uw_ntevent/uw_mfevent may accept another function to run, then
.....it will cause depth off in that function (such as dynamic rotate, repaint,...), it will go wrong with depth off
.....so set depth off only before drawing and after uw_ntevent/uw_mfevent called
.....If set depth off here without getout this function and call dynamic
.....rotate, it coould have a black hole
.....Yurong 10/28/02
	uw_gldepth_mask(0);
*/
#if UU_COMP!=UU_WIN2K
	sav_cur = uw_mfget_cursor();
#else
	sav_cur = uw_ntgetcur_cursor();
#endif
/*
.....Initialize pocket picking if NCL window and IPV window swaped
*/
/*
.....if UM_pocket_hwnd already setup in other function, don't reset it
*/
	old_pocket = 1;
	if ((UM_swap_ipv)&&(UM_pocket_hwnd==NULL))
	{
		UM_pocket_hwnd = um_get_pocket_window(UM_IPV_WINDOW);
		old_pocket = 0;
	}
/*
.....Display the requested cursor
*/
	ic = cursorno;
	if (ic == 21) ic = 4;
/*
.....Save original location
*/
	iorg[0] = xy[0] ; iorg[1] = xy[1];
/*
.....Flush output buffer
*/
	ud_updatews(UG_SUPPRESS);
/*
......Rubberband line or rectangle
*/
	iprev[0] = xy[0];   iprev[1] = xy[1];
  	if ((cursorno == 4) || (cursorno == 5))
	{
/*
.....mark locator active
*/
		UW_rubber_active = 1;
/*
......save drawing buffer because we may change drawing buffer
......Yurong 3/25/02
*/
		savbuf = uw_glgetbuffer();
  		indx = ug_gksstli.curprats.lnbundl.color;
		if (!MSLite) uw_gldrawbuffer(UG_FRONT_BUFFER);
/*
.......Set linestyle and width
.......to normal for rectangle
*/
		if (cursorno == 5)
		{
			uw_gl.linetype = 1;
		}
	}
	if ( cursorno == 21)
	{
/*
.....Calculate initial translation
..... (cursor position - attach point)
*/
		trx = xy[0] - uw_glusrattach[devno-1].x;
		try = xy[1] - uw_glusrattach[devno-1].y;
#if UU_COMP!=UU_WIN2K
		XWarpPointer(uw_xw.disp,None,uw_xw.wd_id,0,0,0,0, 
				xy[0], uw_gl.ypixels-xy[1]);
#else
		{
			int x,y;
			uw_ntgraph_toscrn(xy[0], xy[1], &x, &y);
			uw_ntset_curpos(x, y);
		}
#endif
		savbuf = uw_glgetbuffer();
		if (!MSLite) uw_gldrawbuffer(UG_FRONT_BUFFER);
  	}
	glPushAttrib_d(GL_LIGHTING_BIT);
/*
.....Rubberband until an input event occurs
*/
/*
.....set up matrix
*/
	uw_glpushx();
	if (MSLite)
		uw_glset_context(UM_IPV_WINDOW,UU_TRUE);
/*	glGetIntegerv_d( GL_VIEWPORT , viewport ); */

/*	vpt[0] = 0.; vpt[1] = 0.; vpt[2] = viewport[2] ; vpt[3] = viewport[3];*/

	if ((MSLite)||(UZ_nclipv_view == 1))
	{
		ll[0] = 0.; ll[1] = 0.; ur[0] = PKx; ur[1] = PKy;
	}
	else
	{
		n = ug_gksstli.curvwindex;
		uw_glndctodev(&ug_gksstli.vtran[n].vport.llf,ll);
		uw_glndctodev(&ug_gksstli.vtran[n].vport.urb,ur);
	}
	vpt[0] = ll[0]; vpt[1] = ll[1];
	vpt[2] = ur[0]-ll[0]; vpt[3] = ur[1]-ll[1];
	if (MSLite)
		glViewport_d(vpt[0],vpt[1],vpt[2]-1,vpt[3]-1);
	else
		uw_glViewport(vpt);

	ortho[0] = ll[0] ; ortho[1] = ur[0] ; ortho[2] = ll[1];
	ortho[3] = ur[1] ; ortho[4] = -1000. ; ortho[5] = 1000.;
	if (MSLite)
		glOrtho_d(ortho[0],ortho[1],ortho[2],ortho[3],ortho[4],ortho[5]);
	else
		uw_glOrtho(ortho);
/*
.....Save prompt area labels
.....just in case they are overwritten
.....by another call during text input
.....Bobby  -  9/25/97
*/
#if UU_COMP!=UU_WIN2K
	uw_mfgetprm(pstr); uw_mfgetprlabel(lstr); uw_mfgetprmerr(estr);
#else
	uw_ntsetcursor(ic);
	uw_ntgetprm(pstr); uw_ntgetprlabel(lstr); uw_ntgetprmerr(estr);
#endif

again:;
	irtn = 0;
	erase = 0;
#if UU_COMP != UU_WIN2K
	while ((irtn=uw_mfevent(&event,cxy)) == 0)
#else
	while ((irtn=uw_ntevent(&event,1,cxy,1)) == 0)
#endif
	{
#if UU_COMP != UU_WIN2K
		uw_mfsetcursor(ic);
#else
		uw_ntsetcursor(ic);
#endif
		if (cursorno==4 || cursorno==5 || cursorno==21 )
		{
/*
.......Check if cursor moved enough
.......to redraw the rubberband segment
*/
			if ((abs(cxy[0]-iprev[0]) > 1) || (abs(cxy[1]-iprev[1]) > 1))
			{
				glDepthFunc_d(GL_ALWAYS);	
				uw_gldepth_mask(0);
				uw_glLoadIdentity();
/*
......Erase previous area
*/
				if (!first)
				{
					uw_restore_area(&sbox);
				}
				first = UU_FALSE;
				if ((MSLite)||(UZ_nclipv_view == 1))
				{
					uw_glset_context(UM_IPV_WINDOW,UU_TRUE);
					glDepthFunc_d(GL_ALWAYS);	
					uw_gldepth_mask(0);
					glViewport_d(vpt[0],vpt[1],vpt[2]-1,vpt[3]-1);
					glMatrixMode_d(GL_PROJECTION);
					uw_glLoadIdentity();
					glOrtho_d(ortho[0],ortho[1],ortho[2],ortho[3],ortho[4],ortho[5]);
					glMatrixMode_d(GL_MODELVIEW);
					uw_glLoadIdentity();
				}
				switch (cursorno)
				{
/*
......Redraw the rubberband line
*/
				case 4:
/*
.........Calculate new screen area
*/
					if (xy[0]<cxy[0])
					{
						sbox.ll[0] = xy[0];
						sbox.ur[0] = cxy[0];
					}
					else
					{
						sbox.ll[0] = cxy[0];
						sbox.ur[0] = xy[0];
					}
					if (xy[1]<cxy[1])
					{
						sbox.ll[1] = xy[1];
						sbox.ur[1] = cxy[1];
					}
					else
					{
						sbox.ll[1] = cxy[1];
						sbox.ur[1] = xy[1];
					}	
					sbox.ll[0] -= 2; sbox.ur[0] += 2;
					sbox.ll[1] -= 2; sbox.ur[1] += 2;
					S_calc_box(&sbox,ll,ur);
/*
.....Draw the new line
*/
					uw_gllighting(UU_FALSE);
					uw_glcolor(indx);
					uw_gldrawbuffer(UG_FRONT_BUFFER);
					glBegin_d(GL_LINES);
						glVertex2i_d(cxy[0],cxy[1]);          
						glVertex2i_d(xy[0], xy[1]);          
					glEnd_d();
					uw_glflush();
					iprev[0] = cxy[0];
					iprev[1] = cxy[1];
					break;
/*
......Redraw the rubberband rectangle
*/
				case 5:
/*
.........Calculate new screen area
*/
					if (iorg[0]<cxy[0])
					{
						sbox.ll[0] = iorg[0];
						sbox.ur[0] = cxy[0];
					}
					else
					{
						sbox.ll[0] = cxy[0];
						sbox.ur[0] = iorg[0];
					}
					if (iorg[1]<cxy[1])
					{
						sbox.ll[1] = iorg[1];
						sbox.ur[1] = cxy[1];
					}
					else
					{
						sbox.ll[1] = cxy[1];
						sbox.ur[1] = iorg[1];
					}	
					sbox.ll[0] -= 2; sbox.ur[0] += 2;
					sbox.ll[1] -= 2; sbox.ur[1] += 2;
					S_calc_box(&sbox,ll,ur);
/*
...........Draw the new rectangle
*/
					points[0].x = iorg[0];
					points[0].y = iorg[1];
					points[1].x = cxy[0];
					points[1].y = iorg[1];
					points[2].x = cxy[0];
					points[2].y = cxy[1];
					points[3].x = iorg[0];
					points[3].y = cxy[1];
					points[4].x = iorg[0];
					points[4].y = iorg[1];
					if ((UZ_nclipv_view == 1)||(MSLite))
					{
						for (i=0;i<4;i++) points[i].y = ur[1] - points[i].y;
					}
					uw_gllighting(UU_FALSE);
					glDisable_d(GL_LIGHTING);
					uw_glcolor(1);
					if (!MSLite) uw_gldrawbuffer(UG_FRONT_BUFFER);
					glBegin_d(GL_LINE_LOOP);
						for(i=0; i<4; i++)
							glVertex2i_d(points[i].x, points[i].y); 
						glVertex2i_d(points[0].x, points[0].y); 
					glEnd_d();
					uw_glflush();
					iprev[0] = cxy[0];
					iprev[1] = cxy[1];
					break;
/*
.......Redraw user defined cursor
*/
   			case 21:
/*
.........Calculate new screen area
*/
					xf = cxy[0] - iorg[0] + trx;
					yf = cxy[1] - iorg[1] + try;
					sbox.ll[0] = uw_glusrllx[devno-1] + xf - 2;
					sbox.ll[1] = uw_glusrlly[devno-1] + yf - 2;
					sbox.ur[0] = sbox.ll[0] + uw_glusrwid[devno-1] + 4;
					sbox.ur[1] = sbox.ll[1] + uw_glusrhgt[devno-1] + 4;
					S_calc_box(&sbox,ll,ur);
/*
........Redraw cutter
*/
					uw_gldrawbuffer(UG_FRONT_BUFFER);
					glPushMatrix_d();
					glTranslatef_d(xf, yf, (GLfloat)0.0);
					glCallList_d(uw_glusrcursor[devno-1]);
					glPopMatrix_d();
					uw_glflush();
					iprev[0] = cxy[0];
					iprev[1] = cxy[1];
					break; /* HERE */
				} /* end switch */
			glDepthFunc_d(GL_LEQUAL);	
			uw_gldepth_mask(Ssave_mask);
			} /* end if moved enough */
		} /* end if 4,5 */
/*
.....Restore prompt & error text strings
*/
#if UU_COMP != UU_WIN2K 
		uw_mfwrplabel(lstr);
		uw_mfwrprm(pstr);
		uw_mfprmerr(estr);
#else
		uw_ntwrplabel(lstr);
		uw_ntwrprm(pstr);
		uw_ntprmerr(estr);
#endif
	} /* end of event loop */
/*
.....Don't return on Button Release
*/
	if (irtn == 6) goto again;
/*
.....Erase last if rubberbanding or dragging
*/
	if ((cursorno == 4 || cursorno == 5 || cursorno == 21) && !first)
	{
		glDepthFunc_d(GL_ALWAYS);	
		uw_gldepth_mask(0);
		uw_glLoadIdentity();
		uw_restore_area(&sbox);
	}
/*
.....Set up return parameters
*/
	*k = event;
	xy[0] = cxy[0];
	xy[1] = cxy[1];
/*
.....Take down the cursor
*/
/*
.....why when SELECT, put cursor back ?
.....changed, all should set to wait cursor
.....Yurong 2/17/99
*/
/*
	if (SELECT_UP) uw_mfsetcursor(1);
	else uw_mfsetcursor(21);
*/
/*
.....set cursor back to before
.....Yurong
*/
#if UU_COMP != UU_WIN2K
	uw_mfsetcursor(sav_cur);
#else
	uw_ntsetcursor(sav_cur);
#endif

/*
.....Restore overlay mode
.....and drawing buffer
*/
	if (cursorno == 4 || cursorno == 5 || cursorno == 21 && !MSLite)
		uw_gldrawbuffer(savbuf);
/*
.....reset depth mask
.....Yurong 11/11/98
*/
	glDepthFunc_d(GL_LEQUAL);	
	uw_gldepth_mask(Ssave_mask);
/*
.....restore matrix
*/
	uw_glpopx(); 
	glPopAttrib_d();
	if ((UM_swap_ipv) && (old_pocket==0))
		UM_pocket_hwnd = NULL;
	UW_rubber_active = 0;
	return(irtn);
}

/***********************************************************************
**    I_FUNCTION : int uw_glinitloc() ---> UG_DINITLOC
**      
**	DESCRIPTION:
**		Initialize a drag segment if pet #21.
**
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
***********************************************************************/
void uw_glinitloc(prms,reply)
UG_initdev *prms;
Gstatus *reply;
{
	int pet,dev,ws;
	Glocst *locpt;
	UG_segstli *segpt;
	int attach_pt[2], pt1[2], pt2[2];
	Gnpoint3 np1,np2;
	Gfloat ndc1[2], ndc2[2];
	Gseg segno , newsegno;
	Gdrect earea;
	Gwrect3 rect;

/*
.....Get the device number and
.....workstation id
*/
  	*reply = UG_OK;
	dev = prms->devno - 1;
	ws = prms->id;
/*
.....Check for legal device number
*/
	if (dev > NCHDEV-1)
	{
/*		fprintf(ug_gksos.erfile,"ginitloc(ws=%d) - bad device no=%d\n",
			(*prms).id,dev+1);*/
		*reply=UG_NONE;
		return;
	}
	else 
	{  
		*reply=UG_OK;
	}
/*
.....Delete previous current cursor
*/
	if (uw_glusrcursor[dev] > 0)
	{
		glDeleteLists_d(uw_glusrcursor[dev], 1);
	}
/*
.....Get the pet number
*/
	locpt = &((*ug_gksstli.wsopen[ws].inptr).locdata[dev]);
	pet = locpt->pet;
	if (pet==21)
	{
/*
.....Create a raster image of the segment
.....to be dragged
*/
		segno = locpt->record.seg;
  		segpt = ug_segac(segno);
		uw_glndctodev(&locpt->record.attach,attach_pt);
			
		if (segpt->wcboxok!=1)
		{
			ug_segextrema(segpt, &rect);
			ug_xform(rect.llf.x,rect.llf.y,rect.llf.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		}
		else
			ug_xform(segpt->wcbox.llf.x,segpt->wcbox.llf.y,segpt->wcbox.llf.z,&np1,ug_cxform[ug_gksstli.curvwindex]);
		ndc1[0] = np1.x;
		ndc1[1] = np1.y;
		uw_glndctodev(ndc1, pt1);
		
		if (segpt->wcboxok!=1)
		{
			ug_segextrema(segpt, &rect);
			ug_xform(rect.urb.x,rect.urb.y,rect.urb.z,&np2,ug_cxform[ug_gksstli.curvwindex]);
		}
		else
			ug_xform(segpt->wcbox.urb.x,segpt->wcbox.urb.y,segpt->wcbox.urb.z,&np2,ug_cxform[ug_gksstli.curvwindex]);
		ndc2[0] = np2.x;
		ndc2[1] = np2.y;
		uw_glndctodev(ndc2, pt2);

		uw_glusrwid[dev] = abs(pt1[0] - pt2[0]);
		uw_glusrhgt[dev] = abs(pt1[1] - pt2[1]);
		uw_glusrllx[dev] = min(pt1[0], pt2[0]);
		uw_glusrlly[dev] = min(pt1[1], pt2[1]);
/*
.....Make a raster version of the segment to be drag
*/
		newsegno = 2*UG_MAXSEGNO+dev;
/*
.....Set the echo area to entire ndc viewport
*/
		earea.ll.x = 0.0;    earea.ll.y = 0.0;
		earea.ur.x = 1.0;    earea.ur.y = 1.0;
		
/*
.....This makes the raster segment
*/
		ug_drasseg(ws,segno,newsegno,&earea);
/*
.....Make OpenGL cursor object
*/
		if (uw_glusrcursor[dev]==0)
			uw_glusrcursor[dev] = glGenLists_d(1);
		glNewList_d(uw_glusrcursor[dev], GL_COMPILE);

		uw_gl.rasmode = 1; 
/*
.....Now draw the segment
*/
		segpt = ug_segac(newsegno); 
		segpt->segatts.gvis=UG_VISIBLE;
		ug_view0(newsegno,0);
		uw_gl.rasmode = 0;
		glEndList_d();
/*
.....Delete the DIGS raster segment
*/
		ug_deleseg(newsegno);
/*
.....Save the attach point
*/
		uw_glndctodev(&locpt->record.attach,attach_pt);
		uw_glusrattach[dev].x = attach_pt[0];
		uw_glusrattach[dev].y = attach_pt[1];
	}
}
/*********************************************************************
**    I_FUNCTION     : S_calc_box(box)
**         Calculate the actual box extents as it fits in a viewport.
**
**    PARAMETERS
**       INPUT  :
**          box.llx    = Lower left in X of box.
**          box.lly    = Lower left in Y of box.
**          box.urx    = Upper right in X of box.
**          box.ury    = Upper right in Y of box.
**          ll         = Lower left of viewport.
**          ur         = Upper right of viewport.
**       OUTPUT :
**          box.ll[0]  = Lower left in X of box.
**          box.ll[1]  = Lower left in Y of box.
**          box.ur[0]  = Upper right in X of box.
**          box.ur[1]  = Upper right in Y of box.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_calc_box(box,ll,ur)
TRK_box *box;
int ll[2],ur[2];
{
	if (box->ur[0] > ur[0]) box->ur[0] = ur[0];
	if (box->ur[1] > ur[1]) box->ur[1] = ur[1];
	if (box->ll[0] < ll[0]) box->ll[0] = ll[0];
	if (box->ll[1] < ll[1]) box->ll[1] = ll[1];
}
#endif
