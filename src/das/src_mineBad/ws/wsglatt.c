#include "zsysdep.h"
#ifdef UU_OPENGL
/*********************************************************************
**    NAME         :  wsglatt.c
**
**		GKS openGL workstation attribute routines
**       CONTAINS:
**
**	uw_gllinetype 
**	uw_gllinewidth
**  uw_glcolor
** uw_gltxtext
** uw_glmenutxsz
**	uw_getsave_linetype
**	uw_getsave_linewid
**	uw_setsave_linetype
**	uw_setsave_linewid
**
**    MODULE NAME AND RELEASE LEVEL 
**       wsglatt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:06
*********************************************************************/

#include <stdio.h>
#include "udebug.h"					/* GKS/PHIGS pkg defs */
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include <math.h>						/* Iris math lib header */
#include "wsgl.h"
#include "wsglfun.h"

#if UU_COMP == UU_WIN2K
#include "wsntglfunc.h"
#else
#include "wsxw.h"
#endif

static int current_linetype = 0;
static Gfloat current_linewid = 1.0;

typedef struct { Gint op; Gws id; Gscale wid; } Sprms;
/*********************************************************************
**    I_FUNCTION     :  uw_gllinewidth(width, force)
**       Set current line width.
**		We have to reset line width to 1.0 inside
**		openGL calling list if the openGL calling list change 
**		the line width. Always keep the line width = 1.0.
**    PARAMETERS   
**       INPUT  : 
**          width: line width
**				force: 1: force to set line width
**						0: if it is current line width we using now, it will not 
**							reset the line width
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gllinewidth (width, force)
Gfloat width;
{
	GLfloat glwid;
	uu_denter(UU_GITRC,(us,"uw_gllinewidth()"));
/*
...Do not allow width < 1.0
*/
	glwid = (GLfloat)width;
	if(glwid < 1) glwid = 1.0;
	if ((force) || (glwid!=current_linewid))
	{
		glLineWidth_d(glwid);
		current_linewid = glwid;
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  uw_gllinetype(type, force)
**       Set current line type.
**		We have to reset line type to "default type" inside
**		openGL calling list if the openGL calling list change 
**		the line type. Always keep the line type as SOLID.
**		
**    PARAMETERS   
**       INPUT  : 
**				type: line type
**				force: 1: force to set line type
**						0: if it is same as current type, it will not 
**							reset the line type
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gllinetype(type, force)
int type, force;
{
	uu_denter(UU_GITRC,(us,"uw_gllinetype()"));
	if (type<1) type = 0;
	else if(type>9) type = 8;
	else type = type-1;
/*
...enable line type
*/	
	if ((force) || (type!=current_linetype))
	{
		glCallList_d(gl_line_style+type);
		current_linetype = type;
	}
	uu_dexit;
}
/*********************************************************************
**    FUNCTION     :  uw_glcolor(index)
**       Set current color.
**    PARAMETERS   
**       INPUT  : 
**					index: color index
**       OUTPUT :  
**					none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glcolor( index)
int index;
{ 
	int i;
	GLfloat color[3];
/*
.....if the max color value has not been set
.....just simply return, otherwise, i will be <0 and 
.....uw_color_table[-1][1] is wrong
*/
	if (uw_gl.maxgcolors<=0) return;
/*
...if color not valid, set to max color
*/
	i = index;
	if (index < 0) i = 0;
#if UU_COMP == UU_WIN2K
	if (index >= 64) i = 63;
#else
	if (index >= 16) i = 15;
#endif

	color[0] = (GLfloat)(uw_color_table[i][0]/255.0);
	color[1] = (GLfloat)(uw_color_table[i][1]/255.0);
	color[2] = (GLfloat)(uw_color_table[i][2]/255.0);
	glColor3f_d(color[0], color[1], color[2]);
}
/*********************************************************************
**    I_FUNCTION : uw_glset_linewidth(prms,reply) ------> UG_DLINEWIDTH
**      
**    DESCRIPTION:
**        Set line width. 
**        prms->wid is a floating point number that is the scale factor
**        for the line width. Most devices take an integer pixel value
**        when setting linewidth. Just cast prms->wid to int.
**        0.0 is a valid scale, make it the default line width = 1.
**
**    PARAMETERS   
**       INPUT  : 
**          prms->wid = scale factor
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glset_linewidth(prms,reply) 		/* set linewidth scale factor */
Sprms *prms;
{
	Gfloat width = prms->wid;
   	if (width == 0.0) width = 1.0;
/*	uw_gl.linewidth = width; */
	uw_gllinewidth (width, 0);
}

/*********************************************************************
**    FUNCTION     :  uw_glsetlinetype()
**       Set current line type.
**			This function only remember line type, openGL line
**			type acturally set use function uw_gllinetype
**    PARAMETERS   
**       INPUT  : prms[2]: line type
**       OUTPUT :  
**					none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glsetlinetype(prms,reply)
int prms[];
int reply[];
{
	uw_gl.linetype = prms[2];
	uw_gllinetype (uw_gl.linetype, 0);
}

/*********************************************************************
**    I_FUNCTION : uw_gltxtext(ws,s,ht,wid,pos.concat,extent) 
**				------> UG_DTXTEXT
**      
**    DESCRIPTION:
**       Get the text extent of a string.
**
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltxtext(ws,s,ht,wid,pos,concat,extent)
Gws ws;
Gchar *s;
Gndc *ht,*wid;
Gwpoint3 *pos;
Gwpoint3 *concat;
Gwrect3 *extent;
{
   int n,rasdims[2],raswid,rasht;
   Gfloat ndcdims[2];
   UG_dtext prms;
/*
.....Get the string length
*/
   	n = strlen(s);
/*
.....Calculate the NDC box of the
.....text rectangle
.......Based on hardware text
*/
   	if (ug_gksstli.curprats.txbundl.fp.prec != UG_STROKE)
	{
   		ndcdims[0] = uw_glfont.chrwid;
   		ndcdims[1] = uw_glfont.chrhgt;
   		uw_glndctodev(ndcdims,rasdims);
   		raswid = n * rasdims[0];
   		rasht = rasdims[1];
   		*ht = uw_glfont.chrphgt/uw_gl.wsxform.sf;
   		*wid = (uw_glfont.chrpwid*n)/uw_gl.wsxform.sf;
   	}
/*
........Based on hershey text
*/
   	else
	{
   		prms.pos.x = pos->x;   	
   		prms.pos.y = pos->y;   	
   		prms.pos.z = pos->z;   	
   		prms.slen = n;
   		strcpy(prms.s,s);
   		ug_fchext(&prms,concat,extent);
   	}
}


/***********************************************************************
**    I_FUNCTION : int uw_glmenutxsz(prms,reply) ---> UG_DMENUTEXTSIZE
**      
**		DESCRIPTION:
**			Return menu text height and width.
**
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          reply[0] = text height
**          reply[1] = text width
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
***********************************************************************/
void uw_glmenutxsz(prms,reply)	
int prms[];
Gfloat reply[];
{

	reply[0] = uw_glfont.chrhgt;
	reply[1] = uw_glfont.chrwid;
}

/***********************************************************************
**    I_FUNCTION : uw_glmarker_size(mtype, msize)
**      
**		DESCRIPTION:
**			Return marker's size in WC
**
**    PARAMETERS   
**       INPUT  : 
**          mtype: marker type
**       OUTPUT :  
**          msize: marker size in WC
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
***********************************************************************/
void uw_glmarker_size(mtype, msize)
int mtype;
double *msize;
{
	int ds;
	int rast[2];
	Gfloat ndc[2];
	Gfloat wx1, wy1, wz1;
	Gfloat wx2, wy2, wz2;

	switch(mtype-1 )
	{         
		case 0:  /* dot */
			ds = 1;
			break;

		case 1: /* plus */
			ds = 11;
			break;

		case 2: /* star */
			ds = 11;
			break;

		case 3: /* circle */
			ds = 11;
			break;

		case 4: /* cross        */
			ds = 19;
			break;

		case 5: /* triangle */
			ds = 11;
			break;

		case 6:  /* diamond */
			ds = 11;
			break;

		case 7:  /* square */
			ds = 11;
			break;

		case 8: /* double circle */
			ds = 12;
			break;

		case 9:  /* large dot */
			ds = 11;
			break;

		case 10:  /* cube */
			ds = 11;
			break;

		default:
			ds = 1;
			break;
	}
/*
.....convert dev cord to WC
*/
	rast[0] = rast[1] = ds;
	uw_gldevtondc(rast,ndc);
	ug_ndcw3(ug_gksstli.curvwindex, &wx1,&wy1,&wz1, ndc[0],ndc[1],0);
	rast[0] = rast[1] = 0;
	uw_gldevtondc(rast,ndc);
	ug_ndcw3(ug_gksstli.curvwindex, &wx2,&wy2,&wz2, ndc[0],ndc[1],0);

	*msize = wx1 - wx2;
}

/***********************************************************************
**    I_FUNCTION : uw_setsave_linetype(linetype)
**      
**		DESCRIPTION:
**			Set the saved line type value
**
**    PARAMETERS   
**       INPUT  : 
**          linetype: line type value to be set
**       OUTPUT :  
**          none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
***********************************************************************/
void uw_setsave_linetype(linetype)
int linetype;
{
	current_linetype = linetype;
}


/***********************************************************************
**    I_FUNCTION : uw_setsave_linewid (linewid)
**      
**		DESCRIPTION:
**			Set the saved line width value
**
**    PARAMETERS   
**       INPUT  : 
**          linewid: line width value to be set
**       OUTPUT :  
**          none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
***********************************************************************/
void uw_setsave_linewid (linewid)
Gfloat linewid;
{
	current_linewid = linewid;
}

/***********************************************************************
**    I_FUNCTION : uw_getsave_linetype
**      
**		DESCRIPTION:
**			Get the saved line type value
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**          
**    RETURNS      : saved line type value
**    SIDE EFFECTS : none
**    WARNINGS     : none
***********************************************************************/
int uw_getsave_linetype()
{
	return current_linetype;
}

/***********************************************************************
**    I_FUNCTION : uw_getsave_linewid
**      
**		DESCRIPTION:
**			Get the saved line width value
**
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**          
**    RETURNS      : saved line width value
**    SIDE EFFECTS : none
**    WARNINGS     : none
***********************************************************************/
Gfloat uw_getsave_linewid()
{
	return current_linewid;
}

#endif
