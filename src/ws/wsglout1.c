#include "usysdef.h"
#ifdef UU_OPENGL

/*********************************************************************
**  NAME:  wsglout1.c
**
**      Calls to OpenGL functions for primitives to view a DIGS segment
**
**    CONTAINS:
**
**       uw_glshade_cmd
**       uw_glshade_poly
**       uw_glpolyln3_cmd
**       uw_glpolyln2_cmd
**       uw_glpolylnras_cmd
**       uw_glfill3_cmd
**       uw_glfill_cmd
**       uw_glfillras_cmd
**       uw_glmarker3op
**       uw_glmarker2op
**       uw_glmarkerrasop
**       uw_gltextop
**       uw_gldebug_opcode
**
**    MODULE NAME AND RELEASE LEVEL 
**       wsglout1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:07
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
#include <math.h>

#include "gsegop.h"
#include "ginqatt.h"
#include "ginqatt2.h"
#include "ginqatt3.h"

#include "udebug.h"
#include "zsysdep.h"
#include "driver.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "gsegac.h"
#if UU_COMP != UU_WIN2K 
#include "wsxw.h"
#endif
#include "ginqatti.h"
#include "gmat4.h"
#include "view.h"
#include "wsgl.h"
#include "gviw.h"
#include "view1.h"
#include "wsglfun.h"

extern int uw_glhicolor;
extern int DEBUG_SHOW_TESS;
extern int DEBUG_SHOW_TESS_UV;		
FILE *test_fptr;
void uw_glshade_poly ();

/******************************************************************
**    FUNCTION     :  uw_glshade_cmd (n,cmd,loffset)
**       Shade triangles
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - shade command
**          loffset - offset iff TRUE
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glshade_cmd (n, cmd, loffset)
int n;
UG_shadearea *cmd;
UU_LOGICAL loffset;
{
	if (loffset)
	{
		glEnable_d(GL_POLYGON_OFFSET_FILL);
		glPolygonOffset_d(1.0,1.0);
	}

	uw_glshade_poly(cmd->len,cmd->pts,&(cmd->pts[cmd->len]),cmd->type);

	if (loffset)
	{
		glDisable_d(GL_POLYGON_OFFSET_FILL);
	}

}

/******************************************************************
**    FUNCTION     :  uw_glshade_poly (len,pts,norms,type)
**       Shade triangles
**    PARAMETERS
**       INPUT  :
**          len    - number of vertices
**          pts    - vertices
**          norms  - normals
**          type   - 0 = Triangles, 1 = Polygons
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glshade_poly (len,pts,norms,type)
int len,type;
Gwpoint3 *pts;
Gwpoint3 *norms;
{
	Gwpoint3 *p,*n;
	int i,k, color, lucency;

	p = pts;
	n = norms;
	color = gqlinecolor ();

	lucency = ug_get_lucency();

	if (!DEBUG_SHOW_TESS)
	{
		if (color >= 0) 
		{
			uw_gllighting(UU_TRUE);
			if (lucency<100)
			{
				glBlendFunc_d(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
			}
			uw_gldefine_material (color, lucency, 1);
			if (type == 0) glBegin_d(GL_TRIANGLES);
			else glBegin_d(GL_POLYGON);
			for (i = 0; i < len ; i++)
			{
				glNormal3f_d(n[i].x,n[i].y,n[i].z);
				glVertex3f_d(p[i].x,p[i].y, p[i].z);
			}
			glEnd_d();
			if (lucency<100)
			{
				glBlendFunc_d(GL_ONE, GL_ZERO);
			}
		}
	}
	else
/*
... show tessellation (debug) in white color
*/
	{
		uw_gllighting(UU_FALSE);
		uw_glcolor (color); 
		if (type == 0)
		{
			for (i = 0; i < len; )
			{
				glBegin_d(GL_LINE_LOOP);
				for (k=0;k<3;k++, i++) glVertex3f (p[i].x,p[i].y, p[i].z);
				glEnd_d();
			}
			glBegin_d(GL_LINES);
			for (i = 0; i < len; i++)
			{
				glVertex3f_d(p[i].x,p[i].y, p[i].z);
				glVertex3f_d(p[i].x,p[i].y, p[i].z);
/*
..... do not show normals. eduard 01/19/2000
..... The statement above appears twice, since otherwise tess. is 
..... displayed wrong (with criss-crossing).
				glVertex3f_d(p[i].x+n[i].x,p[i].y+n[i].y, p[i].z+n[i].z);
*/
			}
			glEnd_d();
		}
		else
		{
			glBegin_d(GL_LINE_LOOP);
			for (i = 0; i < len; i++) glVertex3f_d(p[i].x,p[i].y, p[i].z);
			glEnd_d();
			glBegin_d(GL_LINES);
			for (i = 0; i < len; i++)
			{
				glVertex3f_d(p[i].x,p[i].y, p[i].z);
				glVertex3f_d(p[i].x,p[i].y, p[i].z);
/*
..... do not show normals. eduard 01/19/2000
				glVertex3f_d(p[i].x+n[i].x,p[i].y+n[i].y, p[i].z+n[i].z);
*/
			}
			glEnd_d();
		}
	}
}

/******************************************************************
**    FUNCTION     :  uw_glpolyln3_cmd (n,cmd)
**       polyline
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - polyln command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpolyln3_cmd (n,cmd)
int n;
UG_plylna3op *cmd;
{
	int i;
	uw_gllighting(UU_FALSE);
	uw_glcolor (gqlinecolor ());
	glBegin_d(GL_LINE_STRIP);
	for (i=0; i < cmd->len; i++)
	{
		glVertex3f_d(cmd->pts[i].x, cmd->pts[i].y, cmd->pts[i].z);
	}
	glEnd_d();
}

/******************************************************************
**    FUNCTION     :  uw_glrmhid_cmd (n,cmd)
**       Fill outline of segment n
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - shade command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added for hidden line removal
.....Yurong
*/
void uw_glrmhid_cmd (n, cmd)
int n;
UG_shadearea *cmd;
{
	Gwpoint3 *p;
	int i;
	p = cmd->pts;
	glEnable_d(GL_POLYGON_OFFSET_FILL);
	glPolygonOffset_d(1.0,1.0);
	
	uw_gllighting(UU_FALSE);
	glColor3f_d((GLclampf)UV_background.colors[0][0],
		(GLclampf)UV_background.colors[0][1],
		(GLclampf)UV_background.colors[0][2]);
/*	uw_glcolor (0);*/
	if (cmd->type == 0) glBegin_d(GL_TRIANGLES);
	else glBegin_d(GL_POLYGON);
		for (i = 0; i < cmd->len ; i++)
		{
			glVertex3f_d(p[i].x,p[i].y, p[i].z);
		}
	glEnd_d();
	glDisable_d(GL_POLYGON_OFFSET_FILL);
}

/******************************************************************
**    FUNCTION     :  uw_glpolyln2_cmd (n,cmd)
**       UG_PLYLNA2OP command
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpolyln2_cmd (n,cmd)
int n;
UG_plylna2op *cmd;
{
	int i;

	uw_gllighting(UU_FALSE);
	uw_glcolor (gqlinecolor ());
	glBegin_d(GL_LINE_STRIP);
		for (i=0; i < cmd->len; i++)
			glVertex3f_d(cmd->pts[i].x, cmd->pts[i].y, (GLfloat)0.);
	glEnd_d();
}
/******************************************************************
**    FUNCTION     :  uw_glpolylnras_cmd (n,cmd)
**       UG_PLYLNRASOP command
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpolylnras_cmd (n,cmd)
int n;
UG_plylnrasop *cmd;
{
	int i;

	uw_gllighting(UU_FALSE);
	uw_glcolor (gqlinecolor ());
	glBegin_d(GL_LINE_STRIP);

	for (i=0; i < cmd->len; i++)
		glVertex3f_d(cmd->pts[i].x, cmd->pts[i].y, (GLfloat)0.);

	glEnd_d();
}
/******************************************************************
**    FUNCTION     :  uw_glfill3_cmd (n,cmd)
**       UG_FLAREA3OP command
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glfill3_cmd (n,cmd)
int n;
UG_flarea3op *cmd;
{
	int i;

	uw_gllighting(UU_FALSE);
	uw_glcolor (gqfillcolor());

	glBegin_d(GL_POLYGON);

	for (i=0; i < cmd->len; i++)
		glVertex3f_d(cmd->pts[i].x,cmd->pts[i].y, cmd->pts[i].z);

	glEnd_d();
/*
...Outline in current line color if edge flag is on
*/
	if( gqfilledge() == UG_ON)
	{
		uw_glcolor (gqlinecolor ());
		glBegin_d(GL_LINE_STRIP);

		for (i=0; i<cmd->len; i++)
			glVertex3f_d(cmd->pts[i].x,cmd->pts[i].y, cmd->pts[i].z);

		glEnd_d();
	}
}
/******************************************************************
**    FUNCTION     :  uw_glfill_cmd (n,cmd)
**       UG_FLAREAOP command
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glfill_cmd (n,cmd)
int n;
UG_flareaop *cmd;
{
	int i;

	uw_gllighting(UU_FALSE);
	uw_glcolor (gqfillcolor());

	glBegin_d(GL_POLYGON);

	for (i=0; i<cmd->len; i++)
		glVertex3f_d(cmd->pts[i].x,cmd->pts[i].y, (GLfloat)0);

	glEnd_d();
/*
... Outline in current line color if edge flag is on
*/
	if(gqfilledge() == UG_ON)
	{
		uw_glcolor (gqlinecolor());
		glBegin_d(GL_LINE_STRIP);

		for (i=0; i<cmd->len; i++)
			glVertex3f_d(cmd->pts[i].x,cmd->pts[i].y, (GLfloat)0.);

		glEnd_d();
	}
}
/******************************************************************
**    FUNCTION     :  uw_glfillras_cmd (n,cmd)
**       UG_FLAREARASOP command
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glfillras_cmd (n,cmd)
int n;
UG_flarearasop *cmd;
{
	int i;

	uw_gllighting(UU_FALSE);
	uw_glcolor (gqfillcolor());

	glBegin_d(GL_POLYGON);

	for (i=0; i<cmd->len; i++)
		glVertex3i_d(cmd->pts[i+1].x,cmd->pts[i+1].y, 0);

	glEnd_d();
/*
... Outline in current line color if edge flag is on
*/
	if(gqfilledge() == UG_ON)
	{
		uw_glcolor (gqlinecolor());
		glBegin_d(GL_LINE_STRIP);

		for (i=0; i<cmd->len; i++)
			glVertex3i_d(cmd->pts[i].x,cmd->pts[i].y, 0);

		glEnd_d();
	}
}
/******************************************************************
**    FUNCTION     :  uw_glmarker3op (n,cmd)
**       UG_PLYMKA3OP command
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmarker3op (n,cmd)
int n;
UG_plymka3op *cmd;
{
	int i;

	for (i=0; i<cmd->len; i++)
		uw_glmarker ( cmd->pts[i].x, cmd->pts[i].y, cmd->pts[i].z, gqmarktype() );
}
/******************************************************************
**    FUNCTION     :  uw_glmarker2op (n,cmd)
**       UG_PLYMKA2OP command
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmarker2op (n,cmd)
int n;
UG_plymka2op *cmd;
{
	int i;

	for (i=0; i<cmd->len; i++)
		uw_glmarker ( cmd->pts[i].x, cmd->pts[i].y, 0., gqmarktype() );
}
/******************************************************************
**    FUNCTION     :  uw_glmarkerrasop (n,cmd)
**       UG_PLYMKRASOP command
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmarkerrasop (n,cmd)
int n;
UG_plymkrasop *cmd;
{
	int i;
	for (i=0; i<cmd->len; i++)
		uw_glmarker( (float)cmd->pts[i].x, (float)cmd->pts[i].y, 0., gqmarktype() );
}
/******************************************************************
**    FUNCTION     :  uw_gltextop (n,cmd)
**       UG_TEXTOP command
**    PARAMETERS
**       INPUT  :
**          n   - DIGS segment number
**          cmd - command
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltextop (n,cmd)
int n;
UG_textop *cmd;
{
	UG_dtext txt;

	ug_text (&cmd->position, cmd->string);
	txt.id = 0;
	txt.pos.x = cmd->position.x;
	txt.pos.y = cmd->position.y;
	txt.pos.z = cmd->position.z;
	if ((cmd->len)<UG_TEXT_MAX_SIZE)
		strcpy (txt.s,cmd->string);
	else
		strncpy (txt.s,cmd->string,UG_TEXT_MAX_SIZE-1);

/*
.....'uw_gltext' is called from 'ug_text'
.....Bobby - 7/18/07
*/
/*	uw_gltext (&txt);*/
}
/******************************************************************
**    FUNCTION  :  uw_gldebug_opcode (opcode)
**       A debug function
**    PARAMETERS
**       INPUT  :
**          opcode  - operation code
**       OUTPUT :
**          prints opcode
**    RETURNS      : 0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldebug_opcode (opcode)
int opcode;
{
	switch (opcode)
	{
		case UG_NOOP: break;
		case UG_PAGEOP:
			printf("    UG_PAGEOP\n"); ug_pag(); break;
		case UG_PLYLNA3OP:
			printf("    UG_PLYNA3OP\n"); break;
		case UG_PLYLNA2OP:
			printf("    UG_PLYNA2OP\n"); break;
		case UG_PLYLNRASOP:
			printf("    UG_PLYNARASOP\n"); break;
		case UG_PLYMKA3OP:
			printf("    UG_PLYMKA3OP\n"); break;
		case UG_PLYMKA2OP:
			printf("    UG_PLYMKA2OP\n"); break;
		case UG_PLYMKRASOP:
			printf("    UG_PLYMKRASOP\n"); break;
		case UG_TEXTOP:
			printf("    UG_TEXTOP\n"); break;
		case UG_TEXTRASOP:
			printf("    UG_TEXTRASOP\n"); break;
		case UG_FLAREA3OP:
			printf("    UG_FLAREA3OP\n"); break;
		case UG_FLAREAOP:
			printf("    UG_FLAREAOP\n"); break;
		case UG_FLAREARASOP:
			printf("    UG_FLAREAOP\n"); break;
		case UG_SHADEAREAOP:
			printf("    UG_SHADEAREAOP\n"); break;
		case UG_CELLOP:
			 printf("    UG_CELLOP\n"); break;
		case UG_CELLRUNOP:
			printf("    UG_CELLRUNOP\n");  break;
		case UG_CELLRASOP:
			printf("    UG_CELLRASOP\n"); break;
		case UG_CELLRUNRASOP:
			printf("    UG_CELLRUNRASOP\n"); break;
		case UG_CALLOP:
			printf("    UG_CALLOP\n"); break;
		case UG_SNTRANOP:
			printf("    UG_SNTRANOP\n"); break;
		case UG_MTRANOP:
			printf("    UG_MTRANOP\n"); break;
		case UG_LMTRANOP:
			printf("    UG_LMTRANOP\n"); break;
		case UG_LSTYLOP:
			printf("    UG_LSTYLOP\n"); break;
		case UG_LWIDOP:
			printf("    UG_LWIDOP\n"); break;
		case UG_FONTOP:
			printf("    UG_FONTOP\n"); break;
		case UG_CHEXPOP:
			printf("    UG_FONTOP\n"); break;
		case UG_CHPLANEOP:
			printf("    UG_CHPLANEOP\n"); break;
		case UG_CHUP3OP:
			printf("    UG_CHUP3OP\n"); break;
		case UG_CHUP2OP:
			printf("    UG_CHUP2OP\n"); break;
		case UG_CHPATHOP:
			printf("    UG_CHPATHOP\n"); break;
		case UG_CHSPACEOP:
			printf("    UG_CHSPACEOP\n"); break;
		case UG_CHJUSTOP:
			printf("    UG_CHJUSTOP\n"); break;
		case UG_SYMBOLOP:
			printf("    UG_SYMBOLOP\n"); break;
		case UG_PICKIDOP:
			printf("    UG_PICKIDOP\n"); break;
		case UG_EDGEFLAGOP:
			printf("    UG_EDGEFLAGOP\n"); break;
		case UG_DFATSOP:
			printf("    UG_DFATSOP\n"); break;
		case UG_LNCOLROP:
			printf("    UG_LNCOLROP\n"); break;
		case UG_MKCOLROP:
			printf("    UG_MKCOLROP\n"); break;
		case UG_TXCOLROP:
			printf("    UG_TXCOLROP\n"); break;
		case UG_FACOLROP:
			printf("    UG_FACOLROP\n"); break;
		case UG_CHHGTOP:
			printf("    UG_CHHGTOP\n"); break;
		default:
			printf("    DEFAULT\n"); break;
	}
}	

#endif
