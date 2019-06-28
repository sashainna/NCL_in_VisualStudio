/*********************************************************************
**    NAME         :  gnout.c -- generate output primitives
**       CONTAINS:
**
**   ug_npage
**   ug_nput3
**	  ug_nput2
**	  ug_npla3
**	  ug_npla2
**	  ug_npma3
**	  ug_npma2
**	  ug_nfa3
**	  ug_nfa2
**	  ug_ntext
**	  ug_ncall
**   ug_nflnm3
**   ug_nshad3
**
**			NOTES:	This file was modified for iris precision fix.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       gnout.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:22
*********************************************************************/
#include <stdio.h>
#include "zsysdep.h"
#include "gsegop.h"
#include "gviw.h"
#include "g.h"
#include "gvlib.h"
#include "udebug.h"
#include "gmat4.h"
#include "gconvert.h"

#define INTSIZ(x) ((x+sizeof(int)-1)/sizeof(int))
#define INTSIZEOF(x) ((sizeof(x)+sizeof(int)-1)/sizeof(int))


/*********************************************************************
**    I_FUNCTION     :  ug_npage(n) -- generate  UG_PAGEOP cmd.
**    PARAMETERS   
**       INPUT  : UG_LSI n		List to put cmd into.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_npage(n)                        /* generate ug_page command */
UG_LSI n;                           /* segment number */
{
	UG_pageop npage;
	uu_denter(UU_GITRC,(us,"ug_npage(%d)",n));
	npage.elttype=UG_PAGEOP;
	ug_lsins(n,&npage,INTSIZEOF(UG_pageop));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nput3(n,points,len,cmd) -- put coords in cmd.
**				add coordinates to a segment command
**    PARAMETERS   
**       INPUT  : 
**				UG_LSI n,				list to add coords to
**					len;				number of points to add
**				Gwpoint3 points[];3D WC/NDC coords array
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nput3(n,points,len,cmd)            /* put coords in cmd */
UG_LSI n;
int len;
Gwpoint3 points[];
UG_plylna3op *cmd;
{ int i,siz;
	 uu_denter(UU_GITRC,(us,"ug_nput3(%d,points,%d)",n,len));
	cmd->len=len;
				/* Check precision of segment data vs. precision of program. */
				/* Do pointer assignment if same, individual element copy if not. */
	for (i=0; i<len; i++)
	{
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		ug_GtoIpoint3(cmd->pts[i],points[i]);
#else
  		zbytecp(cmd->pts[i],points[i]);
#endif
	}
											/* Segpoint is correct size of segment data */
  siz=sizeof(UG_plylna3op)-sizeof((*cmd).pts) + len*sizeof(Segpoint3);
  ug_lsins(n,cmd,INTSIZ(siz));
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nput2(n,points,len,cmd)--put 2D coords in seg cmd.
**    PARAMETERS   
**       INPUT  : 
**				int len;				number of points to add to cmd
**				UG_LSI n;				list cmd is in
**				Gwpoint points[];	2D WC/NDC coord array
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nput2(n,points,len,cmd)              /* put coords in ug_cmd.gcreal */
int len;
UG_LSI n;
Gwpoint points[];
UG_plylna2op *cmd;
{	int i,siz;
	 uu_denter(UU_GITRC,(us,"ug_nput2(%d,points,%d)",n,len));
	cmd->len=len;
	for (i=0; i<len; i++)
	{
		/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			ug_GtoIpoint(cmd->pts[i],points[i]);
#else
  			zbytecp(cmd->pts[i],points[i]);
#endif
	}
	siz=sizeof(UG_plylna2op)-sizeof((*cmd).pts)+len*sizeof(Segpoint);
	ug_lsins(n,cmd,INTSIZ(siz));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_npla3(n,len,points) -- gen a polyline 3D cmd.
**    PARAMETERS   
**       INPUT  : 
**				UG_LSI n,				list to put cmd in
**					len;				number of points in polyline
**				Gwpoint3 points[];3D WC/NDC coord array
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_npla3(n,len,points)            /* gen a polyline abs 3 command */
UG_LSI n;
int len;
Gwpoint3 points[];
{
	uu_denter(UU_GITRC,(us,"ug_npla3(%d,%d,points))",n,len));
	(*(UG_plylna3op *)&ug_cmd).elttype=UG_PLYLNA3OP;
	ug_nput3(n,points,len,(UG_plylna3op *)&ug_cmd);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_npla2(n,len,points) -- gen 2D polyline cmd.
**    PARAMETERS   
**       INPUT  : 
**				UG_LSI n,				list to put polyline in
**					len;				number of points in polyline
**				Gwpoint points[];	array of 2d WC/NDC coords
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_npla2(n,len,points)              /* gen polyline abs 2 command */
UG_LSI n;
int len;
Gwpoint points[];
{
   uu_denter(UU_GITRC,(us,"ug_npla2(%d,%d,points)",n,len));
  (*(UG_plylna2op *)&ug_cmd).elttype=UG_PLYLNA2OP;
  ug_nput2(n,points,len,(UG_plylna2op *)&ug_cmd);
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_npma3(n,len,points) --  gen 3D polymarker cmd.
**    PARAMETERS   
**       INPUT  : 
**				UG_LSI n,					list to put cmd in
**					len;					number of points to 'marker'
**				Gwpoint3 points[];	3D WC/NDC points array
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_npma3(n,len,points)                /* gen polymarker abs 3 */
UG_LSI n;
int len;
Gwpoint3 points[];
{
	 uu_denter(UU_GITRC,(us,"ug_npma3(%d,%d,points)",n,len));
  (*(UG_plymka3op *)&ug_cmd).elttype=UG_PLYMKA3OP;
  ug_nput3(n,points,len,(UG_plylna3op *)&ug_cmd);
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_npma2(n,len,points) -- gen 2D polymarker cmd.
**    PARAMETERS   
**       INPUT  : 
**				int len;				number of points to 'polymark'
**				UG_LSI n;			list to put cmd in
**				Gwpoint points[];	2D WC/NDC point array
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_npma2(n,len,points)                  /* gen polymarker abs 2 */
int len;
UG_LSI n;
Gwpoint points[];
{
	 uu_denter(UU_GITRC,(us,"ug_npma2(%d,%d,points)",n,len));
  (*(UG_plymka2op *)&ug_cmd).elttype=UG_PLYMKA2OP;
  ug_nput2(n,points,len,(UG_plylna2op *)&ug_cmd);
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nfa3(n,len,points)-- gen a fillarea 3 command
**    PARAMETERS   
**       INPUT  : 
**				UG_LSI n;				list to put cmd in
**				int	len;				number of points defining area
**				Gwpoint3 points[];	array of 3D WC/NDC points
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nfa3(n,len,points)            /* gen a fillarea 3 command */
UG_LSI n;
int len;
Gwpoint3 points[];
{
	uu_denter(UU_GITRC,(us,"ug_nfa3(%d,%d,points))",n,len));
	(*(UG_flarea3op *)&ug_cmd).elttype=UG_FLAREA3OP;
	ug_nput3(n,points,len,(UG_plylna3op *)&ug_cmd);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nfa2(n,len,points)-- gen a fillarea 2D command
**    PARAMETERS   
**       INPUT  : 
**				UG_LSI n;				list to put cmd in
**				int	len;				number of points defining area
**				Gwpoint points[];	array of 2D WC/NDC points
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nfa2(n,len,points)            /* gen a fillarea 2 command */
UG_LSI n;
int len;
Gwpoint points[];
{
	uu_denter(UU_GITRC,(us,"ug_nfa2(%d,%d,points))",n,len));
	(*(UG_flareaop *)&ug_cmd).elttype=UG_FLAREAOP;
	ug_nput2(n,points,len,(UG_plylna2op *)&ug_cmd);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nflnm3(n,len,points,norm)-- 
**		Generate a fill area norm 3 command.
**    PARAMETERS   
**       INPUT  : 
**				int n,					segment number to put cmd in
**					len;					number of points defining area
**				Gwpoint3 points[];	array of 3D WC/NDC verticies
**				Gwpoint3 norm[];		array of 3D WC/NDC normals
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nflnm3(n,len,points,norms)
UG_LSI n;
int len;
Gwpoint3 points[];
Gwpoint3 norms[];
{
	int i,siz;

	uu_denter(UU_GTRC,(us,"ug_nflnm3(%d,%d,points))",n,len));

	(*(UG_flareanorm3op *)&ug_cmd).elttype=UG_FLAREANORM3OP;
	(*(UG_flareanorm3op *)&ug_cmd).len = len;

		/* Do ptr assignment if same precision, individual element copy if not. */
	for (i=0; i<2*len; i+=2)
	{
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		ug_GtoIpoint3( (*(UG_flareanorm3op *)&ug_cmd).pts[i],points[i]);
		ug_GtoIpoint3( (*(UG_flareanorm3op *)&ug_cmd).norms[i],norms[i]);
#else
		zbytecp((*(UG_flareanorm3op *)&ug_cmd).pts[i], points[i]);
  		zbytecp((*(UG_flareanorm3op *)&ug_cmd).pts[i+1], norms[i]);
#endif
	}

	/* Calculate size in bytes of this command */
	siz = sizeof(UG_flareanorm3op) - 
			sizeof((*(UG_flareanorm3op *)&ug_cmd).pts) +
			2*len*sizeof(Segpoint3);

	ug_lsins(n,(UG_flareanorm3op *)&ug_cmd,INTSIZ(siz));

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_ntext(n,posn,str) -- generate a gtext command
**    PARAMETERS   
**       INPUT  : 
**				UG_LSI n;			list to put command in
**				Gwpoint3 *posn;	3D WC/NDC coord of start position
**				char str[];			character string to display
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_ntext(n,posn,str)              /* gen a gtext command */
UG_LSI n;
Gwpoint3 *posn;
char str[];
{
	int len,siz;
	uu_denter(UU_GITRC,(us,"ug_ntext(%d,%g %g,%g,%s)",
		n,(*posn).x,(*posn).y,(*posn).z,str));
	len=strlen(str);
	(*(UG_textop *)&ug_cmd).elttype=UG_TEXTOP;
	(*(UG_textop *)&ug_cmd).len=len;

		/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		ug_GtoIpoint3((*(UG_textop *)&ug_cmd).position,(*posn));
#else
  		zbytecp((*(UG_textop *)&ug_cmd).position,(*posn));
#endif
	strcpy((*(UG_textop *)&ug_cmd).string,str);
	siz=sizeof(UG_textop) -sizeof((*(UG_textop *)&ug_cmd).string)+len+1;
	ug_lsins(n,(UG_textop *)&ug_cmd,INTSIZ(siz));	/* size includes null */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_ncall(n,m) -- gen a call segment cmd.
**			generate a call to segment 'm' in segment 'n' using xform
**    PARAMETERS   
**       INPUT  : 
**				UG_LSI n -- segment number to put cmd in.
**				int m -- segment number to call.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_ncall(n,m)                /* gen a call to segment m */
UG_LSI n;
int m;
{
	/*struct {int op; int segno; Gtran xform;} prms;*/
	uu_denter(UU_GITRC,(us,"ug_ncall(%d,%d,ats)",n,m));
  (*(UG_callop *)&ug_cmd).elttype=UG_CALLOP;
  (*(UG_callop *)&ug_cmd).segno=m;
  ug_lsins(n,(UG_callop *)&ug_cmd,INTSIZEOF(UG_callop));
  uu_dexit;
}
/*********************************************************************
**    I_FUNCTION     :  ug_nshad3 (n,len,points,norms,type)
**    -- gen a shading command
**    PARAMETERS   
**       INPUT  : 
**				UG_LSI n;				list to put cmd in
**				int	len;				number of points defining area
**				Gwpoint3 points[];	array of 3D WC/NDC points
**				Gwpoint3 norms[]; 	array of 3D WC/NDC normal vectors at points
**				int type;			0 = Triangles, 1 = Polygons
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_nshad3 (n,len,points,norms,type)
UG_LSI n;
int len,type;
Gwpoint3 points[];
Gwpoint3 norms[];
{
	int i,siz;
	((UG_shadearea *)&ug_cmd)->elttype = UG_SHADEAREAOP;
	((UG_shadearea *)&ug_cmd)->len = len;
	((UG_shadearea *)&ug_cmd)->type = type;

		/* Do ptr assignment if same precision, individual element copy if not. */
	for (i=0; i<len; i++)
	{
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		ug_GtoIpoint3( (*(UG_shadearea *)&ug_cmd).pts[i],points[i]);
		ug_GtoIpoint3( (*(UG_shadearea *)&ug_cmd).pts[i+len],norms[i]);
#else
		zbytecp((*(UG_shadearea *)&ug_cmd).pts[i], points[i]);
		zbytecp((*(UG_shadearea *)&ug_cmd).pts[i+len], norms[i]);
#endif
	}

	/* Calculate size in bytes of this command */
	siz = sizeof(UG_shadearea) - 
			sizeof((*(UG_shadearea *)&ug_cmd).pts) +
			2*len*sizeof(Segpoint3);

	ug_lsins(n,(UG_shadearea *)&ug_cmd,INTSIZ(siz));

	return (0);
}

