
/*********************************************************************
**    NAME         :  ws747out.c
**    CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws747out.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:04
*********************************************************************/
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ginqatt.h"
#include "ws7475.h"


extern 	Gs7475 uw_7475;					/* declare workstation local data */
typedef struct {int id; Gws ws; int n; Gipoint *points;} flarearasprms;

/*------------------------- local declarations --------------------------*/

/*------------------------- output primitives ---------------------------*/

/*-------------------------------------------------------> UG_DPOLYLN */
uw_7475polyln(prms,reply) 			/* polyline */
struct {int id; Gws ws; int n; Gwpoint *points;} *prms;
int reply[];						/* no reply parameters */
{
	/* draw the polyline specified in (*prms) in the current
	polyline color. If a segment is
	open ( i.e. UG_DCRESEG or UG_DOPNSEG has been called),
	save the polyline in the segment and don't actually
	draw it unless the segment is VISIBLE. (*prms).n is the
	number of 2D world coordinates (untransformed) contained
	in (*prms).points. Simulation routine ug_dpolyln(prms,reply)
	may be used if desired. It will transform coordinates and
	break up the polyline into individual lines and make n-1
	calls to the UG_DDRWOP secondary entry point if the segment is
	VISIBLE. 
		The UG_DLNCINDEX routine is called every time the current
	line color index is changed. Alternatively, the external structure
	ug_gksstli.curprats.lnbundl.color always contains the current
	line color index. */
}

/*-------------------------------------------------------> UG_DPOLYLN3 */
/*********************************************************************
**    I_FUNCTION :  uw_7475polyln3(prms,reply)			
**		Draw the 3D polyline specified in *prms. If a segment is
**		open(i.e. UG_DCRESEG or UG_DOPNSEG has been called), 
**		save the polyline in the segment and don't actually
**		draw it unless the segment is VISIBLE. (*prms).n is the
**		number of 3D world coordinates (untransformed) contained
**		in (*prms).points. Simulation routine ug_dpolyln3(prms,reply)
**		can be used if desired. It will transform coordinates and break
**		up the polyline into individual lines and make n-1
**		calls to the UG_DDRWOP secondary entry point if the segment is
**		VISIBLE.
**			The UG_DLNCINDEX routine is called every time the current
**		line color index is changed. Alternatively, the external structure
**		ug_gksstli.curprats.lnbundl.color always contains the current
**		line color index. 
**			Call the simulation routine to simulate the dashed lines.
**	
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475polyln3(prms,reply)			/* polyline (3-d) */
struct {int id; Gws ws; int n; Gwpoint3 *points;} *prms;

{
	uu_denter(UU_GITRC,(us,"enter uw_7475polyln3"));

	/* Added to display motion on quick plot. kathy */
	if (ug_gksstli.curvwindex == 0)
		ug_gksstli.curvwindex = 1;
/*
.....Added separate check for line type & width
.....as 'ug_d5polyln3' will plot line types on curves
.....but will not plot line widths while 'ug_d4polyln3'
.....will plot most line types and all line widths.
.....This is because curves are broken up into multiple
.....short polylines when drawn, which are not long enough
.....to generate dashes.
.....Bobby  -  7/24/91
*/
	if (((ug_gksstli.curprats.lnbundl.type.typeno>=2) &&
		 (ug_gksstli.curprats.lnbundl.type.typeno<=8)))
		ug_d5polyln3(prms);	/* software simulate all the line types */
	else if ((gqlinewidth() >= 2.0))
		ug_d4polyln3(prms);	/* software simulate all the line widths */
	else 						/* linetype<2 = solid line */
		ug_dpolyln3(prms);
	uu_dexit;
}	/* uw_7475polyln3 */


/*--------------------------------------------------------> UG_DPOLYMK */
uw_7475polymk(prms,reply)				/* polymarker */
struct {int id; Gws ws; int n; Gwpoint *points;} *prms;
int reply[];						/* no output parameters */
{
	/* draw the polymarker specified in (*prms). Use the current
	polymarker attributes found in ug_gksstli.curprats, defined
	in gkstbl.h. If a segment is open ( i.e. UG_DCRESEG or UG_DOPNSEG 
	has been called), save the polymarker in the segment 
	and don't actually draw it unless the segment is VISIBLE. 
	(*prms).n is the number of 2D world coordinates (untransformed) 
	contained in (*prms).points. Simulation routine ug_dpolymk(prms,reply)
	may be used if desired. It will transform coordinates and
	break up the polymarker into individual markers and make n-1
	calls to the UG_DPNTOP secondary entry point if the segment is
	VISIBLE.
		The UG_DMKCINDEX routine is called every time the current
	marker color index is changed. Alternatively, the external structure
	ug_gksstli.curprats.mkbundl.color always contains the current
	marker color index. */
}

/*--------------------------------------------------------> UG_DPOLYMK3 */
uw_7475polymk3(prms,reply)			/* polymarker (3-d) */
struct {int id; Gws ws; int n; Gwpoint3 *points;} *prms;
int reply[];						/* no output parameters */
{
	/* Draw the 3D polymarker specified in (*prms). Use the current
	polymarker attributes found in ug_gksstli.curprats, defined
	in gkstbl.h. If a segment is open ( i.e. UG_DCRESEG or UG_DOPNSEG 
	has been called), save the polymarker in the segment 
	and don't actually draw it unless the segment is VISIBLE. 
	(*prms).n is the number of 3D world coordinates (untransformed) 
	contained in (*prms).points. Simulation routine ug_dpolymk(prms,reply)
	may be used if desired. It will transform coordinates and
	break up the polymarker into individual markers and make n-1
	calls to the UG_DPNTOP secondary entry point if the segment is
	VISIBLE. */
}



/*--------------------------------------------------------> UG_DTEXT */
/*********************************************************************
**    I_FUNCTION :  uw_7475text(prms,reply)				
**		display the text in (*prms).s at position (*prms).pos. Use the
**		current text attributes as found in ug_gksstli.curprats, defined in
**		gkstbl.h 
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475text(prms,reply)				/* text */
UG_dtext *prms;
int reply[];					/* no reply parameters */
/* UG_dtext defined in gksdidd.h as follows:
	typedef struct {	Gint op; Gws id;	/* opcode and workstation id
	Gwpoint3 pos;				/* starting position of text in world coords
	Gint slen;					/* length of s
	Gchar s[100];				/* text string
} UG_dtext; */

{
	int xy[2],i,n;
	Gnpoint3 npos;				/* text position in NDC coords */

	uu_denter(UU_GITRC,(us,"ws7475 gtext(%g %g %g %s)",
				(*prms).pos.x,(*prms).pos.y,(*prms).pos.z,(*prms).s));
	n = strlen((*prms).s);
	uu_denter2(UU_GITRC,(us,"PREC=%d",ug_gksstli.curprats.txbundl.fp.prec));
	uu_dexit;

	if (ug_gksstli.curprats.txbundl.fp.prec!=UG_STROKE) {
		if ((n<1)||(n>136)) {
			uu_denter2(UU_GITRC,(us,"ws7475 gtext bad strlen=%d",n));
			uu_dexit;
		}
		if (n>0) {							/* no-op for null string */
			for (i=0; i<n; i++) {
				if (((*prms).s[i]<' ')||((*prms).s[i]>126)) {
					(*prms).s[i]=' '; 			/* change all ctrl chars to blanks*/
				}
			}
			uu_denter2(UU_GITRC,(us,"7475text curvwindex=%d",
					ug_gksstli.curvwindex));
			uu_dexit;
			ug_xform((*prms).pos.x,(*prms).pos.y,(*prms).pos.z,	/* to NDC */
						&npos,ug_cxform[ug_gksstli.curvwindex]);
			uu_denter2(UU_GITRC,(us,"ws7475text after xform %g,%g,%g, curvwindex=%d",
				npos.x,npos.y,npos.z,ug_gksstli.curvwindex));
			uu_dexit;
			uw_7475ndctodev(&npos,xy,uw_7475.wid);
			/* ------
			xy[0] = xy[0] - DEVXHALF;
			xy[1] = xy[1] - DEVYHALF;
			--------- */
			/*xy[0]=npos.x*DEVXMAX - DEVXHALF;	/* change to integer NDC coords*/
			/*xy[1]=npos.y*DEVYMAX - DEVYHALF;
			/*if (uw_7475.menuxform.flag==1) g7475mxform(xy); */
			/* apply menu xform */
			/* if (uw_7475.menuxform.flag==1) uw_7475mxform(xy);	*/
			/*uw_7475ndctodev(&(*prms).pos,xy);	/* convert to device coords */
			uw_7475rastext(xy,(*prms).s);
			uu_ttflout(uw_7475.ttfd);
		}
	}
	else ug_hershy(prms,0);					/* else hershey font it */

	uu_dexit;
}	/* uw_7475text */




/*********************************************************************
**    I_FUNCTION :  uw_7475rastext(xy,s)
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475rastext(xy,p) /* put up graphics text at xy (dev coords) */
int xy[2]; char *p;

{
	int	len;
	int	xy1[2], tmpx, tmpy;
	char	num[20];

	uu_denter(UU_GITRC,(us,"uw_7475rastext(%d %d,%s)",xy[0],xy[1],p));
/*
.....Set text pen number
.....Bobby  -  7/15/91
*/
	uw_7475pen (ug_gksstli.curprats.txbundl.color);


	/*if (uw_7475.curr_text_index != curr_color)		/* current text color  */
	/*		{ */
		 /*uu_ttput(uw_7475.ttfd,"\033gB",3);			/* set color */
		 /*g7475int(ug_gksstli.curprats.txbundl.color);
		 uu_ttflout(uw_7475.ttfd); */
		 /*curr_color = uw_7475.curr_text_index;
		} */
	/*if (uw_7475.rasht != curr_rasht)					/* character height */
		/*{
					/* change character height  */
		 /*curr_rasht = uw_7475.rasht;
		} */

	if (uw_7475.isrotate)
	  {
		tmpx = -xy[1] + uw_7475.vpy;
		tmpy = xy[0];
		xy[0] = tmpx;		xy[1] = tmpy;
	uu_denter2(UU_GITRC,(us, "7475rastext,xy=%d,%d",xy[0],xy1[1]));
	uu_dexit; 
	  }
	xy[0] = xy[0] - DEVXHALF;
	xy[1] = xy[1] - DEVYHALF;

	uu_ttput(uw_7475.ttfd,"PUPA ",5);	/* pen up and move */
	sprintf(num, "%d,%d; ", xy[0],xy[1]);
	uu_ttput(uw_7475.ttfd,num,strlen(num));	
	if (uw_7475.isrotate)
		uu_ttput(uw_7475.ttfd,"DI 0,1;",7);			/* rotate the label dirextion*/
	uu_ttput(uw_7475.ttfd,"LB ",3);	/* write character */
	len = strlen(p);
	while (len-- > 0)
		 uu_ttput(uw_7475.ttfd,p++,1);
	uu_ttput(uw_7475.ttfd,"\003",1);
	if (uw_7475.isrotate)
		uu_ttput(uw_7475.ttfd,"DI 1,0;",7);	/* rotate back the label dirextion*/
	uu_ttput(uw_7475.ttfd,"PU;",3);	/* pen up */
	uu_ttflout(uw_7475.ttfd);
	uu_dexit;
}	/* uw_7475rastext */



/*--------------------------------------------------------> UG_DFLAREA */
uw_7475flarea(prms,reply) 			/* fill area */
int prms[],reply[];
{
	/* not implemented yet */
}

/*-------------------------------------------------------> UG_DFLAREA3 */
uw_7475flarea3(prms,reply) 			/* fill area 3D */
struct {int id; Gws ws; int n; Gwpoint3 *points;} *prms;
int reply[];
{
	/* display a closed 3D fill area defined by the (*prms).n points
		in (*prms).points. */
}

/*********************************************************************
**    I_FUNCTION     :  int uw_7475flarearas(prms,reply)
**			UG_DFLAREARAS jump-table entry
**       Fill area (raster)
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_7475flarearas(prms,reply)
flarearasprms *prms;
int reply[];
{
	int i,j;
	UG_line	fline;
	Gnpoint	startpt;

	uu_denter(UU_GITRC,(us,"uw_7475flarearas(npts=%d,raspts)",(*prms).n));
	j = ((*prms).n < MAX_FILLAREA_PTS) ? (*prms).n : MAX_FILLAREA_PTS;
	uw_7475devtondc(&((*prms).points[0]),&(fline.p1));
	startpt = fline.p1;
	for ( i=1; i<j; i++ )
	  {
		uw_7475devtondc(&((*prms).points[i]),&(fline.p2));
		uw_7475drwop(&fline);
		fline.p1 = fline.p2;
	  }
	fline.p2 = startpt;
	uw_7475drwop(&fline);
	uu_dexit;
}	/* uw_7475flarearas */


/*-------------------------------------------------------> UG_DCELL */
uw_7475cell(prms,reply) 				/* cell array */
UG_cell *prms;						/* UG_cell defined in gksdidd.h */
int reply[];
{
/* (*(*prms).a) is an array of color indices of size (*prms).dx by 
	(*prms).dy. Display it in parallelogram whose lower left corner
	is (*prms).np, lower right is (*prms).nq, upper left is (*prms).nr.
	P,Q,R are in ndc coordinates */
}

/*-------------------------------------------------------> UG_DGDP */
uw_7475gdp(prms,reply)					/* generalized drawing primitive */
int prms[],reply[];
{
	/* not implemented yet */
}

