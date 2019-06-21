/********************************************************************* 
**  NAME:  ws7580out.c
**
**      output functions section.
**
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       ws758out.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:05
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ws7580.h"
#include "ws7580e.h"
#include "ginqatti.h"


extern Gs7580 uw_7580;					/* declare workstation local data */
typedef struct {int id; Gws ws; int n; Gipoint *points;} flarearasprms;

/*------------------------- local declarations --------------------------*/

/*------------------------- output primitives ---------------------------*/

/*-------------------------------------------------------> UG_DPOLYLN */
uw_7580polyln(prms,reply) 			/* polyline */
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
**    I_FUNCTION :  uw_7580polyln3(prms,reply)			
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

uw_7580polyln3(prms,reply)			/* polyline (3-d) */
struct {int id; Gws ws; int n; Gwpoint3 *points;} *prms;

{
	uu_denter(UU_GITRC,(us,"enter uw_7580polyln3"));

	/* Added to display motion on quick plot. kathy */
	if (ug_gksstli.curvwindex == 0)
		ug_gksstli.curvwindex=1;
/*
.....Removed as was not in 'uw_7475polyln3'
.....Bobby  -  8/9/91
*/
/*	ug_dredrawvis();*/

/*
.....Added separate check for line type & width
.....as 'ug_d5polyln3' will plot line types on curves
.....but will not plot line widths while 'ug_d4polyln3'
.....will plot most line types and all line widths.
.....This is because curves are broken up into multiple
.....short polylines when drawn, which are not long enough
.....to generate dashes.
.....Bobby  -  8/9/91
*/
	if (((ug_gksstli.curprats.lnbundl.type.typeno>=2) &&
		 (ug_gksstli.curprats.lnbundl.type.typeno<=8)))
		ug_d5polyln3(prms);
	else if ((gqlinewidth() >= 2.0))
		ug_d4polyln3(prms);	/* software simulate all the line type */
	else 						/* linetype<2 = solid line */
		ug_dpolyln3(prms);
	uu_dexit;
}	/* uw_7580polyln3 */


/*--------------------------------------------------------> UG_DPOLYMK */
uw_7580polymk(prms,reply)				/* polymarker */
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
uw_7580polymk3(prms,reply)			/* polymarker (3-d) */
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
**    I_FUNCTION :  uw_7580text(prms,reply)				
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

uw_7580text(prms,reply)				/* text */
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

	uu_denter(UU_GITRC,(us,"ws7580 gtext(%g %g %g %s)",
				(*prms).pos.x,(*prms).pos.y,(*prms).pos.z,(*prms).s));
	n = strlen((*prms).s);
	uu_denter2(UU_GITRC,(us,"PREC=%d",ug_gksstli.curprats.txbundl.fp.prec));
	uu_dexit;
	if (ug_gksstli.curprats.txbundl.fp.prec!=UG_STROKE) {
		if ((n<1)||(n>136)) {
			uu_denter2(UU_GITRC,(us,"ws7580 gtext bad strlen=%d",n));
			uu_dexit;
		}
		if (n>0) {							/* no-op for null string */
			for (i=0; i<n; i++) {
				if (((*prms).s[i]<' ')||((*prms).s[i]>126)) {
					(*prms).s[i]=' '; 			/* change all ctrl chars to blanks*/
				}
			}
			uu_denter2(UU_GITRC,(us,"7580text curvwindex=%d",
					ug_gksstli.curvwindex));
			uu_dexit;
			ug_xform((*prms).pos.x,(*prms).pos.y,(*prms).pos.z,	/* to NDC */
						&npos,ug_cxform[ug_gksstli.curvwindex]);
			uu_denter2(UU_GITRC,(us,"ws7580text after xform %g,%g,%g, curvwindex=%d",
				npos.x,npos.y,npos.z,ug_gksstli.curvwindex));
			uu_dexit;
			uw_7580ndctodev(&npos,xy,uw_7580.wid);
			/* ------
			xy[0] = xy[0] - DEVXHALF;
			xy[1] = xy[1] - DEVYHALF;
			--------- */
			/*xy[0]=npos.x*DEVXMAX - DEVXHALF;	/* change to integer NDC coords*/
			/*xy[1]=npos.y*DEVYMAX - DEVYHALF;
			/*if (uw_7580.menuxform.flag==1) g7580mxform(xy); */
			/* apply menu xform */
			/* if (uw_7580.menuxform.flag==1) uw_7580mxform(xy);	*/
			/*uw_7580ndctodev(&(*prms).pos,xy);	/* convert to device coords */
			uw_7580rastext(xy,(*prms).s);
			uu_ttflout(uw_7580.ttfd);
		}
	}
	else ug_hershy(prms,0);					/* else hershey font it */

	uu_dexit;
}	/* uw_7580text */




/*********************************************************************
**    I_FUNCTION :  uw_7580rastext(xy,s)
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

uw_7580rastext(xy,p) /* put up graphics text at xy (dev coords) */
int xy[2]; char *p;

{
	int	len;
	int	xy1[2], tmpx, tmpy;
	char	num[20];

	uu_denter(UU_GITRC,(us,"uw_7580rastext(%d %d,%s)",xy[0],xy[1],p));
/*
.....Set text pen number
.....Bobby  -  8/9/91
*/
	uw_7580pen (ug_gksstli.curprats.txbundl.color);

	/*if (uw_7580.curr_text_index != curr_color)		/* current text color  */
	/*		{ */
		 /*uu_ttput(uw_7580.ttfd,"\033gB",3);			/* set color */
		 /*g7580int(ug_gksstli.curprats.txbundl.color);
		 uu_ttflout(uw_7580.ttfd); */
		 /*curr_color = uw_7580.curr_text_index;
		} */
	/*if (uw_7580.rasht != curr_rasht)					/* character height */
		/*{
					/* change character height  */
		 /*curr_rasht = uw_7580.rasht;
		} */

	if (uw_7580.isrotate)
	  {
		tmpx = -xy[1] + uw_7580.vpy;
		tmpy = xy[0];
		xy[0] = tmpx;		xy[1] = tmpy;
/*
.....X half limits become Y half limits
.....when rotating plot
.....Bobby  -  8/26/91
*/
		xy[0] = xy[0] - DEVYHALF_758;
		xy[1] = xy[1] - DEVXHALF_758;
	uu_denter2(UU_GITRC,(us, "758rastext,xy=%d,%d",xy[0],xy1[1]));
	uu_dexit; 
	  }
	else
	{
		xy[0] = xy[0] - DEVXHALF_758;
		xy[1] = xy[1] - DEVYHALF_758;
	}

	uu_ttput(uw_7580.ttfd,"PUPA ",5);	/* pen up and move */
	sprintf(num, "%d,%d; ", xy[0],xy[1]);
	uu_ttput(uw_7580.ttfd,num,strlen(num));	
	if (uw_7580.isrotate)
		uu_ttput(uw_7580.ttfd,"DI 0,1;",7);			/* rotate the label dirextion*/
	uu_ttput(uw_7580.ttfd,"LB ",3);	/* write character */
	len = strlen(p);
	while (len-- > 0)
		 uu_ttput(uw_7580.ttfd,p++,1);
	uu_ttput(uw_7580.ttfd,"\003",1);
	if (uw_7580.isrotate)
		uu_ttput(uw_7580.ttfd,"DI 1,0;",7);	/* rotate back the label dirextion*/
	uu_ttput(uw_7580.ttfd,"PU;",3);	/* pen up */
	uu_ttflout(uw_7580.ttfd);
	uu_dexit;
}	/* uw_7580rastext */



/*--------------------------------------------------------> UG_DFLAREA */
uw_7580flarea(prms,reply) 			/* fill area */
int prms[],reply[];
{
	/* not implemented yet */
}

/*-------------------------------------------------------> UG_DFLAREA3 */
uw_7580flarea3(prms,reply) 			/* fill area 3D */
struct {int id; Gws ws; int n; Gwpoint3 *points;} *prms;
int reply[];
{
	/* display a closed 3D fill area defined by the (*prms).n points
		in (*prms).points. */
}


/*********************************************************************
**    I_FUNCTION     :  int uw_7580flarearas(prms,reply)
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
int uw_7580flarearas(prms,reply)
flarearasprms *prms;
int reply[];
{
	int i,j;
	UG_line	fline;
	Gnpoint	startpt;

	uu_denter(UU_GITRC,(us,"uw_7580flarearas(npts=%d,raspts)",(*prms).n));
	j = ((*prms).n < MAX_FILLAREA_PTS) ? (*prms).n : MAX_FILLAREA_PTS;
	uw_7580devtondc(&((*prms).points[0]),&(fline.p1));
	startpt = fline.p1;
	for ( i=1; i<j; i++ )
	  {
		uw_7580devtondc(&((*prms).points[i]),&(fline.p2));
		uw_7580drwop(&fline);
		fline.p1 = fline.p2;
	  }
	fline.p2 = startpt;
	uw_7580drwop(&fline);
	uu_dexit;
}	/* uw_7580flarearas */


/*-------------------------------------------------------> UG_DCELL */
uw_7580cell(prms,reply) 				/* cell array */
UG_cell *prms;						/* UG_cell defined in gksdidd.h */
int reply[];
{
/* (*(*prms).a) is an array of color indices of size (*prms).dx by 
	(*prms).dy. Display it in parallelogram whose lower left corner
	is (*prms).np, lower right is (*prms).nq, upper left is (*prms).nr.
	P,Q,R are in ndc coordinates */
}

/*-------------------------------------------------------> UG_DGDP */
uw_7580gdp(prms,reply)					/* generalized drawing primitive */
int prms[],reply[];
{
	/* not implemented yet */
}


typedef Gfloat point3[3];
/*********************************************************************
**    I_FUNCTION     :  uw_7580polylnras(prms) -- raster polyline 
**    PARAMETERS   
**       INPUT  : 
**          input
**				struct {
**					int id;
**					Gws ws;
**					int n;
**					Gipoint  *points;
**				} *prms			structure contains the function id,
**									workstation id, number of points,
**									and a pointer to an array of raster coords
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_7580polylnras(prms)
struct {int id; Gws ws; int n; Gipoint  *points;} *prms;
{
	int i;
	Gws ws;
	Gdpoint3 pos1,pos2;					/* really are float raster points */
	Gfloat dist;
	point3 *points;
	Gfloat width;
	int	p1[3], p2[3];

	uu_denter(UU_GITRC,(us,"uw_7580polylnras(ws=%d,n=%d, points[0]=%d %d)",
			(*prms).ws,(*prms).n,(*prms).points[0].x,(*prms).points[0].y));

	ws=(*prms).ws;
	if (ug_find.find==UU_FALSE){
		if((width = gqlinewidth()) < 2.0)
			{
			for (i=0; i<(*prms).n-1; i++)	
			  {
				/* call workstation's UG_DRASLINE entry to draw one line */
				/*--
				(*(ug_gksstli.wsopen[ws].connid)[UG_DDRWOP])
					((*prms).points[i].x,(*prms).points[i].y,
			 		(*prms).points[i+1].x,(*prms).points[i+1].y);
				---*/
				p1[0] = (*prms).points[i].x;
				p1[1] = (*prms).points[i].y;
			 	p2[0] = (*prms).points[i+1].x;
				p2[1] = (*prms).points[i+1].y;
				uwi_7580drwop1(p1,p2);
			  }
			}
		else
			{
			points = (point3*)uu_toolmalloc(prms->n * sizeof(point3));
			for (i=0; i<(*prms).n; i++)	
				points[i][0] = (Gfloat)prms->points[i].x;
				points[i][1] = (Gfloat)prms->points[i].y;
				points[i][2] = 0.0;
			ug_dlinewidth(ws,width, prms->n, points);
			uu_toolfree(points);
			}
		}
	else {											/* finding, don't draw */
		for (i=0; i<(*prms).n-1; i++)	{
			/* see if the line is within eps of ug_find.x,y */
			pos1.x=(*prms).points[i].x; pos1.y=(*prms).points[i].y; 
			pos1.z=0.0;
			pos2.x=(*prms).points[i+1].x; pos2.y=(*prms).points[i+1].y; 
			pos2.z=0.0;
			if (ug_closln(&ug_find.x,&pos2,&pos1,ug_find.epsx,
				 ug_find.epsy,&dist,ug_find.pikmode)==1) {	/* found it */
				 ug_find.found=UU_TRUE; ug_find.dist=dist;
				 ug_find.epsx=dist; ug_find.epsy=dist;
			}
		}
	}
	uu_dexit;
} /* uw_7580polylnras */

/*
.....added by Yurong
.....8/4/97
*/
uw_758advan_frame()
{
	uu_ttput(uw_7580.ttfd, "FR;",3);
}

