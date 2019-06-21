/********************************************************************* 
**  NAME:  ws104out.c
**
**      output functions section.
**
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws104out.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:04
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
#include	"umath.h"
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "ginqatti.h"
#include "gerror.h"
#include "ws104.h"


extern Gs104 uw_104;				/* declare workstation local data */
extern WSPTINFO	jpbuf[];		/* information from pass 1 */
extern int	uw_1043ind;
typedef struct {int id; Gws ws; int n; Gipoint *points;} flarearasprms;

/*------------------------- local declarations --------------------------*/

/*------------------------- output primitives ---------------------------*/

/*-------------------------------------------------------> UG_DPOLYLN */
uw_104polyln(prms,reply) 			/* polyline */
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
**	I_FUNCTION :  uw_104polyln3(prms,reply)
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
**	PARAMETERS
**		INPUT  :
**			input
**		OUTPUT :
**			output
**	RETURNS        : none
**	SIDE EFFECTS   : none
**	WARNINGS       : none
******************************************************************/

uw_104polyln3(prms,reply)			/* polyline (3-d) */
struct {int id; Gws ws; int n; Gwpoint3 *points;} *prms;
/*
.....Added this routine using 'ws758out.c' as an example
.....Bobby  -  8/26/91
*/
{

/*
.....Display motion on quick plot
*/
	if (ug_gksstli.curvwindex == 0) ug_gksstli.curvwindex=1;
/*
.....'ug_d5polyln3' draws various line types correctly
*/
	if (((ug_gksstli.curprats.lnbundl.type.typeno>=2) &&
		 (ug_gksstli.curprats.lnbundl.type.typeno<=8)))
		ug_d5polyln3(prms);
/*
.....'ug_d4polyln3' draws line widths correctly
*/
	else if ((gqlinewidth() >= 2.0))
		ug_d4polyln3(prms);
/*
.....Solid line
*/
	else
		ug_dpolyln3(prms);
}

/*--------------------------------------------------------> UG_DPOLYMK */
uw_104polymk(prms,reply)				/* polymarker */
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
uw_104polymk3(prms,reply)			/* polymarker (3-d) */
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
**    I_FUNCTION :  uw_104text(prms,reply)				
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

uw_104text(prms,reply)				/* text */
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

	uu_denter(UU_GITRC,(us,"ws104 gtext(%g %g %g %s)",
				(*prms).pos.x,(*prms).pos.y,(*prms).pos.z,(*prms).s));
	n = strlen((*prms).s);
	uu_denter2(UU_GITRC,(us,"PREC=%d",ug_gksstli.curprats.txbundl.fp.prec));
	uu_dexit;
	if (ug_gksstli.curprats.txbundl.fp.prec!=UG_STROKE) {
		if ((n<1)||(n>136)) {
			uu_denter2(UU_GITRC,(us,"ws104 gtext bad strlen=%d",n));
			uu_dexit;
		}
		if (n>0) {							/* no-op for null string */
			for (i=0; i<n; i++) {
				if (((*prms).s[i]<' ')||((*prms).s[i]>126)) {
					(*prms).s[i]=' '; 			/* change all ctrl chars to blanks*/
				}
			}
			uu_denter2(UU_GITRC,(us,"104text curvwindex=%d",
					ug_gksstli.curvwindex));
			uu_dexit;
			ug_xform((*prms).pos.x,(*prms).pos.y,(*prms).pos.z,	/* to NDC */
						&npos,ug_cxform[ug_gksstli.curvwindex]);
			uu_denter2(UU_GITRC,(us,"ws104text after xform %g,%g,%g, curvwindex=%d",
				npos.x,npos.y,npos.z,ug_gksstli.curvwindex));
			uu_dexit;
			uw_104ndctodev(&npos,xy,uw_104.wid);
			uw_104rastext(xy,(*prms).s);
			uu_ttflout(uw_104.ttfd);
		}
	}
	else ug_hershy(prms,0);					/* else hershey font it */
	uu_dexit;
}	/* uw_104text */




/*********************************************************************
**    I_FUNCTION :  uw_104rastext(xy,s)
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

uw_104rastext(xy,p) /* put up graphics text at xy (dev coords) */
int xy[2]; char *p;

{
	int	len;
	int	xy1[2], tmpx, tmpy;

	uu_denter(UU_GITRC,(us,"uw_104rastext(%d %d,%s)",xy[0],xy[1],p));

	if (uw_104.isrotate)
	  {
		tmpx = -xy[1] + uw_104.vpy;
		xy[1] = xy[0];
		xy[0] = tmpx;		
	uu_denter2(UU_GITRC,(us, "104rastext,xy=%d,%d",xy[0],xy[1]));
	uu_dexit; 
	  }
	len = strlen(p);
	jpbuf[uw_1043ind].type = TEXT104;

	/* changed the text color. kathy
	jpbuf[uw_1043ind].color = 1;
	*/
	jpbuf[uw_1043ind].color = ug_gksstli.curprats.txbundl.color;
	jpbuf[uw_1043ind].pt1[0] = xy[0];
	jpbuf[uw_1043ind].pt1[1] = xy[1];
	jpbuf[uw_1043ind].pt2[0] = (uw_104.isrotate)? UU_TRUE : UU_FALSE;
	jpbuf[uw_1043ind].str = (char *) uu_malloc(sizeof(char)*len+1);
	strcpy(jpbuf[uw_1043ind].str, p);
	uw_1043ind++;
	if (uw_1043ind == MAXJP)
		{
		 uw_104dmpline();
		 uw_1043ind = 0;
		}

	uu_dexit;
}	/* uw_104rastext */



/*********************************************************************
**    I_FUNCTION :  uw_104symbols(ptr)
**       output the symbols and update the address
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_104symbols(ptr)
WSPTINFO	*ptr;

{
	int	pt1[2], pt2[2];
	float	xadd;
	int	temp, step[2], len;
	int	ixa, iya;
	float angle, height;
	char	length[1];

	uu_denter(UU_GITRC,(us,"uw_104symbols"));
	if (ptr->pt2[0])		/* A size paper, need to rotate */
	  angle = 1.5707963;		/* 90 degree */
	else
		angle = 0;
	height = 0.15;
	ixa = cos(angle)*STEP104*height*16/15;
 	iya = sin(angle)*STEP104*height*16/15;
	len = strlen(ptr->str);
	if ((buflen+7+5+len) > BUF104SIZE)		/* if the symbol command longer the 
														empty buffer, flush the output */
		uw_104buflout();
 	uu_ttput(uw_104.ttfd,".(", 2);			/* 907 default character set */
	uu_ttput(uw_104.ttfd,"&", 1);			/* controller symbol scalling code */
	pt1[0] = 0;		pt1[1] = 0;
	pt2[0] = ixa;	pt2[1] = iya;
	uw_104mvpen(pt1,pt2);
	uu_ttput(uw_104.ttfd, "%", 1);			/* symbol string code */
	length[0] = len + BIAS104;
	uu_ttput(uw_104.ttfd, length, 1);			/* symbol length */
	uu_ttput(uw_104.ttfd, ptr->str, len);
	buflen = buflen + 5 + len;
			/* update the address */
	xadd = 0;
	if (pt2[0] < 0)	xadd = -15;
	temp = xadd + 15*pt2[0]/16;
	step[0] = len*temp;
	xadd = 0;
	if (pt2[1] < 0)	xadd = -15;
	temp = xadd + 15*pt2[1]/16;
	step[1] = len*temp;
	ptr->pt2[0] = ptr->pt1[0] + step[0];
	ptr->pt2[1] = ptr->pt1[1] + step[1];
	uu_free(ptr->str);
	uu_dexit;

}	/* uw_104symbols */



/*--------------------------------------------------------> UG_DFLAREA */
uw_104flarea(prms,reply) 			/* fill area */
int prms[],reply[];
{
	/* not implemented yet */
}

/*-------------------------------------------------------> UG_DFLAREA3 */
uw_104flarea3(prms,reply) 			/* fill area 3D */
struct {int id; Gws ws; int n; Gwpoint3 *points;} *prms;
int reply[];
{
	/* display a closed 3D fill area defined by the (*prms).n points
		in (*prms).points. */
}


/*********************************************************************
**    I_FUNCTION     :  int uw_104flarearas(prms,reply)
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
int uw_104flarearas(prms,reply)
flarearasprms *prms;
int reply[];
{
	int i,j;
	UG_line	fline;
	Gnpoint	startpt;

	uu_denter(UU_GITRC,(us,"uw_104flarearas(npts=%d,raspts)",(*prms).n));
	j = ((*prms).n < MAX_FILLAREA_PTS) ? (*prms).n : MAX_FILLAREA_PTS;
	uw_104devtondc(&((*prms).points[0]),&(fline.p1));
	startpt = fline.p1;
	for ( i=1; i<j; i++ )
	  {
		uw_104devtondc(&((*prms).points[i]),&(fline.p2));
		uw_104drwop(&fline);
		fline.p1 = fline.p2;
	  }
	fline.p2 = startpt;
	uw_104drwop(&fline);
	uu_dexit;
}	/* uw_104flarearas */

/*-------------------------------------------------------> UG_DCELL */
uw_104cell(prms,reply) 				/* cell array */
UG_cell *prms;						/* UG_cell defined in gksdidd.h */
int reply[];
{
/* (*(*prms).a) is an array of color indices of size (*prms).dx by 
	(*prms).dy. Display it in parallelogram whose lower left corner
	is (*prms).np, lower right is (*prms).nq, upper left is (*prms).nr.
	P,Q,R are in ndc coordinates */
}

/*-------------------------------------------------------> UG_DGDP */
uw_104gdp(prms,reply)					/* generalized drawing primitive */
int prms[],reply[];
{
	/* not implemented yet */
}

#define UG_TRUE 1
#define UG_FALSE 0

typedef Gfloat point3[3];

/*********************************************************************
**    I_FUNCTION     :  uw_104polylnras(prms) -- raster polyline 
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
uw_104polylnras(prms)
struct {int id; Gws ws; int n; Gipoint  *points;} *prms;
{
	int i;
	Gws ws;
	Gdpoint3 pos1,pos2;					/* really are float raster points */
	Gfloat dist;
	char us[120];
	point3 *points;
	Gfloat width;
	int	p1[3], p2[3];


	uu_denter2(UU_GITRC,(us,"uw_104polylnras(ws=%d,n=%d, points[0]=%d %d)",
			(*prms).ws,(*prms).n,(*prms).points[0].x,(*prms).points[0].y));

	ws=(*prms).ws;
	if (ug_find.find==UG_FALSE){
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
				uwi_104drwop1(p1,p2);
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
				 ug_find.found=UG_TRUE; ug_find.dist=dist;
				 ug_find.epsx=dist; ug_find.epsy=dist;
			}
		}
	}
	uu_dexit;
} /* uw_104polylnras */
