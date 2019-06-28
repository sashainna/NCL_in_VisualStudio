
/*********************************************************************
**    NAME         :  atxtcom.c
**       CONTAINS:
**			ua_saveattr_txt
**			ua_restattr_txt
**			ua_setattr_txt
**			ua_getline_txt
**			ua_strg_deltas
**			ua_txt_origin
**			ua_arctxt_origin
**			ua_arctxt_parms
**			ua_get_texts
**			ua_get_prompt_text
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       atxtcom.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:40
*********************************************************************/

#include "ustrings.h"
#include "usysdef.h"
#include "usysg.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "mcrv.h"
#include "adraft.h"
#include "adrfcom.h"
#include "dasnog.h"
#include "atext.h"
#if UU_COMP==UU_WIN2K
#include "mmath.h"
#endif

/* #define	HUGE	1.000000e+030 */

static int		save_font;		/* save area for save/restore */
static Gtxprec		save_prec;
static UU_REAL        save_exp;
static UU_REAL  		save_spacing;
static UU_REAL  		save_height;
static Gtxpath		save_path;
static Gtxhor		save_alignh;
static Gtxver		save_alignv;
static UU_REAL save_charup[3];

void ua_saveattr_txt(),ua_setattr_txt(),ua_restattr_txt();

/*********************************************************************
**    I_FUNCTION :  ua_txt_origin(e,txtattr,lreset)
**       Locate the text origin according to the text attribute.
**    PARAMETERS   
**       INPUT  : 
** 			e         pointer to text entity
**			txtattr   pointer to text attribute 
**			lreset    flag to reset the origin or not
**       OUTPUT :  
**          e->position   updated text origin
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ua_txt_origin(e,txtattr,lreset)
struct UA_txt_rec	*e;             /* text record */
struct UA_txtattr_rec	*txtattr;	/* text attribute */
UU_LOGICAL	lreset;					/* flag to reset the origin if UU_TRUE */
{
	UM_vector	xaxis;
	Gwpoint3	concat;
	Gwrect3  extent;
	int		linelen;
	char		line[1025];
	int		txtpos;
	UU_REAL	curloc[3];
	UU_REAL	dist[3];
	UU_REAL	dx, dy, ddy,um_ptplndis();
	UU_REAL  len;
	UM_coord	cc, urb;
	UM_vector	vc, char_up_vector;
	UM_vector	upvc, uxvc, vc0, vc1, vc2;
	UU_LOGICAL	first = UU_TRUE;

	uu_denter(UU_STRC,(us, "enter ua_txt_origin,position=<%g,%g,%g>",
		e->position[0],e->position[1],e->position[2]));
	ua_saveattr_txt();
	ua_setattr_txt(txtattr);

	txtpos = 0;
	dy = dx = 0.0;
	um_cross(txtattr->up,txtattr->plane,xaxis);
	uu_dprint(UU_STRC,(us,"plane=<%g,%g,%g>,up=<%g,%g,%g>,xaxis=<%g,%g,%g>",
		txtattr->plane[0], txtattr->plane[1], txtattr->plane[2],
		txtattr->up[0], txtattr->up[1], txtattr->up[2],
		xaxis[0],xaxis[1],xaxis[2]));
	um_vctovc((*e).position,curloc);

/*
.....Adjust vectors for character path
*/
	um_vctovc(txtattr->up,char_up_vector);
	switch (txtattr->path)
	{
/*
........Left
*/
	case 1:
		um_vctmsc(xaxis,-1.,xaxis);
		break;
/*
........Up
*/
	case 2:
		um_vctmsc(char_up_vector,-1.,char_up_vector);
/*
		um_vctmsc(xaxis,-1.,vc1);
		um_vctmsc(char_up_vector,-1.,xaxis);
		um_vctovc(vc1,char_up_vector);
*/
		break;
/*
........Down
*/
	case 3:
/*
		um_vctmsc(xaxis,-1.,vc1);
		um_vctovc(char_up_vector,xaxis);
		um_vctovc(vc1,char_up_vector);
*/
		break;
	}

		/*---  Calculate the box's dx and dy ---*/
	linelen = ua_getline_txt((*e).tchar,(*e).no_tchar-1,line,&txtpos);
	if (txtattr->path <= 1)
		um_vctmsc(char_up_vector,-(txtattr->height*(txtattr->line_spacing+1)),
			dist);

	while (linelen > 0)
	  {
		gqtextextent3(UD_ksws,curloc,line,&concat,&extent);
		if (first)
		  {
			um_vctovc(&extent.urb,urb);
			first = UU_FALSE;
		  }
		uu_dprint(UU_STRC,(us,"textextent returns <%g,%g,%g>, <%g,%g,%g>",
			extent.llf.x,extent.llf.y,extent.llf.z,
			extent.urb.x,extent.urb.y,extent.urb.z));
		um_vcmnvc(&extent.urb,&extent.llf,vc); /* get the diagonal vector */
		len = fabs(um_dot(vc,xaxis));					 /* get dx */
		uu_dprint(UU_STRC,(us,"vc=%g,%g,%g; len=%g",vc[0],vc[1],vc[2],len));
		if (txtattr->path == 2 || txtattr->path == 3)
		{
			dx += len;
			um_vctmsc(xaxis,len,dist);
		}
		else
		{
			if (len > dx) dx = len;
		}
		linelen = ua_getline_txt((*e).tchar,(*e).no_tchar-1,line,&txtpos);
		um_vcplvc(curloc,dist,curloc);
	  }

	um_vcmnvc(&extent.llf,urb,vc); /* get the diagonal vector */
	dy = fabs(um_dot(vc,char_up_vector));					 /* get dy */

/*
....Get the distance from e->position to plane(extent low left corner, char_up_vector)
*/
	ddy = um_ptplndis(e->position,&extent.llf,char_up_vector);

	uu_dprint(UU_STRC,(us,"ddy=%g",ddy));

	cc[0] = 0.;
	if (txtattr->path > 1) cc[0] = (extent.urb.x-extent.llf.x) / 2.;
	if (!lreset)
	{
		(*e).dx = dx;
		(*e).dy = dy;
	}
	ua_restattr_txt();

	switch( (*txtattr).entity_site )
	{
	 case UA_TOP_LEFT:
		cc[0] = cc[0];
		cc[1] = ddy - dy;
		cc[2] = 0.0;
		break;

	 case UA_MIDDLE_LEFT:
		cc[0] = cc[0];
		cc[1] = ddy - (dy/2.0);
		cc[2] = 0.0;
		break;

	 case UA_BOTTOM_LEFT:
		cc[0] = cc[0];
		cc[1] = ddy;
		cc[2] = 0.0;
		break;

	 case UA_TOP_CENTER:
		cc[0] = cc[0]-dx/2.0;
		cc[1] = ddy - dy;
		cc[2] = 0.0;
		break;

	 case UA_MIDDLE_CENTER:
		cc[0] = cc[0]-dx/2.0;
		cc[1] = ddy - (dy/2.0);
		cc[2] = 0.0;
		break;

 	 case UA_BOTTOM_CENTER:
		cc[0] = cc[0]-dx/2.0;
		cc[1] = ddy;
		cc[2] = 0.0;
		break;

	 case UA_TOP_RIGHT:
		cc[0] = cc[0]-dx;
		cc[1] = ddy - dy;
		cc[2] = 0.0;
		break;

	 case UA_MIDDLE_RIGHT:
		cc[0] = cc[0]-dx;
		cc[1] = ddy - (dy/2.0);
		cc[2] = 0.0;
		break;

	 case UA_BOTTOM_RIGHT:
		cc[0] = cc[0]-dx;
		cc[1] = ddy;
		cc[2] = 0.0;
		break;
	}

	um_unitvc(char_up_vector,upvc);
	um_unitvc(xaxis,uxvc);
	um_vctmsc(uxvc,cc[0],vc0);		/* get -x vector */
	um_vctmsc(upvc,cc[1],vc1);		/* get up vector */
	um_vcplvc(vc0,vc1,vc2);

	if (!lreset)
		um_vcplvc(e->position,vc2,e->position);	/* get text origin */
	else
		um_vcmnvc(e->position,vc2,e->position);	/* reset text origin */
	uu_dprint(UU_STRC,(us,"origin=<%g,%g,%g>",e->position[0],e->position[1],
				 e->position[2]));
	uu_dexit;
} 	/* ua_txt_origin */

/*********************************************************************
**    I_FUNCTION :  UU_LOGICAL ua_arctxt_origin(e, txtattr, arc, clockwise)
**       Locate the text origin on an arc.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : UU_TRUE if everything is ok.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_arctxt_origin(e, txtattr, arc, clockwise)
struct UA_txt_rec		*e;				/* text record */
struct UA_txtattr_rec	*txtattr;	/* text attribute */
struct UM_circle_rec *arc;				/* arc record  */
UU_LOGICAL	clockwise;					/* is text go clockwise? */
{
	UU_REAL	radius, ang;
	UU_REAL	xlen, ylen;
	UU_REAL	svec[3], yaxis[3];
	UU_REAL	xcomp[3], ycomp[3];
	UU_REAL	endpt[3];
	UU_REAL	scale, num;
	int		mod;
	UU_LOGICAL	status, ud_yesno();


	uu_denter(UU_STRC,(us,"enter ua_arctxt_origin,txtdite=%d",txtattr->entity_site));

	um_vcmnvc(e->position,arc->center,svec);
	um_unitvc(svec,svec);
	um_cross(txtattr->plane,svec,yaxis);
	switch ((*txtattr).entity_site)
	{
	 case UA_TOP_LEFT:
	 case UA_MIDDLE_LEFT:
	 case UA_BOTTOM_LEFT:
		scale = 0;
		break;

	 case UA_TOP_CENTER:
	 case UA_MIDDLE_CENTER:
 	 case UA_BOTTOM_CENTER:
		scale = .5;
		break;

	 case UA_TOP_RIGHT:
	 case UA_MIDDLE_RIGHT:
	 case UA_BOTTOM_RIGHT:
		scale = 1;
		break;
	}

	status = ua_arctxt_parms(e, txtattr, arc, clockwise,&radius,&ang);
	if (fabs(ang) > UA_TXT_TWOPI)		/* text is longer than a circle */
	  {
/*		if(ud_yesno(0, "Text is longer than whole circle, continue?", "Question?")==UU_FALSE)*/
	  	  {
			status = UU_FALSE;
			goto done;
	  	  }
	  }
	ang = ang*scale;
	num = ang / UA_TXT_TWOPI;
	mod = num;
	if ((UU_REAL)mod > num)		mod = mod - 1;
	ang = ang - (UA_TXT_TWOPI*mod);
	uu_dprint(UU_STRC,(us,"mod=%d,ang =%g",mod,ang));

	xlen = radius * cos(ang);
	um_vctmsc(svec,xlen,xcomp);
	ylen = radius * sin(ang);
	um_vctmsc(yaxis,ylen,ycomp);
	um_vcplvc(xcomp,ycomp,endpt);
	um_vcplvc(arc->center,endpt,e->position);
	uu_dprint(UU_STRC,(us,"position=%g,%g,%g",e->position[0],e->position[1],
					 e->position[2]));
	/* Use upvec as a sign of text direction, if the text direction is clockwise
		then the upvec has the same direction of arc center to the text origin.
		Otherwise, it is opsite. */
	um_vcmnvc(e->position,arc->center,txtattr->up);
	um_unitvc(txtattr->up, txtattr->up);
	if (!clockwise)
		um_vctmsc(txtattr->up,(UU_REAL) -1.0,txtattr->up);

done:
	uu_dexit;
	return(status);
} 	/* ua_txt_origin */

/*********************************************************************
**    I_FUNCTION :  ua_arctxt_parms(e, txtattr, arc, clockwise,radius,ang,ifl)
**       Locates the delta angle and radius of text on an arc.
**    PARAMETERS   
**       INPUT  : 
**          e          Annotation entity.
**          txtattr    Text attributes.
**          arc        Arc entity.
**          clockwise  UU_TRUE = CLW, UU_FALSE = CCLW
**       OUTPUT :  
**          radius     Radius of arc as referenced by text origin.
**          angle      Delta angle of text.
**    RETURNS      : UU_TRUE if everything is ok.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_arctxt_parms(e, txtattr, arc, clockwise,radius,angle)
struct UA_txt_rec		*e;				/* text record */
struct UA_txtattr_rec	*txtattr;	/* text attribute */
struct UM_circle_rec *arc;				/* arc record  */
UU_LOGICAL	clockwise;					/* is text go clockwise? */
UU_REAL *radius,*angle;
{
	int	linelen;
	char	*chptr;
	Gwpoint3		concat;
	Gwrect3		extent;
	int	ind;
	char	str[2];
	UU_REAL	chdist, theda;
	UU_REAL	svec[3], yaxis[3];
	UU_REAL		textdir;			/* sign of text direction */
	Gwpoint3 plane, *save_plane, *gqtxplane();
	Gwpoint3 tmpup, *save_up;
	Gwpoint  *gqcharup();
	UU_LOGICAL	status;
	UU_REAL	sign;

	ua_saveattr_txt();
	ua_setattr_txt(txtattr);

	linelen = e->no_tchar;
	*angle = 0.0;
	ind = 0;
	save_plane = gqtxplane();
	save_up = (Gwpoint3 *)gqcharup();          
		/* set the regular construction plane to get the character's width */
	plane.x=0.0;  	plane.y=0.0;  	plane.z=1.0;
	tmpup.x=0.0;  	tmpup.y=1.0;  	tmpup.z=0.0;
 	gstxplane(&plane);
	gscharup3(&tmpup);
	*radius = um_dcccc(arc->center,e->position);
	um_vcmnvc(e->position,arc->center,svec);
	um_unitvc(svec,svec);
	um_cross(txtattr->plane,svec,yaxis);

	switch ((*txtattr).entity_site)
	{
 	case UA_TOP_LEFT:
 	case UA_TOP_CENTER:
 	case UA_TOP_RIGHT:
		*radius = *radius - txtattr->height;
		break;

 	case UA_MIDDLE_LEFT:
 	case UA_MIDDLE_CENTER:
 	case UA_MIDDLE_RIGHT:
		*radius = *radius - txtattr->height/2.;
		break;
	}

	/* user text direction and the text path to decide the real text direction*/
	textdir = clockwise? 1.0 : -1.0;
	sign = (txtattr->path==(int)UG_TP_LEFT)? -textdir : textdir; 

		/* since different character might have different width we have to sum up
			the individual width */
	ind =0;
	str[1] = '\0';
	chptr = e->tchar;
	theda = asin(0.);
   while (ind++<linelen)
	  {
		str[0] = *chptr++;
		gqtextextent3(UD_ksws,e->position,str,&concat,&extent);
		chdist = extent.urb.x - extent.llf.x;
		theda = chdist / (2.*(*radius));
		theda = asin(theda);
		theda = sign*2.0*theda;
		*angle = *angle + theda;
	  }

	status = UU_TRUE;
done:
 	gstxplane(save_plane);
	gscharup3(save_up);
	ua_restattr_txt();
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  void	ua_strg_deltas(text_block)
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

void	ua_strg_deltas(text_block)
struct UA_txt_blk	(*text_block);
{
	UU_REAL	concat[3];
	UU_REAL	bounds[2][3];

	uu_denter(UU_STRC,(us,"ua_strg_deltas(text_block=%s)", "..."));

	{
		UU_REAL	us_t146[3];
		us_t146[0] = 0.000000e+000;
		us_t146[1] = 0.000000e+000;
		us_t146[2] = 0.000000e+000;
		us_textextent(us_t146,(*text_block).tstring,concat,bounds);
	}
	(*text_block).dx = bounds[2-1][0];
	(*text_block).dy = bounds[2-1][1];
	uu_dexit;
} 	/* ua_strg_deltas */


/*********************************************************************
**    I_FUNCTION :  ua_saveattr_txt()
**			Save GKS text attributes.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_saveattr_txt()
{
	UU_REAL sgqcharspace(),sgqcharexp(),sgqcharheight();
	uu_denter(UU_STRC,(us,"ua_saveattr_txt()"));

	save_font = sgqtextfont();
	save_prec = (Gtxprec)sgqtextprec();
	save_exp = sgqcharexp();
	save_spacing = sgqcharspace();
	save_height = sgqcharheight();
	save_path = (Gtxpath)sgqtextpath();
	save_alignh = (Gtxhor)sgqtextalignh();
	save_alignv = (Gtxver)sgqtextalignv();
	sgqcharup(save_charup);
	uu_dexit;
} /* ua_saveattr_txt */


/*********************************************************************
**    I_FUNCTION :  ua_setattr_txt(txtattr)
**       Set GKS text attributes from text attribute.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_setattr_txt(txtattr)
struct UA_txtattr_rec	*txtattr;
{
	Gtxfp	fp;
	Gtxalign	align;
	int		status;

	uu_denter(UU_STRC,(us,"enter ua_setattr_txt"));

	fp.font = (*txtattr).font;
	fp.prec = (Gtxprec)(*txtattr).prec;
	status = gstextfp(&fp);
	status = gscharexp((*txtattr).expn);
	status = gscharspace((*txtattr).spacing);
	status = gscharheight((*txtattr).height);
	status = gstextpath((*txtattr).path);
/*----
	switch( (*e).txt_just )
	{
	case 0:
		{
			align.hor = UG_TH_LEFT;
		}
		break;
	case 1:
		{
			align.hor = UG_TH_RIGHT;
		}
		break;
	case 2:
		{
			align.hor = UG_TH_CENTRE;
		}
		break;
	}
---*/
	align.hor = (Gtxhor)(*txtattr).align_hor;
	/* align.ver = UG_TV_BOTTOM; */
	align.ver = (Gtxver)(*txtattr).align_ver;
	status = gstextalign(&(align));
/*--- ua_getcpln(&((*e)),cporig,cpxaxis,cpyaxis,cpzaxis); ---*/

	status = gstxplane(txtattr->plane);
	status = gscharup3(txtattr->up);

/*----
	status = gstxplane(cpzaxis);
	{
		UU_REAL	us_t144[3];
		ua_get_upvector(&((*e)),(*e).txt_blk[itxt-1].tangle,us_t144);
		status = gscharup3(us_t144);
	}
---*/
	uu_dexit;
} /* ua_setattr_txt */

/*********************************************************************
**    I_FUNCTION :  int ua_getline_txt(stg, stglen, line, pos)
**       Get the next line from the text string - lines are terminatted 
**			by a carriage-return.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_getline_txt(stg, stglen, line, pos)
char		stg[];
int		stglen;
char		line[];
int		*pos;
{
	int		linelen;
	int		posbeg;

	uu_denter(UU_STRC,(us,"ua_getline_txt(stg=%s, stglen=%d, line=%s, pos=%d)",
									stg, stglen, line, *pos));

	if (*pos >= stglen )
	  {
		uu_dexit;
		return(0);
	  }
	linelen = 0;
	posbeg = *pos;
	for(;;)
	  {
		if (!((*pos < stglen)&&(stg[*pos] != '\n'))) 
		    break;
		*pos = (*pos)+1;
		linelen = linelen + 1;
	  }
	if (linelen>0)
	  {
		strncpy(line, stg+posbeg, (*pos)-posbeg);
		line[(*pos)-posbeg] = '\0';
	  }
	else
		strcpy(line,"");
	*pos = (*pos)+1;
	uu_dprint(UU_STRC,(us,"line=%s",line));
	uu_dexit;
	return(linelen);
} 	/* ua_getline_txt */

/*********************************************************************
**    I_FUNCTION :  void	ua_restattr_txt()
**       Restore current GKS text attributes from pribate storage
**			set by ua_saveattr_txt.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ua_restattr_txt()
{
	Gtxfp	fp;
	Gtxalign	align;
	int		status;

	uu_denter(UU_STRC,(us,"ua_restattr_txt()"));

	fp.font = save_font;
	fp.prec = save_prec;
	status = gstextfp(&(fp));
	status = gscharexp(save_exp);
	status = gscharspace(save_spacing);
	status = gscharheight(save_height);
	status = gstextpath(save_path);
	align.hor = save_alignh;
	align.ver = save_alignv;
	status = gstextalign(&(align));
	status = gscharup3(save_charup);
	uu_dexit;
} 	/* ua_restattr_txt */

/*********************************************************************
**    I_FUNCTION :  ua_get_texts(prompt, maxsize, text, num_chars, num_line)
**       Get user's input string for the note.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void ua_get_texts(prompt, maxsize, text, num_chars, num_lines)
int prompt;
int maxsize;
char text[];
int *num_chars;
int *num_lines;
{
	int i,nc,nco,linenum;
	int *ans;
	char tbuf[1025];
/*
.....Get the note text
*/
	strcpy(tbuf,text);
	ans = (int *)tbuf;
	ud_form("anotes.frm",&ans,&ans);
	nc = strlen(tbuf);
	if (nc >= maxsize)
	{
		nc = maxsize - 1;
		tbuf[maxsize-1] = '\0';
	}
	strcpy(text,"");
	nco = 0;
	linenum = 0;
/*
.....Strip carriage returns from text
.....leaving new line characters
*/
	for (i=0;i<nc;i++)
	{
		if (tbuf[i] != '\r') text[nco++] = tbuf[i];
		if (tbuf[i] == '\n' && i != nc-1) linenum++;
	}
	text[nco] = '\0';
	if (nco > 0 && tbuf[nc-1] != '\n')
	{
		strcat(text,"\n");
		nco++;
		linenum++;
	}
	*num_chars = nco;
	*num_lines = linenum;
}	/* ua_get_texts */

/*********************************************************************
**    I_FUNCTION :  ua_get_prompt_text(prompt,text,num_chars)
**       Put out the specific prompt then get the user's message back.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void	ua_get_prompt_text(prompt,text,num_chars)
int		prompt;
char		text[];
int		*num_chars;

{
	int		char_cnt;
	char		line_text[UA_MAX_TEXT_LEN];
	int		status;

	uu_denter(UU_STRC,(us,"ua_get_prompt_text(prompt=%s)",prompt));

	strcpy(text,"");
	*num_chars = 0;
	do
	  {
		status = ud_ddas(UD_DASSTRING,prompt,line_text,UA_MAX_TEXT_LEN,&char_cnt,
								UD_NODEFAULT);
		if (char_cnt <= 0) 	
			break;
		else
		  {
			*num_chars = *num_chars + char_cnt + 1;
			if (*num_chars >= UA_TEXT_BUFSZ)
			  break;
		  }
		strcat(text,line_text);
		strcat(text,"\n");
	  } while (UU_TRUE);
	uu_dexit;
}	/* ua_get_texts */


