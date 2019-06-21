
/*********************************************************************
**    NAME         :  atxtdrw.c
**       CONTAINS: support routines for text entity
**			int ua_p33_text
**			int ua_draw_text
**			int uai_draw_text
**			uai_drw_txt_on_arc
**			uai_draw_text_box
**			uai_calc_text_bounds
**			uai_calc_text_box
**			uai_calc_arctext_box
**			uai_atof
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       atxtdrw.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       08/17/15 , 18:00:20
*********************************************************************/

#include "ustrings.h"
#include "usysdef.h"
#include "usysg.h"
#include	"atext.h"
#include "ginqatt2.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "gtbl.h"	
#include "ginqdsiz.h"
#include "ginqatt.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h" 
#include "modef.h" 
#include "mattr.h"
#include "mdebug.h"
#include	"adrfcom.h"

void uai_calc_arctext_box();
static void S_draw_text_displst();

/*********************************************************************
**    E_FUNCTION : ua_p33_text(eptr)
**				This function prints the text string of a text entity.
**    PARAMETERS   
**       INPUT  : 
**          eptr			pointer to the text entity to be printed.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/

int ua_p33_text(eptr)
struct UA_txt_rec *eptr;

{
	struct UA_txtattr_rec	attr;
	int 							status;
	UU_LOGICAL					blanked;

	uu_denter(UU_STRC,(us,"ua_p33_text(eptr->key:%d)", eptr->key));

	status = UU_FAILURE; /* assume failure */

	sprintf(UM_sbuf,"TEXT ENTITY %d", eptr->key);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf,"arckey %d", eptr->arckey);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf,"	%s", eptr->tchar);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf, "	no_tchar:%d position:%g,%g,%g,",
		eptr->no_tchar, eptr->position[0], eptr->position[1], eptr->position[1]);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf, "	dx:%g, dy:%g, tangle:%g,", eptr->dx,eptr->dy,eptr->tangle);
	um_pscroll(UM_sbuf);

	if (uc_retrieve_attr(eptr->key, &attr) != UU_SUCCESS)
		goto done;
	if (ur_retrieve_blanked(eptr->key, &blanked) != UU_SUCCESS)
		goto done;

	sprintf(UM_sbuf,"	TEXT ATTRIBUTES FOR TEXT ENTITY %d", attr.key);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf,"	use_count:%d, color:%d, layer:%d, pen:%d, line_style:%d", 
				attr.use_count, attr.color, attr.layer, attr.pen, attr.line_style);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf,"	line_width:%g, displayable:%d, selectable:%d",
				attr.line_width, attr.displayable, attr.selectable);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf,"	blanked:%d, font:%d, prec:%d, expn:%g, spacing:%g",
				blanked, attr.font, attr.prec, attr.expn, attr.spacing);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf,"	height:%g, up(vector):%g, %g, %g", attr.height,
				attr.up[0], attr.up[1], attr.up[2]);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf,"	plane:%g %g %g, path:%d", attr.plane[0],
		attr.plane[1], attr.plane[2], attr.path);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf,"	align_hor:%d, align_ver:%d, txt_dens=%g", 
				attr.align_hor, attr.align_ver,attr.txt_dens);
	um_pscroll(UM_sbuf);

	sprintf(UM_sbuf,"	slant:%g, sub_sup:%g, line_spacing=%g", 
				attr.slant, attr.sub_sup, attr.line_spacing);
	um_pscroll(UM_sbuf);
	status = UU_SUCCESS;
done:
	uu_dexit;
	return(status);
}	/* ua_p33_text */


/*********************************************************************
**    E_FUNCTION :  int ua_draw_text( eptr, tfmat, attr)
**       Draw the text primitive pointed to by eptr.
**       Save the current GKS state for each attribute before
**       setting so we can reset them after drawing the text.
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to text entity
**				tfmat					transformation matrix 
**				attrptr				pointer to attribute bundle
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_draw_text(eptr, tfmat, attrptr)
struct UA_txt_rec *eptr;          /* ptr to text data */
UM_transf tfmat;
struct UA_txtattr_rec *attrptr;

{

	uu_denter(UU_STRC,(us,"ua_draw_text(eptr->key:%d,tfmat:%x,attrptr:%x)",
						eptr->key, tfmat, attrptr));

/*
.....Text contains display list
*/
	if (eptr->no_displst != 0)
	{
		S_draw_text_displst(eptr,tfmat,attrptr);
	}
	else
	{
	/* check on box over-ride */
		if(UA_text_box_ovrd == 0)
		{
			uai_draw_text(eptr, tfmat, attrptr);
		}
		else
		{
			if(eptr->arckey == 0)
				uai_draw_text_box(eptr, tfmat, attrptr);
			else
				uai_draw_text(eptr, tfmat, attrptr);
		}
	}

fexit:;
	uu_dexit;
	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION :  int uai_draw_text( eptr, tfmat, attr)
**       Draw the text primitive pointed to by eptr.
**       Save the current GKS state for each attribute before
**       setting so we can reset them after drawing the text.
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to text entity
**				tfmat					transformation matrix 
**				attrptr				pointer to attribute bundle
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uai_draw_text(eptr, tfmat, attrptr)
struct UA_txt_rec *eptr;          /* ptr to text data */
UM_transf tfmat;
struct UA_txtattr_rec *attrptr;

{
	UM_coord char_position;
	UM_vector char_up_vector, char_pln_norm, xaxis, yaxis, zaxis;
	UU_REAL char_height, char_exp, scale, char_dens, d;
	Gerror   gstatus;
	Gtxfp    fp, *pfp, save_fp;
	Gchrexp  expn, save_exp;
	Gchrsp   spacing, save_spacing;
	Gcolor   color, save_color;
	Gchrht   height, save_height;
	Gwpoint  *pup;
	Gwpoint3 up, save_up, plane, *save_plane, position, concat;
	Gwrect3 extent;
	Gtxpath  path, save_path;
	Gtxalign align, *palign, save_align;
	int	i, ind, relnum;
	char	*chptr, tstring[200];
	UM_coord 	tloc, sys_origin, sys_axis[3], dist;
	UU_LOGICAL	first;
	UU_LOGICAL uj_miplotting();

	uu_denter(UU_STRC,(us,"ua_draw_text(eptr->key:%d,tfmat:%x,attrptr:%x)",
						eptr->key, tfmat, attrptr));

	/* determine the character height, text plane normal, text up vector,
		and character postion */
	if (!um_is_idmat(tfmat))
		{
		uu_dprint(UU_STRC,(us,"enter not idmat"));
		um_unitvc(attrptr->up, yaxis);
		um_unitvc(attrptr->plane, zaxis);
		um_cross(yaxis, zaxis, xaxis);

		um_vctmtf(attrptr->up, tfmat, char_up_vector);
		um_vctmtf(attrptr->plane, tfmat, char_pln_norm);
		um_cctmtf(eptr->position, tfmat, char_position);

		um_vctmtf(xaxis, tfmat, xaxis);
		scale = um_mag(char_up_vector);
		char_height = attrptr->height * um_mag(char_up_vector);
		char_exp = um_mag(xaxis) / scale;
		}
	else
		{
		uu_dprint(UU_STRC,(us,"enter idmat"));
		um_vctovc(attrptr->up, char_up_vector);
		um_vctovc(attrptr->plane, char_pln_norm);
		um_vctovc(eptr->position, char_position);
		um_cross(char_up_vector, char_pln_norm, xaxis);
		char_height = attrptr->height;
		char_exp = attrptr->expn;
/*
.....No transformation matrix
.....Set scale to 1.0
.....Bobby  -  7/16/91
*/
		scale = 1.0;
		}

			/* save and set font/prec */
	pfp = gqtextfp();          
	save_fp.font = pfp-> font;
	save_fp.prec = pfp-> prec;

			/* set font and precision */
	fp.font = attrptr->font;       
 	fp.prec = (Gtxprec)attrptr->prec;
 	gstextfp(&fp);

			/* save and set text color */
	save_color = gqtextcolor();	
	color = attrptr->color;
	if(!uj_miplotting())
		gstextcolor(color);
	else
		gstextcolor(uj_setpen(attrptr->pen));

		/* set line density */
	char_dens = attrptr->txt_dens;
	gslinewidth(char_dens);

			/* save current expansion */
	save_exp = gqcharexp();  
 	gscharexp(char_exp);

			/* save/set character spacing */
	save_spacing = gqcharspace(); 
 	spacing = attrptr->spacing;
 	gscharspace(spacing);

			/* save/set character height */
	save_height = gqcharheight(); 
 	gscharheight(char_height);

			/* save/set text path */
	save_path = gqtextpath();
	path = (Gtxpath)attrptr->path;          
 	gstextpath(path);

			/* set text alignment */
	palign = gqtextalign();
	save_align.hor = palign->hor;
	save_align.ver = palign->ver;
	align.hor = (Gtxhor)attrptr->align_hor;  
	align.ver = (Gtxver)attrptr->align_ver;
	gstextalign(&align);

			/* set text plane normal vector */
	save_plane = gqtxplane();
	um_vctovc(char_pln_norm, &plane);  
 	gstxplane(&plane);
	uu_dprint(UU_STRC,(us,"after gstxtplane"));

			/* save/set char up vector */
	pup = gqcharup();          
	um_vctovc(pup, &save_up);

	relnum = -1;	/* unknown relation */
	if (eptr->arckey != 0)	/* Find out whether text is associated with an arc */
		ur_retrieve_data_relnum(eptr->arckey,&relnum);
	um_getcpln(sys_origin,sys_axis[0],sys_axis[1],sys_axis[2]);
	if (relnum == UM_CIRCLE_REL)
		uai_drw_txt_on_arc(eptr, tfmat, attrptr);
	else								/* single text entity */
	  {
				/* set char up vector */
 		um_vctovc(char_up_vector, &up);  
 		gscharup3(&up);

			/* rotate construction upvec if have text angle */
		if (eptr->tangle != 0.0)
			um_setcpln_yaxis(attrptr->up);	


			/* draw the text */
		um_vctovc(char_position, &position);
		chptr = eptr->tchar;
		ind = 0;
		first = UU_TRUE;
		while (ind < eptr->no_tchar)
	  	  {
				/* strip out the carriage return */
			for (i=0; ((*chptr!='\n')&&(ind<eptr->no_tchar)); ind++,i++)
		  	  {
				tstring[i] = *chptr;
				chptr++;
		  	  }
			ind++;
			chptr++;
			tstring[i] = '\0';
	   	uai_parse_string(tstring, &position, char_height, char_exp,
									char_up_vector, char_pln_norm,attrptr->sub_sup);
			if (ind < eptr->no_tchar)
		  	  {
				if (first)
			  	  {
/*
.....If path = UP or DOWN then
.....use character width for next line
*/
					if (attrptr->path > 1)
					{
						gqtextextent3(UD_ksws,eptr->position,tstring,&concat,&extent);
						d = extent.urb.x - extent.llf.x;
						um_vctmsc(xaxis,d,dist);
					}
/*
.....RIGHT or LEFT path
*/
					else
						um_vctmsc(attrptr->up,-(attrptr->height*
							(attrptr->line_spacing+1)*scale), dist);
					first = UU_FALSE;
			  	  }
					/* find the next line's starting position */
				um_vcplvc(&position,dist,&position);
		      }
	      }
		}

/*-------- reset attributes to previous state -------------------*/
	gstextfp(&save_fp);
	gstextcolor(save_color);
	gscharexp(save_exp);
	gscharspace(save_spacing);
	gscharheight(save_height);
	gscharup(&save_up);
	gstextpath(save_path);
	gstextalign(&save_align);
 	gstxplane(save_plane);
	if (eptr->tangle != 0.0)
		um_setcpln_yaxis(sys_axis[1]);

fexit:;
	uu_dexit;
	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION :  uai_drw_txt_on_arc(eptr,tfmat,attrptr)
**       display the text associated with an arc.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uai_drw_txt_on_arc(eptr, tfmat, attrptr) 
struct UA_txt_rec *eptr;          /* ptr to text data */
UM_transf tfmat;
struct UA_txtattr_rec *attrptr;

{
	struct UM_circle_rec arc;
	Gwpoint3		concat, position, char_position;
	Gwrect3		extent;
	int	ind;
	char	*chptr, str[2];
	UU_LOGICAL	first;
	UU_REAL	chdist, radius, theda, ang, xlen, ylen, dist;
	UM_coord	svec, yaxis, upvec, xcomp, ycomp, prept, endpt;
	UM_vector vc1;
	UU_REAL	dot, textdir, sign;
	Gwpoint3 up, tmpup, plane;

	uu_denter(UU_STRC,(us,"uai_drw_txt_on_arc(arckey=%d,space=%g)",eptr->arckey,
							 attrptr->spacing));

	ur_dump_tuple(UA_TEXT_REL,eptr);

	arc.key = eptr->arckey;
	if (uc_retrieve_data(&arc, sizeof(arc)) != UU_SUCCESS)
		goto done;
	uu_dprint(UU_STRC,(us,"after retrieve data"));


		/* set the regular construction plane to get the character's width */
	plane.x=0.0;  	plane.y=0.0;  	plane.z=1.0;
	tmpup.x=0.0;  	tmpup.y=1.0;  	tmpup.z=0.0;
	ang = 0.0;
	ind = 0;
	str[1] = '\0';
	chptr = eptr->tchar;
	um_vctovc(eptr->position,&position);
	radius = um_dcccc(arc.center,eptr->position);
	um_vcmnvc(eptr->position,arc.center,svec);
	um_vctovc(svec,prept);
	um_unitvc(svec,svec);
	um_cross(attrptr->plane,svec,yaxis);
		/* use the up vector to find out the text direction */
	if ((dot=um_dot(svec,attrptr->up)) < 0.0)
		textdir = -1.0;			/* text direction is counterclockwise */
	else
		textdir = 1.0;
		/* set the text (angle) direction by the text's path */
	sign = (attrptr->path==(int)UG_TP_LEFT)? textdir : -textdir;
	uu_dprint(UU_STRC,(us,"path=%d",attrptr->path));

	um_vctovc(eptr->position,&char_position);
/*   while ((ind<eptr->no_tchar)&&(*chptr!='\n'))*/
   while (ind<eptr->no_tchar)
	  {
 		gstxplane(&plane);
		gscharup3(&tmpup);
		str[0] = *chptr++;
		gqtextextent3(UD_ksws,&char_position,str,&concat,&extent);
		chdist = extent.urb.x - extent.llf.x;
/*
.....Newline character
.....move to next line
*/
		if (str[0] == '\n')
		{
			dist = -(attrptr->height*(attrptr->line_spacing+1));
			um_vctmsc(svec,dist,vc1);
			um_vcplvc(&char_position,vc1,&char_position);
			radius = radius + dist;
			um_vcmnvc(&char_position,arc.center,prept);
			um_vctovc(&char_position,&position);
			ind++;
			ang = 0.;
			continue;
		}
			/* Use a normal plane to figure out the char width */
		theda = sign*2.0*asin(chdist/(2.0*radius));
		uu_dprint(UU_STRC,(us,"theda=%g",theda));

 			/* set the plane normal back to the arc normal*/
 		gstxplane(attrptr->plane);/* set the plane normal back to the arc normal*/
		ang = ang + theda;
		if (fabs(ang) > UA_TXT_TWOPI)
			break;
		
		xlen = radius * cos(ang);
		um_vctmsc(svec,xlen,xcomp);
		ylen = radius * sin(ang);
		um_vctmsc(yaxis,ylen,ycomp);
		um_vcplvc(xcomp,ycomp,endpt);
		um_vcplvc(prept,endpt,upvec);
		um_unitvc(upvec,upvec);
		if (textdir == -1.0)
			um_vctmsc(upvec,(UU_REAL) -1.0,upvec);
		um_vctovc(upvec,&up);
			/* set char up vector */
 		gscharup3(&up);
		gtext(&position,str);

		um_vctovc(endpt,prept);
		um_vcplvc(arc.center,endpt,&position);
		ind++;
	  }
done:
	uu_dexit;
	return(UU_SUCCESS);

}	/* uai_drw_txt_on_arc */

/*********************************************************************
**    E_FUNCTION :  int S_draw_text_displst( eptr, tfmat, attr)
**       Draw the text primitive pointed to by eptr using the 
**       inherent display list.
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to text entity
**				tfmat					transformation matrix 
**				attrptr				pointer to attribute bundle
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_draw_text_displst(eptr,tfmat,attrptr)
struct UA_txt_rec *eptr;
UM_transf tfmat;
struct UA_txtattr_rec *attrptr;
{
	int i,npts;
	UM_coord *pts;
	UM_covec *ptv;
	UU_REAL char_dens;
	Gcolor color,save_color;
	UU_LOGICAL uj_miplotting();
/*
.....Save and set text color
*/
	save_color = gqlinecolor();	
	color = attrptr->color;
	if(!uj_miplotting())
		gslinecolor(color);
	else
		gslinecolor(uj_setpen(attrptr->pen));
/*
.....Set line density
*/
	char_dens = attrptr->txt_dens;
	gslinewidth(char_dens);
/*
.....Draw the text
*/
	ptv = (UM_covec *)eptr->displst;
	npts = eptr->no_displst;
	pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
	for (i=0;i<npts;i++)
	{
		if (um_mag(&ptv[i][3]) > UM_FUZZ)
			um_translate_point(ptv[i],.0001,&ptv[i][3],pts[i]);
		else
			um_vctovc(ptv[i],pts[i]);
	}
	ncl_displst_display(pts,tfmat);
	uu_free(pts);
/*
.....Reset the line color
*/
	gstextcolor(save_color);
}

/*********************************************************************
**    E_FUNCTION :  int uai_draw_text_box( eptr, tfmat, attr)
**       Draw a box around the text primitive pointed to by eptr.
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to text entity
**				tfmat					transformation matrix 
**				attrptr				pointer to attribute bundle
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uai_draw_text_box(eptr, tfmat, attrptr)
struct UA_txt_rec *eptr;          /* ptr to text data */
UM_transf tfmat;
struct UA_txtattr_rec *attrptr;

{
	Glntype line_style;
	Gcolor   color;
	UM_coord corn[5];
	int status;
/*
.....Calculate the text box
*/
	uai_calc_text_box(eptr, tfmat, attrptr, corn);
/*
.....Save and set text color
*/
	color = attrptr->color;
	gslinecolor(color);
	line_style.typeno = 0;
	line_style.npatn = 0;
	gslinetype(&line_style);
	gslinewidth((UU_REAL) 1.0);
/*
.....Draw the box
*/
	status = gpolyline3(5, corn);

	return (UU_SUCCESS);
}	

/*********************************************************************
**    E_FUNCTION :  uai_calc_text_bounds(eptr,box)
**       Calculates a bounding box around the text primitive pointed to
**       by eptr.
**    PARAMETERS   
**       INPUT  : 
**				eptr					Pointer to text entity.
**       OUTPUT :  
**          box 					Bounding box of text.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uai_calc_text_bounds(eptr,box)
struct UA_txt_rec *eptr;          /* ptr to text data */
{
	int i,j,n,nlists,status,relnum,ifl;
	UU_LOGICAL cirfl;
	UU_REAL orad[3];
	UM_coord cpt,pt,corn[5];
	UM_vector ovec[3];
	UM_covec *ptv;
	UM_transf tfmat;
	struct UA_txtattr_rec attr;
/*
.....Get transformation and attributes
*/
	status = uc_retrieve_transf(eptr->key,tfmat);
	if (status != UU_SUCCESS) goto done;
	status = uc_retrieve_attr(eptr->key,&attr);
	if (status != UU_SUCCESS) goto done;
/*
.....Determine if text is on an arc
*/
	cirfl = UU_FALSE;
	if (eptr->arckey != 0)
	{
		if (ur_retrieve_data_relnum(eptr->arckey,&relnum) == UU_SUCCESS)
		{
			if (relnum == UM_CIRCLE_REL) cirfl = UU_TRUE;
		}
	}
/*
.....Projected text
*/
	if (eptr->no_displst != 0)
	{
		ptv = (UM_covec *)eptr->displst;
		nlists = ptv[0][0];
		ptv++;
		for (i=0;i<nlists;i++)
		{
			n = ptv[0][0];
			ptv++;
			for (j=0;j<n;j++)
			{
				um_mcstoccs(0,ptv,ptv);
				if (i == 0 && j == 0) ncl_init_box(ptv,box);
				else ncl_update_box(ptv,box);
				ptv++;
			}
		}
	}
/*
.....Text on an arc
*/
	else if (cirfl)
	{
		uai_calc_arctext_box(eptr,tfmat,&attr,cpt,ovec,orad);
		um_translate_point(cpt,orad[0],ovec[0],pt);
		um_mcstoccs(0,pt,pt);
		ncl_init_box(pt,box);
		for (i=0;i<3;i++)
		{
			for (j=0;j<3;j++)
			{
				um_translate_point(cpt,orad[j],ovec[i],pt);
				um_mcstoccs(0,pt,pt);
				ncl_update_box(pt,box);
			}
		}
	}
/*
.....Standard text
*/
	else
	{
		ncl_mcstf(&ifl,tfmat);
		um_inverttf(tfmat,tfmat);
		uai_calc_text_box(eptr,tfmat,&attr,corn,UU_TRUE);
		ncl_init_box(corn[0],box);
		for (i=0;i<4;i++) ncl_update_box(corn[i],box);
	}
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uai_calc_text_box( eptr, tfmat, attr, corn)
**       Calculates a box around the text primitive pointed to by eptr.
**    PARAMETERS   
**       INPUT  : 
**				eptr					pointer to text entity
**				tfmat					transformation matrix 
**				attrptr				pointer to attribute bundle
**       OUTPUT :  
**          corn					Corners of box (ll, lr, ur, ul, ll)
**    RETURNS      : UU_FAILURE if problem, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uai_calc_text_box(eptr, tfmat, attrptr, corn)
struct UA_txt_rec *eptr;
UM_transf tfmat;
struct UA_txtattr_rec *attrptr;
UM_coord corn[];
{
	int nline,i;
	UM_coord char_position;
	UM_vector char_up_vector, char_pln_norm, xaxis, yaxis, zaxis,vc1;
	UU_REAL char_dx, char_dy, scale, size, x_offset, y_offset;
	UM_coord del_x, del_y, org_x, org_y;

	if (!um_is_idmat(tfmat))
		{
		um_vctmtf(attrptr->up, tfmat, char_up_vector);
		um_vctmtf(attrptr->plane, tfmat, char_pln_norm);
		um_cctmtf(eptr->position, tfmat, char_position);
		scale = um_mag(char_up_vector);
		}
	else
		{
		um_vctovc(attrptr->up, char_up_vector);
		um_vctovc(attrptr->plane, char_pln_norm);
		um_vctovc(eptr->position, char_position);
		scale = 1.0;
		}
	um_cross(char_up_vector, char_pln_norm, xaxis);
	um_unitvc(char_up_vector, char_up_vector);
	um_unitvc(char_pln_norm, char_pln_norm);
	um_unitvc(xaxis, xaxis);
{
/*
.....Adjust vectors for character path
*/
	switch (attrptr->path)
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
		um_vctovc(char_up_vector,xaxis);
		um_vctovc(vc1,char_up_vector);
*/
		break;
/*
........Down
*/
	case 3:
/*
		um_vctmsc(xaxis,-1.,vc1);
		um_vctmsc(char_up_vector,-1.,xaxis);
		um_vctovc(vc1,char_up_vector);
*/
		break;
	}
}

	char_dx = eptr->dx*scale;
	if (attrptr->path > 1)
	{
		nline = 1;
		for (i=0;i<eptr->no_tchar-1;i++)
			if (eptr->tchar[i] == '\n') nline++;
		char_position[0] = char_position[0] - (char_dx/nline)/2.;
	}
	char_dy = eptr->dy*scale;
	size = attrptr->height*scale;
	um_vctmsc(xaxis, char_dx, del_x);
	um_vctmsc(char_up_vector, char_dy, del_y);

	um_vctovc(char_position, corn[0]);

	x_offset = 0.0;
	if (attrptr->path <= 1) y_offset = size;
	else y_offset = 0.;

	um_vctmsc(xaxis, x_offset, org_x);
	um_vcplvc(corn[0], org_x, corn[0]);
	um_vctmsc(char_up_vector, y_offset, org_y);
	um_vcplvc(corn[0], org_y, corn[0]);

	um_vcplvc(corn[0], del_x, corn[1]);
	um_vcmnvc(corn[1], del_y, corn[2]);
	um_vcmnvc(corn[2], del_x, corn[3]);
	um_vcplvc(corn[3], del_y, corn[4]);

	return (UU_SUCCESS);
}	

/*********************************************************************
**    E_FUNCTION :  uai_calc_arctext_box(eptr,tfmat,attr,cpt,ovec,orad)
**       Calculates a box sweep around the arc text primitive pointed to
**       by eptr.
**    PARAMETERS   
**       INPUT  : 
**				eptr      pointer to text entity
**				tfmat     transformation matrix 
**				attrptr   pointer to attribute bundle
**       OUTPUT :  
**          cpt       Circle center point.
**          ovec      Vectors from circle center to start, center, and end
**                    of text box sweep.
**          orad      Radii of arc to top, middle, and bottom of text box
**                    sweep.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uai_calc_arctext_box(e, tfmat, txtattr, cpt, ovec, orad)
struct UA_txt_rec *e;
UM_transf tfmat;
struct UA_txtattr_rec *txtattr;
UM_coord cpt;
UM_vector ovec[];
UU_REAL orad[];
{
	int	status,i;
	UU_REAL	radius, ang;
	UU_REAL	xlen, ylen;
	UU_REAL	svec[3], yaxis[3];
	UU_REAL	xcomp[3], ycomp[3],dlt[3];
	UU_REAL	scale, num;
	int mod,site;
	struct UM_circle_rec arc;
/*
.....Get the text arc
*/
	arc.key = e->arckey;
	status = uc_retrieve_data(&arc,sizeof(arc));
	um_vctovc(arc.center,cpt);
/*
.....Set the regular construction plane
.....to get the character's width
*/
	um_vcmnvc(e->position,arc.center,svec);
	um_unitvc(svec,svec);
	um_cross(txtattr->plane,svec,yaxis);
/*
.....Since different characters might have
.....different widths we have to sum up
.....the individual widths
*/
	site = txtattr->entity_site;
	txtattr->entity_site = 2;
	ua_arctxt_parms(e,txtattr,&arc,UU_TRUE,&radius,&ang);
	txtattr->entity_site = site;
/*
.....Calculate box sweep vectors
*/
	dlt[0] = 0.;
	dlt[1] = -(ang / 2.);
	dlt[2] = -ang;
/*
.....Calculate radii to top, middle, bottom of character
*/
	orad[0] = radius + txtattr->height;
	orad[1] = radius + txtattr->height/2.;
	orad[2] = radius;
/*
.....Adjust angle to fall on character
*/
	num = ang / UA_TXT_TWOPI;
	mod = num;
	if ((UU_REAL)mod > num)		mod = mod - 1;
	ang = ang - (UA_TXT_TWOPI*mod);
/*
.....Calculate Start, Center, and End vectors
*/
	for (i=0;i<3;i++)
	{
		xlen = radius * cos(dlt[i]);
		um_vctmsc(svec,xlen,xcomp);
		ylen = radius * sin(dlt[i]);
		um_vctmsc(yaxis,ylen,ycomp);
		um_vcplvc(xcomp,ycomp,ovec[i]);
		um_unitvc(ovec[i],ovec[i]);
  }
/*
.....End of routine
*/
done:
	return;
}

/*********************************************************************
**    E_FUNCTION :  int uai_parse_string(string, position, height,
**														expn, up, norm,sub_sup)
**       Parse NOTE string look for super/sub-scripts and draw resulting strings
**
**    PARAMETERS   
**       INPUT  : 
**				string				NOTE string
**				position				current string origin
**				height/expn			current character height and expansion factors
**				up/norm				current cahracter plane up and normal vectors
**				subht					super/sub-script height ratio
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uai_parse_string(string, position, height, expn, up, norm,subht)

char   	string[];
Gwpoint3 *position;
UU_REAL  height, expn,subht;
UM_coord up, norm;

	{
	UM_coord xaxis, x_off, y_off;
	UU_REAL hgt_scale, u_off_set, d_off_set, del_x, del_y, new_h, ff, strsize;
	int status, cur_pos, num_chars, indx, sub_sup;
	Gwpoint3		concat, new_pos, tmpup, plane;
	Gwrect3		extent;
	char  tmp_str[256];

	uu_denter(UU_STRC,(us,"uai_parse_string str = %s", string));

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	/* set-up defaults */
	u_off_set = .6;
	d_off_set = .5;
	hgt_scale = subht;
	um_cross(up, norm, xaxis);
	um_unitvc(xaxis, xaxis);
	um_vctovc(position, &new_pos);
	plane.x=0.0;  	plane.y=0.0;  	plane.z=1.0;
	tmpup.x=0.0;  	tmpup.y=1.0;  	tmpup.z=0.0;
	num_chars = strlen(string);
	cur_pos = indx = 0;

	/* begin parsing input string */
	while (cur_pos < num_chars)
		{
		if(string[cur_pos] != '[')
			{
			if (string[cur_pos] == '\\' && string[cur_pos + 1] == '[')
				 {
			   tmp_str[indx++] = string[(cur_pos++) + 1];
			   cur_pos++;
				 }
      else
         tmp_str[indx++] = string[cur_pos++];
			}
		else
			{
			if(indx > 0)
				{
				uu_dprint(UU_STRC,(us,"tmp_str = %s",tmp_str));
				tmp_str[indx] = '\0';
				gtext(&new_pos, tmp_str);
 				gstxplane(&plane);
				gscharup3(&tmpup);
				gqtextextent3(UD_ksws,&new_pos,tmp_str,&concat,&extent);
				del_x = extent.urb.x - extent.llf.x;
 				gstxplane(norm);
				gscharup3(up);
				um_vctmsc(xaxis, del_x, x_off);
				um_vcplvc(&new_pos, x_off, &new_pos);
				}
			cur_pos++;
			indx = 0;
			while(string[cur_pos] != ']' && cur_pos < num_chars)
				{
				uu_dprint(UU_STRC,(us,"current char = %c",string[cur_pos]));
				switch(string[cur_pos])
					{
					case 'U':
						sub_sup = 1;
						cur_pos++;
						if((string[cur_pos] >= '0' && string[cur_pos] <= '9')
														  || string[cur_pos] == '.')
							{
							uai_parse_real(string, &cur_pos, num_chars, &u_off_set);
							if(u_off_set == -999.99) u_off_set = 0.6;
							}
						break;
					case 'L':
						sub_sup = 2;
						cur_pos++;
						if((string[cur_pos] >= '0' && string[cur_pos] <= '9')
														  || string[cur_pos] == '.')
							{
							uai_parse_real(string, &cur_pos, num_chars, &d_off_set);
							if(d_off_set == -999.99) d_off_set = 0.5;
							}
						break;
					case 'H':
						cur_pos++;
						if((string[cur_pos] >= '0' && string[cur_pos] <= '9')
														  || string[cur_pos] == '.')
							{
							uai_parse_real(string, &cur_pos, num_chars, &hgt_scale);
							if(hgt_scale == -999.99) hgt_scale = subht;
							}
						break;
					default:
						cur_pos++;
						break;
					}
				}
			cur_pos++;
			indx = 0;
			while(string[cur_pos] != ']' && cur_pos < num_chars)
				{
				tmp_str[indx] = string[cur_pos];
				indx++;
				cur_pos++;
				}
			uu_dprint(UU_STRC,(us,"tmp_str = %s", tmp_str));
			cur_pos++;
			if(indx > 0)
				{
				tmp_str[indx] = '\0';
 				gstxplane(&plane);
				gscharup3(&tmpup);
				gqtextextent3(UD_ksws,&new_pos,tmp_str,&concat,&extent);
				switch(sub_sup)
					{
					case 1:
						del_y = u_off_set * (extent.urb.y - extent.llf.y);
						break;
					case 2:
						del_y = d_off_set * (extent.urb.y - extent.llf.y);
						break;
					}
				um_vctmsc(up, del_y, y_off);
				new_h = hgt_scale*(height);
				gscharheight(new_h);
				gqtextextent3(UD_ksws,&new_pos,tmp_str,&concat,&extent);
				del_x = extent.urb.x - extent.llf.x;
				gstxplane(norm);
				gscharup3(up);
				switch(sub_sup)
					{
					case 1:
						um_vcplvc(&new_pos, y_off, &new_pos);
						gtext(&new_pos, tmp_str);
						um_vctmsc(xaxis, del_x, x_off);
						um_vcplvc(&new_pos, x_off, &new_pos);
						um_vcmnvc(&new_pos, y_off, &new_pos);
						break;
					case 2:
						um_vcmnvc(&new_pos, y_off, &new_pos);
						gtext(&new_pos, tmp_str);
						um_vctmsc(xaxis, del_x, x_off);
						um_vcplvc(&new_pos, x_off, &new_pos);
						um_vcplvc(&new_pos, y_off, &new_pos);
						break;
					}
				gscharheight(height);
				hgt_scale = subht;
				d_off_set = .5;
				u_off_set = .6;
				}
			indx = 0;
			}
		}

	if(indx > 0)
		{
		tmp_str[indx] = '\0';
		gtext(&new_pos, tmp_str);
		}
		uu_dexit;
	return(UU_SUCCESS);
	}	
/*********************************************************************
**    E_FUNCTION :  int uai_parse_real(string, cur_pos, num_char, ff)
**       Parse input string for a REAL number
**
**    PARAMETERS   
**       INPUT  : 
**				string				NOTE string
**				cur_pos				current character position
**				num_char				number of characters in the string
**       OUTPUT :  
**          ff						returned real number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uai_parse_real(string, cur_pos, num_char, ff)

char   	string[];
int      *cur_pos, num_char;
UU_REAL  *ff;

	{
	int pos, indx;
	char tmp_str[20];
	UU_TRUEDOUBLE ans, uai_atof();


	uu_denter(UU_STRC,(us,"uai_parse_real"));

	/*------------------------------------------------------------------------
	** Start of executable code
	**----------------------------------------------------------------------*/

	pos = *cur_pos;
	indx = 0;
	tmp_str[indx] = string[pos];
	pos++;
	indx++;
	while(((string[pos] >= '0' && string[pos] <= '9') || string[pos] == '.') 
									 && pos < num_char)
		{
		tmp_str[indx] = string[pos];
		indx++;
		pos++;
		}
	if(indx > 0)
		{
		tmp_str[indx] = '\0';
		ans = uai_atof(tmp_str);
		*ff = ans;
		}
	else
		{
		*ff = -999.99;
		}
	*cur_pos = pos;
	return(UU_SUCCESS);
	uu_dexit;
	}	

/*********************************************************************
**    I_FUNCTION     :  UU_TRUEDOUBLE uig_atof(buff)
**          Convert ASCII string to a double precision number.
**    PARAMETERS   
**       INPUT  : 
**          buff                    character string     
**       OUTPUT :  
**          output
**    RETURNS      : double precision number
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_TRUEDOUBLE uai_atof(buff)
   char buff[];
   {
   int dloc,eloc,i,j,k,len;
   UU_TRUEDOUBLE dd,expn,power,tt;
   UU_TRUEDOUBLE ten = 10.0;
   UU_TRUEDOUBLE one = 1.0;
   UU_TRUEDOUBLE zero = 0.0;
   UU_TRUEDOUBLE one_ten = 0.1;

   /*------------------------------------------------------------------------
   ** Start of executable code
   **----------------------------------------------------------------------*/

   len = strlen(buff);
   dloc = eloc = -1;

   /* locate decimal point and exponent if any */

   for(i=0;i<len;i++)
      {
      if(buff[i] == '.')
         {
         dloc = i;
         }
      else if(buff[i] == 'E' || buff[i] == 'D')
         {
         eloc = i;
         }
      }
   
   /* get power of ten */

   expn = zero;
   power = one;
   if(eloc >  0)
      {
      for(i=(len-1);i>eloc;i--)
         {
         if(buff[i] == '-')
            {
            expn = -expn;
            }
         else if(buff[i] != '+' && buff[i] != ' ')
            {
            dd = buff[i] - '0';
            expn = expn + dd * power;
            power = power * ten;
            }
         }
      tt = ten;
      dd = expn;
      expn = one;
      if(dd < zero)
         {
         dd = -dd;
         tt = one_ten;
         }
      if(fabs(dd-zero) > one_ten)
         {
         for(power=dd;power>zero;power=(power-one))
               expn = expn * tt;
         }
      len = eloc;
      }
   else
      {
      expn = one;
      eloc = len;
      }

   /* compute fractional part */

   tt = zero;
   power = one_ten;
	if(dloc != -1)
		{
   	if(len > dloc)
			{
			for(i=(dloc+1);i<eloc;i++)
				{
				if(buff[i] != ' ')
					{
					dd = buff[i] - '0';
					tt = tt + dd * power;
					power = power * one_ten;
					}
				}
			}
		}
	else
		{
		dloc = len;
		}
		

   /* compute integral part */

   power = one;
   if(dloc > 0)
      {
      for(i=(dloc-1);i>-1;i--)
         {
         if(buff[i] == '-')
            {
            tt = -tt;
            }
         else
            {
            if(buff[i] != ' ' && buff[i] != '+')
               {
               dd = buff[i] - '0';
               tt = tt + dd * power;
               power = power * ten;
               }
            }
         }
      }

   /* now apply exponent */

   tt = tt * expn;
   return(tt);

   }
