/*********************************************************************
**    NAME         : atxtutl.c
**       CONTAINS:
**				ua_saveattr_text
**				ua_restattr_text
**				ua_setattr_text
**				ua_getline_text
**				ua_string_deltas
**				ua_text_box
**				ua_text_offset
**				ua_text_origin
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       atxtutl.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:42
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) atxtutl.c 25.1 04/29/15 15:05:42 single"};
#else
static char uu_sccsident[]={"@(#) atxtutl.c 25.1 04/29/15 15:05:42 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"

static int		save_font;		/* save area for save/restore */
static Gtxprec		save_prec;
static UU_REAL        save_exp;
static UU_REAL  		save_spacing;
static UU_REAL  		save_height;
static Gtxpath		save_path;
static Gtxhor		save_alignh;
static Gtxver		save_alignv;
static UU_REAL save_charup[3];
UU_REAL sgqcharspace(),sgqcharexp(),sgqcharheight();

/*********************************************************************
**    E_FUNCTION     : ua_setattr_text(e, itxt)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_setattr_text(e, itxt)
struct UA_generic_draft	(*e);
int		itxt;
	{
	UU_REAL	cpxaxis[3];
	UU_REAL	cpyaxis[3];
	UU_REAL	cpzaxis[3];
	UU_REAL	cporig[3];
	/*struct Gtxfp	fp; */
	Gtxfp	fp;
	/* struct Gtxalign	align; */
	Gtxalign	align;
	int		status;

	uu_denter(UU_STRC,(us,"ua_setattr_text(e=%s,itxt=%d)", "...", itxt));

	fp.font = (*e).txt_blk[itxt-1].text.line_font;
	fp.prec = UA_txt_precision;
	status = gstextfp(&(fp));
	status = gscharexp((*e).txt_blk[itxt-1].char_expansion);
	status = gscharspace((*e).txt_blk[itxt-1].char_space);
	status = gscharheight((*e).txt_blk[itxt-1].txt_size);
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
	align.ver = UG_TV_BOTTOM;
	status = gstextalign(&(align));
	ua_getcpln(&((*e)),cporig,cpxaxis,cpyaxis,cpzaxis);
	status = gstxplane(cpzaxis);
		{
		UU_REAL	us_t144[3];
		ua_get_upvector(&((*e)),(*e).txt_blk[itxt-1].tangle,us_t144);
		status = gscharup3(us_t144);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_text_offset(e, itxt, offset_cc)
**			Get text box print origin relative to lower left corner
**			based on all text attributes except text angle, in
**			construction coordinates.
**    PARAMETERS   
**       INPUT  : 
**				e								entity draft record
**				itxt							entity txt_blk element to analyze
**       OUTPUT :  
**				e.txt_blk[itxt].dx		text box width
**				e.txt_blk[itxt].dy		text box height
**				offset_cc					vector from box lower left corner
**												to print origin- in constr coord
**												without any text angle
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_text_offset(e, itxt, offset_cc)
struct UA_generic_draft	(*e);
int		itxt;
UU_REAL	offset_cc[3];
	{
	UU_REAL	boxll_cc[3];
	int		linelen;
	UU_REAL	boxur_cc[3];
	UU_REAL	rot_yaxis[3];
	UU_REAL	cc[3];
	UU_REAL	concat[3];
	UU_REAL	dimorig_cc[3];
	UU_REAL	curloc[3];
	UU_REAL	sys_axis[3][3];
	UU_REAL	wc[3];
	UU_REAL	concat_cc[3];
	UU_REAL	extent[2][3];
	UU_REAL	curloc_cc[3];
	char		line[1025];
	int		txtpos;
	UU_REAL	extent_cc[2][3];
	UU_REAL	sys_origin[3];

	uu_denter(UU_STRC,(us,
		"ua_text_offset(e=%s, itxt=%d, offset_cc=%s)", "...", itxt, "..."));

	ua_saveattr_text();
/* NCL - added by kathy to fix char_height */
save_height = (*e).txt_blk[itxt-1].txt_size;

	ua_setattr_text(&((*e)),itxt);
	if( ( (*e).txt_blk[itxt-1].tangle!=0.000000e+000 ) )
		{
		um_getcpln(sys_origin,sys_axis[0],sys_axis[1],sys_axis[3
		    -1]);
		ua_get_upvector(&((*e)),(*e).txt_blk[itxt-1].tangle,
		rot_yaxis);
		um_setcpln_yaxis(rot_yaxis);
		}
	txtpos = 1;
	um_vctovc((*e).dim_origin,curloc);
	um_mcstoccs(0,(*e).dim_origin,dimorig_cc);
	boxll_cc[0] = 1.000000e+030;
	boxll_cc[1] = 1.000000e+030;
	boxll_cc[2] = 1.000000e+030;
	boxur_cc[0] = -1.000000e+030;
	boxur_cc[1] = -1.000000e+030;
	boxur_cc[2] = -1.000000e+030;
	linelen = ua_getline_text((*e).txt_blk[itxt-1].tstring,(*e).
	    txt_blk[itxt-1].char_cnt,line,&(txtpos));
	for(;;)
		{
		if( ! (( linelen>0 )) ) goto us_l145;
		us_textextent(curloc,line,concat,extent);
		um_mcstoccs(0,extent[0],extent_cc[0]);
		um_mcstoccs(0,extent[1],extent_cc[1]);
		if( ( extent_cc[0][0]<boxll_cc[0] ) )
			{
			boxll_cc[0] = extent_cc[0][0];
			}
		if( ( extent_cc[0][1]<boxll_cc[1] ) )
			{
			boxll_cc[1] = extent_cc[0][1];
			}
		if( ( extent_cc[0][2]<boxll_cc[2] ) )
			{
			boxll_cc[2] = extent_cc[0][2];
			}
		if( ( extent_cc[1][0]>boxur_cc[0] ) )
			{
			boxur_cc[0] = extent_cc[1][0];
			}
		if( ( extent_cc[1][1]>boxur_cc[1] ) )
			{
			boxur_cc[1] = extent_cc[1][1];
			}
		if( ( extent_cc[1][2]>boxur_cc[2] ) )
			{
			boxur_cc[2] = extent_cc[1][2];
			}
		linelen = ua_getline_text((*e).txt_blk[itxt-1].tstring,(*e).
		    txt_blk[itxt-1].char_cnt,line,&(txtpos));
		um_mcstoccs(0,curloc,curloc_cc);
		curloc_cc[1] = ( curloc_cc[1]-( ( 1.000000e+000+(*e).txt_blk
		    [itxt-1].line_spacing )*(*e).txt_blk[itxt-1].txt_size ) );
		um_cpltrans(curloc_cc,curloc);
		}
us_l145: 
	;
	cc[0] = boxll_cc[0];
	cc[1] = boxll_cc[1];
	cc[2] = boxll_cc[2];
	um_vcmnvc(dimorig_cc,cc,offset_cc);
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_offset: offset_cc=<%g,%g,%g>\n",
		offset_cc[0],offset_cc[1],offset_cc[2]);
		us_write(4,us);
		}
#endif
	(*e).txt_blk[itxt-1].dx = ( boxur_cc[0]-boxll_cc[0] );
	(*e).txt_blk[itxt-1].dy = ( boxur_cc[1]-boxll_cc[1] );
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_offset: txdx=%g,txdy=%g\n",(*e).txt_blk[
		    itxt-1].dx,(*e).txt_blk[itxt-1].dy);
		us_write(4,us);
		}
#endif
	if( ( (*e).txt_blk[itxt-1].tangle!=0.000000e+000 ) )
		{
		um_setcpln_yaxis(sys_axis[1]);
		}
	ua_restattr_text();
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_text_origin(e, itxt, offset_cc, corners)
**			Set text block origin (world coord offset) and return
**			the text box corners in world coord.
**			If have text angle, rotate the offset_cc and corner points.
**    PARAMETERS   
**       INPUT  : 
**				e								drafting entity record
**				itxt							text block element to use
**				offset_cc					offset vector from lower left
**												corner of textbox to text origin
**												in box (constr coords).
**       OUTPUT :  
**				corners[4]					four corners in world coord
**				e.txt_blk[itxt].origin	world coord offset text origin
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_text_origin(e, itxt, offset_cc, corners)
struct UA_generic_draft	(*e);
int		itxt;
UU_REAL	offset_cc[3];
UU_REAL	corners[4][3];
	{
	UU_REAL	orig[3];
	int		linelen;
	UU_REAL	orig_cc[3];
	UU_REAL	rot_yaxis[3];
	UU_REAL	cc[3];
	UU_REAL	dimorig_cc[3];
	UU_REAL	origoff_cc[3];
	UU_REAL	sys_axis[3][3];
	UU_REAL	wc[3];
	UU_REAL	corners_cc[4][3];
	char		line[1025];
	UU_REAL	sys_origin[3];

	uu_denter(UU_STRC,(us,
		"ua_text_origin(e=%s, itxt=%d, offset_cc=<%g,%g,%g>, corners=%s)",
		"...", itxt, offset_cc[0],offset_cc[1],offset_cc[2], "..."));

	if( ( (*e).txt_blk[itxt-1].tangle!=0.000000e+000 ) )
		{
		um_getcpln(sys_origin,sys_axis[0],sys_axis[1],sys_axis[3
		    -1]);
		ua_get_upvector(&((*e)),(*e).txt_blk[itxt-1].tangle,
		rot_yaxis);
		um_setcpln_yaxis(rot_yaxis);
		}
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_origin: offset_cc=<%g,%g,%g>\n",
		offset_cc[0],offset_cc[1],offset_cc[2]);
		us_write(4,us);
		}
#endif
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_origin: dx=%g,dy=%g\n",(*e).txt_blk[itxt
		    -1].dx,(*e).txt_blk[itxt-1].dy);
		us_write(4,us);
		}
#endif
	um_mcstoccs(0,(*e).dim_origin,dimorig_cc);
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_origin: dimorig_cc=<%g,%g,%g>\n",
		dimorig_cc[0],dimorig_cc[1],dimorig_cc[2]);
		us_write(4,us);
		}
#endif
	switch( (*e).entity_site )
		{
		case 0:
			{
			cc[0] = 0.000000e+000;
			cc[1] = (*e).txt_blk[itxt-1].dy;
			cc[2] = 0.000000e+000;
			}
			break;
		case 1:
			{
			cc[0] = 0.000000e+000;
			cc[1] = ( (*e).txt_blk[itxt-1].dy/2.000000e+000 );
			cc[2] = 0.000000e+000;
			}
			break;
		case 2:
			{
			cc[0] = 0.000000e+000;
			cc[1] = 0.000000e+000;
			cc[2] = 0.000000e+000;
			}
			break;
		case 3:
			{
			cc[0] = ( (*e).txt_blk[itxt-1].dx/2.000000e+000 );
			cc[1] = (*e).txt_blk[itxt-1].dy;
			cc[2] = 0.000000e+000;
			}
			break;
		case 4:
			{
			cc[0] = ( (*e).txt_blk[itxt-1].dx/2.000000e+000 );
			cc[1] = ( (*e).txt_blk[itxt-1].dy/2.000000e+000 );
			cc[2] = 0.000000e+000;
			}
			break;
		case 5:
			{
			cc[0] = ( (*e).txt_blk[itxt-1].dx/2.000000e+000 );
			cc[1] = 0.000000e+000;
			cc[2] = 0.000000e+000;
			}
			break;
		case 6:
			{
			cc[0] = (*e).txt_blk[itxt-1].dx;
			cc[1] = (*e).txt_blk[itxt-1].dy;
			cc[2] = 0.000000e+000;
			}
			break;
		case 7:
			{
			cc[0] = (*e).txt_blk[itxt-1].dx;
			cc[1] = ( (*e).txt_blk[itxt-1].dy/2.000000e+000 );
			cc[2] = 0.000000e+000;
			}
			break;
		case 8:
			{
			cc[0] = (*e).txt_blk[itxt-1].dx;
			cc[1] = 0.000000e+000;
			cc[2] = 0.000000e+000;
			}
			break;
		}
	um_vcmnvc(offset_cc,cc,origoff_cc);
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_origin: origoff_cc=<%g,%g,%g>\n",
		origoff_cc[0],origoff_cc[1],origoff_cc[2]);
		us_write(4,us);
		}
#endif
	um_vcplvc(dimorig_cc,origoff_cc,orig_cc);
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_origin: orig_cc=<%g,%g,%g>\n",orig_cc[0]
		    ,orig_cc[1],orig_cc[2]);
		us_write(4,us);
		}
#endif
	um_vcmnvc(orig_cc,offset_cc,corners_cc[0]);
	corners_cc[1][0] = ( corners_cc[0][0]+(*e).txt_blk[itxt
	    -1].dx );
	corners_cc[1][1] = corners_cc[0][1];
	corners_cc[1][2] = corners_cc[0][2];
	corners_cc[2][0] = corners_cc[1][0];
	corners_cc[2][1] = ( corners_cc[0][1]+(*e).txt_blk[itxt
	    -1].dy );
	corners_cc[2][2] = corners_cc[0][2];
	corners_cc[3][0] = corners_cc[0][0];
	corners_cc[3][1] = corners_cc[2][1];
	corners_cc[3][2] = corners_cc[0][2];
	um_cpltrans(corners_cc[0],corners[0]);
	um_cpltrans(corners_cc[1],corners[1]);
	um_cpltrans(corners_cc[2],corners[2]);
	um_cpltrans(corners_cc[3],corners[3]);
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_origin: corners[ll1]=<%g,%g,%g>\n",
		corners[0][0],corners[0][1],corners[0][2]);
		us_write(4,us);
		}
#endif
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_origin: corners[lr2]=<%g,%g,%g>\n",
		corners[1][0],corners[1][1],corners[1][2]);
		us_write(4,us);
		}
#endif
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_origin: corners[ur3]=<%g,%g,%g>\n",
		corners[2][0],corners[2][1],corners[2][2]);
		us_write(4,us);
		}
#endif
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_origin: corners[ul4]=<%g,%g,%g>\n",
		corners[3][0],corners[3][1],corners[3][2]);
		us_write(4,us);
		}
#endif
	um_cpltrans(orig_cc,orig);
	um_vcmnvc(orig,(*e).dim_origin,(*e).txt_blk[itxt-1].origin);
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_box: txorig=<%g,%g,%g>\n",(*e).txt_blk[
		    itxt-1].origin[0],(*e).txt_blk[itxt-1].origin[1],(*e).
		    txt_blk[itxt-1].origin[2]);
		us_write(4,us);
		}
#endif
	if( ( (*e).txt_blk[itxt-1].tangle!=0.000000e+000 ) )
		{
		um_setcpln_yaxis(sys_axis[1]);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_string_deltas(text_block)
**       Calculate the text string dx and dy values.
**    PARAMETERS   
**       INPUT  : 
**          text_block - Text block to caluculate new delta values.
**       OUTPUT :  
**          text_block - Text block with new delta values.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_string_deltas(text_block)
struct UA_txt_blk	(*text_block);
	{
	UU_REAL	concat[3];
	UU_REAL	bounds[2][3];

	uu_denter(UU_STRC,(us,"ua_string_deltas(text_block=%s)", "..."));

		{
		UU_REAL	us_t146[3];
		us_t146[0] = 0.000000e+000;
		us_t146[1] = 0.000000e+000;
		us_t146[2] = 0.000000e+000;
		us_textextent(us_t146,(*text_block).tstring,concat,bounds);
		}
	(*text_block).dx = bounds[1][0];
	(*text_block).dy = bounds[1][1];
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_saveattr_text()
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_saveattr_text()
	{
	Gtxprec sgqtextprec();
	Gtxhor sgqtextalignh();
	Gtxver sgqtextalignv();
	Gtxpath	sgqtextpath();

	uu_denter(UU_STRC,(us,"ua_saveattr_text()"));

	save_font = sgqtextfont();
	save_prec = sgqtextprec();
	save_exp = sgqcharexp();
	save_spacing = sgqcharspace();

/* NCL - commented by kathy to fix char height
	save_height = sgqcharheight();
*/
	save_path = sgqtextpath();
	save_alignh = sgqtextalignh();
	save_alignv = sgqtextalignv();
	sgqcharup(save_charup);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : int		ua_getline_text(stg, stglen, line, pos)
**			Get the next line from the text string - lines are
**			terminated by a carriage-return.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_getline_text(stg, stglen, line, pos)
char		stg[1];
int		stglen;
char		line[1];
int		(*pos);
	{
	int		linelen;
	int		posbeg;

	uu_denter(UU_STRC,(us,
		"ua_getline_text(stg=%s,stglen=%d,line=%s,pos=%d)",stg,stglen,line,*pos));

	if( ( (*pos)>stglen ) )
		{
		uu_dexit;
		return(0);
		}
	linelen = 0;
	posbeg = (*pos);
	for(;;)
		{
		if( ! (( ( (*pos)<=stglen )&&( stg[(*pos)-1]!='\n' ) )
		    ) ) goto us_l147;
		(*pos) = ( (*pos)+1 );
		linelen = ( linelen+1 );
		}
us_l147: 
	;
	if( ( linelen>0 ) )
		{
		strncpy( line, stg + posbeg - 1, (( (*pos)-1 ) - posbeg + 1)
		    );
		line[ (( (*pos)-1 ) - posbeg + 1) ] = '\0';
		}
	else
		{
		strcpy(line,"");
		}
	(*pos) = ( (*pos)+1 );
	uu_dexit;
	return(linelen);
	}
/*********************************************************************
**    E_FUNCTION     : ua_restattr_text()
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_restattr_text()
	{
	Gtxfp	fp;
	Gtxalign	align;
	int		status;

	uu_denter(UU_STRC,(us,"ua_restattr_text()"));

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
	}
/*********************************************************************
**    E_FUNCTION     : ua_text_box(itxt, e, corners)
**       C
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_text_box(itxt, e, corners)
int		itxt;
struct UA_generic_draft	(*e);
UU_REAL	corners[4][3];
	{
	UU_REAL	boxll_cc[3];
	int		linelen;
	UU_REAL	boxur_cc[3];
	UU_REAL	rot_yaxis[3];
	UU_REAL	cc[3];
	UU_REAL	concat[3];
	UU_REAL	dimorig_cc[3];
	UU_REAL	curloc[3];
	UU_REAL	wc[3];
	UU_REAL	sys_axis[3][3];
	UU_REAL	corners_cc[4][3];
	UU_REAL	concat_cc[3];
	UU_REAL	extent[2][3];
	UU_REAL	offset_cc[3];
	UU_REAL	curloc_cc[3];
	char		line[1025];
	int		txtpos;
	UU_REAL	extent_cc[2][3];
	UU_REAL	sys_origin[3];

	uu_denter(UU_STRC,(us,
		"ua_text_box(itxt=%d, e=%s, corners=%s)", itxt, "...", "..."));

	ua_saveattr_text();
	ua_setattr_text(&((*e)),itxt);
	if( ( (*e).txt_blk[itxt-1].tangle!=0.000000e+000 ) )
		{
		um_getcpln(sys_origin,sys_axis[0],sys_axis[1],sys_axis[3
		    -1]);
		ua_get_upvector(&((*e)),(*e).txt_blk[itxt-1].tangle,
		rot_yaxis);
		um_setcpln_yaxis(rot_yaxis);
		}
	txtpos = 1;
	um_vctovc((*e).dim_origin,curloc);
	um_mcstoccs(0,(*e).dim_origin,dimorig_cc);
	boxll_cc[0] = 1.000000e+030;
	boxll_cc[1] = 1.000000e+030;
	boxll_cc[2] = 1.000000e+030;
	boxur_cc[0] = -1.000000e+030;
	boxur_cc[1] = -1.000000e+030;
	boxur_cc[2] = -1.000000e+030;
	linelen = ua_getline_text((*e).txt_blk[itxt-1].tstring,(*e).
	    txt_blk[itxt-1].char_cnt,line,&(txtpos));
	for(;;)
		{
		if( ! (( linelen>0 )) ) goto us_l148;
		us_textextent(curloc,line,concat,extent);
		um_mcstoccs(0,extent[0],extent_cc[0]);
		um_mcstoccs(0,extent[1],extent_cc[1]);
		if( ( extent_cc[0][0]<boxll_cc[0] ) )
			{
			boxll_cc[0] = extent_cc[0][0];
			}
		if( ( extent_cc[0][1]<boxll_cc[1] ) )
			{
			boxll_cc[1] = extent_cc[0][1];
			}
		if( ( extent_cc[0][2]<boxll_cc[2] ) )
			{
			boxll_cc[2] = extent_cc[0][2];
			}
		if( ( extent_cc[1][0]>boxur_cc[0] ) )
			{
			boxur_cc[0] = extent_cc[1][0];
			}
		if( ( extent_cc[1][1]>boxur_cc[1] ) )
			{
			boxur_cc[1] = extent_cc[1][1];
			}
		if( ( extent_cc[1][2]>boxur_cc[2] ) )
			{
			boxur_cc[2] = extent_cc[1][2];
			}
		linelen = ua_getline_text((*e).txt_blk[itxt-1].tstring,(*e).
		    txt_blk[itxt-1].char_cnt,line,&(txtpos));
		um_mcstoccs(0,curloc,curloc_cc);
		curloc_cc[1] = ( curloc_cc[1]-( ( 1.000000e+000+(*e).txt_blk
		    [itxt-1].line_spacing )*(*e).txt_blk[itxt-1].txt_size ) );
		um_cpltrans(curloc_cc,curloc);
		}
us_l148: 
	;
	switch( (*e).entity_site )
		{
		case 0:
			{
			cc[0] = boxll_cc[0];
			cc[1] = boxur_cc[1];
			cc[2] = boxll_cc[2];
			}
			break;
		case 1:
			{
			cc[0] = boxll_cc[0];
			cc[1] = ( boxur_cc[1]-( ( boxur_cc[1]-boxll_cc[1] )/
			    2.000000e+000 ) );
			cc[2] = boxll_cc[2];
			}
			break;
		case 2:
			{
			cc[0] = boxll_cc[0];
			cc[1] = boxll_cc[1];
			cc[2] = boxll_cc[2];
			}
			break;
		case 3:
			{
			cc[0] = ( boxur_cc[0]-( ( boxur_cc[0]-boxll_cc[0] )/
			    2.000000e+000 ) );
			cc[1] = boxur_cc[1];
			cc[2] = boxll_cc[2];
			}
			break;
		case 4:
			{
			cc[0] = ( boxur_cc[0]-( ( boxur_cc[0]-boxll_cc[0] )/
			    2.000000e+000 ) );
			cc[1] = ( boxur_cc[1]-( ( boxur_cc[1]-boxll_cc[1] )/
			    2.000000e+000 ) );
			cc[2] = boxll_cc[2];
			}
			break;
		case 5:
			{
			cc[0] = ( boxur_cc[0]-( ( boxur_cc[0]-boxll_cc[0] )/
			    2.000000e+000 ) );
			cc[1] = boxll_cc[1];
			cc[2] = boxll_cc[2];
			}
			break;
		case 6:
			{
			cc[0] = boxur_cc[0];
			cc[1] = boxur_cc[1];
			cc[2] = boxll_cc[2];
			}
			break;
		case 7:
			{
			cc[0] = boxur_cc[0];
			cc[1] = ( boxur_cc[1]-( ( boxur_cc[1]-boxll_cc[1] )/
			    2.000000e+000 ) );
			cc[2] = boxll_cc[2];
			}
			break;
		case 8:
			{
			cc[0] = boxur_cc[0];
			cc[1] = boxll_cc[1];
			cc[2] = boxll_cc[2];
			}
			break;
		}
	um_cpltrans(cc,wc);
	um_vcmnvc((*e).dim_origin,wc,(*e).txt_blk[itxt-1].origin);
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_box: txorig=<%g,%g,%g>\n",(*e).txt_blk[
		    itxt-1].origin[0],(*e).txt_blk[itxt-1].origin[1],(*e).
		    txt_blk[itxt-1].origin[2]);
		us_write(4,us);
		}
#endif
	um_vcmnvc(dimorig_cc,cc,offset_cc);
		{
		UU_REAL	us_t149[3];
		us_t149[0] = boxll_cc[0];
		us_t149[1] = boxll_cc[1];
		us_t149[2] = boxll_cc[2];
		um_vcplvc(us_t149,offset_cc,corners_cc[0]);
		}
		{
		UU_REAL	us_t150[3];
		us_t150[0] = boxur_cc[0];
		us_t150[1] = boxll_cc[1];
		us_t150[2] = boxur_cc[2];
		um_vcplvc(us_t150,offset_cc,corners_cc[1]);
		}
		{
		UU_REAL	us_t151[3];
		us_t151[0] = boxur_cc[0];
		us_t151[1] = boxur_cc[1];
		us_t151[2] = boxur_cc[2];
		um_vcplvc(us_t151,offset_cc,corners_cc[2]);
		}
		{
		UU_REAL	us_t152[3];
		us_t152[0] = boxll_cc[0];
		us_t152[1] = boxur_cc[1];
		us_t152[2] = boxll_cc[2];
		um_vcplvc(us_t152,offset_cc,corners_cc[3]);
		}
	(*e).txt_blk[itxt-1].dx = ( boxur_cc[0]-boxll_cc[0] );
	(*e).txt_blk[itxt-1].dy = ( boxur_cc[1]-boxll_cc[1] );
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_box: txdx=%g,txdy=%g\n",(*e).txt_blk[
		    itxt-1].dx,(*e).txt_blk[itxt-1].dy);
		us_write(4,us);
		}
#endif
	um_cpltrans(corners_cc[0],corners[0]);
	um_cpltrans(corners_cc[1],corners[1]);
	um_cpltrans(corners_cc[2],corners[2]);
	um_cpltrans(corners_cc[3],corners[3]);
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_box: corners[1]=<%g,%g,%g>\n",corners[0
		    ][0],corners[0][1],corners[0][2]);
		us_write(4,us);
		}
#endif
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_box: corners[2]=<%g,%g,%g>\n",corners[1
		    ][0],corners[1][1],corners[1][2]);
		us_write(4,us);
		}
#endif
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_box: corners[3]=<%g,%g,%g>\n",corners[2
		    ][0],corners[2][1],corners[2][2]);
		us_write(4,us);
		}
#endif
#if UU_DEBUG
		{
		char us[1024];
		sprintf(us,"ua_text_box: corners[4]=<%g,%g,%g>\n",corners[3
		    ][0],corners[3][1],corners[3][2]);
		us_write(4,us);
		}
#endif
	if( ( (*e).txt_blk[itxt-1].tangle!=0.000000e+000 ) )
		{
		um_setcpln_yaxis(sys_axis[1]);
		}
	ua_restattr_text();
	uu_dexit;
	}
