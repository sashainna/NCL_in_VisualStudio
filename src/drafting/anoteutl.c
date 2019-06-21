/*********************************************************************
**    NAME         : anoteutl.c
**       CONTAINS:
** 			ua_saveattr_note
**				ua_restattr_note
**				ua_setattr_note
**				ua_getline_note
**				ua_setbox_note
**			
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       anoteutl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:36
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) anoteutl.c 3.2 2/2/88 17:05:59 single"};
#else
static char uu_sccsident[]={"@(#) anoteutl.c 3.2 2/2/88 17:05:59 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"

static int save_font;		/* save area FOR save/restore */
static Gtxprec	save_prec;
static UU_REAL save_exp;
static UU_REAL	save_spacing;
static UU_REAL	save_height;
static Gtxpath	save_path;
static Gtxhor save_alignh;
static Gtxver save_alignv;
static UU_REAL save_charup[3]; 
UU_REAL sgqcharspace(),sgqcharexp(),sgqcharheight();

/*********************************************************************
**    E_FUNCTION     : ua_setbox_note(e, itxt)
**			Get text box lower left, upper right coordinate starting
**			at location FOR multi-line text STRING. Set the text block
**			origin according TO the txt_just and the entity_site.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_setbox_note(e, itxt)
struct UA_generic_draft	(*e);
int	itxt;
	{
	int		linelen;
	UU_REAL	concat[3];
	UU_REAL	curloc[3];
	UU_REAL	extent[2][3];
	UU_REAL	boxll[3];
	char		line[1025];
	int		txtpos;
	UU_REAL	boxur[3];

	uu_denter(UU_STRC,(us,
		"ua_setbox_note(e=%s, itxt=%d)", "...", itxt));

	txtpos = 1;
	um_vctovc((*e).txt_blk[itxt-1].origin,curloc);
	boxll[0] = 1.000000e+030;
	boxll[1] = 1.000000e+030;
	boxll[2] = 1.000000e+030;
	boxur[0] = -1.000000e+030;
	boxur[1] = -1.000000e+030;
	boxur[2] = -1.000000e+030;
	linelen = ua_getline_note((*e).txt_blk[itxt-1].tstring,(*e).
	    txt_blk[itxt-1].char_cnt,line,&(txtpos));
	for(;;)
		{
		if( ! (( linelen>0 )) ) goto us_l140;
		us_textextent(curloc,line,concat,extent);
		if( ( extent[0][0]<boxll[0] ) )
			{
			boxll[0] = extent[0][0];
			}
		if( ( extent[0][1]<boxll[1] ) )
			{
			boxll[1] = extent[0][1];
			}
		if( ( extent[0][2]<boxll[2] ) )
			{
			boxll[2] = extent[0][2];
			}
		if( ( extent[1][0]>boxur[0] ) )
			{
			boxur[0] = extent[1][0];
			}
		if( ( extent[1][1]>boxur[1] ) )
			{
			boxur[1] = extent[1][1];
			}
		if( ( extent[1][2]>boxur[2] ) )
			{
			boxur[2] = extent[1][2];
			}
		linelen = ua_getline_note((*e).txt_blk[itxt-1].tstring,(*e).
		    txt_blk[itxt-1].char_cnt,line,&(txtpos));
		curloc[1] = ( curloc[1]-( ( 1.000000e+000+(*e).txt_blk[itxt
		    -1].line_spacing )*(*e).txt_blk[itxt-1].txt_size ) );
		}
us_l140: 
	;
	switch( (*e).entity_site )
		{
		case 0:
			{
				{
				UU_REAL	us_t141[3];
				UU_REAL	us_t142[3];
				us_t141[0] = 0.000000e+000;
				us_t141[1] = 0.000000e+000;
				us_t141[2] = 0.000000e+000;
				us_t142[0] = boxll[0];
				us_t142[1] = boxur[1];
				us_t142[2] = boxll[2];
				um_vcmnvc(us_t141,us_t142,(*e).txt_blk[itxt-1].origin);
				}
			}
			break;
		case 1:
			{
				{
				UU_REAL	us_t143[3];
				UU_REAL	us_t144[3];
				us_t143[0] = 0.000000e+000;
				us_t143[1] = 0.000000e+000;
				us_t143[2] = 0.000000e+000;
				us_t144[0] = boxll[0];
				us_t144[1] = ( boxur[1]-( ( boxur[1]-boxll[1] )/
				    2.000000e+000 ) );
				us_t144[2] = boxll[2];
				um_vcmnvc(us_t143,us_t144,(*e).txt_blk[itxt-1].origin);
				}
			}
			break;
		case 2:
			{
				{
				UU_REAL	us_t145[3];
				UU_REAL	us_t146[3];
				us_t145[0] = 0.000000e+000;
				us_t145[1] = 0.000000e+000;
				us_t145[2] = 0.000000e+000;
				us_t146[0] = boxll[0];
				us_t146[1] = boxll[1];
				us_t146[2] = boxll[2];
				um_vcmnvc(us_t145,us_t146,(*e).txt_blk[itxt-1].origin);
				}
			}
			break;
		case 3:
			{
				{
				UU_REAL	us_t147[3];
				UU_REAL	us_t148[3];
				us_t147[0] = 0.000000e+000;
				us_t147[1] = 0.000000e+000;
				us_t147[2] = 0.000000e+000;
				us_t148[0] = ( boxur[0]-( ( boxur[0]-boxll[0] )/
				    2.000000e+000 ) );
				us_t148[1] = boxur[1];
				us_t148[2] = boxll[2];
				um_vcmnvc(us_t147,us_t148,(*e).txt_blk[itxt-1].origin);
				}
			}
			break;
		case 4:
			{
				{
				UU_REAL	us_t149[3];
				UU_REAL	us_t150[3];
				us_t149[0] = 0.000000e+000;
				us_t149[1] = 0.000000e+000;
				us_t149[2] = 0.000000e+000;
				us_t150[0] = ( boxur[0]-( ( boxur[0]-boxll[0] )/
				    2.000000e+000 ) );
				us_t150[1] = ( boxur[1]-( ( boxur[1]-boxll[1] )/
				    2.000000e+000 ) );
				us_t150[2] = boxll[2];
				um_vcmnvc(us_t149,us_t150,(*e).txt_blk[itxt-1].origin);
				}
			}
			break;
		case 5:
			{
				{
				UU_REAL	us_t151[3];
				UU_REAL	us_t152[3];
				us_t151[0] = 0.000000e+000;
				us_t151[1] = 0.000000e+000;
				us_t151[2] = 0.000000e+000;
				us_t152[0] = ( boxur[0]-( ( boxur[0]-boxll[0] )/
				    2.000000e+000 ) );
				us_t152[1] = boxll[1];
				us_t152[2] = boxll[2];
				um_vcmnvc(us_t151,us_t152,(*e).txt_blk[itxt-1].origin);
				}
			}
			break;
		case 6:
			{
				{
				UU_REAL	us_t153[3];
				UU_REAL	us_t154[3];
				us_t153[0] = 0.000000e+000;
				us_t153[1] = 0.000000e+000;
				us_t153[2] = 0.000000e+000;
				us_t154[0] = boxur[0];
				us_t154[1] = boxur[1];
				us_t154[2] = boxll[2];
				um_vcmnvc(us_t153,us_t154,(*e).txt_blk[itxt-1].origin);
				}
			}
			break;
		case 7:
			{
				{
				UU_REAL	us_t155[3];
				UU_REAL	us_t156[3];
				us_t155[0] = 0.000000e+000;
				us_t155[1] = 0.000000e+000;
				us_t155[2] = 0.000000e+000;
				us_t156[0] = boxur[0];
				us_t156[1] = ( boxur[1]-( ( boxur[1]-boxll[1] )/
				    2.000000e+000 ) );
				us_t156[2] = boxll[2];
				um_vcmnvc(us_t155,us_t156,(*e).txt_blk[itxt-1].origin);
				}
			}
			break;
		case 8:
			{
				{
				UU_REAL	us_t157[3];
				UU_REAL	us_t158[3];
				us_t157[0] = 0.000000e+000;
				us_t157[1] = 0.000000e+000;
				us_t157[2] = 0.000000e+000;
				us_t158[0] = boxur[0];
				us_t158[1] = boxll[1];
				us_t158[2] = boxll[2];
				um_vcmnvc(us_t157,us_t158,(*e).txt_blk[itxt-1].origin);
				}
			}
			break;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_saveattr_note()
**       Save current GKS text attributes in private storage - use
**			in conjunction with ua_restattr_note.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_saveattr_note()
	{
	Gtxprec sgqtextprec();
	Gtxhor sgqtextalignh();
	Gtxver sgqtextalignv();
	Gtxpath	sgqtextpath();

	uu_denter(UU_STRC,(us,"ua_saveattr_note()"));

	save_font = sgqtextfont();
	save_prec = sgqtextprec();
	save_exp = sgqcharexp();
	save_spacing = sgqcharspace();
	save_height = sgqcharheight();
	save_path = sgqtextpath();
	save_alignh = sgqtextalignh();
	save_alignv = sgqtextalignv();
	sgqcharup(save_charup);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : int		ua_getline_note(stg, stglen, line, pos)
**			Get the next line FROM the text STRING - lines are
**			terminated BY a carriage-RETURN.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int		ua_getline_note(stg, stglen, line, pos)
char		stg[1];
int		stglen;
char		line[1];
int		(*pos);
	{
	int		linelen;
	int		posbeg;

	uu_denter(UU_STRC,(us,"ua_getline_note(stg=%s,stglen=%d,line=%s,pos=%d)",
		stg, stglen, line, *pos));

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
		    ) ) goto us_l159;
		(*pos) = ( (*pos)+1 );
		linelen = ( linelen+1 );
		}
us_l159: 
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
**    E_FUNCTION     : ua_restattr_note()
**       Restore current GKS  text attributes FROM private storage
**			set BY ua_saveattr_note.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_restattr_note()
	{
	Gtxfp	fp;
	Gtxalign	align;
	int		status;

	uu_denter(UU_STRC,(us,"ua_restattr_note()"));

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
**    E_FUNCTION     : ua_setattr_note(e, itxt)
**       Set GKS text attributes FROM drafting entity in text block
**			element itxt_blk.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_setattr_note(e, itxt)
struct UA_generic_draft	(*e);
int		itxt;
	{
	Gtxfp	fp;
	Gtxalign	align;
	int		status;

	uu_denter(UU_STRC,(us,"ua_setattr_note(e=%s, itxt=%d)", "...", itxt));

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
		{
		UU_REAL	us_t160[3];
		ua_get_upvector(&((*e)),(*e).txt_blk[itxt-1].tangle,us_t160)
			;
		status = gscharup3(us_t160);
		}
	uu_dexit;
	}
