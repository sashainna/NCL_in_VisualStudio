
/*********************************************************************
**    NAME         : adisplay.c
**       CONTAINS:
**    		 ua_display_text_on_arc(entity, i)
**      		 ua_disp1_drafting
**      		 ua_disp1_ovrd_box
**      		 ua_disp1_ovrd_box_polar
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       adisplay.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:33
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) adisplay.c 25.1 04/29/15 15:05:33 single"};
#else
static char uu_sccsident[]={"@(#) adisplay.c 25.1 04/29/15 15:05:33 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "go.h"
#include "umath.h"
#include "mdcoord.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
 
/*********************************************************************
**    E_FUNCTION     : ua_display_text_on_arc(entity, i)
**       display text printed on an arc
**    PARAMETERS   
**       INPUT  : 
**          entity -	generic entity record of entity to display
**				i		 - text block to display
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_display_text_on_arc(entity, i)
struct UA_generic_draft	(*entity);
int		i;
	{
	char		eos[2], back_slash[2], cr[2], tstring[1025], textstr[2],
			tmpchar[1025];
	int		disply_text_cnt, sign, last, num_lines, num_char, next,
			text_cnt, j, status, first, next_char;
	UM_coord	first_text_loc_cc, text_loc_wc, text_loc_cc, first_text_loc_wc,
			text_vec, temp_vec, upvector, center_pt_cc, tmp1, tmp2;
	UU_REAL	delta_theta, theta, text_length, first_angle, ang, temp;

	uu_denter(UU_STRC,(us,"SAL ua_display_text_on_arc(entity=%s, i=%d)",
		"...", i));

	/* initilize local data */
	strcpy(cr,"\n");
	strcpy(back_slash,"\\");
	strcpy(eos,"");
	num_char = entity->txt_blk[i-1].char_cnt;
	strcpy(tstring,entity->txt_blk[i-1].tstring);
	text_cnt = 0;
	num_lines = 1;

	/* loop over characters in the string */
	for(last = 1;last<=num_char;last++)
		{
		tmpchar[0] = tstring[last-1];
		tmpchar[1] = '\0';
		if( ( strcmp( tmpchar, cr ) == 0 ) )
			{
			if( ( last>text_cnt ) )
				{
				text_cnt = ( last-1 );
				}
			last = 0;
			}
		}

	if( ( num_lines==1 ) ) text_cnt = last;
	disply_text_cnt = 0;

	for(j=1;j<=last;j++)
		{
		disply_text_cnt = ( disply_text_cnt+1 );
		tmpchar[0] = tstring[j-1];
		tmpchar[1] = '\0';
		if( ( strcmp( tmpchar, back_slash ) == 0 ) ) j++;
		}

	if( ( disply_text_cnt==0 ) ) disply_text_cnt = 1;
	text_length = entity->txt_blk[i-1].dx;
	um_vcmnvc(entity->dim_origin,entity->arc_blk[0].center_pt,tmp1);
	theta = ( text_length/um_mag(tmp1) );
	um_vcmnvc(entity->dim_origin,entity->arc_blk[0].center_pt,tmp1);
	temp = um_mag(tmp1);
	delta_theta = ( theta/( (UU_REAL)disply_text_cnt ) );
	first_angle = entity->txt_blk[i-1].tangle;
	um_vcplvc(entity->dim_origin,entity->txt_blk[i-1].origin,
			first_text_loc_wc);
	um_mcstoccs(0,first_text_loc_wc,first_text_loc_cc);
	um_mcstoccs(0,entity->arc_blk[0].center_pt,center_pt_cc) ;
	um_vcmnvc(first_text_loc_cc,center_pt_cc,temp_vec);
	tmp1[0] = 0.0;
	tmp1[1] = 1.0;
	tmp1[2] = 0.0;
	if( ( um_dot(temp_vec,tmp1)>=0.0 ) )
		sign = 1;
	else
		sign = -1;

	for(next = 1; next<=entity->txt_blk[i-1].char_cnt;)
		{
		next_char = 0;
		ang = first_angle;
		um_vctovc(first_text_loc_wc,text_loc_wc);
		for(;;)
			{
			tmpchar[0] = tstring[next-1];
			tmpchar[1] = '\0';
			if(!((strcmp(tmpchar,cr)!= 0 )&&( next<=text_cnt )))
				goto end_loop3;
			ang = ( first_angle-( ( (UU_REAL)( sign*next_char ) )*
			 			   delta_theta ) );
			ua_get_upvector(&((*entity)),ang,upvector);
			status = gscharup3(upvector);
			tmpchar[0] = tstring[next-1];
			tmpchar[1] = '\0';
			if( ( strcmp( tmpchar, back_slash ) == 0 ) )
				{
				strncpy( textstr, tstring + next - 1, (( next+1 ) - next + 1) );
				textstr[ (( next+1 ) - next + 1) ] = '\0';
				next = ( next+1 );
				}
			else
				{
				textstr[0] = tstring[next-1];
				textstr[1] = '\0';
				}
			status = gtext(text_loc_wc,textstr);
			um_mcstoccs(0,text_loc_wc,text_loc_cc);
			um_vcmnvc(text_loc_cc,center_pt_cc,text_vec);
			tmp1[0] = ( ( cos(delta_theta)*text_vec[0] )+( ( ( 
				(UU_REAL)sign)*sin(delta_theta))*text_vec[1]));
			tmp1[1] = (((((UU_REAL)( -sign ) )*sin(delta_theta) )
			    *text_vec[0] )+( cos(delta_theta)*text_vec[1] ) );
			tmp1[2] = 0.0;
			um_vcplvc(center_pt_cc,tmp1,text_loc_cc);
			um_cpltrans(text_loc_cc,text_loc_wc);
			next = ( next+1 );
			next_char = ( next_char+1 );
			}
end_loop3:;
		next = ( next+1 );
		um_mcstoccs(0,first_text_loc_wc,first_text_loc_cc);
		first_text_loc_cc[1] = ( first_text_loc_cc[1]-( entity->
		    	txt_blk[i-1].txt_size*(entity->txt_blk[i-1].line_spacing+
			    1.0 ) ) );
		um_cpltrans(first_text_loc_cc,first_text_loc_wc);
		}
	if( ( entity->txt_blk[i-1].tangle!=0.0 ) )
		{
		tmp1[0] = 0.0;
		tmp1[1] = 1.0;
		tmp1[2] = 0.0;
		um_setcpln_yaxis(tmp1);
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_disp1_ovrd_box(entity, upvector, xvector, cr)
**       Replace dimension text display with box
**    PARAMETERS   
**       INPUT  : 
**          entity 					-	generic entity record of entity to display
**				upvector, xvector	   -  box coord. system
**				cr							- character string terminator
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_disp1_ovrd_box(entity, upvector, xvector, cr)
struct UA_generic_draft	*entity;
UM_coord upvector, xvector;
char     cr[2];
	{
	int i, j, k, text_len, first, tcolor, tstyle, status;
	char tstring[1025], tmp_char[1025];
	UU_REAL delta;
	UM_coord boxll_cc, boxur_cc, extent[2], extent_cc[2], corn_cc[4],
				corn[5],tmp1, tmp2, tmp3;

	uu_denter(UU_STRC,(us,"ua_disp1_ovrd_box"));

	/* check if any text blocks */
	if(entity->txt_blk_use == 0) goto fexit;

	/* initialize text box */
	for(i=0;i<3;i++)
		{
		boxll_cc[i] = 1.000000e+020;
		boxur_cc[i] = -1.000000e+020;
		}

	/* cycle thru text associated text blocks */
	for(i=0;i < (entity->txt_blk_use );i++)
		{
		if(entity->txt_blk[i].subtype != polar_arc_data) /* skip polar arcs */
			{

			switch( entity->txt_just )  /* determine origin */
				{
				case 0: /* LEFT JUSTIFIED */
					um_vcplvc(entity->txt_blk[i].origin,
											entity->dim_origin ,extent[0]);
					break;
				case 1: /* RIGHT JUSTIFIED */
					um_vcplvc(entity->txt_blk[i].origin,
									entity->dim_origin ,tmp1);
					um_vctmsc(xvector,entity->txt_blk[i].dx,tmp2);
					um_vcmnvc(tmp1,tmp2,extent[0]);
					break;
				case 2: /* CENTERED */
					um_vcplvc(entity->txt_blk[i].origin,
										entity->dim_origin ,tmp1);
					um_vctmsc(xvector,( entity->txt_blk[i].dx/2.0),tmp2);
					um_vcmnvc(tmp1,tmp2,extent[0]);
					break;
				}

			/* computed box extent */
			um_vctmsc(xvector,entity->txt_blk[i].dx,tmp1);
			um_vcplvc(extent[0],tmp1,tmp2);
			um_vctmsc(upvector,entity->txt_blk[i].dy,tmp3);
			um_vcplvc(tmp2,tmp3,extent[1]);
			um_mcstoccs(0,extent[0],extent_cc[0]);
			um_mcstoccs(0,extent[1],extent_cc[1]);
			text_len = entity->txt_blk[i].char_cnt;
			strcpy(tstring,entity->txt_blk[i].tstring);
			first = 1;
			for(j=1;j>text_len;j++)
				{
				tmp_char[0] = tstring[j-1];
				tmp_char[1] = '\0';
				if( ( ( strcmp( tmp_char, cr ) == 0 )&&( j!=text_len ) ) )
					{
					first = ( first+1 );
					}
				}
			if( ( first>1 ) )
				{
				delta = ( entity->txt_blk[i].dy/( (UU_REAL)first ) );
				extent_cc[0][1] = (extent_cc[0][1]-
										(((UU_REAL)( first -1))*delta ) );
				extent_cc[1][1] = ( extent_cc[1][1]-
										( ( (UU_REAL)( first -1 ) )*delta ) );
				}

			/* add text block extent into overall box */
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
			}
		}

	/* transform overall box */
	um_vctovc(boxll_cc,corn_cc[0]);
	corn_cc[1][0] = boxur_cc[0];
	corn_cc[1][1] = boxll_cc[1];
	corn_cc[1][2] = boxur_cc[2];
	um_vctovc(boxur_cc,corn_cc[2]);
	corn_cc[3][0] = boxll_cc[0];
	corn_cc[3][1] = boxur_cc[1];
	corn_cc[3][2] = boxll_cc[2];
	for(i=0;i<4;i++)
		{
		um_cpltrans(corn_cc[i],corn[i]);
		}
	um_vctovc(corn[0],corn[4]);

	/* display box as a polyline */
	tcolor = entity->txt_blk[0].text.color;
	tstyle = UA_ext_line_font;
	status = us_set_dispattr(entity->key,tcolor,tstyle, (UU_REAL) 1.0,2);
	status = gpolyline3(5,corn);

fexit:;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_disp1_ovrd_box_polar(entity, upvector, xvector, cr)
**       Replace polar arc dimension text display with box
**    PARAMETERS   
**       INPUT  : 
**          entity 					-	generic entity record of entity to display
**				upvector, xvector	   -  box coord. system
**				cr							- character string terminator
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_disp1_ovrd_box_polar(entity, upvector, xvector, cr)
struct UA_generic_draft	*entity;
UM_coord upvector, xvector;
char     cr[2];
	{
	int i, j, k, text_len, first, tcolor, tstyle, status;
	char tstring[1025], tmp_char[1025];
	UU_REAL delta;
	UM_coord boxll_cc, boxur_cc, extent[2], extent_cc[2], corn_cc[4],
				corn[5],tmp1, tmp2, tmp3;

	uu_denter(UU_STRC,(us,"ua_disp1_ovrd_box"));

	/* check if any text blks */
	if(entity->txt_blk_use == 0) goto fexit;

	/* initialize text box */
	for(i=0;i<3;i++)
		{
		boxll_cc[i] = 1.000000e+020;
		boxur_cc[i] = -1.000000e+020;
		}

	/* cycle thru text associated text blocks */
	for(i=0;i < (entity->txt_blk_use );i++)
		{
		if(entity->txt_blk[i].subtype == polar_arc_data) /* skip polar arcs */
			{

			switch( entity->txt_just )  /* determine origin */
				{
				case 0: /* LEFT JUSTIFIED */
					um_vcplvc(entity->txt_blk[i].origin,
											entity->dim_origin ,extent[0]);
					break;
				case 1: /* RIGHT JUSTIFIED */
					um_vcplvc(entity->txt_blk[i].origin,
									entity->dim_origin ,tmp1);
					um_vctmsc(xvector,entity->txt_blk[i].dx,tmp2);
					um_vcmnvc(tmp1,tmp2,extent[0]);
					break;
				case 2: /* CENTERED */
					um_vcplvc(entity->txt_blk[i].origin,
										entity->dim_origin ,tmp1);
					um_vctmsc(xvector,( entity->txt_blk[i].dx/2.0),tmp2);
					um_vcmnvc(tmp1,tmp2,extent[0]);
					break;
				}

			/* computed box extent */
			um_vctmsc(xvector,entity->txt_blk[i].dx,tmp1);
			um_vcplvc(extent[0],tmp1,tmp2);
			um_vctmsc(upvector,entity->txt_blk[i].dy,tmp3);
			um_vcplvc(tmp2,tmp3,extent[1]);
			um_mcstoccs(0,extent[0],extent_cc[0]);
			um_mcstoccs(0,extent[1],extent_cc[1]);
			text_len = entity->txt_blk[i].char_cnt;
			strcpy(tstring,entity->txt_blk[i].tstring);
			first = 1;
			for(j=1;j>text_len;j++)
				{
				tmp_char[0] = tstring[j-1];
				tmp_char[1] = '\0';
				if( ( ( strcmp( tmp_char, cr ) == 0 )&&( j!=text_len ) ) )
					{
					first = ( first+1 );
					}
				}
			if( ( first>1 ) )
				{
				delta = ( entity->txt_blk[i].dy/( (UU_REAL)first ) );
				extent_cc[0][1] = (extent_cc[0][1]-
										(((UU_REAL)( first -1))*delta ) );
				extent_cc[1][1] = ( extent_cc[1][1]-
										( ( (UU_REAL)( first -1 ) )*delta ) );
				}

			/* add text block extent into overall box */
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
			}
		}

	/* transform overall box */
	um_vctovc(boxll_cc,corn_cc[0]);
	corn_cc[1][0] = boxur_cc[0];
	corn_cc[1][1] = boxll_cc[1];
	corn_cc[1][2] = boxur_cc[2];
	um_vctovc(boxur_cc,corn_cc[2]);
	corn_cc[3][0] = boxll_cc[0];
	corn_cc[3][1] = boxur_cc[1];
	corn_cc[3][2] = boxll_cc[2];
	for(i=0;i<4;i++)
		{
		um_cpltrans(corn_cc[i],corn[i]);
		}
	um_vctovc(corn[0],corn[4]);

	/* display box as a polyline */
	tcolor = entity->txt_blk[0].text.color;
	tstyle = UA_ext_line_font;
	status = us_set_dispattr(entity->key,tcolor,tstyle, (UU_REAL) 1.0,2);
	status = gpolyline3(5,corn);

fexit:;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_disp1_drafting(entity, tfmat)
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
ua_disp1_drafting(entity, tfmat)
struct UA_generic_draft	(*entity);
UU_REAL	tfmat[4][3];
	{
	char		textstr[1025], eos[2], back_slash[2], cr[2], tstring[1025],
				tmpchar[1025];
	Gtxfp	fp;
	Gtxfp	txtfprec;
	Gtxalign	save_txalign;
	Gtxalign	txalign;
	Gtxprec	save_prec;
	int		text_len, pos, tcolor, i, save_color, j, status, tstyle,
				first, ccnt, save_font, last, segname;
	UM_coord	save_upvector[3],upvector,tloc_cc, sys_xaxis, sys_yaxis,
			sys_zaxis,xvector, tloc_wc, origin, txt_pos, save_zaxis,
			ln_pts[2], xaxis, yaxis, zaxis, sys_origin;
	UU_REAL	theight, save_spacing, spacing, save_theight, scal_factor,
				density, save_expansion, expansion;
	UU_REAL sgqcharspace(),sgqcharexp(),sgqcharheight();
	UU_LOGICAL	tstat;
	Gtxprec sgqtextprec();
	Gtxhor sgqtextalignh();
	Gtxver sgqtextalignv();

	uu_denter(UU_STRC,(us,"SAL ua_disp1_drafting(entity=%s, tfmat=%s)",
			"...", "..."));

	strcpy(cr,"\n");
	strcpy(back_slash,"\\");
	strcpy(eos,"");
	ua_transform_drafting(&((*entity)),tfmat,UU_FALSE);
	save_font = sgqtextfont();
	save_prec = sgqtextprec();
	save_color = sgqtextcolor();
	sgqtxplane(save_zaxis);
	sgqcharup(save_upvector);
	save_expansion = sgqcharexp();
	save_spacing = sgqcharspace();
/* NCL - commented by kathy to fix the char height
	save_theight = sgqcharheight();
*/
	save_txalign.hor = sgqtextalignh();
	save_txalign.ver = sgqtextalignv();
	um_getcpln(sys_origin,sys_xaxis,sys_yaxis,sys_zaxis);
	ua_getcpln(&((*entity)),origin,xaxis,yaxis,zaxis);
	um_setcpln_origin(origin);
	um_setcpln_zaxis(zaxis);
	um_setcpln_yaxis(yaxis);
	um_setcpln_xaxis(xaxis);
	if( ( UA_text_box_ovrd==0 ) )
		{
		status = gstxplane(zaxis);
		if( ( ( entity->txt_blk_use>10 )||( entity->txt_blk_use< 0 ) ) )
			goto fexit;
		for(i=1;i<=entity->txt_blk_use;i++)
			{
			pos = 0;
			if( ( ( entity->txt_blk[i-1].char_cnt>1024 )||( entity->
			    txt_blk[i-1].char_cnt<0 ) ) ) goto fexit;
			tcolor = entity->txt_blk[i-1].text.color;
			tstyle = UA_ext_line_font;
			density = entity->txt_blk[i-1].text.line_density;
			status = us_set_dispattr(entity->key,tcolor,tstyle,density ,1);
			expansion = entity->txt_blk[i-1].char_expansion;
			status = gscharexp(expansion);
			spacing = entity->txt_blk[i-1].char_space;
			status = gscharspace(spacing);
			theight = entity->txt_blk[i-1].txt_size;
			status = gscharheight(theight);
			txtfprec.font = entity->txt_blk[i-1].text.line_font;
			txtfprec.prec = UA_txt_precision;
			status = gstextfp(&(txtfprec));
			switch( entity->txt_just )
				{
				case 0:
					txalign.hor = UG_TH_LEFT;
					break;
				case 1:
					txalign.hor = UG_TH_RIGHT;
					break;
				case 2:
					txalign.hor = UG_TH_CENTRE;
					break;
				}
		txalign.ver = UG_TV_BOTTOM;
		status = gstextalign(&(txalign));
		if ( ( (entity->etype != UA_ARC_LEN_DIM) &&
			(entity->etype != UA_ANGULAR_DIM) )
			||( entity->txt_orent==0 )
			||( entity->arc_blk_use==0) )
			{
			ua_get_upvector(&((*entity)),entity->txt_blk[i-1].tangle,upvector);
			status = gscharup3(upvector);
			if( entity->txt_blk[i-1].tangle!=0.0 ) um_setcpln_yaxis(upvector);
			um_vcplvc(entity->dim_origin,entity->txt_blk[i-1].origin,tloc_wc);
			text_len = entity->txt_blk[i-1].char_cnt;
			strcpy(tstring,entity->txt_blk[i-1].tstring);

			for(first = 1;first <= text_len;first = last + 1)
				{
				for(last = first;;last++)
					{
					tmpchar[0] = tstring[last-1];
					tmpchar[1] = '\0';
					if( ! (( ( strcmp( tmpchar, cr ) != 0 )
						&&( last<=text_len ))) ) goto end_loop5;
					}
end_loop5:;
					tmpchar[0] = tstring[last-1];
					tmpchar[1] = '\0';
					if( ( strcmp( tmpchar, cr ) == 0 ) )
						{
						strncpy( textstr, tstring + first - 1,
							(( last-1 ) - first + 1) );
						textstr[ (( last-1 ) - first + 1) ] = '\0';
						}
					else
						{
						strncpy( textstr, tstring + first - 1,
							(last - first + 1) );
						textstr[ (last - first + 1) ] = '\0';
						}
					status = gtext(tloc_wc,textstr);
					um_mcstoccs(0,tloc_wc,tloc_cc);
					tloc_cc[1] = ( tloc_cc[1]-( entity->txt_blk[i-1].txt_size*
					    ( entity->txt_blk[i-1].line_spacing+1.0 ) ) );
					um_cpltrans(tloc_cc,tloc_wc);
					}
				if( ( entity->txt_blk[i-1].tangle!=0.0) ) um_setcpln_yaxis(yaxis);
				}
			else
				ua_display_text_on_arc(&((*entity)),i);
			}
		}
	else
		{
		if( ( entity->txt_blk[0].tangle!=0.0) )
			{
			ua_get_upvector(&((*entity)),entity->txt_blk[0].tangle, upvector);
			um_cross(upvector,zaxis,xvector);
			um_setcpln_xaxis(xvector);
			um_setcpln_yaxis(upvector);
			}
		else
			{
			um_vctovc(yaxis,upvector);
			um_vctovc(xaxis,xvector);
			}
		ua_disp1_ovrd_box(entity, upvector, xvector, cr);
		if(entity->etype == UA_POLAR_DIM)
								ua_disp1_ovrd_box_polar(entity, upvector, xvector, cr);
		}

	um_setcpln_yaxis(yaxis);
	um_setcpln_xaxis(xaxis);
	if( ( entity->etype==UA_CROSSHATCH ) )
		{
		ua_disp_xhatch(&((*entity)),tfmat);
		}
	else
		{
		if( ( ( entity->line_blk_use>5 )||( entity->line_blk_use <0 ) ) )
																							goto fexit;
		for(i=1;i<=entity->line_blk_use;i++)
			{
			if( ( ( entity->line_blk[i-1].num_pts>50 )
						||( entity->  line_blk[i-1].num_pts<0 ) ) ) goto fexit;
			tcolor = entity->line_blk[i-1].line.color;
			tstyle = entity->line_blk[i-1].line.line_font;
			density = entity->line_blk[i-1].line.line_density;
			status = us_set_dispattr(entity->key,tcolor,tstyle,density,2);
			for(j=1;j<=entity->line_blk[i-1].num_pts;j++,j++)
				{
				ln_pts[0][0] = entity->line_blk[i-1].line_seg[j-1][0];
				ln_pts[0][1] = entity->line_blk[i-1].line_seg[j-1][1];
				ln_pts[0][2] = entity->line_blk[i-1].line_seg[j-1][2];
				ln_pts[1][0] = entity->line_blk[i-1].line_seg[j][0];
				ln_pts[1][1] = entity->line_blk[i-1].line_seg[j][1];
				ln_pts[1][2] = entity->line_blk[i-1].line_seg[j][2];
				status = gpolyline3(2,ln_pts);
				}
			}
		}
	if( ( ( entity->arc_blk_use>5 )||( entity->arc_blk_use<0) ) ) goto fexit;

	for(i=1;i<=entity->arc_blk_use;i++)
		{
		tcolor = entity->arc_blk[i-1].arc.color;
		tstyle = entity->arc_blk[i-1].arc.line_font;
		density = entity->arc_blk[i-1].arc.line_density;
		status = us_set_dispattr(entity->key,tcolor,tstyle,density ,2);
		if( ( ( entity->arc_blk[i-1].num_pts>50 )
					||( entity->arc_blk[i-1].num_pts<0 ) ) ) goto fexit;
		for(j=1;j<=entity->arc_blk[i-1].num_pts;j++,j++)
			{
			ua_disp1_arc(entity->key,entity->arc_blk[i-1].center_pt,
					entity->arc_blk[i-1].radius,
					entity->arc_blk[i-1].angles[ j-1],
					entity->arc_blk[i-1].angles[( j+1 )-1],
					tcolor,tstyle, origin,xaxis,yaxis,zaxis);
			}
		}

	/* check if drawing a PARTS LIST TABLE */
	if(entity->etype == UA_PL_DIM) ua_pl_draw(entity, tfmat);

	scal_factor = ( entity->stub_length/UA_ldr_stub_len );
	scal_factor = ( UA_ldr_stub_len/entity->stub_length );
	ua_arrowhead(entity->arrow_blk_use,entity->arrow_blk, scal_factor);

fexit:
	status = gstextcolor(save_color);
	fp.font = save_font;
	fp.prec = save_prec;
	status = gstextfp(&(fp));
	status = gscharexp(save_expansion);
	status = gscharspace(save_spacing);
/* NCL - changed by kathy to fix char height
	status = gscharheight(save_theight);
*/
	status = gscharheight(theight);
/* end NCL */
	status = gstextalign(&(save_txalign));
	status = gscharup3(save_upvector);
	status = gstxplane(save_zaxis);
	um_setcpln_origin(sys_origin);
	um_setcpln_zaxis(sys_zaxis);
	um_setcpln_yaxis(sys_yaxis);
	um_setcpln_xaxis(sys_xaxis);
	uu_dexit;
	}
