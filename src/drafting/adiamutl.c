/*********************************************************************
**    NAME         : adiamutl.c
**       CONTAINS:
**					ua_dia_sym_adjust
**					ua_dia_inside
**					ua_dia_outside
**					ua_dia_oneside
**					ua_dia_shaft_cre
**					ua_dia_shaft_regen
**					ua_dia_shaft
**					ua_dia_cyln_cre
**					ua_dia_cyln_regen
**					ua_dia_cyln
**					ua_dia_outiso
**					ua_dia_plus
**					ua_dia_rep_dim
**					ua_dia_cre_rep_dim
**	
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       adiamutl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:33
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) adiamutl.c 4.3 8/11/89 13:34:27 single"};
#else
static char uu_sccsident[]={"@(#) adiamutl.c 4.3 8/11/89 13:34:27 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "dasnog.h"
#include "mdcoord.h"

#define UA_OUT_BTW     0
#define UA_IN_BTW      1
#define UA_IN_NOT_BTW  2
#define UA_OUT_NOT_BTW 3

extern int UD_draftable[UD_NMENTWD];
extern int UD_draft_line[UD_NMENTWD];
/*********************************************************************
**    E_FUNCTION     : ua_dia_cyln_regen(rdim)
**       Regenerate diam dimension after text edit
**    PARAMETERS   
**       INPUT  : 
**				rdim						drafting record
**				center
**       OUTPUT :  
**				rdim						undated drafting record
**				center
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_dia_cyln_regen(rdim)
struct UA_generic_draft	(*rdim);
	{

	uu_denter(UU_STRC,(us,"ua_dia_cyln_regen(rdim=%s)", "..."));

	ua_dia_cyln_cre(&(*rdim));
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_dia_shaft()
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
ua_dia_shaft()
	{
	struct UA_PLOCREC	plocrec;
	struct UA_generic_draft	rdim;
	int		relation;
	int		nint;
	int		cent_key;
	int		dir;
	int		ablock;
	int		num;
	int		curr_key;
	int		prev_key;
	int		subtype;
	int		ok;
	int		s_status;
	int		prev_txt_blk;
	int		i;
	int		j;
	int		d_stat;
	int		k;
	int		l;
	int		m;
	int		n;
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	origin[3];
	UU_LOGICAL	redo;
	UU_LOGICAL	status;
	UU_LOGICAL	first;

	uu_denter(UU_STRC,(us,"ua_dia_shaft()"));

	subtype = 7;
	ua_init_entity(UA_DIA_IN_DIM,subtype,&(rdim));
	first = UU_TRUE;
get_line:
	ud_lgeo(UU_TRUE,UD_draftable);
	s_status = ua_select_ent_subf(107,&(rdim),1,&(plocrec));
	if( ( s_status!=UA_OPCOMPLETE ) )
		{
		uu_dexit;
		return;
		}
	cent_key = rdim.asso_blk[0].key;
	ok = uc_draft_type(cent_key,&(relation));
	if( ( relation!=UA_DRAFT_LINE ) )
		{
		goto get_line;
		}
main_loop:
	ua_init_entity(UA_DIA_IN_DIM,subtype,&(rdim));
	redo = UU_FALSE;
	rdim.txt_just = UA_CENTER;
	rdim.entity_site = UA_TOP_CENTER;
	rdim.txt_place = UA_MANUAL;
	rdim.asso_blk_use = 1;
	rdim.asso_blk[0].asso_type = 4;
	rdim.asso_blk[0].modifier = 4;
	rdim.asso_blk[0].key = cent_key;
	rdim.asso_blk[0].location[0] = 0.000000e+000;
	rdim.asso_blk[0].location[1] = 0.000000e+000;
	rdim.asso_blk[0].location[2] = 0.000000e+000;
	ablock = ( rdim.asso_blk_use+1 );
get:
	ud_lgeo(UU_TRUE,UD_draftable);
	s_status = ua_select_ent_subf(108,&(rdim),ablock,&(plocrec))
		;
	switch( s_status )
		{
		case UA_REJECT:
			{
			uu_dexit;
			return;
			}
		case UA_ALT_ACTION:
			{
			if( ( first==UU_FALSE ) )
				{
				redo = UU_TRUE;
				rdim.key = prev_key;
				s_status = uc_retrieve_data(&(rdim),sizeof(struct 
							    UA_generic_draft	));
				if( ( s_status==0 ) )
					{
					rdim.arc_blk_use = 0;
					rdim.line_blk_use = 0;
					rdim.arrow_blk_use = 0;
					rdim.asso_blk_use = 2;
					rdim.txt_blk_use = ( prev_txt_blk-2 );
					j = 1;
					for(;;)
						{
						if( j > 10 ) 	break;
						rdim.txt_blk[j-1].tangle = UA_text_ang;
us_l170:
						j++ ;
						}
us_l171: 
					;
					curr_key = rdim.asso_blk[1].key;
					}
				else
					{
					redo = UU_FALSE;
					first = UU_TRUE;
					goto main_loop;
					}
				}
			else
				{
				uu_dexit;
				return;
				}
			}
			break;
		case UA_OPCOMPLETE:
			{
			curr_key = rdim.asso_blk[ablock-1].key;
			ok = uc_draft_type(curr_key,&(relation));
			if( ( relation!=1 ) )
				{
				goto get;
				}
			rdim.asso_blk_use = ablock;
			curr_key = rdim.asso_blk[ablock-1].key;
			}
			break;
		}
	status = ua_dia_org(&(rdim),redo);
	if( ( !status ) )
		{
		uu_dexit;
		return;
		}
	ua_dia_shaft_cre(&(rdim));
	if( ( redo==UU_TRUE ) )
		{
		s_status = ua_update_entity(prev_key,&(rdim));
		if( ( s_status!=0 ) )
			{
			ua_create_entity(&(rdim),&(curr_key));
			}
		}
	else
		{
		ua_create_entity(&(rdim),&(curr_key));
		}
	uc_display(&(rdim));
	prev_key = rdim.key;
	prev_txt_blk = rdim.txt_blk_use;
	first = UU_FALSE;
	goto main_loop;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_shaft_cre(rdim)
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
ua_dia_shaft_cre(rdim)
struct UA_generic_draft	(*rdim);
	{
	int		relation;
	int		nint;
	int		cent_key;
	int		dir;
	int		ablock;
	int		subtype;
	int		num;
	int		curr_key;
	int		prev_key;
	int		ok;
	int		s_status;
	int		prev_txt_blk;
	int		i;
	int		j;
	int		d_stat;
	int		k;
	int		l;
	int		m;
	int		n;
	UU_REAL	c_pt[3];
	UU_REAL	dia_vec[3];
	UU_REAL	cent_ept[3];
	UU_REAL	cent_vec[3];
	UU_REAL	dia_ept[3];
	UU_REAL	dim_vec[3];
	UU_REAL	ept[3];
	UU_REAL	v1[3];
	UU_REAL	cent_spt[3];
	UU_REAL	v2[3];
	UU_REAL	dim_ept[3];
	UU_REAL	dia_spt[3];
	UU_REAL	spt[3];
	UU_REAL	origin[3];
	UU_REAL	corner[4][3];
	UU_REAL	dim_spt[3];
	UU_REAL	dis1;
	UU_REAL	int_pts[2][3];
	UU_REAL	ent_origin[3];
	UU_REAL	ent_pt[3];
	UU_REAL	int_pt[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	center[3];
	UU_LOGICAL	redo;
	UU_LOGICAL	status;
	UU_LOGICAL	first;

	uu_denter(UU_STRC,(us,"ua_dia_shaft_cre(rdim=%s)", "..."));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
	curr_key = (*rdim).asso_blk[1].key;
	uc_draft_line(curr_key,spt,ept);
	um_nptpln(spt,origin,zaxis,dia_spt);
	um_nptpln(ept,origin,zaxis,dia_ept);
		{
		UU_REAL	us_t172[3];
		um_vcmnvc(dia_ept,dia_spt,us_t172);
		um_unitvc(us_t172,dia_vec);
		}
	cent_key = (*rdim).asso_blk[0].key;
	uc_draft_line(cent_key,spt,ept);
	um_nptpln(spt,origin,zaxis,cent_spt);
	um_nptpln(ept,origin,zaxis,cent_ept);
		{
		UU_REAL	us_t173[3];
		um_vcmnvc(cent_ept,cent_spt,us_t173);
		um_unitvc(us_t173,cent_vec);
		}
	um_nptpln((*rdim).dim_origin,origin,zaxis,ent_origin);
	um_vcmnvc(ent_origin,cent_spt,v1);
	dis1 = um_dot(v1,cent_vec);
	um_vctmsc(cent_vec,dis1,v2);
	um_vcplvc(cent_spt,v2,c_pt);
		{
		UU_REAL	us_t174[3];
		um_vcmnvc(c_pt,ent_origin,us_t174);
		um_unitvc(us_t174,v1);
		}
	um_ilnln(ent_origin,v1,dia_spt,dia_vec,&(nint),int_pt);
	dis1 = um_dcccc(c_pt,int_pt);
	dis1 = dis1;
	ua_dia_txt(&((*rdim)),dis1,corner,center);
		{
		UU_REAL	us_t175[3];
		um_vcmnvc(ent_origin,int_pt,us_t175);
		um_unitvc(us_t175,v1);
		}
	ua_box_int(corner,int_pt,v1,int_pts);
	if( ( (*rdim).stack_grid==1 ) )
		{
			{
			UU_REAL	us_t176[3];
			um_cross(zaxis,v1,us_t176);
			um_unitvc(us_t176,v2);
			}
		um_vctovc(int_pts[0],c_pt);
		ua_box_move(v2,dia_spt,dia_vec,(*rdim).grid_dist,&((*rdim)),
		corner,c_pt);
		ua_box_int(corner,int_pt,v1,int_pts);
		}
	n = ( (*rdim).line_blk_use+1 );
	(*rdim).line_blk_use = n;
	(*rdim).line_blk[n-1].num_pts = 2;
	(*rdim).line_blk[n-1].subtype = dim_line;
	(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
	(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
	(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
	um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[0]);
	um_vctovc(int_pt,(*rdim).line_blk[n-1].line_seg[1]);
	m = ( (*rdim).arrow_blk_use+1 );
	(*rdim).arrow_blk_use = m;
	um_vctovc(int_pt,(*rdim).arrow_blk[m-1].location);
		{
		UU_REAL	us_t177[3];
		um_vcmnvc((*rdim).line_blk[n-1].line_seg[1],(*rdim).
		    line_blk[n-1].line_seg[0],us_t177);
		(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,
		us_t177);
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_sym_adjust(i, rdim, dl_pt, corner)
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
ua_dia_sym_adjust(i, rdim, dl_pt, corner)
int		i;
struct UA_generic_draft	(*rdim);
UU_REAL	dl_pt[3];
UU_REAL	corner[4][3];
	{
	int		j;
	UU_REAL	del_off[3];
	UU_REAL	c_pt[3];
	UU_REAL	lab_box[4][3];
	UU_REAL	mdx[3];
	UU_REAL	mdy[3];
	UU_REAL	sdx[3];
	UU_REAL	sdy[3];
	UU_REAL	new_pt[3];

	uu_denter(UU_STRC,(us,"ua_dia_sym_adjust(i=%d, rdim=%s, dl_pt=%s,\
		corner=%s)", i, "...", "...", "..."));

	ua_text_box(i,&((*rdim)),lab_box);
	um_vcmnvc(lab_box[1],lab_box[0],sdx);
	um_vcmnvc(lab_box[3],lab_box[0],sdy);
		{
		UU_REAL	us_t178[3];
		UU_REAL	us_t179[3];
		UU_REAL	us_t180[3];
		um_vctmsc(sdx,(UU_REAL) 1.0 / 2.000000e+000,us_t179);
		um_vcplvc(lab_box[0],us_t179,us_t178);
		um_vctmsc(sdy,(UU_REAL) 1.0 / 2.000000e+000,us_t180);
		um_vcplvc(us_t178,us_t180,c_pt);
		}
	um_vcmnvc(corner[1],corner[0],mdx);
	um_vcmnvc(corner[3],corner[0],mdy);
	switch( (*rdim).dia_place )
		{
		case UA_ABOVE:
			{
				{
				UU_REAL	us_t181[3];
				UU_REAL	us_t182[3];
				UU_REAL	us_t183[3];
				um_vctmsc(mdy,(UU_REAL) 5.000000e-001,us_t182);
				um_vcplvc(dl_pt,us_t182,us_t181);
				um_vctmsc(sdy,(UU_REAL) 6.000000e-001,us_t183);
				um_vcplvc(us_t181,us_t183,new_pt);
				}
			}
			break;
		case UA_BELOW:
			{
				{
				UU_REAL	us_t184[3];
				UU_REAL	us_t185[3];
				UU_REAL	us_t186[3];
				um_vctmsc(mdy,(UU_REAL) 5.000000e-001,us_t185);
				um_vcmnvc(dl_pt,us_t185,us_t184);
				um_vctmsc(sdy,(UU_REAL) 6.000000e-001,us_t186);
				um_vcmnvc(us_t184,us_t186,new_pt);
				}
			}
			break;
		case UA_BEFORE:
			{
				{
				UU_REAL	us_t187[3];
				UU_REAL	us_t188[3];
				um_vctmsc(mdx,(UU_REAL) 5.000000e-001,us_t188);
				um_vcmnvc(dl_pt,us_t188,us_t187);
				um_vcmnvc(us_t187,sdx,new_pt);
				}
			}
			break;
		case UA_AFTER:
			{
				{
				UU_REAL	us_t189[3];
				UU_REAL	us_t190[3];
				UU_REAL	us_t191[3];
				um_vctmsc(mdx,(UU_REAL) 5.000000e-001,us_t190);
				um_vcplvc(dl_pt,us_t190,us_t189);
				um_vctmsc(sdx,(UU_REAL) 6.000000e-001,us_t191);
				um_vcplvc(us_t189,us_t191,new_pt);
				}
			}
			break;
		}
	um_vcmnvc(new_pt,c_pt,del_off);
	um_vcplvc((*rdim).txt_blk[i-1].origin,del_off,(*rdim).
	    txt_blk[i-1].origin);
	j = 1;
	for(;;)
		{
		if( j > 4 ) 	break;
		um_vcplvc(lab_box[j-1],del_off,lab_box[j-1]);
us_l192:
		j++ ;
		}
us_l193: 
	;
	switch( (*rdim).dia_place )
		{
		case UA_ABOVE:
			{
			corner[2][1] = lab_box[2][1];
			corner[3][1] = lab_box[3][1];
			}
			break;
		case UA_BELOW:
			{
			corner[0][1] = lab_box[0][1];
			corner[1][1] = lab_box[1][1];
			}
			break;
		case UA_BEFORE:
			{
			corner[0][0] = lab_box[0][0];
			corner[3][0] = lab_box[3][0];
			}
			break;
		case UA_AFTER:
			{
			corner[1][0] = lab_box[1][0];
			corner[2][0] = lab_box[2][0];
			}
			break;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_cyln()
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
ua_dia_cyln()
	{
	struct UA_PLOCREC	plocrec;
	struct UA_generic_draft	rdim;
	int		relation;
	int		nint;
	int		cent_key;
	int		dir;
	int		ablock;
	int		num;
	int		curr_key;
	int		prev_key;
	int		subtype;
	int		ok;
	int		s_status;
	int		prev_txt_blk;
	int		i;
	int		j;
	int		d_stat;
	int		k;
	int		l;
	int		m;
	int		n;
	UU_LOGICAL	status;
	UU_LOGICAL	first;
	UU_LOGICAL	redo;

	uu_denter(UU_STRC,(us,"ua_dia_cyln()"));

	subtype = 8;
	first = UU_TRUE;
main_loop:
	ua_init_entity(UA_DIA_IN_DIM,subtype,&(rdim));
	redo = UU_FALSE;
	rdim.txt_just = UA_CENTER;
	rdim.entity_site = UA_TOP_CENTER;
	rdim.txt_place = UA_MANUAL;
	ablock = 1;
get:
	ud_lgeo(UU_TRUE,UD_draftable);
	s_status = ua_select_ent_subf(28,&(rdim),ablock,&(plocrec));
	switch( s_status )
		{
		case UA_REJECT:
			{
			uu_dexit;
			return;
			}
		case UA_ALT_ACTION:
			{
			if( ( first==UU_FALSE ) )
				{
				redo = UU_TRUE;
				rdim.key = prev_key;
				s_status = uc_retrieve_data(&(rdim),sizeof(struct 
				    UA_generic_draft	));
				if( ( s_status==0 ) )
					{
					rdim.arc_blk_use = 0;
					rdim.line_blk_use = 0;
					rdim.arrow_blk_use = 0;
					rdim.asso_blk_use = 2;
					rdim.txt_blk_use = ( prev_txt_blk-2 );
					j = 1;
					for(;;)
						{
						if( j > UA_NUM_TXTBLKS ) 	break;
						rdim.txt_blk[j-1].tangle = UA_text_ang;
us_l194:
						j++ ;
						}
us_l195: 
					;
					rdim.stack_grid = UA_GRID_OFF;
					goto next_input;
					}
				else
					{
					redo = UU_FALSE;
					first = UU_TRUE;
					goto main_loop;
					}
				}
			else
				{
				uu_dexit;
				return;
				}
			}
		case 1:
			{
			curr_key = rdim.asso_blk[ablock-1].key;
			ok = uc_draft_type(curr_key,&(relation));
			if( ( relation!=UA_DRAFT_LINE ) )
				{
				goto get;
				}
			rdim.asso_blk_use = ablock;
			}
			break;
		}
get2:
	ablock = 2;
	ud_lgeo(UU_TRUE,UD_draftable);
	s_status = ua_select_ent_subf(29,&(rdim),ablock,&(plocrec));
	if( ( s_status!=UA_OPCOMPLETE ) )
		{
		uu_dexit;
		return;
		}
	else
		{
		curr_key = rdim.asso_blk[ablock-1].key;
		ok = uc_draft_type(curr_key,&(relation));
		if( ( relation!=UA_DRAFT_LINE ) )
			{
			goto get2;
			}
		rdim.asso_blk_use = ablock;
		}
next_input:
	status = ua_dia_org(&(rdim),redo);
	if( ( !status ) )
		{
		uu_dexit;
		return;
		}
	ua_dia_cyln_cre(&(rdim));
	if( ( redo==UU_TRUE ) )
		{
		s_status = ua_update_entity(prev_key,&(rdim));
		if( ( s_status!=0 ) )
			{
			ua_create_entity(&(rdim),&(curr_key));
			}
		}
	else
		{
		ua_create_entity(&(rdim),&(curr_key));
		}
	uc_display(&(rdim));
	prev_key = rdim.key;
	prev_txt_blk = rdim.txt_blk_use;
	first = UU_FALSE;
	goto main_loop;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_oneside(rdim, corner, center, normal, 
**									radius, pt1, cir_pt, end_pt)
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
ua_dia_oneside(rdim, corner, center, normal, 
radius, pt1, cir_pt, end_pt)
struct UA_generic_draft	(*rdim);
UU_REAL	corner[4][3];
UU_REAL	center[3];
UU_REAL	normal[3];
UU_REAL	radius;
UU_REAL	pt1[3];
UU_REAL	cir_pt[3];
UU_REAL	end_pt[3];
	{
	int		num;
	int		i;
	int		j;
	int		k;
	int		l;
	int		m;
	int		n;
	UU_REAL	sign;
	UU_REAL	cir_int_pt[2][3];
	UU_REAL	tst_pt[2][3];
	UU_REAL	temp[3];
	UU_REAL	pt2[3];
	UU_REAL	corn_mod[4][3];
	UU_REAL	off_s_pt[2][3];
	UU_REAL	origin[3];
	UU_REAL	cent_pt[3];
	UU_REAL	txt_ang;
	UU_REAL	pt[4][3];
	UU_REAL	vec1[3];
	UU_REAL	vec2[3];
	UU_REAL	dis1;
	UU_REAL	dis2;
	UU_REAL	int_pts[2][3];
	UU_REAL	ref_pt[3];
	UU_REAL	int_pt[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	vnorm[3];
	UU_REAL	dl_pt[3];

	uu_denter(UU_STRC,(us,"ua_dia_oneside(rdim=%s, corner=%s,\
		center=<%g,%g,%g>, normal=<%g,%g,%g>, radius=%g, pt1=%s,\
		cir_pt=%s, end_pt=%s)", "...", "...", center[0],center[1],center[2],
		normal[0],normal[1],normal[2], radius, "...", "...", "..."));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
		{
		UU_REAL	us_t196[3];
		UU_REAL	us_t197[3];
		UU_REAL	us_t198[3];
		UU_REAL	us_t199[3];
		UU_REAL	us_t200[3];
		um_vcmnvc(corner[3],corner[0],us_t198);
		um_vctmsc(us_t198,(UU_REAL) 1.0 / 2.000000e+000,us_t197);
		um_vcplvc(corner[0],us_t197,us_t196);
		um_vcmnvc(corner[1],corner[0],us_t200);
		um_vctmsc(us_t200,(UU_REAL) 1.0 / 2.000000e+000,us_t199);
		um_vcplvc(us_t196,us_t199,cent_pt);
		}
	if( ( (*rdim).txt_orent==UA_TXT_HORIZ ) )
		{
		um_vctovc(cent_pt,ref_pt);
		}
	else
		{
			{
			UU_REAL	us_t201[3];
			UU_REAL	us_t202[3];
			um_vcmnvc(corner[3],corner[0],us_t202);
			um_vctmsc(us_t202,(UU_REAL) 1.0 / 2.000000e+000,us_t201);
			um_vcplvc(corner[0],us_t201,ref_pt);
			}
		}
	um_ilnpln(ref_pt,zaxis,center,normal,&(num),int_pt);
		{
		UU_REAL	us_t203[3];
		um_vcmnvc(center,int_pt,us_t203);
		um_unitvc(us_t203,vec1);
		}
	um_ilncir(center,vec1,center,normal,radius,&(num),pt);
	j = us_clospnt(2,pt,int_pt);
	i = 1;
	for(;;)
		{
		if( i > 2 ) 	break;
		um_vctovc(pt[i-1],temp);
		um_ilnpln(temp,zaxis,origin,zaxis,&(num),int_pt);
		um_vctovc(int_pt,pt[i-1]);
us_l204:
		i++ ;
		}
us_l205: 
	;
	um_vctovc(pt[j-1],cir_pt);
	um_vctovc(pt[j-1],cir_int_pt[0]);
	if( ( j==1 ) )
		{
		um_vctovc(pt[1],cir_int_pt[1]);
		}
	else
		{
		um_vctovc(pt[0],cir_int_pt[1]);
		}
	if( ( (*rdim).txt_orent>UA_TXT_HORIZ ) )
		{
		(*rdim).entity_site = UA_MIDDLE_LEFT;
		um_vctovc(ref_pt,(*rdim).dim_origin);
			{
			UU_REAL	us_t206[3];
			um_cross(zaxis,vec1,us_t206);
			um_unitvc(us_t206,vnorm);
			}
		if( ( um_dot(vnorm,yaxis)<0.000000e+000 ) )
			{
			um_vctmsc(vnorm,-1.000000e+000,vnorm);
			}
		txt_ang = um_angle2p(yaxis,vnorm,zaxis);
			{
			int		us_t209;
			us_t209 = (*rdim).txt_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t209 ) 	break;
				(*rdim).txt_blk[i-1].tangle = txt_ang;
us_l207:
				i++ ;
				}
us_l208: 
			;
			}
		ua_box_site(&((*rdim)),corn_mod,dl_pt);
		/*
		i = (*rdim).txt_blk_use;
		ua_dia_sym_adjust(i,&((*rdim)),dl_pt,corn_mod);
		*/
		ua_box_frame(&((*rdim)),corn_mod);
		dis1 = um_dcccc(corn_mod[0],corn_mod[3]);
		dis1 = ( dis1-( 2.000000e+000*(*rdim).gap_to_geo ) );
			{
			int		us_t212;
			us_t212 = (*rdim).txt_blk_use;
			i = 1;
			for(;;)
				{
				if( i > us_t212 ) 	break;
					{
					UU_REAL	us_t213[3];
					um_vctmsc(vnorm,dis1,us_t213);
					um_vcplvc((*rdim).txt_blk[i-1].origin,us_t213,(*rdim).
					    txt_blk[i-1].origin);
					}
us_l210:
				i++ ;
				}
us_l211: 
			;
			}
			{
			UU_REAL	us_t214[3];
			um_vcmnvc(cir_int_pt[0],cir_int_pt[1],us_t214);
			um_unitvc(us_t214,vec1);
			}
		um_cross(zaxis,vec1,vec2);
		um_ilnln(cir_int_pt[0],vec1,end_pt,vec2,&(num),int_pt);
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].num_pts = 2;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		um_vctovc(int_pt,(*rdim).line_blk[n-1].line_seg[0]);
		um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[1]);
		}
	else
		{
		um_ilnpln(center,zaxis,origin,zaxis,&(num),cent_pt);
			{
			UU_REAL	us_t215[3];
			um_vcmnvc(cent_pt,cir_pt,us_t215);
			um_unitvc(us_t215,vec1);
			}
		ua_box_int(corner,cent_pt,vec1,int_pts);
			{
			UU_REAL	us_t216[3];
			um_vcmnvc(cir_int_pt[1],int_pts[0],us_t216);
			um_unitvc(us_t216,vec1);
			}
		um_cross(zaxis,vec1,vec2);
		um_ilnln(cent_pt,vec1,end_pt,vec2,&(num),int_pt);
		if( ( num==0 ) )
			{
				{
				UU_REAL	us_t217[3];
				um_vcmnvc(cir_pt,int_pts[1],us_t217);
				um_vcmnvc(int_pts[0],us_t217,int_pt);
				}
			}
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].num_pts = 4;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
			{
			UU_REAL	us_t218[3];
			um_vcmnvc(int_pts[0],cir_pt,us_t218);
			um_unitvc(us_t218,vec1);
			}
			{
			UU_REAL	us_t219[3];
			um_vcmnvc(cent_pt,cir_pt,us_t219);
			um_unitvc(us_t219,vec2);
			}
		if( ( um_dot(vec1,vec2)<-5.000000e-001 ) )
			{
			um_vctovc(int_pt,(*rdim).line_blk[n-1].line_seg[0]);
			dis1 = um_dcccc(cir_int_pt[1],int_pts[0]);
			dis2 = um_dcccc(cir_int_pt[1],int_pts[1]);
			if( ( dis1<dis2 ) )
				{
				um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[1]);
				}
			else
				{
				um_vctovc(int_pts[1],(*rdim).line_blk[n-1].line_seg[1]);
				}
			(*rdim).line_blk[n-1].num_pts = 2;
			}
		else
			{
			um_vctovc(int_pt,(*rdim).line_blk[n-1].line_seg[0]);
			um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[1]);
			um_vctovc(int_pts[1],(*rdim).line_blk[n-1].line_seg[2]);
			um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[3]);
			}
		}
	m = ( (*rdim).arrow_blk_use+1 );
	(*rdim).arrow_blk_use = m;
	um_vctovc(cir_pt,(*rdim).arrow_blk[m-1].location);
		{
		UU_REAL	us_t220[3];
		um_vcmnvc((*rdim).line_blk[n-1].line_seg[1],(*rdim).
		    line_blk[n-1].line_seg[0],us_t220);
		(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,
		us_t220);
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_cyln_cre(rdim)
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
ua_dia_cyln_cre(rdim)
struct UA_generic_draft	(*rdim);
	{
	int		relation;
	int		nint;
	int		cent_key;
	int		dir;
	int		ablock;
	int		orig_loc_type;
	int		num;
	int		curr_key;
	int		prev_key;
	int		subtype;
	int		ok;
	int		s_status;
	int		loc1;
	int		loc2;
	int		prev_txt_blk;
	int		i;
	int		j;
	int		d_stat;
	int		k;
	int		l;
	int		m;
	int		n;
	UU_REAL	ept[3];
	UU_REAL	v1[3];
	UU_REAL	v2[3];
	UU_REAL	base_pt[3];
	UU_REAL	l1_vec[3];
	UU_REAL	dim_segs[4][3];
	UU_REAL	sign;
	UU_REAL	move_vect[3];
	UU_REAL	move_dist;
	UU_REAL	l1_int_pt[3];
	UU_REAL	l2_int_pt[3];
	UU_REAL	l2_vec[3];
	UU_REAL	off_set;
	UU_REAL	spt[3];
	UU_REAL	l1_ept[3];
	UU_REAL	l2_ept[3];
	UU_REAL	origin[3];
	UU_REAL	corner[4][3];
	UU_REAL	l1_spt[3];
	UU_REAL	l2_spt[3];
	UU_REAL	vec1[3];
	UU_REAL	vec2[3];
	UU_REAL	dis1;
	UU_REAL	dis2;
	UU_REAL	int_pts[2][3];
	UU_REAL	mid_pt[3];
	UU_REAL	ent_origin[3];
	UU_REAL	npt1[3];
	UU_REAL	npt2[3];
	UU_REAL	xaxis[3];
	UU_REAL	base_vec[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	center[3];
	UU_LOGICAL	status;
	UU_LOGICAL	first;
	UU_LOGICAL	redo;

	uu_denter(UU_STRC,(us,"ua_dia_cyln_cre(rdim=%s)", "..."));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
	um_nptpln((*rdim).dim_origin,origin,zaxis,ent_origin);
	uc_draft_line((*rdim).asso_blk[0].key,spt,ept);
	um_nptpln(spt,origin,zaxis,l1_spt);
	um_nptpln(ept,origin,zaxis,l1_ept);
		{
		UU_REAL	us_t221[3];
		um_vcmnvc(l1_ept,l1_spt,us_t221);
		um_unitvc(us_t221,l1_vec);
		}
	uc_draft_line((*rdim).asso_blk[1].key,spt,ept);
	um_nptpln(spt,origin,zaxis,l2_spt);
	um_nptpln(ept,origin,zaxis,l2_ept);
		{
		UU_REAL	us_t222[3];
		um_vcmnvc(l2_ept,l2_spt,us_t222);
		um_unitvc(us_t222,l2_vec);
		}
	um_vcmnvc(ent_origin,l1_spt,v1);
	dis1 = um_dot(v1,l1_vec);
	um_vctmsc(l1_vec,dis1,v2);
	um_vcplvc(l1_spt,v2,l1_int_pt);
		{
		UU_REAL	us_t223[3];
		um_vcmnvc(l1_int_pt,ent_origin,us_t223);
		um_unitvc(us_t223,v1);
		}
	um_ilnln(ent_origin,v1,l2_spt,l2_vec,&(nint),l2_int_pt);
	dis1 = ( um_dcccc(l1_int_pt,l2_int_pt)/2.000000e+000 );
	ua_dia_txt(&((*rdim)),dis1,corner,center);
		{
		UU_REAL	us_t224[3];
		um_vcmnvc(l2_int_pt,ent_origin,us_t224);
		um_unitvc(us_t224,v1);
		}
	ua_box_int(corner,l2_int_pt,v1,int_pts);
	um_vctovc(int_pts[0],dim_segs[0]);
	dis1 = um_dcccc(dim_segs[0],l1_int_pt);
	dis2 = um_dcccc(dim_segs[0],l2_int_pt);
	if( ( dis1<dis2 ) )
		{
		um_vctovc(l1_int_pt,dim_segs[1]);
		um_vctovc(l2_int_pt,dim_segs[2]);
		um_vctovc(int_pts[1],dim_segs[3]);
		um_vctovc(l1_spt,base_pt);
		um_vctovc(l1_vec,base_vec);
		off_set = dis1;
		}
	else
		{
		um_vctovc(l2_int_pt,dim_segs[1]);
		um_vctovc(l1_int_pt,dim_segs[2]);
		um_vctovc(int_pts[1],dim_segs[3]);
		um_vctovc(l2_spt,base_pt);
		um_vctovc(l2_vec,base_vec);
		off_set = dis2;
		}
	if( ( (*rdim).txt_place!=UA_MANUAL ) )
		{
		if( ( ( orig_loc_type!=UA_IN_BTW )&&( orig_loc_type!=UA_OUT_BTW ) ) )
			{
			if( ( (*rdim).stack_grid==UA_GRID_ON ) )
				{
					{
					UU_REAL	us_t225[3];
					um_cross(zaxis,v1,us_t225);
					um_unitvc(us_t225,v2);
					}
				um_vctovc(dim_segs[0],ent_origin);
				ua_box_move(v2,base_pt,base_vec,(*rdim).grid_dist,&((*rdim))
				    ,corner,ent_origin);
				ua_box_int(corner,l2_int_pt,v1,int_pts);
				um_vctovc(int_pts[0],dim_segs[0]);
				um_vctovc(int_pts[1],dim_segs[3]);
				off_set = um_dcccc(dim_segs[0],dim_segs[1]);
				}
			}
		else
			{
				{
				UU_REAL	us_t226[3];
				UU_REAL	us_t227[3];
				um_vcmnvc(l2_int_pt,l1_int_pt,us_t227);
				um_vctmsc(us_t227,(UU_REAL) 1.0 / 2.000000e+000,us_t226);
				um_vcplvc(l1_int_pt,us_t226,mid_pt);
				}
			move_dist = um_dcccc(mid_pt,ent_origin);
				{
				UU_REAL	us_t228[3];
				UU_REAL	us_t229[3];
				um_vcmnvc(mid_pt,ent_origin,us_t229);
				um_unitvc(us_t229,us_t228);
				um_vctmsc(us_t228,move_dist,move_vect);
				}
			um_vcplvc((*rdim).dim_origin,move_vect,(*rdim).dim_origin);
			i = 1;
			for(;;)
				{
				if( i > 4 ) 	break;
				um_vcplvc(corner[i-1],move_vect,corner[i-1]);
us_l230:
				i++ ;
				}
us_l231: 
			;
			um_nptpln((*rdim).dim_origin,origin,zaxis,ent_origin);
			ua_box_int(corner,l2_int_pt,v1,int_pts);
			um_vctovc(int_pts[0],dim_segs[0]);
			um_vctovc(int_pts[1],dim_segs[3]);
			off_set = um_dcccc(dim_segs[0],dim_segs[1]);
			}
		}
		{
		UU_REAL	us_t232[3];
		um_vcmnvc(l1_int_pt,ent_origin,us_t232);
		um_unitvc(us_t232,vec1);
		}
		{
		UU_REAL	us_t233[3];
		um_vcmnvc(l2_int_pt,ent_origin,us_t233);
		um_unitvc(us_t233,vec2);
		}
	sign = um_dot(vec1,vec2);
	if( ( sign<0.000000e+000 ) )
		{
		if( um_ptinseg(l1_spt,l1_int_pt,l1_ept) )
			{
			orig_loc_type = UA_IN_BTW;
			}
		else
			{
			orig_loc_type = UA_IN_NOT_BTW;
			}
		}
	else
		{
		if( um_ptinseg(l1_spt,l1_int_pt,l1_ept) )
			{
			orig_loc_type = UA_OUT_BTW;
			}
		else
			{
			orig_loc_type = UA_OUT_NOT_BTW;
			}
		}
	dis1 = um_dcccc(l1_spt,l1_int_pt);
	dis2 = um_dcccc(l1_ept,l1_int_pt);
	if( ( dis1<dis2 ) )
		{
		um_vctovc(l1_spt,npt1);
		}
	else
		{
		um_vctovc(l1_ept,npt1);
		}
	dis1 = um_dcccc(l2_spt,l2_int_pt);
	dis2 = um_dcccc(l2_ept,l2_int_pt);
	if( ( dis1<dis2 ) )
		{
		um_vctovc(l2_spt,npt2);
		}
	else
		{
		um_vctovc(l2_ept,npt2);
		}
		{
		UU_REAL	us_t234[3];
		um_vcmnvc(l1_int_pt,npt1,us_t234);
		um_unitvc(us_t234,vec1);
		}
		{
		UU_REAL	us_t235[3];
		um_vcmnvc(l2_int_pt,npt2,us_t235);
		um_unitvc(us_t235,vec2);
		}
	if( ( (*rdim).arrow_place==UA_ARROWS_IN ) )
		{
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		switch( orig_loc_type )
			{
			case UA_OUT_BTW:
				{
				(*rdim).line_blk[n-1].num_pts = 2;
				loc1 = 1;
				loc2 = 2;
				um_vctovc(dim_segs[0],(*rdim).line_blk[n-1].line_seg[0])
					;
				um_vctovc(dim_segs[2],(*rdim).line_blk[n-1].line_seg[1])
					;
				}
				break;
			case UA_IN_BTW:
				{
				(*rdim).line_blk[n-1].num_pts = 4;
				loc1 = 2;
				loc2 = 1;
				um_vctovc(dim_segs[0],(*rdim).line_blk[n-1].line_seg[0]) ;
				um_vctovc(dim_segs[1],(*rdim).line_blk[n-1].line_seg[1]) ;
				um_vctovc(dim_segs[3],(*rdim).line_blk[n-1].line_seg[2]) ;
				um_vctovc(dim_segs[2],(*rdim).line_blk[n-1].line_seg[3]) ;
				}
				break;
			case UA_IN_NOT_BTW:
				{
				(*rdim).line_blk[n-1].num_pts = 8;
				loc1 = 2;
				loc2 = 1;
					{
					UU_REAL	us_t236[3];
					um_vctmsc(vec1,(*rdim).gap_to_geo,us_t236);
					um_vcplvc(npt1,us_t236,(*rdim).line_blk[n-1].line_seg[0]);
					}
					{
					UU_REAL	us_t237[3];
					um_vctmsc(vec1,(*rdim).ext_past_line,us_t237);
					um_vcplvc(l1_int_pt,us_t237,(*rdim).line_blk[n-1].line_seg[2-1]);
					}
					{
					UU_REAL	us_t238[3];
					um_vctmsc(vec2,(*rdim).gap_to_geo,us_t238);
					um_vcplvc(npt2,us_t238,(*rdim).line_blk[n-1].line_seg[2]);
					}
					{
					UU_REAL	us_t239[3];
					um_vctmsc(vec2,(*rdim).ext_past_line,us_t239);
					um_vcplvc(l2_int_pt,us_t239,(*rdim).line_blk[n-1].line_seg[3]);
					}
				um_vctovc(dim_segs[0],(*rdim).line_blk[n-1].line_seg[4]) ;
				um_vctovc(dim_segs[1],(*rdim).line_blk[n-1].line_seg[5]) ;
				um_vctovc(dim_segs[2],(*rdim).line_blk[n-1].line_seg[6]) ;
				um_vctovc(dim_segs[3],(*rdim).line_blk[n-1].line_seg[7]) ;
				}
				break;
			case UA_OUT_NOT_BTW:
				{
				(*rdim).line_blk[n-1].num_pts = 6;
				loc1 = 1;
				loc2 = 2;
					{
					UU_REAL	us_t240[3];
					um_vctmsc(vec1,(*rdim).gap_to_geo,us_t240);
					um_vcplvc(npt1,us_t240,(*rdim).line_blk[n-1].line_seg[0]);
					}
					{
					UU_REAL	us_t241[3];
					um_vctmsc(vec1,(*rdim).ext_past_line,us_t241);
					um_vcplvc(l1_int_pt,us_t241,(*rdim).line_blk[n-1].line_seg[1]);
					}
					{
					UU_REAL	us_t242[3];
					um_vctmsc(vec2,(*rdim).gap_to_geo,us_t242);
					um_vcplvc(npt2,us_t242,(*rdim).line_blk[n-1].line_seg[2]);
					}
					{
					UU_REAL	us_t243[3];
					um_vctmsc(vec2,(*rdim).ext_past_line,us_t243);
					um_vcplvc(l2_int_pt,us_t243,(*rdim).line_blk[n-1].line_seg[3]);
					}
				um_vctovc(dim_segs[0],(*rdim).line_blk[n-1].line_seg[4]) ;
				um_vctovc(dim_segs[2],(*rdim).line_blk[n-1].line_seg[5]) ;
				}
				break;
			}
		m = ( (*rdim).arrow_blk_use+1 );
		(*rdim).arrow_blk_use = m;
		um_vctovc(dim_segs[1],(*rdim).arrow_blk[m-1].location);
			{
			UU_REAL	us_t244[3];
			um_vcmnvc(dim_segs[loc1-1],dim_segs[loc2-1],us_t244);
			(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis, us_t244);
			}
		m = ( (*rdim).arrow_blk_use+1 );
		(*rdim).arrow_blk_use = m;
		um_vctovc(dim_segs[2],(*rdim).arrow_blk[m-1].location);
			{
			UU_REAL	us_t245[3];
			um_vcmnvc(dim_segs[loc2-1],dim_segs[loc1-1],us_t245);
			(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,
			us_t245);
			}
		}
	else
		{
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		switch( orig_loc_type )
			{
			case UA_OUT_BTW:
				{
				(*rdim).line_blk[n-1].num_pts = 4;
				loc1 = 2;
				loc2 = 1;
				um_vctovc(dim_segs[0],(*rdim).line_blk[n-1].line_seg[0]) ;
				um_vctovc(dim_segs[1],(*rdim).line_blk[n-1].line_seg[1]) ;
				um_vctovc(dim_segs[2],(*rdim).line_blk[n-1].line_seg[2]) ;
					{
					UU_REAL	us_t246[3];
					um_vcmnvc(dim_segs[1],dim_segs[0],us_t246);
					um_unitvc(us_t246,v1);
					}
					{
					UU_REAL	us_t247[3];
					um_vctmsc(v1,off_set,us_t247);
					um_vcplvc(dim_segs[2],us_t247,(*rdim).line_blk[n-1].
					    line_seg[3]);
					}
				}
				break;
			case UA_IN_BTW:
				{
				(*rdim).line_blk[n-1].num_pts = 4;
				loc1 = 1;
				loc2 = 2;
					{
					UU_REAL	us_t248[3];
					um_vcmnvc(dim_segs[1],dim_segs[0],us_t248);
					um_unitvc(us_t248,v1);
					}
					{
					UU_REAL	us_t249[3];
					um_vctmsc(v1,( 3.000000e+000*(*rdim).arrow_size ),us_t249);
					um_vcplvc(dim_segs[1],us_t249,(*rdim).line_blk[n-1].
					    line_seg[0]);
					}
				um_vctovc(dim_segs[1],(*rdim).line_blk[n-1].line_seg[1])
					;
					{
					UU_REAL	us_t250[3];
					um_vctmsc(v1,( 3.000000e+000*(*rdim).arrow_size ),us_t250);
					um_vcmnvc(dim_segs[2],us_t250,(*rdim).line_blk[n-1].
					    line_seg[2]);
					}
				um_vctovc(dim_segs[2],(*rdim).line_blk[n-1].line_seg[3])
					;
				}
				break;
			case UA_IN_NOT_BTW:
				{
				(*rdim).line_blk[n-1].num_pts = 8;
				loc1 = 1;
				loc2 = 2;
					{
					UU_REAL	us_t251[3];
					um_vctmsc(vec1,(*rdim).gap_to_geo,us_t251);
					um_vcplvc(npt1,us_t251,(*rdim).line_blk[n-1].line_seg[0]);
					}
					{
					UU_REAL	us_t252[3];
					um_vctmsc(vec1,(*rdim).ext_past_line,us_t252);
					um_vcplvc(l1_int_pt,us_t252,(*rdim).line_blk[n-1].line_seg[2
					    -1]);
					}
					{
					UU_REAL	us_t253[3];
					um_vctmsc(vec2,(*rdim).gap_to_geo,us_t253);
					um_vcplvc(npt2,us_t253,(*rdim).line_blk[n-1].line_seg[2]);
					}
					{
					UU_REAL	us_t254[3];
					um_vctmsc(vec2,(*rdim).ext_past_line,us_t254);
					um_vcplvc(l2_int_pt,us_t254,(*rdim).line_blk[n-1].line_seg[4-1]);
					}
					{
					UU_REAL	us_t255[3];
					um_vcmnvc(dim_segs[1],dim_segs[0],us_t255);
					um_unitvc(us_t255,v1);
					}
					{
					UU_REAL	us_t256[3];
					um_vctmsc(v1,( 3.000000e+000*(*rdim).arrow_size ),us_t256);
					um_vcplvc(dim_segs[1],us_t256,(*rdim).line_blk[n-1].
					    line_seg[0]);
					}
				um_vctovc(dim_segs[1],(*rdim).line_blk[n-1].line_seg[1])
					;
					{
					UU_REAL	us_t257[3];
					um_vctmsc(v1,( 3.000000e+000*(*rdim).arrow_size ),us_t257);
					um_vcmnvc(dim_segs[2],us_t257,(*rdim).line_blk[n-1].
					    line_seg[2]);
					}
				}
				break;
			case UA_OUT_NOT_BTW:
				{
				(*rdim).line_blk[n-1].num_pts = 8;
				loc1 = 2;
				loc2 = 1;
					{
					UU_REAL	us_t258[3];
					um_vcmnvc(dim_segs[1],dim_segs[0],us_t258);
					um_unitvc(us_t258,v1);
					}
					{
					UU_REAL	us_t259[3];
					um_vctmsc(vec1,(*rdim).gap_to_geo,us_t259);
					um_vcplvc(npt1,us_t259,(*rdim).line_blk[n-1].line_seg[0]);
					}
					{
					UU_REAL	us_t260[3];
					um_vctmsc(vec1,(*rdim).ext_past_line,us_t260);
					um_vcplvc(l1_int_pt,us_t260,(*rdim).line_blk[n-1].line_seg[2
					    -1]);
					}
					{
					UU_REAL	us_t261[3];
					um_vctmsc(vec2,(*rdim).gap_to_geo,us_t261);
					um_vcplvc(npt2,us_t261,(*rdim).line_blk[n-1].line_seg[2]);
					}
					{
					UU_REAL	us_t262[3];
					um_vctmsc(vec2,(*rdim).ext_past_line,us_t262);
					um_vcplvc(l2_int_pt,us_t262,(*rdim).line_blk[n-1].line_seg[4
					    -1]);
					}
				um_vctovc(dim_segs[2],(*rdim).line_blk[n-1].line_seg[4]) ;
					{
					UU_REAL	us_t263[3];
					um_vctmsc(v1,off_set,us_t263);
					um_vcplvc(dim_segs[2],us_t263,(*rdim).line_blk[n-1].
					    line_seg[5]);
					}
				um_vctovc(dim_segs[0],(*rdim).line_blk[n-1].line_seg[6]) ;
				um_vctovc(dim_segs[1],(*rdim).line_blk[n-1].line_seg[7]) ;
				}
				break;
			}
		m = ( (*rdim).arrow_blk_use+1 );
		(*rdim).arrow_blk_use = m;
		um_vctovc(dim_segs[1],(*rdim).arrow_blk[m-1].location);
			{
			UU_REAL	us_t264[3];
			um_vcmnvc(dim_segs[loc1-1],dim_segs[loc2-1],us_t264);
			(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,
			us_t264);
			}
		m = ( (*rdim).arrow_blk_use+1 );
		(*rdim).arrow_blk_use = m;
		um_vctovc(dim_segs[2],(*rdim).arrow_blk[m-1].location);
			{
			UU_REAL	us_t265[3];
			um_vcmnvc(dim_segs[loc2-1],dim_segs[loc1-1],us_t265);
			(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,
			us_t265);
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_shaft_regen(rdim)
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
ua_dia_shaft_regen(rdim)
struct UA_generic_draft	(*rdim);
	{

	uu_denter(UU_STRC,(us,"ua_dia_shaft_regen(rdim=%s)", "..."));

	ua_dia_shaft_cre(&(*rdim));
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_outside(rdim, corner, center, normal, 
**											radius, pt1, cir_pt, dir)
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
ua_dia_outside(rdim, corner, center, normal, 
radius, pt1, cir_pt, dir)
struct UA_generic_draft	(*rdim);
UU_REAL	corner[4][3];
UU_REAL	center[3];
UU_REAL	normal[3];
UU_REAL	radius;
UU_REAL	pt1[3];
UU_REAL	cir_pt[3];
int		(*dir);
	{
	int		num;
	int		i;
	int		j;
	int		k;
	int		l;
	int		m;
	int		n;
	UU_REAL	off_s_pt[2][3];
	UU_REAL	origin[3];
	UU_REAL	pt[4][3];
	UU_REAL	vec1[3];
	UU_REAL	vec2[3];
	UU_REAL	dis1;
	UU_REAL	dis2;
	UU_REAL	tst_pt[2][3];
	UU_REAL	temp[3];
	UU_REAL	pt2[3];
	UU_REAL	edge1_pt[3];
	UU_REAL	edge2_pt[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];

	uu_denter(UU_STRC,(us,"ua_dia_outside(rdim=%s, corner=%s,\
		center=<%g,%g,%g>, normal=<%g,%g,%g>, radius=%g, pt1=%s,\
		cir_pt=%s, dir=%d)", "...", "...", center[0],center[1],center[2],
		normal[0],normal[1],normal[2], radius, "...", "...", *dir));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
		{
		UU_REAL	us_t266[3];
		UU_REAL	us_t267[3];
		um_vcmnvc(corner[3],corner[0],us_t267);
		um_vctmsc(us_t267,(UU_REAL) 1.0 / 2.000000e+000,us_t266);
		um_vcplvc(corner[0],us_t266,edge1_pt);
		}
	um_vcmnvc(corner[1],corner[0],vec2);
	um_vcplvc(edge1_pt,vec2,edge2_pt);
	um_unitvc(vec2,vec2);
	i = 1;
	for(;;)
		{
		if( i > 2 ) 	break;
		switch( i )
			{
			case 1:
				{
					{
					UU_REAL	us_t270[3];
					um_vctmsc(vec2,(*rdim).stub_length,us_t270);
					um_vcmnvc(edge1_pt,us_t270,off_s_pt[i-1]);
					}
				}
				break;
			case 2:
				{
					{
					UU_REAL	us_t271[3];
					um_vctmsc(vec2,(*rdim).stub_length,us_t271);
					um_vcplvc(edge2_pt,us_t271,off_s_pt[i-1]);
					}
				}
				break;
			}
		um_vctovc(off_s_pt[i-1],temp);
		um_ilnpln(temp,zaxis,center,normal,&(num),pt2);
		um_vctovc(pt2,tst_pt[i-1]);
			{
			UU_REAL	us_t272[3];
			um_vcmnvc(center,tst_pt[i-1],us_t272);
			um_unitvc(us_t272,vec1);
			}
		um_ilncir(center,vec1,center,normal,radius,&(num),pt);
		if( ( num>1 ) )
			{
			j = us_clospnt(2,pt,tst_pt[i-1]);
			um_vctovc(pt[j-1],tst_pt[i-1]);
			}
		else
			{
			um_vctovc(pt[0],tst_pt[i-1]);
			}
us_l268:
		i++ ;
		}
us_l269: 
	;
	i = 1;
	for(;;)
		{
		if( i > 2 ) 	break;
		um_vctovc(tst_pt[i-1],temp);
		um_ilnpln(temp,zaxis,origin,zaxis,&(num),pt2);
		um_vctovc(pt2,tst_pt[i-1]);
us_l273:
		i++ ;
		}
us_l274: 
	;
	dis1 = um_dcccc(off_s_pt[0],tst_pt[0]);
	dis2 = um_dcccc(off_s_pt[1],tst_pt[1]);
	if( ( dis1<dis2 ) )
		{
		um_vctovc(tst_pt[0],cir_pt);
		um_vctovc(edge1_pt,pt1);
		(*dir) = 3;
		}
	else
		{
		um_vctovc(tst_pt[1],cir_pt);
		um_vctovc(edge2_pt,pt1);
		(*dir) = 4;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_inside(rdim, corner, center, normal, radius
**										, pt1, cir_pt)
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
ua_dia_inside(rdim, corner, center, normal, radius
, pt1, cir_pt)
struct UA_generic_draft	(*rdim);
UU_REAL	corner[4][3];
UU_REAL	center[3];
UU_REAL	normal[3];
UU_REAL	radius;
UU_REAL	pt1[3];
UU_REAL	cir_pt[3];
	{
	int		num;
	int		i;
	int		j;
	int		k;
	int		l;
	int		m;
	int		n;
	UU_REAL	sign;
	UU_REAL	cir_int_pt[2][3];
	UU_REAL	tst_pt[2][3];
	UU_REAL	temp[3];
	UU_REAL	pt2[3];
	UU_REAL	corn_mod[4][3];
	UU_REAL	arrow_pts[2][3];
	UU_REAL	off_s_pt[2][3];
	UU_REAL	offset[3];
	UU_REAL	origin[3];
	UU_REAL	cent_pt[3];
	UU_REAL	dim_line_endpts[2][2][3];
	UU_REAL	txt_ang;
	UU_REAL	pt[4][3];
	UU_REAL	vec1[3];
	UU_REAL	vec2[3];
	UU_REAL	dis1;
	UU_REAL	int_pts[2][3];
	UU_REAL	dis2;
	UU_REAL	ref_pt[3];
	UU_REAL	edge1_pt[3];
	UU_REAL	edge2_pt[3];
	UU_REAL	int_pt[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	vnorm[3];
	UU_REAL	dl_pt[3];

	uu_denter(UU_STRC,(us,"ua_dia_inside(rdim=%s, corner=%s,\
		center=<%g,%g,%g>, normal=<%g,%g,%g>, radius=%g, pt1=%s, cir_pt=%s)",
		"...", "...", center[0],center[1],center[2], normal[0],normal[1],
		normal[2], radius, "...", "..."));

	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
		{
		UU_REAL	us_t275[3];
		UU_REAL	us_t276[3];
		UU_REAL	us_t277[3];
		UU_REAL	us_t278[3];
		UU_REAL	us_t279[3];
		um_vcmnvc(corner[3],corner[0],us_t277);
		um_vctmsc(us_t277,(UU_REAL) 1.0 / 2.000000e+000,us_t276);
		um_vcplvc(corner[0],us_t276,us_t275);
		um_vcmnvc(corner[1],corner[0],us_t279);
		um_vctmsc(us_t279,(UU_REAL) 1.0 / 2.000000e+000,us_t278);
		um_vcplvc(us_t275,us_t278,cent_pt);
		}
	if( ( (*rdim).txt_orent==UA_TXT_HORIZ ) )
		{
		um_vctovc(cent_pt,ref_pt);
		}
	else
		{
			{
			UU_REAL	us_t280[3];
			UU_REAL	us_t281[3];
			um_vcmnvc(corner[3],corner[0],us_t281);
			um_vctmsc(us_t281,(UU_REAL) 1.0 / 2.000000e+000,us_t280);
			um_vcplvc(corner[0],us_t280,ref_pt);
			}
		}
	um_ilnpln(ref_pt,zaxis,center,normal,&(num),pt1);
		{
		UU_REAL	us_t282[3];
		um_vcmnvc(center,pt1,us_t282);
		um_unitvc(us_t282,vec1);
		}
	um_ilncir(center,vec1,center,normal,radius,&(num),pt);
	if( ( num>1 ) )
		{
		j = us_clospnt(2,pt,pt1);
		um_vctovc(pt[j-1],cir_pt);
		}
	else
		{
		j = 1;
		um_vctovc(pt[0],cir_pt);
		}
		{
		UU_REAL	us_t283[3];
		um_vcmnvc(cir_pt,pt1,us_t283);
		um_unitvc(us_t283,vec2);
		}
	sign = um_dot(vec1,vec2);
	if( ( sign<0.000000e+000 ) )
		{
		i = 1;
		for(;;)
			{
			if( i > 2 ) 	break;
			um_vctovc(pt[i-1],temp);
			um_ilnpln(temp,zaxis,origin,zaxis,&(num),int_pt);
			um_vctovc(int_pt,pt[i-1]);
us_l284:
			i++ ;
			}
us_l285: 
		;
		um_vctovc(pt[j-1],cir_pt);
		um_vctovc(pt[j-1],cir_int_pt[0]);
		if( ( j==1 ) )
			{
			um_vctovc(pt[1],cir_int_pt[1]);
			}
		else
			{
			um_vctovc(pt[0],cir_int_pt[1]);
			}
		if( ( (*rdim).txt_orent>UA_TXT_HORIZ ) )
			{
			(*rdim).entity_site = UA_MIDDLE_LEFT;
			um_vctovc(ref_pt,(*rdim).dim_origin);
				{
				UU_REAL	us_t286[3];
				um_cross(zaxis,vec1,us_t286);
				um_unitvc(us_t286,vnorm);
				}
			if( ( um_dot(vnorm,yaxis)<0.000000e+000 ) )
				{
				um_vctmsc(vnorm,-1.000000e+000,vnorm);
				}
			txt_ang = um_angle2p(yaxis,vnorm,zaxis);
				{
				int		us_t289;
				us_t289 = (*rdim).txt_blk_use;
				i = 1;
				for(;;)
					{
					if( i > us_t289 ) 	break;
					(*rdim).txt_blk[i-1].tangle = txt_ang;
us_l287:
					i++ ;
					}
us_l288: 
				;
				}
			ua_box_site(&((*rdim)),corn_mod,dl_pt);
			/*
			i = (*rdim).txt_blk_use;
			ua_dia_sym_adjust(i,&((*rdim)),dl_pt,corn_mod);
			*/
			ua_box_frame(&((*rdim)),corn_mod);
			dis1 = um_dcccc(corn_mod[0],corn_mod[3]);
			dis1 = ( dis1-( 2.000000e+000*(*rdim).gap_to_geo ) );
				{
				int		us_t292;
				us_t292 = (*rdim).txt_blk_use;
				i = 1;
				for(;;)
					{
					if( i > us_t292 ) 	break;
						{
						UU_REAL	us_t293[3];
						um_vctmsc(vnorm,dis1,us_t293);
						um_vcplvc((*rdim).txt_blk[i-1].origin,us_t293,(*rdim).
						    txt_blk[i-1].origin);
						}
us_l290:
					i++ ;
					}
us_l291: 
				;
				}
			n = ( (*rdim).line_blk_use+1 );
			(*rdim).line_blk_use = n;
			(*rdim).line_blk[n-1].num_pts = 2;
			(*rdim).line_blk[n-1].subtype = dim_line;
			(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
			(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
			(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
			um_vctovc(cir_int_pt[1],(*rdim).line_blk[n-1].line_seg[1
			    -1]);
			um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[1]);
			um_vctovc(cir_pt,arrow_pts[0]);
			um_vctovc(cir_int_pt[1],arrow_pts[1]);
			}
		else
			{
			um_ilnpln(center,zaxis,origin,zaxis,&(num),cent_pt);
				{
				UU_REAL	us_t294[3];
				um_vcmnvc(cent_pt,cir_pt,us_t294);
				um_unitvc(us_t294,vec1);
				}
			ua_box_int(corner,cent_pt,vec1,int_pts);
			n = ( (*rdim).line_blk_use+1 );
			(*rdim).line_blk_use = n;
			(*rdim).line_blk[n-1].num_pts = 4;
			(*rdim).line_blk[n-1].subtype = dim_line;
			(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
			(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
			(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
			um_vctovc(cir_int_pt[1],(*rdim).line_blk[n-1].line_seg[0]);
			um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[1]);
			um_vctovc(int_pts[1],(*rdim).line_blk[n-1].line_seg[2]);
			um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[3]);
			um_vctovc(cir_pt,arrow_pts[0]);
			um_vctovc(cir_int_pt[1],arrow_pts[1]);
			}
		}
	else
		{
			{
			UU_REAL	us_t295[3];
			UU_REAL	us_t296[3];
			um_vcmnvc(corner[3],corner[0],us_t296);
			um_vctmsc(us_t296,(UU_REAL) 1.0 / 2.000000e+000,us_t295);
			um_vcplvc(corner[0],us_t295,edge1_pt);
			}
		um_vcmnvc(corner[1],corner[0],vec2);
		um_vcplvc(edge1_pt,vec2,edge2_pt);
		um_unitvc(vec2,vec2);
		i = 1;
		for(;;)
			{
			if( i > 2 ) 	break;
			switch( i )
				{
				case 1:
					{
						{
						UU_REAL	us_t299[3];
						um_vctmsc(vec2,(*rdim).stub_length,us_t299);
						um_vcmnvc(edge1_pt,us_t299,off_s_pt[i-1]);
						}
					}
					break;
				case 2:
					{
						{
						UU_REAL	us_t300[3];
						um_vctmsc(vec2,(*rdim).stub_length,us_t300);
						um_vcplvc(edge2_pt,us_t300,off_s_pt[i-1]);
						}
					}
					break;
				}
			um_vctovc(off_s_pt[i-1],temp);
			um_ilnpln(temp,zaxis,center,normal,&(num),pt2);
			um_vctovc(pt2,tst_pt[i-1]);
				{
				UU_REAL	us_t301[3];
				um_vcmnvc(center,tst_pt[i-1],us_t301);
				um_unitvc(us_t301,vec1);
				}
			um_ilncir(center,vec1,center,normal,radius,&(num),pt);
			if( ( num>1 ) )
				{
				j = us_clospnt(2,pt,tst_pt[i-1]);
				um_vctovc(pt[j-1],tst_pt[i-1]);
				um_vctovc(pt[j-1],dim_line_endpts[i-1][0]);
				if( ( j==1 ) )
					{
					um_vctovc(pt[1],dim_line_endpts[i-1][1]);
					}
				else
					{
					um_vctovc(pt[0],dim_line_endpts[i-1][1]);
					}
				}
			else
				{
				um_vctovc(pt[0],tst_pt[i-1]);
				um_vctovc(pt[0],dim_line_endpts[i-1][0]);
				um_vctovc(pt[0],dim_line_endpts[i-1][1]);
				}
us_l297:
			i++ ;
			}
us_l298: 
		;
		i = 1;
		for(;;)
			{
			if( i > 2 ) 	break;
			um_vctovc(tst_pt[i-1],temp);
			um_ilnpln(temp,zaxis,origin,zaxis,&(num),pt2);
			um_vctovc(pt2,tst_pt[i-1]);
us_l302:
			i++ ;
			}
us_l303: 
		;
		dis1 = um_dcccc(off_s_pt[0],tst_pt[0]);
		dis2 = um_dcccc(off_s_pt[1],tst_pt[1]);
		if( ( dis1<dis2 ) )
			{
			um_vctovc(edge1_pt,pt1);
			um_vctovc(tst_pt[0],cir_pt);
			um_vctovc(off_s_pt[0],pt2);
			j = 1;
			}
		else
			{
			um_vctovc(edge2_pt,pt1);
			um_vctovc(tst_pt[1],cir_pt);
			um_vctovc(off_s_pt[1],pt2);
			j = 2;
			}
		i = 1;
		for(;;)
			{
			if( i > 2 ) 	break;
			um_vctovc(dim_line_endpts[j-1][i-1],temp);
			um_ilnpln(temp,zaxis,origin,zaxis,&(num),ref_pt);
			um_vctovc(ref_pt,dim_line_endpts[j-1][i-1]);
us_l304:
			i++ ;
			}
us_l305: 
		;
		um_vctovc(dim_line_endpts[j-1][1],arrow_pts[0]);
		um_vctovc(dim_line_endpts[j-1][0],arrow_pts[1]);
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].num_pts = 6;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		um_vctovc(dim_line_endpts[j-1][0],(*rdim).line_blk[n-1].
		    line_seg[0]);
		um_vctovc(dim_line_endpts[j-1][1],(*rdim).line_blk[n-1].
		    line_seg[1]);
		um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[2]);
		um_vctovc(pt2,(*rdim).line_blk[n-1].line_seg[3]);
		um_vctovc(pt2,(*rdim).line_blk[n-1].line_seg[4]);
		um_vctovc(pt1,(*rdim).line_blk[n-1].line_seg[5]);
		}
	m = ( (*rdim).arrow_blk_use+1 );
	(*rdim).arrow_blk_use = m;
	um_vctovc(arrow_pts[0],(*rdim).arrow_blk[m-1].location);
		{
		UU_REAL	us_t306[3];
		um_vcmnvc((*rdim).line_blk[n-1].line_seg[1],(*rdim).
		    line_blk[n-1].line_seg[0],us_t306);
		(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,
		us_t306);
		}
	m = ( (*rdim).arrow_blk_use+1 );
	(*rdim).arrow_blk_use = m;
	um_vctovc(arrow_pts[1],(*rdim).arrow_blk[m-1].location);
		{
		UU_REAL	us_t307[3];
		um_vcmnvc((*rdim).line_blk[n-1].line_seg[0],(*rdim).
		    line_blk[n-1].line_seg[1],us_t307);
		(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis, us_t307);
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_dia_outiso(rdim, corner, center, normal, radius,
**			pt1, cir_pt, dir)
**    This function is used in ISO dimensioning for dimensioning the
**    diameter of a circle, when the dimension is to be placed outside
**    the circumference of the circle.
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
ua_dia_outiso(rdim, corner, center, normal, radius,
	pt1, cir_pt, dir)
struct UA_generic_draft	(*rdim);
UU_REAL	corner[4][3];
UU_REAL	center[3];
UU_REAL	normal[3];
UU_REAL	radius;
UU_REAL	pt1[3];
UU_REAL	cir_pt[3];
int		(*dir);
	{
	UU_REAL	sign;
	UU_REAL	tst_pt[2][3];
	UU_REAL	cir_int_pt[2][3];
	UU_REAL	temp[3];
	UU_REAL	pt2[3];
	UU_REAL	corn_mod[4][3];
	int		num;
	UU_REAL	off_s_pt[2][3];
	UU_REAL	origin[3];
	UU_REAL	cent_pt[3];
	UU_REAL	txt_ang;
	UU_REAL	pt[4][3];
	UU_REAL	vec1[3];
	UU_REAL	vec2[3];
	UU_REAL	dis1;
	UU_REAL	dis2;
	UU_REAL	int_pts[2][3];
	int		i;
	int		j;
	int		k;
	UU_REAL	ref_pt[3];
	UU_REAL	ext_pt_vec[3];
	UU_REAL ext_pt[3];
	UU_REAL	tmp[3];
	int		l;
	int		m;
	int		n;
	UU_REAL	edge1_pt[3];
	UU_REAL	edge2_pt[3];
	UU_REAL	int_pt[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	vnorm[3];
	UU_REAL	dl_pt[3];
	UU_REAL	magtude;

	uu_denter(UU_STRC,(us,"ua_dia_outiso(center=<%g,%g,%g>,radius=%g)",
		center[0],center[1],center[2],radius));

	(*rdim).line_blk_use = 0;
	ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
		{
		UU_REAL	us_t232[3];
		UU_REAL	us_t233[3];
		UU_REAL	us_t234[3];
		UU_REAL	us_t235[3];
		UU_REAL	us_t236[3];
		um_vcmnvc(corner[3],corner[0],us_t234);
		um_vctmsc(us_t234,(UU_REAL) 1.0 / 2.0,us_t233);
		um_vcplvc(corner[0],us_t233,us_t232);
		um_vcmnvc(corner[1],corner[0],us_t236);
		um_vctmsc(us_t236,(UU_REAL) 1.0 / 2.0,us_t235);
		um_vcplvc(us_t232,us_t235,cent_pt);
		}
	if( ( (*rdim).txt_orent==0 ) )
		um_vctovc(cent_pt,ref_pt);
	else
		{
		UU_REAL	us_t237[3];
		UU_REAL	us_t238[3];
		UU_REAL	rad_line[3];
		UU_REAL rad_line_angle;
		UU_REAL hold_mag;
		um_vcmnvc(corner[1],corner[2],us_t238);
		hold_mag = um_mag(us_t238);	
		for(i=0;i<3;i++)
			us_t237[i] = us_t238[i] / hold_mag;
		hold_mag = hold_mag + (*rdim).txt_gap;
		um_vctmsc(us_t237,hold_mag,us_t237);
		um_vcmnvc((*rdim).dim_origin,center,rad_line);
		rad_line_angle = ua_dir_angle(zaxis,xaxis,rad_line);
		switch ((*rdim).txt_orent)
			{
			case 3:
				if ((rad_line_angle > 1.570796)
				    &&(rad_line_angle < 4.712389))
					um_vcplvc(corner[0],us_t237,ref_pt);
				else
					um_vcplvc(corner[1],us_t237,ref_pt);
				break;
			default:
				if ((rad_line_angle < 1.570796)
				    ||(rad_line_angle > 4.712389))
					um_vcplvc(corner[2],us_t237,ref_pt);
				else
					um_vcplvc(corner[3],us_t237,ref_pt);
				break;
			}
		}
	um_ilnpln(ref_pt,zaxis,center,normal,&(num),pt1);
		{
		UU_REAL	us_t239[3];
		um_vcmnvc(center,pt1,us_t239);
		um_unitvc(us_t239,vec1);
		}
	um_ilncir(center,vec1,center,normal,radius,&(num),pt);
	if( ( num>1 ) )
		{
		j = us_clospnt(2,pt,pt1);
		um_vctovc(pt[j-1],cir_pt);
		um_vctovc(pt[j-1],cir_int_pt[0]);
		um_vctovc(pt[0],cir_int_pt[1]);
		}
	else
		{
		j = 1;
		um_vctovc(pt[0],cir_pt);
		um_vctovc(pt[j-1],cir_int_pt[0]);
		um_vctovc(pt[1],cir_int_pt[1]);
		}
		{
		UU_REAL	us_t240[3];
		um_vcmnvc(cir_pt,pt1,us_t240);
		um_unitvc(us_t240,vec2);
		}
	sign = um_dot(vec1,vec2);
	um_vctovc(cir_pt,temp);
	um_ilnpln(temp,zaxis,origin,zaxis,&(num),cir_pt);
	if( ( sign<0.0 ) )
		{
		if( ( (*rdim).txt_orent>0 ) )
			{
			(*rdim).entity_site = 1;
				{
				UU_REAL	us_t241[3];
				um_cross(zaxis,vec1,us_t241);
				um_unitvc(us_t241,vnorm);
				}
			if( ( um_dot(vnorm,yaxis)<0.0 ) )
				{
				um_vctmsc(vnorm,-1.0,vnorm);
				}
			txt_ang = um_angle2p(yaxis,vnorm,zaxis);
			for(i=1;i<=(*rdim).txt_blk_use;i++)
				(*rdim).txt_blk[i-1].tangle = txt_ang;
			n = ( (*rdim).line_blk_use+1 );
			(*rdim).line_blk_use = n;
			(*rdim).line_blk[n-1].num_pts = 2;
			(*rdim).line_blk[n-1].subtype = dim_line;
			(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
			(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
			(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
			um_vctovc(center,(*rdim).line_blk[n-1].line_seg[0]);
			um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[1]);
			}
		else
			{
			um_ilnpln(center,zaxis,origin,zaxis,&(num),cent_pt);
				{
				UU_REAL	us_t249[3];
				um_vcmnvc(cent_pt,cir_pt,us_t249);
				um_unitvc(us_t249,vec1);
				}
			ua_box_int(corner,cent_pt,vec1,int_pts);
			n = ( (*rdim).line_blk_use+1 );
			(*rdim).line_blk_use = n;
			(*rdim).line_blk[n-1].num_pts = 4;
			(*rdim).line_blk[n-1].subtype = dim_line;
			(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
			(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
			(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
				{
				UU_REAL	us_t250[3];
				um_vcmnvc(int_pts[0],cir_pt,us_t250);
				um_unitvc(us_t250,vec1);
				}
				{
				UU_REAL	us_t251[3];
				um_vcmnvc(center,cir_pt,us_t251);
				um_unitvc(us_t251,vec2);
				}
			if( ( um_dot(vec1,vec2)<-5.000000e-001 ) )
				{
				um_vctovc(center,(*rdim).line_blk[n-1].line_seg[0]);
				dis1 = um_dcccc(int_pt,int_pts[0]);
				dis2 = um_dcccc(int_pt,int_pts[1]);
				if( ( dis1<dis2 ) )
					um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[1]);
				else
					um_vctovc(int_pts[1],(*rdim).line_blk[n-1].line_seg[1]);
				(*rdim).line_blk[n-1].num_pts = 2;
				}
			else
				{
				um_vctovc(center,(*rdim).line_blk[n-1].line_seg[0]);
				um_vctovc(int_pts[0],(*rdim).line_blk[n-1].line_seg[1]);
				um_vctovc(int_pts[1],(*rdim).line_blk[n-1].line_seg[2]);
				um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[3]);
				}
			}
		ua_getcpln(&((*rdim)),origin,xaxis,yaxis,zaxis);
		m = ( (*rdim).arrow_blk_use+1 );
		(*rdim).arrow_blk_use = m;
		um_vctovc(cir_pt,(*rdim).arrow_blk[m-1].location);
			{
			UU_REAL	us_t252[3];
			um_vcmnvc((*rdim).line_blk[n-1].line_seg[1],(*rdim).
			    line_blk[n-1].line_seg[0],us_t252);
			(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,
			us_t252);
			}
		}
	else
		{
		if( ( (*rdim).txt_orent==0 ) )
			{
			UU_REAL	us_t253[3];
			UU_REAL	us_t254[3];
			um_vcmnvc(corner[3],corner[0],us_t254);
			um_vctmsc(us_t254,(UU_REAL) 1.0 / 2.0,us_t253);
			um_vcplvc(corner[0],us_t253,edge1_pt);
			um_vcmnvc(corner[1],corner[0],vec2);
			um_vcplvc(edge1_pt,vec2,edge2_pt);
			um_unitvc(vec2,vec2);
			for(i=1;i<=2;i++)
				{
				switch( i )
					{
					case 1: {
						UU_REAL	us_t257[3];
						um_vctmsc(vec2,(*rdim).stub_length,us_t257);
						um_vcmnvc(edge1_pt,us_t257,off_s_pt[i-1]);
						break;
						}
					case 2:	{
						UU_REAL	us_t258[3];
						um_vctmsc(vec2,(*rdim).stub_length,us_t258);
						um_vcplvc(edge2_pt,us_t258,off_s_pt[i-1]);
						break;
						}
					}
				um_vctovc(off_s_pt[i-1],temp);
				um_ilnpln(temp,zaxis,center,normal,&(num),pt2);
				um_vctovc(pt2,tst_pt[i-1]);
					{
					UU_REAL	us_t259[3];
					um_vcmnvc(center,tst_pt[i-1],us_t259);
					um_unitvc(us_t259,vec1);
					}
				um_ilncir(center,vec1,center,normal,radius,&(num),pt);
				if( ( num>1 ) )
					{
					j = us_clospnt(2,pt,tst_pt[i-1]);
					um_vctovc(pt[j-1],tst_pt[i-1]);
					}
				else
					um_vctovc(pt[0],tst_pt[i-1]);
				}
			for(i=0;i<2;i++)
				{
				um_vctovc(tst_pt[i],temp);
				um_ilnpln(temp,zaxis,origin,zaxis,&(num),pt2);
				um_vctovc(pt2,tst_pt[i]);
				}
			dis1 = um_dcccc(off_s_pt[0],tst_pt[0]);
			dis2 = um_dcccc(off_s_pt[1],tst_pt[1]);
			if( ( dis1<dis2 ) )
				{
				um_vctovc(edge1_pt,pt1);
				um_vctovc(tst_pt[0],cir_pt);
				(*dir) = 3;
				}
			else
				{
				um_vctovc(edge2_pt,pt1);
				um_vctovc(tst_pt[1],cir_pt);
				(*dir) = 4;
				}
			}
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].num_pts = 2;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[0]);
		um_vctovc(ref_pt,(*rdim).line_blk[n-1].line_seg[1]);
		m = (*rdim).arrow_blk_use + 1;
		(*rdim).arrow_blk_use = m;
		um_vctovc(cir_pt,(*rdim).arrow_blk[m-1].location);
		um_vcmnvc((*rdim).line_blk[n-1].line_seg[1],(*rdim).line_blk[n-1].line_seg[0],tmp);
		(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,tmp) + 3.141593;
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].num_pts = 2;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		um_vctovc(cir_pt,(*rdim).line_blk[n-1].line_seg[0]);
		um_vctovc(cir_int_pt[1],(*rdim).line_blk[n-1].line_seg[1]);
		m = (*rdim).arrow_blk_use + 1;
		(*rdim).arrow_blk_use = m;
		um_vctovc(cir_int_pt[1],(*rdim).arrow_blk[m-1].location);
			{
			UU_REAL	us_t361[3];
			um_vcmnvc((*rdim).line_blk[n-1].line_seg[0],(*rdim).
			    line_blk[n-1].line_seg[1],us_t361);
			(*rdim).arrow_blk[m-1].aangle = ua_dir_angle(zaxis,xaxis,
			us_t361);
			}
		{
		UU_REAL	us_t362[3];
		UU_REAL	us_t363[3];
		magtude = (3.0 * (*rdim).arrow_size);
		um_vcmnvc(cir_int_pt[1],center,us_t362);
		um_unitvc(us_t362,ext_pt_vec);
		um_vctmsc(ext_pt_vec,magtude,us_t363);
		um_vcplvc(cir_int_pt[1],us_t363,ext_pt);
		}
		n = ( (*rdim).line_blk_use+1 );
		(*rdim).line_blk_use = n;
		(*rdim).line_blk[n-1].num_pts = 2;
		(*rdim).line_blk[n-1].subtype = dim_line;
		(*rdim).line_blk[n-1].line.line_font = UA_dim_line_font;
		(*rdim).line_blk[n-1].line.line_density = UA_dim_line_dens;
		(*rdim).line_blk[n-1].line.color = UA_dim_line_color;
		um_vctovc(cir_int_pt[1],(*rdim).line_blk[n-1].line_seg[0]);
		um_vctovc(ext_pt,(*rdim).line_blk[n-1].line_seg[1]);
		}
	uu_dexit;

}
/*********************************************************************
**    E_FUNCTION     : int		ua_dia_plus(rdim, center, xaxis, yaxis )
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
int		ua_dia_plus(rdim, center, xaxis, yaxis )
struct UA_generic_draft	*rdim;
UM_coord	center, xaxis, yaxis;
	{
	int		n;
	UM_coord pt1, pt2, pt3;
	UU_REAL     plus_size;

	uu_denter(UU_STRC,(us,"ua_dia_plus"));

	plus_size = rdim->char_size/4.0;
	um_vctmsc(xaxis, plus_size, pt1);
	um_vcplvc(center, pt1, pt2);
	um_vcmnvc(center, pt1, pt3);
	n = rdim->line_blk_use+1;
	rdim->line_blk_use = n;
	rdim->line_blk[n-1].num_pts = 4;
	rdim->line_blk[n-1].subtype = dim_line;
	um_vctovc(pt2,rdim->line_blk[n-1].line_seg[0]);
	um_vctovc(pt3,rdim->line_blk[n-1].line_seg[1]);
	um_vctmsc(yaxis, plus_size, pt1);
	um_vcplvc(center, pt1, pt2);
	um_vcmnvc(center, pt1, pt3);
	um_vctovc(pt2,rdim->line_blk[n-1].line_seg[2]);
	um_vctovc(pt3,rdim->line_blk[n-1].line_seg[3]);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_dia_rep_dim(subtype)
**       C
**			This function is the user interface portion of
**                      repetitive dimensioning	of equal diameter arcs.
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_dia_rep_dim(subtype)
int     subtype;
	{
	struct UA_generic_draft	rdim;
	struct UA_PLOCREC plocrec1;
	struct UA_PLOCREC plocrec2;
	int     dir;
	int	num;
	int     numofent;
	int     number;
	int     num_arcs;
	int	dim_type;
	int     relation;
	int     curr_key;
	int     next_key;
	int     prev_key;
	int     ok;
	int     s_status;
	int     prev_txt_blk;
	int     d_stat;		
	int	i;
	int	j;
	int	k;
	int	l;
	int	m;
	int	n;
	UU_REAL	corner[4][3];
	UU_REAL	center[3];
	UU_REAL	normal[3];
	UU_REAL	radius;
	UU_REAL	pt1[3];
	UU_REAL	cir_pt[3];
	UU_REAL	off_s_pt[2][3];
	UU_REAL	origin[3];
	UU_REAL	pt[4][3];
	UU_REAL c_spt[3];
	UU_REAL cpln_origin[3];
	UU_REAL c_ept[3];
	UU_REAL dang;
	UU_REAL dummy;
	UU_REAL	vec1[3];
	UU_REAL	vec2[3];
	UU_REAL	dis1;
	UU_REAL	dis2;
	UU_REAL	tst_pt[2][3];
	UU_REAL	temp[3];
	UU_REAL	pt2[3];
	UU_REAL	pt3[3];
	UU_REAL	edge1_pt[3];
	UU_REAL	edge2_pt[3];
	UU_REAL	tmp[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL diam[50];
	UU_REAL differ;
	UU_REAL	dl_pt[3];
	UU_LOGICAL status;
	UU_LOGICAL first;
	UU_LOGICAL redo;

	uu_denter(UU_STRC,(us,"ua_dia_rep_dim()"));

	curr_key = -1;
	us_init_adiam();
	us_init_autility();
	first = UU_TRUE;
	rdim.asso_blk_use = 0;
main_loop:
	if( subtype == 9 )
		ua_init_entity(UA_DIA_IN_DIM,subtype,&(rdim));
	else
		ua_init_entity(UA_RAD_CEN_DIM,subtype,&(rdim));
	ua_getcpln(&(rdim),cpln_origin,xaxis,yaxis,zaxis);
	rdim.asso_blk_use = rdim.asso_blk_use + 1;
	rdim.txt_just = UA_LEFT;
	rdim.entity_site = UA_TOP_LEFT;
	redo = UU_FALSE;
	numofent = 1;
get:
	ud_lgeo(UU_TRUE,UD_draftable);
	/* Select arc to dimension */
	s_status = ua_select_ent_subf(34,&(rdim),1,&(plocrec1));
	switch( s_status )
		{
		case UA_REJECT:
			{
			uu_dexit;
			return;
			}
		case UA_ALT_ACTION:
			{
			uu_dexit;
			return;
			}
		case UA_OPCOMPLETE:
			{
			curr_key = rdim.asso_blk[0].key;
			s_status = um_retrieve_data_relnum(curr_key,&(relation));
			if( (relation == UA_LINEAR_DIM) )
				{
				rdim.key = curr_key;
				uc_retrieve_data(&(rdim),sizeof(struct UA_generic_draft));
				dim_type = rdim.etype;
				num_arcs = rdim.arc_blk_use;
				if( subtype == 9 )
					ua_init_entity(UA_DIA_IN_DIM,subtype,&(rdim));
				else
					ua_init_entity(UA_RAD_CEN_DIM,subtype,&(rdim));
				if( (dim_type == UA_CENTERLINE) )
					{
					if( (num_arcs==0) )
						goto get;
					}
				}
			else
				{
				ok = uc_draft_type(curr_key,&(relation));
				if( (relation != UA_DRAFT_ARC) )
					goto get;
				}
			}
			break;
		}
	s_status = um_retrieve_data_relnum(curr_key,&(relation));
	if( (relation == UA_LINEAR_DIM) )
		ua_centerline_arc(curr_key,center,&(radius),&(dang),normal,
				  c_spt,c_ept,&(dummy));
	else
		uc_draft_arc(curr_key,center,&(radius),&(dang),normal,c_spt,
			     c_ept,&(dummy));		  
	if( (fabs(um_dot(normal,zaxis)) < UA_FUZZ) )
		{
		uu_uerror0(UA_DRAFTING,17);
		goto get;
		}
	diam[0] = radius;
	um_vctovc(center,rdim.asso_blk[0].location);
	/* Select dimension origin */
	status = ua_dia_org(&(rdim),redo);
	if( (!status) )
		{
		uu_dexit;
		return;
		}
loop:
	ud_lgeo(UU_TRUE,UD_draftable);
	/* Select next arc to dimension */
	s_status = ua_select_ent_subf(172,&rdim,2,&plocrec2);
	switch(s_status)
		{
		case UA_REJECT:
			goto get_value;
		case UA_ALT_ACTION:
			{
			uu_dexit;
			return;
			}
		case UA_OPCOMPLETE:
			{
			redo = UU_FALSE;
			next_key = rdim.asso_blk[1].key;
			s_status = um_retrieve_data_relnum(next_key,&(relation));
			if( (relation == UA_LINEAR_DIM) )
				{
				rdim.key = next_key;
				uc_retrieve_data(&(rdim),sizeof(struct UA_generic_draft));
				dim_type = rdim.etype;
				num_arcs = rdim.arc_blk_use;
				if( subtype == 9 )
					ua_init_entity(UA_DIA_IN_DIM,subtype,&(rdim));
				else
					ua_init_entity(UA_RAD_CEN_DIM,subtype,&(rdim));
				if( (dim_type == UA_CENTERLINE) )
					{
					if( (num_arcs==0) )
						goto get;
					}
				}
			else
				{
				ok = uc_draft_type(next_key,&(relation));
				if( (relation != UA_DRAFT_ARC) )
					goto get;
				}
			}
			break;
		}
	s_status = um_retrieve_data_relnum(next_key,&(relation));
	if( (relation == UA_LINEAR_DIM) )
		ua_centerline_arc(next_key,center,&(radius),&(dang),normal,
				  c_spt,c_ept,&(dummy));
	else
		uc_draft_arc(next_key,center,&(radius),&(dang),normal,c_spt,
			     c_ept,&(dummy));		  
	if( (fabs(um_dot(normal,zaxis)) < UA_FUZZ) )
		{
		uu_uerror0(UA_DRAFTING,17);
		goto loop;
		}
	numofent += 1;
	diam[numofent] = radius;
	/* Determine if the two diameters are equal */
	differ = fabs(diam[0] - diam[numofent]);
	if(((int)((differ * pow((UU_REAL)(10.0),(UU_REAL)(rdim.dim_places))) + 0.5)) != 0)
		{
		uu_uerror0(UA_DRAFTING, 59);
		uu_dexit;
		return(0);
		}
	else
		goto loop;

get_value:
	rdim.dim2_value = numofent;

	s_status = um_retrieve_data_relnum(curr_key,&(relation));
	if( (relation == UA_LINEAR_DIM) )
		ua_centerline_arc(curr_key,center,&(radius),&(dang),normal,
				  c_spt,c_ept,&(dummy));
	else
		uc_draft_arc(curr_key,center,&(radius),&(dang),normal,c_spt,
			     c_ept,&(dummy));		  

	s_status = ua_dia_cre_rep_dim(&(rdim),subtype,center,radius,dang,normal);
	if( s_status == 0 )
		goto main_loop;

if( (redo == UU_TRUE) )
	{
	s_status = ua_update_entity(prev_key,&(rdim));
	if( (s_status != 0) )
		ua_create_entity(&(rdim),&(curr_key));
	}
else
	ua_create_entity(&(rdim),&(curr_key));
uc_display(&(rdim));
prev_key = rdim.key;
prev_txt_blk = rdim.txt_blk_use;
first = UU_FALSE;
goto main_loop;
}
/*********************************************************************
**    E_FUNCTION     : ua_dia_cre_rep_dim(rdim,subtype,center,radius,dang,normal)
**       C
**			This function is used for creating repetitive 
**			dimensioning of equal diameter arcs.
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_dia_cre_rep_dim(rdim,subtype,center,radius,dang,normal)
struct UA_generic_draft	*rdim;
int     subtype;
UU_REAL	center[3];
UU_REAL	radius;
UU_REAL dang;
UU_REAL	normal[3];
	{
	int     dir;
	int     numofent;
	int     number;
	int	dim_type;
	int     relation;
	int     s_status;
	int	i;
	int	j;
	int	k;
	int	l;
	int	m;
	int	n;
	UU_REAL	corner[4][3];
	UU_REAL	pt1[3];
	UU_REAL	cir_pt[3];
	UU_REAL	off_s_pt[2][3];
	UU_REAL	origin[3];
	UU_REAL	pt[4][3];
	UU_REAL c_spt[3];
	UU_REAL cpln_origin[3];
	UU_REAL c_ept[3];
	UU_REAL dummy;
	UU_REAL	vec1[3];
	UU_REAL	vec2[3];
	UU_REAL	dis1;
	UU_REAL	dis2;
	UU_REAL	tst_pt[2][3];
	UU_REAL	temp[3];
	UU_REAL	pt2[3];
	UU_REAL	pt3[3];
	UU_REAL	edge1_pt[3];
	UU_REAL	edge2_pt[3];
	UU_REAL	tmp[3];
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL diam[50];
	UU_REAL differ;
	UU_REAL	dl_pt[3];
	UU_LOGICAL status;
	char	rsym[16];
	char	dsym[10];

	uu_denter(UU_STRC,(us,"ua_dia_cre_rep_dim()"));

	(*rdim).txt_blk_use = 0;
	(*rdim).line_blk_use = 0;
	numofent = (*rdim).dim2_value;
text:
	/* subtype of 9 for repetitive diameter */
	if( subtype == 9 )
		(*rdim).dim_value = ( 2.0*radius );
	/* subtype of 10 for repetitive radial */
	else
		(*rdim).dim_value = radius;

	/* Add a text block with number of ents */

	if( ( (*rdim).txt_entry==UA_SYS_TEXT ) )
		{
		/* subtype of 9 for repetitive diameter */
		if( subtype == 9 )
			{
			switch( (*rdim).diam_symbol )
				{
				case UA_NO_SYMBOL:
					{
					goto offset;
					}
				case UA_R_SYMBOL:
					{
					sprintf(rsym,"%dX DIA",numofent);
					number = strlen(rsym);
					}
					break;
				case UA_RAD_SYMBOL:
					{
					sprintf(rsym,"%dX \\T", numofent);
					number = strlen(rsym) - 1;
					}
					break;
				case UA_USER_SYMBOL:
					{
					sprintf(rsym,"%dX %s", numofent,UA_usr_dia_sym);
					number = strlen(rsym);
					}
					break;
				}
			}
		/* subtype of 10 for repetitive radial */
		else
			{
			switch( (*rdim).rad_symb )
				{
				case 0:
					{
					goto offset;
					}
				case 1:
					{
					sprintf(rsym,"%dX R",numofent);
					number = strlen(rsym);
					}
					break;
				case 2:
					{
					sprintf(rsym,"%dX RAD", numofent);
					number = strlen(rsym);
					}
					break;
				case 3:
					{
					sprintf(rsym,"%dX %s", numofent, UA_usr_rad_sym);
					number = strlen(rsym);
					}
					break;
				}
			}
		}
		(*rdim).txt_blk_use = ( (*rdim).txt_blk_use+1 );
		i = (*rdim).txt_blk_use;
		(*rdim).txt_blk[i-1].char_cnt = number;
		strcpy((*rdim).txt_blk[i-1].tstring,rsym);
		(*rdim).txt_blk[i-1].subtype = main_txt1;
		ua_set_dim_text(&(*rdim));
offset:
	ua_box_site(&((*rdim)),corner,dl_pt);
	ua_box_frame(&(*rdim),corner);

	ua_dia_outside(&(*rdim),corner,center,normal,radius,pt1,cir_pt,&(dir));

drw_leader:
	ua_crea_leader(&(*rdim),pt1,cir_pt,dir);
	ua_merge_line_blk(&(*rdim));

	uu_dexit;
	return(1);
	}

