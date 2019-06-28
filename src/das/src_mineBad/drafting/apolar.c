/*********************************************************************
**    NAME         : apolar.c
**       CONTAINS:
**      		 ua_cre_polar
**     	    ua_polar
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       apolar.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:38
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]=	{
	"@(#) apolar.c 3.3 6/8/88 15:58:43 single"	};
#else
static char uu_sccsident[]=	{
	"@(#) apolar.c 3.3 6/8/88 15:58:43 double"	};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "uhep.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mdcoord.h"
#include "mdmatrix.h"
#include "mcrv.h"
#include "modef.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mdeval.h"
#include "mdattr.h"
#include "mattr.h"
#include "msol.h"
#include "mxxx.h"
#include "mdraw.h"
#include "mdebug.h"

extern int UD_draftable[UD_NMENTWD];
extern int UD_draft_line[UD_NMENTWD];

static struct UA_generic_draft		ldim_test;

/*********************************************************************
**    E_FUNCTION     : ua_polar()
**       User interaction routine for the creation of polar
**			coordinate dimensions.
**    PARAMETERS   
**       INPUT  : 
**          none					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_polar()
	{
	struct UA_PLOCREC	plocrec1;
	struct UA_PLOCREC	plocrec2;
	struct UA_generic_draft	ldim;
	struct UA_PLOCREC	polarploc;
	UU_KEY_ID curr_key;
	int	polar_key, previous_txt_blk_use, origin_mode, previous_key,
			polar_modifier, j, count, dummy, polar_asso_type, status,
			polar_org_key, polar_org_asso_type, polar_org_modifier, 
			relation1, relation2, s_status, dim_type, num, rel_num;
	UU_REAL	ext_offset_dist[2], dim_line_length[2], dis1, dis2, radius, dang,
				r_dummy, ext_offset;
	UM_coord	cpln_origin, xaxis, polar_vec, yaxis, zaxis, entity_origin, normal,
				ewc, spt1, ept1, spt2, ept2, vec1, vec2, npt, c_spt, c_ept,
				polar_axis_pt, polar_origin_pt;
	UU_LOGICAL	first_time, redo, ok;
	UU_LOGICAL  um_ptinseg();

	uu_denter(UU_STRC,(us,"ua_polar()"));

	first_time = UU_TRUE;
	count = 0;
	origin_mode = 1;
	ua_init_entity(UA_POLAR_DIM,1,&ldim);
	ldim.txt_just = UA_LEFT;
	ldim.entity_site = UA_MIDDLE_CENTER;
	ext_offset_dist[0] = ext_offset_dist[1] = ldim.gap_to_geo;
	dim_line_length[0] = dim_line_length[1] = 0.0;
	ua_getcpln(&ldim,cpln_origin,xaxis,yaxis,zaxis);

line1:
	/* get polar axis line from user */
	ud_lgeo(UU_TRUE,UD_draftable);
	status = ua_select_ent_subf(138,&ldim,1,&plocrec1);
	if( ( status==UA_REJECT ) )
		{
		uu_dexit;
		return;
		}

	/* check that the user pick a line or a center-line entity */
	ldim.asso_blk_use = 1;
	curr_key = ldim.asso_blk[ldim.asso_blk_use-1].key;
	s_status = um_retrieve_data_relnum(curr_key, &relation1);
	if( relation1 == UA_LINEAR_DIM ) 
		{
		ldim_test.key = curr_key;
		uc_retrieve_data(&ldim_test, sizeof(struct UA_generic_draft));
		dim_type = ldim_test.etype;
		if( ( dim_type != UA_CENTERLINE ) )
			{
			uu_uerror0(UA_DRAFTING,8);
			goto line1;
			}
		}
	else
		{
		status = uc_draft_type(curr_key, &relation1);
		if( ( relation1 != UA_DRAFT_LINE ) )
			{
			uu_uerror0(UA_DRAFTING,8);
			goto line1;
			}
		}
	polar_key = ldim.asso_blk[0].key;
	polar_modifier = ldim.asso_blk[0].modifier;
	polar_asso_type = ldim.asso_blk[0].asso_type;
	uu_move_byte( &plocrec1, &polarploc, sizeof( struct UA_PLOCREC	 ) );

line2:
	/* get intersecting line from user */
	ud_lgeo(UU_TRUE,UD_draftable);
	status = ua_select_ent_subf(139,&ldim,2,&plocrec1);
	if( ( status==UA_REJECT ) )
		{
		uu_dexit;
		return;
		}

	/* check that the user pick a line or a center-line entity */
	ldim.asso_blk_use = 2;
	curr_key = ldim.asso_blk[ldim.asso_blk_use-1].key;
	s_status = um_retrieve_data_relnum(curr_key, &relation2);
	if( ( relation2 == UA_LINEAR_DIM ) )
		{
		ldim_test.key = curr_key;
		uc_retrieve_data(&ldim_test, sizeof(struct UA_generic_draft));
		dim_type = ldim_test.etype;
		if( ( dim_type != UA_CENTERLINE ) )
			{
			uu_uerror0(UA_DRAFTING,8);
			goto line2;
			}
		}
	else
		{
		status = uc_draft_type(curr_key, &relation2);
		if( ( relation2 != UA_DRAFT_LINE ) )
			{
			uu_uerror0(UA_DRAFTING,8);
			goto line2;
			}
		}
	polar_org_key = ldim.asso_blk[1].key;
	polar_org_modifier = ldim.asso_blk[1].modifier;
	polar_org_asso_type = ldim.asso_blk[1].asso_type;

	/* retrieve data for the two lines */
	if( ( relation1==UA_LINEAR_DIM ) )
		{
		ua_centerline_line(ldim.asso_blk[0].key, &polarploc, spt1, ept1);
		}
	else
		{
		uc_draft_line(ldim.asso_blk[0].key, spt1, ept1);
		}
	um_nptpln(spt1,cpln_origin,zaxis,npt);
	um_vctovc(npt, spt1);
	um_nptpln(ept1,cpln_origin,zaxis,npt);
	um_vctovc(npt, ept1);
	um_vctovc(spt1, ldim.asso_blk[0].location);
	um_vctovc(spt1, polar_axis_pt);

	if( ( relation2==UA_LINEAR_DIM ) )
		{
		ua_centerline_line(ldim.asso_blk[1].key, &plocrec1, spt2, ept2);
		}
	else
		{
		uc_draft_line(ldim.asso_blk[1].key,spt2,ept2);
		}
	um_nptpln(spt2,cpln_origin,zaxis,npt);
	um_vctovc(npt, spt2);
	um_nptpln(ept2,cpln_origin,zaxis,npt);
	um_vctovc(npt, ept2);
	um_vctovc(spt2, ldim.asso_blk[1].location);
	um_vctovc(spt2, polar_origin_pt);

	/* intersect lines to determine origin */
	um_vcmnvc(spt1, ept1, vec1);
	um_vcmnvc(spt2, ept2, vec2);
	um_unitvc(vec1, vec1);
	um_unitvc(vec2, vec2);
	um_ilnln(ept1, vec1, ept2, vec2, &num, npt);
	if(num == 0)
		{
		uu_uerror0(UA_DRAFTING,8);
		goto line1;
		}

loop:
	/* main loop to get entities and create a POLAR DIMENSION */
	if( count>0 )
		{
		ua_init_entity(UA_POLAR_DIM, 1, &ldim);
		ldim.txt_just = UA_LEFT;
		ldim.entity_site = UA_MIDDLE_CENTER;
		ldim.asso_blk_use = 2;
		ldim.asso_blk[0].key = polar_key;
		ldim.asso_blk[0].modifier = polar_modifier;
		ldim.asso_blk[0].asso_type = polar_asso_type;
		um_vctovc(polar_axis_pt, ldim.asso_blk[0].location);
		ldim.asso_blk[1].key = polar_org_key;
		ldim.asso_blk[1].modifier = polar_org_modifier;
		ldim.asso_blk[1].asso_type = polar_org_asso_type;
		um_vctovc(polar_origin_pt, ldim.asso_blk[1].location);
		}
	ud_lgeo(UU_TRUE,UD_draftable);
	status = ua_select_ent_subf(140,&ldim,3,&plocrec2);
	switch( status )
		{
		case UA_REJECT:
			{
			uu_dexit;
			return;
			}
		case UA_ALT_ACTION:
			{
			if( ( count==1 ) )
				{
				redo = UU_TRUE;
				ldim.key = previous_key;
				status = uc_retrieve_data(&ldim,sizeof(struct UA_generic_draft));
				if( ( status==0 ) )
					{
					ldim.arc_blk_use = 0;
					ldim.line_blk_use = 0;
					ldim.arrow_blk_use = 0;
					ldim.txt_blk_use = previous_txt_blk_use;
					j = 1;
					for(j=0;j<UA_NUM_TXTBLKS;j++)
						{
						ldim.txt_blk[j].tangle = UA_text_ang;
						}
					origin_mode = 1;
					count = 0;
					ext_offset_dist[0] = ext_offset_dist[1] = ldim.gap_to_geo;
					goto origin;
					}
				else
					{
					redo = UU_FALSE;
					previous_key = 0;
					count = 0;
					origin_mode = 1;
					ua_init_entity(UA_POLAR_DIM, 1, &ldim);
					ldim.txt_just = UA_LEFT;
					ldim.entity_site = UA_MIDDLE_CENTER;
					ua_getcpln(&ldim,cpln_origin,xaxis,yaxis,zaxis);
					ext_offset_dist[0] = ext_offset_dist[1] = ldim.gap_to_geo;
					goto line1;
					}
				}
			else
				{
				uu_dexit;
				return;
				}
			}
		case UA_OPCOMPLETE:
			{
			redo = UU_FALSE;
			}
			break;
		}
	ldim.asso_blk_use = 3;

text:
	/* check for user defined text */
	ok = ua_text_subf(&(ldim));
	if( ( !ok ) )
		{
		goto text;
		}
	previous_txt_blk_use = ldim.txt_blk_use;

origin:
	/* get entity information */
	s_status = um_retrieve_data_relnum(ldim.asso_blk[2].key, &rel_num);
	switch(rel_num)
		{
		case UA_LINEAR_DIM:
			ua_near_on_draft(ldim.asso_blk[2].key, &plocrec2,
						ldim.asso_blk[2].location);
			ext_offset = ldim.grid_dist;
			break;
		case UM_CIRCLE_REL:
			uc_draft_arc(ldim.asso_blk[2].key, ldim.asso_blk[2].location,
						&radius, &dang, normal, c_spt, c_ept, &r_dummy);
			ext_offset = ldim.grid_dist + radius;
			break;
		case UM_CONIC_REL:
			uc_draft_conic(ldim.asso_blk[2].key, ldim.asso_blk[2].location,
						c_spt , c_ept);
			ext_offset = ldim.grid_dist;
			break;
		default:
			uc_draft_endpts(2, &plocrec2.ppick, &plocrec2.ndcloc, &rel_num,
						ldim.asso_blk[2].location, normal);
			ext_offset = ldim.grid_dist;
			break;
		break;
		}
	um_nptpln(ldim.asso_blk[2].location,cpln_origin,zaxis,entity_origin);
	um_vctovc(entity_origin, ldim.asso_blk[2].location);

	/* create polar coordinate dimension */
	status = ua_cre_polar(cpln_origin, xaxis, yaxis, zaxis, spt1,
								ept1, vec1, npt, count, ext_offset, ext_offset_dist, 
								dim_line_length, &ldim);
	if( ( status!=1 ) )
		{
		if( ( status==2 ) )
			{
			uu_uerror0(UA_DRAFTING,26);
			}
		if( ( count==0 ) )
			{
			goto line1;
			}
		else
			{
			goto loop;
			}
		}
	if( redo )
		{
		count = 1;
		status = ua_update_entity(previous_key, &ldim);
		if( ( status!=0 ) )
			{
			ua_create_entity(&ldim, &curr_key);
			}
		}
	else
		{
		ua_create_entity(&ldim,&curr_key);
		count =  count+1 ;
		}
	uc_display(&ldim);
	previous_key = ldim.key;
	goto loop;
	}

/*********************************************************************
**    E_FUNCTION     : int		ua_cre_polar(cpln_origin, xaxis, yaxis, zaxis, 
**											spt1, ept1, vec1, npt, count,
**											ext_offset_dist, dim_line_length, ldim)
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
int	ua_cre_polar(cpln_origin, xaxis, yaxis, zaxis, spt1, ept1, vec1,
						npt, count, ext_offset, ext_offset_dist,
						dim_line_length, ldim)
struct UA_generic_draft	*ldim;
int		count;
UU_REAL	ext_offset, ext_offset_dist[2], dim_line_length[2];
UM_coord	cpln_origin, xaxis, yaxis, zaxis, spt1, ept1, vec1, npt;
	{
	int		nint, use, num, rel_num, i, j, save_site, s_status, iangles, 
				ext_endpt;
	UU_REAL 	diff, temp_extent, off_set, dir_test, dis1, dis2,
				ext_arc_rad, dummy, ang1, ang2, ang12, rotangle, sign_ang,
				angles[4];
	UM_coord	dimvect, move_vect, box[4], extvect, del_vec, center_pt, tmp_vec,
				dl_pt, entity_origin, ext_loc, ext_arc_pt, box_cent;
	UU_LOGICAL	box_inside, inside_cone;
	UU_LOGICAL um_ptinseg();
	UU_REAL ua_dir_angle(), um_mag(), um_dcccc(), ua_arrowang(),
				um_angle2p(), um_dcccc();

	uu_denter(UU_STRC,(us,"ua_cre_polar"));

	um_vctovc(ldim->asso_blk[2].location, entity_origin);

	/* find closest endpoint of polar axis to entity point */
	dis1 = um_dcccc(entity_origin, spt1);
	dis2 = um_dcccc(entity_origin, ept1);
	if(dis1 < dis2)
		{
		um_vctovc(spt1, ext_loc);
		ext_endpt = 0;
		}
	else
		{
		um_vctovc(ept1, ext_loc);
		ext_endpt = 1;
		}
	um_vcmnvc(ext_loc, npt, extvect);
	um_unitvc(extvect, extvect);

	/* compute linear dimension value */
	um_vcmnvc(ldim->asso_blk[2].location, npt, dimvect);
	off_set = um_mag(dimvect);
	um_unitvc(dimvect, dimvect);
	ldim->dim_value = ua_dim_value(shortest, two_dim, dimvect,
								ldim->asso_blk[2].location, npt);
	if( ldim->dim_value < 1.0e-004 )
		{
		uu_uerror0(UA_DRAFTING,16);
		uu_dexit;
		return(0);
		}

	/* set linear text value */
	if(  ldim->txt_entry == UA_SYS_TEXT )
		{
		ua_set_dim_text(ldim);
		}

	/* position text box */
	um_vctmsc(dimvect, 0.5*off_set, ldim->dim_origin);
	um_vcplvc(npt, ldim->dim_origin, ldim->dim_origin);
	um_vctovc(ldim->dim_origin, center_pt);
	ua_box_site(ldim, box, dl_pt);
	ua_box_frame(ldim ,box);
	um_vcmnvc(center_pt, dl_pt, move_vect);
	um_vcplvc(ldim->dim_origin, move_vect, ldim->dim_origin);
	um_vcplvc(dl_pt, move_vect, dl_pt);
	for(i=0;i<4;i++)
		{
		um_vcplvc(box[i], move_vect, box[i]);
		}

	/* set up dimension line blocks */
	ldim->line_blk_use =  1;
	ldim->line_blk[0].subtype = dim_line;
	ldim->line_blk[0].line.line_font = UA_dim_line_font;
	ldim->line_blk[0].line.line_density = UA_dim_line_dens;
	ldim->line_blk[0].line.color = UA_dim_line_color;
	ldim->line_blk[0].num_pts = 4;
	um_vctovc(npt,ldim->line_blk[0].line_seg[0]);
	um_vctovc(ldim->asso_blk[2].location, ldim->line_blk[0].line_seg[1]);
	um_vctovc(npt,ldim->line_blk[0].line_seg[3]);
	um_vctovc(ldim->asso_blk[2].location, ldim->line_blk[0].line_seg[2]);

	/* trim dimension lines against text box */
	ua_trim_line(box, ldim->line_blk[0].line_seg[0],
							ldim->line_blk[0].line_seg[1], tmp_vec);
	um_vctovc(tmp_vec, ldim->line_blk[0].line_seg[1]);
	ua_trim_line(box, ldim->line_blk[0].line_seg[2],
							ldim->line_blk[0].line_seg[3], tmp_vec);
	um_vctovc(tmp_vec, ldim->line_blk[0].line_seg[3]);

	/* set up arrowhead blk */
	ldim->arrow_blk_use = 3;
	um_vctovc(ldim->asso_blk[2].location, ldim->arrow_blk[0].location);
	ldim->arrow_blk[0].aangle = ua_dir_angle(zaxis,xaxis,dimvect);

	/* compute radius of arc dimensiom line */
	um_vctmsc(dimvect, (ldim->gap_to_geo + ext_offset), tmp_vec);
	um_vcplvc(tmp_vec, entity_origin, ext_arc_pt);
	ext_arc_rad = um_dcccc(npt, ext_arc_pt);
	if(ext_arc_rad < dim_line_length[ext_endpt]) 
		{
		ext_arc_rad = dim_line_length[ext_endpt] + ext_offset;
		}
	else
		{
		dim_line_length[ext_endpt] = ext_arc_rad;
		}

	/* add arc dimension arrowheads */
	um_vctmsc(dimvect, ext_arc_rad, tmp_vec);
	um_vcplvc(npt, tmp_vec, ext_arc_pt);
	um_vctovc(ext_arc_pt, ldim->arrow_blk[1].location);
	um_vctmsc(extvect, ext_arc_rad, tmp_vec);
	um_vcplvc(npt, tmp_vec, ldim->arrow_blk[2].location);
	ldim->arrow_blk[1].aangle = ua_arrowang(dimvect, ext_arc_rad,
														ldim->arrow_size, 1);
	ldim->arrow_blk[2].aangle = ua_arrowang(extvect, ext_arc_rad,
														ldim->arrow_size, 0);

	/* set up extension line blk */
	if( ( ldim->ext_line_sup != UA_SUPPRESS_BOTH ) )
		{
		ldim->line_blk_use = ldim->line_blk_use + 1;
		i = ldim->line_blk_use - 1;
		ldim->line_blk[i].num_pts = 0;
		ldim->line_blk[i].subtype = ext_line;
		ldim->line_blk[i].line.line_font = UA_ext_line_font;
		ldim->line_blk[i].line.line_density = UA_ext_line_dens;
		ldim->line_blk[i].line.color = UA_ext_line_color;

		/* check for extension line on or off */
		if( ( ldim->ext_line_sup != UA_SUPPRESS_FIRST ) )
			{
			ldim->line_blk[i].num_pts = 2;
			um_vctmsc(dimvect, ldim->gap_to_geo, tmp_vec);
			um_vcplvc(tmp_vec, entity_origin, ldim->line_blk[i].line_seg[0]);
			um_vctmsc(dimvect, (ldim->ext_past_line + ext_arc_rad), tmp_vec);
			um_vcplvc(tmp_vec, npt, ldim->line_blk[i].line_seg[1]);
			um_vctmsc(dimvect, ext_offset, tmp_vec);
			um_vcplvc(tmp_vec, ldim->line_blk[i].line_seg[0], ext_arc_pt);
			}
		if( ( ldim->ext_line_sup != UA_SUPPRESS_SECOND ) )
			{
			/* first check if extension line required */
			um_vctmsc(extvect, ext_arc_rad, tmp_vec);
			um_vcplvc(tmp_vec, npt, del_vec);
			if(!um_ptinseg(npt, del_vec, ext_loc))
				{
				ldim->line_blk[i].num_pts = ldim->line_blk[i].num_pts + 2;
				um_vctmsc(extvect, ext_offset_dist[ext_endpt], tmp_vec);
				um_vcplvc(tmp_vec, ext_loc, ldim->line_blk[i].line_seg[2]);
				um_vctmsc(extvect, (ldim->ext_past_line + ext_arc_rad), tmp_vec);
				um_vcplvc(tmp_vec, npt, ldim->line_blk[i].line_seg[3]);
				um_vcmnvc(ldim->line_blk[i].line_seg[3], ext_loc, tmp_vec);
				ext_offset_dist[ext_endpt] = um_mag(tmp_vec);
				}
			}
		}

	/* create arc dimension line */
	ldim->arc_blk_use = 1;
	ldim->arc_blk[0].subtype = dim_arc;
	ldim->arc_blk[0].arc.line_font = UA_dim_line_font;
	ldim->arc_blk[0].arc.line_density = UA_dim_line_dens;
	ldim->arc_blk[0].arc.color = UA_dim_line_color;
	ldim->arc_blk[0].radius = ext_arc_rad;
	um_vctovc(npt, ldim->arc_blk[0].center_pt);

	/* calculate angles for dimension arcs */
	ldim->arc_blk[0].num_pts = 2;
	ang1 = um_angle2p(xaxis, dimvect, zaxis);
	ang2 = um_angle2p(xaxis, extvect, zaxis);
	ang12 = um_angle2p(extvect, dimvect, zaxis);
	sign_ang = 1.0;
	if(ang12 > UA_HALFPI)
		{
		ang12 = UA_TWOPI -  ang12;
		sign_ang = -1.0;
		}
	ldim->dim2_value = ang12;
	if(ang1 > ang2)
		{
		ldim->arc_blk[0].angles[0] = ang2;
		ldim->arc_blk[0].angles[1] = ang1;
		dummy = ang2;
		ang2 = ang1;
		ang1 = dummy;
		ldim->arrow_blk[1].aangle = ua_arrowang(dimvect, ext_arc_rad,
														ldim->arrow_size, 1);
		ldim->arrow_blk[2].aangle = ua_arrowang(extvect, ext_arc_rad,
														ldim->arrow_size, 0);
		}
	else
		{
		ldim->arc_blk[0].angles[0] = ang1;
		ldim->arc_blk[0].angles[1] = ang2;
		ldim->arrow_blk[1].aangle = ua_arrowang(dimvect, ext_arc_rad,
														ldim->arrow_size, 0);
		ldim->arrow_blk[2].aangle = ua_arrowang(extvect, ext_arc_rad,
														ldim->arrow_size, 1);
		}

	/* create angular text block */
	ua_init_entity(UA_ANGULAR_DIM, UA_ANGULAR_INTERIOR_DIM, &ldim_test);
	ldim_test.txt_just = UA_LEFT;
	ldim_test.entity_site = UA_MIDDLE_CENTER;
	ldim_test.upper_tol = UA_ang_up_tol_val;
	ldim_test.lower_tol = UA_ang_lo_tol_val;
	ldim_test.dim_value = ldim->dim2_value;

	/* calculate initial text origin */
	um_vctmsc(extvect, ext_arc_rad, tmp_vec);
	um_vcplvc(tmp_vec, npt, del_vec);
	rotangle = sign_ang * ang12/2.0;
	um_vctovc(del_vec, box_cent);
	ua_rotatept(box_cent, zaxis, npt, rotangle);
	um_vctovc(box_cent, ldim_test.dim_origin);

	/* create angular text string */
	ua_set_dim_text(&ldim_test);

	/* construct angular text box */
	ua_box_site(&ldim_test, box, dl_pt);
	ua_box_frame(&ldim_test, box);

	/* center text between polar origin and dimension line */
	um_vcmnvc(box_cent, dl_pt, del_vec);
	um_vcplvc(ldim_test.dim_origin, del_vec, ldim_test.dim_origin);
	um_vcplvc(dl_pt, del_vec, dl_pt);
	for(i=0;i<4;i++)
		{
		um_vcplvc(box[i], del_vec, box[i]);
		}

	/* intersect dimension line with text box */
	ua_iboxarc(box, npt, zaxis, ext_arc_rad, ang1, ang2, &iangles, angles);
	if(iangles > 0)
		{
		for(i=0;i<iangles;i++)
			{
			ldim->arc_blk[0].angles[i] = angles[i];
			}
		ldim->arc_blk[0].num_pts = iangles;
		}

	/* move angular text strings relative to linear dimension origin */
	um_vcmnvc(ldim->dim_origin, ldim_test.dim_origin, del_vec);
	j = ldim->txt_blk_use;
	for(i=0;i<ldim_test.txt_blk_use;i++)
		{
		ua_copy_txt_blk(ldim, &ldim_test, j, i);
		ldim->txt_blk[j].subtype = polar_arc_data;
		um_vcmnvc(ldim_test.txt_blk[i].origin, del_vec,ldim->txt_blk[j].origin);
		j++;
		}
	ldim->txt_blk_use = j;
	 
	uu_dexit;
	return(1);
	}
/*********************************************************************
**    E_FUNCTION     : ua_copy_txt_blk(out, in, iout, iin)
**			Copy a text block drafting modal variables.
**    PARAMETERS   
**       INPUT  : 
**				out,in:			Generic draft records
**				iout,iin:		block numbers 
**
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_copy_txt_blk(out, in, iout, iin)
struct UA_generic_draft	*out;
struct UA_generic_draft	*in;
int		iout;
int		iin;
	{

	uu_denter(UU_STRC,(us,"ua_copy_txt_blk out = %d in = %d", iout, iin));

	out->txt_blk[iout].text.line_font = in->txt_blk[iin].text.line_font;
	out->txt_blk[iout].text.line_density =in->txt_blk[iin].text.line_density;
	out->txt_blk[iout].text.color = in->txt_blk[iin].text.color;
	out->txt_blk[iout].txt_just = in->txt_blk[iin].txt_just;
	strcpy(out->txt_blk[iout].fontname,in->txt_blk[iin].fontname);
	out->txt_blk[iout].slant = in->txt_blk[iin].slant;
	out->txt_blk[iout].tangle = in->txt_blk[iin].tangle;
	out->txt_blk[iout].txt_size = in->txt_blk[iin].txt_size;
	out->txt_blk[iout].sub_super = in->txt_blk[iin].sub_super;
	out->txt_blk[iout].char_expansion = in->txt_blk[iin].char_expansion;
	out->txt_blk[iout].char_space = in->txt_blk[iin].char_space;
	out->txt_blk[iout].line_spacing = in->txt_blk[iin].line_spacing;
	out->txt_blk[iout].char_cnt = in->txt_blk[iin].char_cnt;
	strcpy(out->txt_blk[iout].tstring,in->txt_blk[iin].tstring);
	out->txt_blk[iout].dx = in->txt_blk[iin].dx;
	out->txt_blk[iout].dy = in->txt_blk[iin].dy;
	um_vctovc(in->txt_blk[iin].origin, out->txt_blk[iout].origin);
	uu_dexit;
	}
