/*********************************************************************
**    NAME         : arrcrv.c
**       CONTAINS:
**					ua_arrow
**					ua_arr_cre
**    			ua_regen_arrow
**					
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       aarrcrv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:30
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aarrcrv.c 25.1 04/29/15 15:05:30 single"};
#else
static char uu_sccsident[]={"@(#) aarrcrv.c 25.1 04/29/15 15:05:30 double"};
#endif

#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"
#include "adraft.h"
#include "adrfdefs.h"
#include "adrfcom.h"
#include "mdcoord.h"

/*********************************************************************
**    E_FUNCTION     : ua_arrow()
**       Main function to handle arrowhead at end of a curve.
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_arrow()
	{
	struct UA_PLOCREC	plocrec;
	struct UA_generic_draft	rdim;
	int		relation, ablock, num, curr_key, i, j, d_stat, k, l, m, n,
				status, ok, s_status, subtype;
	UM_coord	v1, v2, v3, pt1_cc, c_spt, pt1, pt2, pt2_cc, center, origin,
				normal, c_ept, xaxis, yaxis, zaxis, tmp1;
	UU_REAL	sine, ang, cosine, radius, ang1, ang2, dis1, dis2, dang, s_arc,
				dummy, rang, uu;

	uu_denter(UU_STRC,(us,"ua_arrow()"));

	/* set local defaults */
	subtype = 1;
	curr_key = -1;
	us_init_autility();
	ua_reset_drafting_view();
	ua_dump_set(0xffff);

	/* loop ever entities picked */
main_loop:
	ua_init_entity(UA_SECT_ARROW,subtype,&(rdim));
	ua_getcpln(&(rdim),origin,xaxis,yaxis,zaxis);
	ablock = ( rdim.asso_blk_use+1 );
	rdim.asso_blk_use = ablock;

	s_status = ua_select_ent_subf(109,&(rdim),ablock,&(plocrec));
	if( ( s_status!=UA_OPCOMPLETE ) )
		{
		uu_dexit;
		return;
		}

	/* check entity picked */
	curr_key = rdim.asso_blk[ablock-1].key;
	ok = uc_draft_type(curr_key,&(relation));
/* NCL */
	if (ok != UU_SUCCESS) relation=0;
	switch( relation )
		{
		case UA_DRAFT_LINE: 		/* a draftable line */
			{
			uc_draft_endpts(2,&(plocrec.ppick),&(plocrec.ndcloc),
															&(relation),pt1,pt2);
			um_nptpln(pt1,origin,zaxis,pt1_cc);
			um_nptpln(pt2,origin,zaxis,pt2_cc);
			um_vcmnvc(pt1_cc,pt2_cc,v1);
			dang = ua_dir_angle(zaxis,xaxis,v1);
			um_vctovc(pt1, rdim.asso_blk[0].location);
			ua_arr_cre(&(rdim),pt1,dang);
			}
			break;
		case UA_DRAFT_ARC:			/* a draftable arc */
			{
			uc_draft_arc(curr_key,center,&(radius),&(dang),normal,c_spt,
																c_ept,&(dummy));
			s_arc = rdim.arrow_size;
			ang = ( s_arc/radius );
			if( um_cceqcc(c_spt,c_ept) )
				{
				um_vctovc(c_spt,pt1);
				uc_near_on_entity(curr_key,&(plocrec.ndcloc),pt2);
				ua_para_on_curve(curr_key, pt2, &uu);
				rdim.asso_blk[0].location[0] = uu;
				um_vcmnvc(c_spt,center,v1);
				um_vcmnvc(pt2,center,v2);
				rang = ua_dir_angle(normal,v1,v2);
				if( ( rang< UA_PI ) )
					{
					dis1 = 0.0;
					dis2 = 2.0;
					}
				else
					{
					ang = ( dang-ang );
					dis1 = 2.0;
					dis2 = 0.0;
					}
				}
			else
				{
				uc_draft_endpts(2,&(plocrec.ppick),&(plocrec.ndcloc),&(
				relation),pt1,pt2);
				um_vctovc(pt1, rdim.asso_blk[0].location);
				dis1 = um_dcccc(pt1,c_spt);
				dis2 = um_dcccc(pt1,c_ept);
				if( ( dis2<dis1 ) )
					{
					ang = ( dang-ang );
					}
				}
			um_vcmnvc(c_spt,center,v1);
			um_cross(normal,v1,v2);
			cosine = cos(ang);
			sine = sin(ang);
			pt2[0] = ( ( ( sine*v2[0] )+( cosine*v1[0] ) )+center[0] );
			pt2[1] = ( ( ( sine*v2[1] )+( cosine*v1[1] ) )+center[1] );
			pt2[2] = ( ( ( sine*v2[2] )+( cosine*v1[2] ) )+center[2] );
			um_vcmnvc(pt2,center,v3);
			um_vcmnvc(pt1,center,v2);
			if( ( dis2<dis1 ) )
				{
				um_cross(normal,v3,v1);
				um_cross(normal,v2,v3);
				}
			else
				{
				um_cross(v3,normal,v1);
				um_cross(v2,normal,v3);
				}
			um_vctmsc(v1,dang,tmp1);
			ang1 = ua_dir_angle(zaxis,xaxis,tmp1);
			um_vctmsc(v3,dang,tmp1);
			ang2 = ua_dir_angle(zaxis,xaxis,tmp1);
			if( ( fabs(( ang1-ang2 ))>1.0 ) )
				ang = ( ang1+ang2 );
			else
				ang = ( ( ang1+ang2 )/2.0);
			ua_arr_cre(&(rdim),pt1,ang);
			}
			break;
	default:
			uu_uerror0(UA_DRAFTING,28);
			goto main_loop;
		}
	ua_create_entity(&(rdim),&(curr_key));
	uc_display(&(rdim));
	goto main_loop;
	}

/*********************************************************************
**    E_FUNCTION     : ua_arr_cre(rdim, pt, dang)
**       Create an arrowhead at the end of an entity.
**    PARAMETERS   
**       INPUT  : 
**				entity							SAL entity record
**				pt									arrow start location
**				dang								arrowhead angle
**       OUTPUT :  
**				entity							updated entity record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_arr_cre(rdim, pt, dang)
struct UA_generic_draft	(*rdim);
UM_coord	pt;
UU_REAL	dang;
	{
	UU_REAL scal_factor;
	int		m;

	uu_denter(UU_STRC,(us, "ua_arr_cre(rdim=%s, pt=<%g,%g,%g>, dang=%g)",
		"...", pt[0],pt[1],pt[2], dang));

	um_get_drwscale(&(scal_factor));
	m = ( rdim->arrow_blk_use+1 );
	rdim->arrow_blk_use = m;
	rdim->arrow_blk[m-1].arrow_type = UA_arrow_symbol;
	rdim->arrow_blk[m-1].arrow.line_density = UA_arrow_dens;
	rdim->arrow_blk[m-1].arrow.color = UA_arrow_color;
	uu_dprint(UU_STRC,(us,"arrow color = %d",UA_arrow_color));
	rdim->arrow_blk[m-1].size = ( UA_arrow_size/scal_factor);
	um_vctovc(pt,rdim->arrow_blk[m-1].location);
	rdim->arrow_blk[m-1].aangle = dang;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_regen_arrow()
**       Main function to handle arrowhead at end of a curve.
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_regen_arrow( rdim)
	struct UA_generic_draft	*rdim;
	{
	int		relation, ablock, num, curr_key, i, j, d_stat, k, l, m, n,
				status, ok, s_status, subtype;
	UM_coord	v1, v2, v3, pt1_cc, c_spt, pt1, pt2, pt2_cc, center, origin,
				normal, c_ept, xaxis, yaxis, zaxis, tmp1;
	UU_REAL	sine, ang, cosine, radius, ang1, ang2, dis1, dis2, dang, s_arc,
				dummy, rang;

	uu_denter(UU_STRC,(us,"ua_regen_arrow()"));

	ua_getcpln(rdim,origin,xaxis,yaxis,zaxis);

	curr_key = rdim->asso_blk[0].key;
	ok = uc_draft_type(curr_key,&relation);
	switch( relation )
		{
		case UA_DRAFT_LINE: 		/* a draftable line */
			ua_regen_endpts(rdim, 0, pt1, pt2);
			um_vctovc(pt1, rdim->asso_blk[0].location);
			um_nptpln(pt1,origin,zaxis,pt1_cc);
			um_nptpln(pt2,origin,zaxis,pt2_cc);
			um_vcmnvc(pt1_cc,pt2_cc,v1);
			dang = ua_dir_angle(zaxis,xaxis,v1);
			ua_arr_cre(rdim, pt1, dang);
			break;
		case UA_DRAFT_ARC:			/* a draftable arc */
			{
			uc_draft_arc(curr_key,center,&radius,&dang,normal,c_spt,
																c_ept,&dummy);
			s_arc = rdim->arrow_size;
			ang = ( s_arc/radius );
			if( um_cceqcc(c_spt,c_ept) )
				{
				um_vctovc(c_spt,pt1);
				ua_pt_on_curve(curr_key, rdim->asso_blk[0].location[0], pt2);
				um_vcmnvc(c_spt,center,v1);
				um_vcmnvc(pt2,center,v2);
				rang = ua_dir_angle(normal,v1,v2);
				if( ( rang< UA_PI ) )
					{
					dis1 = 0.0;
					dis2 = 2.0;
					}
				else
					{
					ang = ( dang-ang );
					dis1 = 2.0;
					dis2 = 0.0;
					}
				}
			else
				{
				ua_regen_endpts(rdim, 0, pt1, pt2);
				um_vctovc(pt1, rdim->asso_blk[0].location);
				dis1 = um_dcccc(pt1,c_spt);
				dis2 = um_dcccc(pt1,c_ept);
				if( ( dis2<dis1 ) )
					{
					ang = ( dang-ang );
					}
				}
			um_vcmnvc(c_spt,center,v1);
			um_cross(normal,v1,v2);
			cosine = cos(ang);
			sine = sin(ang);
			pt2[0] = ( ( ( sine*v2[0] )+( cosine*v1[0] ) )+center[0] );
			pt2[1] = ( ( ( sine*v2[1] )+( cosine*v1[1] ) )+center[1] );
			pt2[2] = ( ( ( sine*v2[2] )+( cosine*v1[2] ) )+center[2] );
			um_vcmnvc(pt2,center,v3);
			um_vcmnvc(pt1,center,v2);
			if( ( dis2<dis1 ) )
				{
				um_cross(normal,v3,v1);
				um_cross(normal,v2,v3);
				}
			else
				{
				um_cross(v3,normal,v1);
				um_cross(v2,normal,v3);
				}
			um_vctmsc(v1,dang,tmp1);
			ang1 = ua_dir_angle(zaxis,xaxis,tmp1);
			um_vctmsc(v3,dang,tmp1);
			ang2 = ua_dir_angle(zaxis,xaxis,tmp1);
			if( ( fabs(( ang1-ang2 ))>1.0 ) )
				ang = ( ang1+ang2 );
			else
				ang = ( ( ang1+ang2 )/2.0);
			ua_arr_cre(rdim,pt1,ang);
			}
			break;
	default:
			uu_uerror0(UA_DRAFTING,28);
		}
	uu_dexit;
	return(0);
	}
