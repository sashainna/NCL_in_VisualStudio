/*********************************************************************
**    NAME         : aregen.c
**       CONTAINS:
**       ua_regen_drafting
**			ua_regen_location
**			ua_loc_old_text_org
**			ua_loc_new_text_org
**			ua_reloc_text_arc
**			ua_reloc_text_angular
**    	ua_get_polar_data
**			ua_user_regen
**			ua_regen_save
**    	ua_regen_restore
**    	ua_asso_check
**    	ua_regen_update_record
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aregen.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:38
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aregen.c 4.3 8/10/89 08:02:34 single"};
#else
static char uu_sccsident[]={"@(#) aregen.c 4.3 8/10/89 08:02:34 double"};
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

#define UA_ASSO_ENDPT_DRAFT 10

struct UA_old_loc{
	UM_coord del_vec;
	UU_REAL  del_length;
	UM_coord vec1;
	UU_REAL  length1;
	UM_coord vec2;
	UU_REAL  length2;};

extern int UA_angular_subtype;
extern int UD_dimensions[UD_NMENTWD];
extern int UD_text[UD_NMENTWD];
extern int UD_editable[UD_NMENTWD];

static  int save_dim_line_font;
static  int save_dim_line_color;
static  int save_ext_line_font;
static  int save_ext_line_color;
static  int save_arrow_symbol;
static  int save_arrow_color;
static  UU_REAL 	save_dim_line_dens;
static  UU_REAL 	save_ext_line_dens;
static  UU_REAL 	save_arrow_dens;

/*********************************************************************
**    E_FUNCTION     : ua_regen_drafting(edrf)
**		Regenerate and display a drafting entity
**    PARAMETERS   
**       INPUT  : 
**          edrf					drafting entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_regen_drafting( edrf)
	struct UA_generic_draft	*edrf;
	{
	int i, j, k, n, status, relation, prev_txt_blk_use, app_txt_blk, count, text_fit_flag, text_rot_flag;
	UU_KEY_ID  ent_key;
	struct UC_entitydatabag e;	
	UM_transf tfmat;			
	UM_coord	base_pt, cpln_origin, xaxis, base_vec, yaxis, zaxis, pt1, pt2,
						tmp_vec, del_vec, spt1, ept1, spt2, ept2, ext_vec, vec1,
						npt;
	UU_REAL off_set, ext_offset_dist1, ext_offset_dist[2],  ext_offset,
				dim_line_length[2];
	UU_LOGICAL base_flg, polar_status;
	UU_LOGICAL ua_get_polar_data();
	struct UA_old_loc text_loc;

	uu_denter(UU_STRC,(us,"ua_regen_drafting()"));

	text_fit_flag = 1;
	text_rot_flag = 1;
	/* check data */
	ua_dump_set(0xffff);
	if( ( edrf->rel_num != UA_LINEAR_DIM ) )
		{
		uu_uerror0(UA_DRAFTING,22);
		goto procexit;
		}

	/* check associativity */
	if(ua_asso_check(edrf) == UU_FAILURE)
		{
		uu_uerror0(UA_DRAFTING,43);
		goto procexit;
		}

	/* get entity construction plane */
	ua_getcpln(edrf,cpln_origin,xaxis,yaxis,zaxis);

	/* switch on drafting entity sub-type */
	switch( edrf->etype )
		{
		case UA_LINEAR_DIM:  /* standard linear dimension  */
			off_set = 0.0;

			/* save data for old text origin */
			ua_loc_old_text_org(edrf, &text_loc);

			/* regenerate asso_blk data for new entity data */
			if((status = ua_regen_location(edrf)) != 0 ) goto procexit;

			/* compute base-pt and base-vector from dim-line */
			for(i=0;i<edrf->line_blk_use;i++)
				{
				if( edrf->line_blk[i].subtype == dim_line )
					{
					um_vctovc(edrf->line_blk[i].line_seg[0], base_pt);
					um_vcmnvc(edrf->line_blk[i].line_seg[1], 
									edrf->line_blk[i].line_seg[0], tmp_vec);
					um_unitvc(tmp_vec, base_vec);
					break;
					}
				}
			
			/* recompute new text origin */
			ua_loc_new_text_org(edrf,&text_loc, xaxis, yaxis);

			/* initialize line/arc/arrow-head/text blk's */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			/* recreate linear dimension data */
			status = ua_cre_lin_dim(cpln_origin,xaxis,yaxis,zaxis,
							base_pt,base_vec,&off_set,edrf,&text_fit_flag,&text_rot_flag);
			if(  status!=1  )
				{
				goto procexit;
				}
			break;

		case UA_CHAIN_DIM:    /* chained linear dimension */
			off_set = 0.0;

			/* save data for old text origin */
			ua_loc_old_text_org(edrf,&text_loc);

			/* regenerate asso_blk data for new entity data */
			if((status = ua_regen_location(edrf)) !=0 ) goto procexit;

			/* re-compute base-pt and base-vector */
			um_vctovc(edrf->asso_blk[3].location,base_pt);
			um_vctovc(edrf->asso_blk[2].location,base_vec);
			base_flg = UU_FALSE;

			/* compute new text origin */
			ua_loc_new_text_org(edrf,&text_loc, xaxis, yaxis);

			/* initialize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			/* create data */
			status = ua_cre_chain(cpln_origin,xaxis,yaxis,zaxis,&
								base_flg,base_pt,base_vec,edrf);
			if( ( status!=1 ) )
				{
				goto procexit;
				}
			break;

		case UA_BASELN_DIM:    /* base-line linear dimension */
			off_set = 0.0;

			/* save data for old text origin */
			ua_loc_old_text_org(edrf,&text_loc);

			/* regenerate asso_blk data for new entity data */
			if((status = ua_regen_location(edrf)) !=0 ) goto procexit;

			/* re-compute base-pt and base-vector */
			um_vctovc(edrf->asso_blk[3].location,base_pt);
			um_vctovc(edrf->asso_blk[2].location,base_vec);
			um_vctovc(edrf->asso_blk[4].location,ext_vec);
			base_flg = UU_FALSE;
			ext_offset_dist1 = edrf->gap_to_geo;
			count = 0;

			/* compute new text origin */
			ua_loc_new_text_org(edrf,&text_loc, xaxis, yaxis);

			/* initialize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			/* create data */
			status = ua_cre_baseln(cpln_origin,xaxis,yaxis,zaxis,&
								base_flg,base_pt,base_vec, count, &ext_offset_dist1,
								ext_vec, edrf);
			if( ( status!=1 ) )
				{
				goto procexit;
				}
			break;

		case UA_ARC_LEN_DIM:         /* arc length dimension */

			/* re-compute text origin */
			ua_reloc_text_arc(edrf);

			/* initilize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			/* create new data */
			ua_arclen_create(edrf);
			break;

		case UA_ANGULAR_DIM:			  /* angular dimension */

			/* re-compute text origin */
			ua_reloc_text_angular(edrf, spt1, ept1, spt2, ept2);

			/* initilize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			/* create new data */
			UA_angular_subtype = edrf->subtype;
			ua_angular_generate(edrf, spt1, ept1, spt2, ept2);
			break;

		case UA_RAD_CEN_DIM:			  /* radius dimension */

			/* re-compute text origin */
			ua_reloc_text_arc(edrf);

			/* initilize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			/* create new data */
			switch( edrf->subtype )
				{
				case 4:
				case 5:
				case 6:
					ua_radius_regen(edrf);
					break;
				case 10:
					ua_diam_regen(edrf);
					break;
				}
			break;

		case UA_DIA_IN_DIM:         /* diameter dimension */

			/* re-compute text origin */
			ua_reloc_text_arc(edrf);

			/* initilize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			/* create new data */
			switch( edrf->subtype )
				{
				case 9:
				case 3:
				case 2:
				case 1:
					ua_diam_regen(edrf);
					break;
				case 7:
					ua_dia_shaft_regen(edrf);
					break;
				case 8:
					ua_dia_cyln_regen(edrf);
					break;
				}
			break;

		case UA_POLAR_DIM:    /* polar dimension */
			ext_offset_dist[0] = ext_offset_dist[1] = edrf->gap_to_geo;
			dim_line_length[0] = dim_line_length[1] = 0;
			count = 0;

			/* regenerate data */
			polar_status = ua_get_polar_data(cpln_origin, xaxis, yaxis, zaxis,
														spt1, ept1, vec1, npt, &ext_offset,
														edrf);
			if( polar_status )
				{
				/* initialize blocks */
				ua_regen_save(edrf);
				ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

				/* create polar coordinate dimension */
				status = ua_cre_polar(cpln_origin, xaxis, yaxis, zaxis, spt1,
									ept1, vec1, npt, count, ext_offset,
									ext_offset_dist, dim_line_length, edrf);
				if( ( status != 1 ) ) goto procexit;
				}
			else goto procexit;
			break;

		case UA_CENTERLINE:
			/* initilize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			status = ua_center_regen(edrf);
			if(status != 0) goto procexit;
			break;

		case UA_CROSSHATCH:
			/* initilize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			status = ua_xhatch_regen(edrf);
			if(status != 0) goto procexit;
			break;

		case UA_LABEL_DIM:
			/* initilize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			status = ua_label_regen(edrf);
			if(status != 0) goto procexit;
			break;

		case UA_BALLOON_DIM:
			/* initilize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			status = ua_balloon_regen(edrf);
			if(status != 0) goto procexit;
			break;

		case UA_SECT_ARROW:
			/* initilize blocks */
			ua_regen_save(edrf);
			ua_edit_init(edrf, &prev_txt_blk_use, &app_txt_blk);

			status = ua_regen_arrow(edrf);
			if(status != 0) goto procexit;
			break;

		case UA_FANDP_TOL:
			break;

		default:
			goto procexit;
		}

	/* regeneration calculation complete - update UNIBASE and display */
	ua_regen_update_record(edrf);

procexit:
	ua_regen_restore();
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : ua_regen_location(e)
**		Regenerate association block data for a LINEAR dimension
**    PARAMETERS   
**       INPUT  : 
**          e					drafting entity 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_regen_location( edrf)
	struct UA_generic_draft	*edrf;
	{
	int 			i, j, k, ablk1, ablk2;
	UU_KEY_ID   ent_key;
	struct 		UC_entitydatabag e;	
	UM_transf 	tfmat;			
	UM_coord	   cpln_origin,  xaxis, yaxis, zaxis, pt1, pt2, tmp_vec;
	int		   rel_num1, rel_num2, nint, closest, asso_type1, asso_type2;
	UM_coord	   near_pt, radius, normal[2], pt[2], unit_vec,
			       				ipt[2], c_spt, far_pt, sp_vec, c_ept;
	UU_REAL     dang, dummy;

	uu_denter(UU_STRC,(us,"ua_regen_location()"));

	/* get entity construct plane */
	ua_getcpln(edrf,cpln_origin,xaxis,yaxis,zaxis);

	/* get associativity type for each entity */
	ablk1 = 0;
	ablk2 = 1;
	asso_type1 = edrf->asso_blk[ablk1].asso_type;
	asso_type2 = edrf->asso_blk[ablk2].asso_type;

	/* switch on entity sub-type */
	switch( edrf->subtype )
		{
		case UA_PERP_DIM:
			{

			/* check on modifier type for the second entity picked */
			if( ( edrf->asso_blk[ablk2].modifier==UA_ASSO_ENDPT ) )
				{
				ua_regen_endpts(edrf,ablk2,pt[1],far_pt);
				}
			else
				{
				if( ( asso_type2==UA_DRAFT_ARC ) )
					{
					uc_draft_arc(edrf->asso_blk[ablk2].key,pt[1],&radius[1],
								&dang,normal[1],c_spt,c_ept,&dummy);
					}
				else
					{
					uc_draft_conic(edrf->asso_blk[ablk2].key,pt[1],c_spt,c_ept);
					}
				}
			ua_regen_endpts(edrf,ablk1,near_pt,far_pt);
			um_vcmnvc(far_pt,near_pt,tmp_vec);
			um_unitvc(tmp_vec,unit_vec);
			um_nptln(pt[1],near_pt,unit_vec,pt[0]);
			if(  edrf->asso_blk[ablk2].modifier==UA_ASSO_TANGPT  )
				{
				um_vcmnvc(pt[1],pt[0],tmp_vec);
				um_unitvc(tmp_vec,unit_vec);
				if( ( asso_type2==UA_DRAFT_ARC ) )
					{
					um_ilncir(pt[1],unit_vec,pt[1],normal[1],radius[1], &nint,ipt);
					if( ( nint==0 ) )
						{
						uu_dexit;
						return(1);
						}
					}
				else
					{
					um_cross(zaxis,unit_vec,tmp_vec);
					um_unitvc(tmp_vec,sp_vec);
					ua_tan_to_conic(sp_vec,edrf->asso_blk[ablk2].key,
											&nint,ipt);
					if( nint==0 )
						{
						uu_dexit;
						return(1);
						}
					}
				closest = um_closept(ipt[0],ipt[1],edrf->asso_blk[ablk2].location);
				um_vctovc(ipt[closest], pt[1]);
				}
			}
			break;
		case UA_THICK_DIM:
			{
			um_vctovc(edrf->asso_blk[ablk1].location, pt[0]);
			ua_regen_endpts(edrf,ablk2,near_pt,far_pt);
			um_vcmnvc(far_pt,near_pt,tmp_vec);
			um_unitvc(tmp_vec,unit_vec);
			um_nptln(pt[0],near_pt,unit_vec,pt[1]);
			}
			break;
		default:
			{
			if(  edrf->asso_blk[ablk1].modifier==UA_ASSO_ENDPT  )
				{
				ua_regen_endpts(edrf,ablk1,pt[0],far_pt);
				}
			else if( edrf->asso_blk[ablk1].modifier==UA_ASSO_ENDPT_DRAFT  )
				{
				ua_regen_endpts(edrf,ablk1, pt[0],tmp_vec);
				}
			else
				{
				if(  asso_type1==UA_DRAFT_ARC  )
					{
					uc_draft_arc(edrf->asso_blk[ablk1].key,pt[0],&radius[0],&dang
										,normal[0],c_spt,c_ept,&dummy);
					}
				else
					{
					uc_draft_conic(edrf->asso_blk[ablk1].key,pt[0],c_spt,c_ept);
					}
				}
			if(  edrf->asso_blk[ablk2].modifier==UA_ASSO_ENDPT  )
				{
				ua_regen_endpts(edrf,ablk2,pt[1],far_pt);
				}
			else if( edrf->asso_blk[ablk2].modifier==UA_ASSO_ENDPT_DRAFT  )
				{
				ua_regen_endpts(edrf,ablk2, pt[1],tmp_vec);
				}
			else
				{
				if(  asso_type2==UA_DRAFT_ARC  )
					{
					uc_draft_arc(edrf->asso_blk[ablk2].key,pt[1],&radius[1],&dang
										,normal[1],c_spt,c_ept,&dummy);
					}
				else
					{
					uc_draft_conic(edrf->asso_blk[ablk2].key,pt[1],c_spt,c_ept);
					}
				}
			if(( edrf->asso_blk[ablk1].modifier==UA_ASSO_TANGPT )||
									(edrf->asso_blk[ablk2].modifier==UA_ASSO_TANGPT ))
				{
				if( edrf->subtype==UA_HORIZ_DIM  )
					{
					um_vctovc(xaxis,unit_vec);
					}
				else if( edrf->subtype==UA_VERT_DIM  )
					{
					um_vctovc(yaxis,unit_vec);
					}
				else
					{
					um_vcmnvc(pt[1],pt[0],tmp_vec);
					um_unitvc(tmp_vec,unit_vec);
					}
				if( edrf->asso_blk[ablk1].modifier==UA_ASSO_TANGPT  )
					{
					if( asso_type1==UA_DRAFT_ARC )
						{
						um_ilncir(pt[0],unit_vec,pt[0],normal[0],radius[0],
						&nint,ipt);
						if(  nint==0  )
							{
							uu_dexit;
							return(1);
							}
						}
					else
						{
						um_cross(zaxis,unit_vec,tmp_vec);
						um_unitvc(tmp_vec,sp_vec);
						ua_tan_to_conic(sp_vec,edrf->asso_blk[ablk1].key,&nint,ipt);
						if(  nint==0  )
							{
							uu_dexit;
							return(1);
							}
						}
					closest=um_closept(ipt[0],ipt[1],edrf->asso_blk[ablk1].location);
					um_vctovc(ipt[closest],pt[0]);
					}
				if( edrf->asso_blk[ablk2].modifier==UA_ASSO_TANGPT  )
					{
					if( asso_type2==UA_DRAFT_ARC )
						{
						um_ilncir(pt[1],unit_vec,pt[1],normal[1],radius[1],
						&nint,ipt);
						if(  nint==0  )
							{
							uu_dexit;
							return(1);
							}
						}
					else
						{
						um_cross(zaxis,unit_vec,tmp_vec);
						um_unitvc(tmp_vec,sp_vec);
						ua_tan_to_conic(sp_vec,edrf->asso_blk[ablk2].key,&nint,ipt);
						if(  nint==0  )
							{
							uu_dexit;
							return(1);
							}
						}
					closest=um_closept(ipt[0],ipt[1],edrf->asso_blk[ablk2].location);
					um_vctovc(ipt[closest],pt[1]);
					}
				}
			}
		}
	um_nptpln(pt[0],cpln_origin,zaxis,ipt[0]);
	um_nptpln(pt[1],cpln_origin,zaxis,ipt[1]);
	um_vctovc(ipt[0],edrf->asso_blk[ablk1].location);
	um_vctovc(ipt[1],edrf->asso_blk[ablk2].location);

procexit:
	uu_dexit;
	return(0);
	}

/*********************************************************************
**    E_FUNCTION     : ua_loc_old_text_org(e, text_loc)
**		Find location of original text
**    PARAMETERS   
**       INPUT  : 
**          e					drafting entity 
**       OUTPUT :  
**          text_loc       return structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_loc_old_text_org(edrf, text_loc)
	struct UA_generic_draft	*edrf;
	struct UA_old_loc *text_loc;
	{
	int i, j, k, num;
	UM_coord	del, pc, p3, int_pt;
	UU_REAL length, dist1, dist2, um_dcccc(), um_mag();

	uu_denter(UU_STRC,(us,"ua_loc_old_text_org()"));

	/* first compute off-set from center of geometric line to text origin */
	um_vcmnvc(edrf->asso_blk[1].location, edrf->asso_blk[0].location, del);
	length = um_mag(del);
	um_unitvc(del, p3);
	length = 0.5*length;
	um_vctmsc(p3,length,p3);
	um_vcplvc(edrf->asso_blk[0].location, p3, pc);

	/* save off-set vector in local structure */
	um_vcmnvc(edrf->dim_origin, pc, text_loc->del_vec);

	/* check if dealing with a HORIZ or VERT dimension */
	if(edrf->subtype == 1 || edrf->subtype == 2)
		{
		um_unitvc(text_loc->del_vec, del);
		um_vcmnvc(edrf->arrow_blk[1].location, edrf->arrow_blk[0].location, p3);
		um_unitvc(p3, p3);
		um_ilnln(pc, del, edrf->arrow_blk[0].location, p3, &num, int_pt);
		if(num > 0)
			{
			um_vcmnvc(pc, int_pt, p3);
			text_loc->del_length = um_mag(p3);
			}
		else
			{
			text_loc->del_length = 0.0;
			}
		dist1 = um_dcccc(edrf->asso_blk[0].location, edrf->arrow_blk[0].location);
		dist2 = um_dcccc(edrf->asso_blk[0].location, edrf->arrow_blk[1].location);
		if(dist1 < dist2)
			{
			text_loc->length1 = dist1;
			j = 1;
			um_vcmnvc(edrf->arrow_blk[0].location, edrf->asso_blk[0].location,
							text_loc->vec1);
			um_unitvc(text_loc->vec1, text_loc->vec1);
			}
		else
			{
			text_loc->length1 = dist2;
			j = 0;
			um_vcmnvc(edrf->arrow_blk[1].location, edrf->asso_blk[0].location,
							text_loc->vec1);
			um_unitvc(text_loc->vec1, text_loc->vec1);
			}
		text_loc->length2 = um_dcccc(edrf->asso_blk[1].location, 
										edrf->arrow_blk[j].location);
		um_vcmnvc(edrf->arrow_blk[j].location, edrf->asso_blk[1].location,
										text_loc->vec2);
		um_unitvc(text_loc->vec2, text_loc->vec2);
		}

procexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_loc_new_text_loc(e, text_loc)
**		Find new location of original text
**    PARAMETERS   
**       INPUT  : 
**          e						drafting entity 
**          text_loc				text_loc
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_loc_new_text_org(edrf, text_loc, xaxis, yaxis)
	struct UA_generic_draft	*edrf;
	struct UA_old_loc       *text_loc;
	UM_coord xaxis, yaxis;
	{
	int i, j, k, num1, num2;
	UM_coord	del, pc, p3, int_pt1, int_pt2, tmp_vec1, tmp_vec2, test_pt;
	UU_REAL length, length1, length2, um_mag(), um_dot();

	uu_denter(UU_STRC,(us,"ua_loc_new_text_org()"));

	/* compute center of the new line between points */
	um_vcmnvc(edrf->asso_blk[1].location, edrf->asso_blk[0].location, del);
	length = um_mag(del);
	um_unitvc(del, p3);
	length = 0.5*length;
	um_vctmsc(p3,length,p3);
	um_vcplvc(edrf->asso_blk[0].location, p3, pc);

	/* now calculate new dimension text origin */
	um_vcplvc(pc, &text_loc->del_vec[0], &edrf->dim_origin[0]);

	/* check if we are working with a HORIZ or VERT dimension */
	if(edrf->subtype == 1 || edrf->subtype == 2)
		{
		if(edrf->subtype ==1)				/* HORIZ */
				um_vctovc(xaxis, del);
		else 										/* VERT */
				um_vctovc(yaxis, del);

		/* calculate intersection point of extension lines and new dim line */
		um_unitvc(text_loc->del_vec, tmp_vec1);
		um_vctmsc(tmp_vec1, text_loc->del_length, tmp_vec1);
		um_vcplvc(pc, tmp_vec1, test_pt);
		um_ilnln(edrf->asso_blk[0].location, text_loc->vec1, test_pt,
								del, &num1, int_pt1);
		um_ilnln(edrf->asso_blk[1].location, text_loc->vec2, test_pt,
								del, &num2, int_pt2);

		/* calculate off-set for text_origin position */
		um_vcmnvc(int_pt1, edrf->asso_blk[0].location, tmp_vec1);
		length1 = um_mag(tmp_vec1);
		um_unitvc(tmp_vec1, tmp_vec1);
		um_vcmnvc(int_pt2, edrf->asso_blk[1].location, tmp_vec2);
		length2 = um_mag(tmp_vec2);
		um_unitvc(tmp_vec2, tmp_vec2);

		/* first check if dimension line intersects geometric line */
		if(um_dot(text_loc->vec1, tmp_vec1) < 0. )
			{
			um_vctmsc(text_loc->vec1, (length1 + text_loc->length1), tmp_vec1);
			um_vcplvc(edrf->dim_origin, tmp_vec1, edrf->dim_origin);
			goto fexit;
			}
		else if(um_dot(text_loc->vec2, tmp_vec2) < 0.)
			{
			um_vctmsc(text_loc->vec2, (length2 + text_loc->length2), tmp_vec2);
			um_vcplvc(edrf->dim_origin, tmp_vec2, edrf->dim_origin);
			goto fexit;
			}

		/* next check that off-set is large enough */
		if(length1 < text_loc->length1)
			{
			um_vctmsc(text_loc->vec1, (text_loc->length1 - length1), tmp_vec1);
			um_vcplvc(edrf->dim_origin, tmp_vec1, edrf->dim_origin);
			goto fexit;
			}
		else if(length2 < text_loc->length2)
			{
			um_vctmsc(text_loc->vec2, (text_loc->length2 - length2), tmp_vec2);
			um_vcplvc(edrf->dim_origin, tmp_vec2, edrf->dim_origin);
			goto fexit;
			}
		}
fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_reloc_text_arc(e)
**		Find new location of original text for radius/diameter/arc-length
**    PARAMETERS   
**       INPUT  : 
**          e						drafting entity 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_reloc_text_arc(edrf)
	struct UA_generic_draft	*edrf;
	{
	int i, j, k, num1, num2, s_status, relation;
	UM_coord	 tmp_vec, center, normal ,c_spt, c_ept;
	UU_REAL dummy, radius, dtheta, um_mag(), um_dot();
	UU_KEY_ID curr_key;

	uu_denter(UU_STRC,(us,"ua_reloc_text_arc()"));
	
	/* retrieve data for arc */
	curr_key = edrf->asso_blk[0].key;
	s_status = um_retrieve_data_relnum(curr_key, &relation);
	if( ( relation==UA_LINEAR_DIM ) )
		{
		ua_centerline_arc(curr_key, center, &radius, &dtheta, normal,c_spt,
					c_ept, &dummy);
		}
	else
		{
		uc_draft_arc(curr_key, center, &radius, &dtheta, normal, c_spt,
					c_ept, &dummy);
		}

	/* calculate off-set vector for original data */
	um_vcmnvc(edrf->dim_origin, edrf->asso_blk[0].location, tmp_vec);

	/* re-compute text origin for new location */
	um_vcplvc(center, tmp_vec, edrf->dim_origin);
	um_vctovc(center, edrf->asso_blk[0].location);

	/* check for special cases */
	switch( edrf->etype )
		{
		case UA_ARC_LEN_DIM:         /* arc length dimension */
			break;
		case UA_RAD_CEN_DIM:			  /* radius dimension */
			um_vcplvc(edrf->asso_blk[1].location, tmp_vec,
												edrf->asso_blk[1].location);
			um_vcplvc(edrf->asso_blk[2].location, tmp_vec,
												edrf->asso_blk[2].location);
			break;
		case UA_DIA_IN_DIM:         /* diameter dimension */
			switch( edrf->subtype )
				{
				case 9:
				case 3:
				case 2:
				case 1:
					/* ua_diam_regen(&edrf); */
					break;
				case 7:
					/* ua_dia_shaft_regen(&edrf); */
					break;
				case 8:
					/* ua_dia_cyln_regen(&edrf); */
					break;
				}
		}

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_reloc_text_angular(e)
**		Find new location of original text for radius/diameter/arc-length
**    PARAMETERS   
**       INPUT  : 
**          e						drafting entity 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_reloc_text_angular(edrf, spt1, ept1, spt2, ept2)
	struct UA_generic_draft	*edrf;
	UM_coord spt1, ept1, spt2, ept2;
	{
	int i, j, k, num1, num2, s_status, relation;
	UU_KEY_ID curr_key;
	UM_coord cpln_origin, xaxis, yaxis, zaxis, del, tmp1, tmp2;

	uu_denter(UU_STRC,(us,"ua_reloc_text_angular()"));

	ua_getcpln(edrf,cpln_origin,xaxis,yaxis,zaxis);

	ua_regen_endpts(edrf, 0, spt1, ept1);
	ua_regen_endpts(edrf, 1, spt2, ept2);

	um_nptpln(spt1,cpln_origin,zaxis,tmp1);
	um_nptpln(spt2,cpln_origin,zaxis,tmp2);

	um_vcmnvc(tmp1, edrf->asso_blk[0].location, del);
	um_vcplvc(edrf->dim_origin, del, edrf->dim_origin);
	um_vctovc(tmp1, edrf->asso_blk[0].location);
	um_vctovc(tmp2, edrf->asso_blk[1].location);

fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_get_polar_data()
**			Re-generate polar dimension data
**    PARAMETERS   
**       INPUT  : 
**          none					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ua_get_polar_data(cpln_origin, xaxis, yaxis, zaxis,
									spt1, ept1, vec1, npt, ext_offset, edrf)

	UM_coord cpln_origin, xaxis, yaxis, zaxis, spt1, ept1, vec1, npt;
	struct UA_generic_draft  *edrf;
	UU_REAL *ext_offset;
	{
	UU_KEY_ID curr_key;
	int i, j, k, rel_num, num, s_status;	
	UU_REAL	radius, dang, r_dummy;
	UM_coord	entity_origin, normal, spt2, ept2, vec2, c_spt, c_ept,
				tmp1_vec, tmp2_vec;
	UU_LOGICAL	status;

	uu_denter(UU_STRC,(us,"ua_get_polar_data()"));

	status = UU_TRUE;

	/* retrieve data for the two lines */
	ua_regen_endpts(edrf, 0, spt1, ept1);
	ua_regen_endpts(edrf, 1, spt2, ept2);
	um_nptpln(spt1,cpln_origin,zaxis,npt);
	um_vctovc(npt, spt1);
	um_nptpln(ept1,cpln_origin,zaxis,npt);
	um_vctovc(npt, ept1);
	um_vctovc(spt1, edrf->asso_blk[0].location);
	um_nptpln(spt2,cpln_origin,zaxis,npt);
	um_vctovc(npt, spt2);
	um_nptpln(ept2,cpln_origin,zaxis,npt);
	um_vctovc(npt, ept2);
	um_vctovc(spt2, edrf->asso_blk[1].location);

	/* intersect lines to determine origin */
	um_vcmnvc(spt1, ept1, vec1);
	um_vcmnvc(spt2, ept2, vec2);
	um_unitvc(vec1, vec1);
	um_unitvc(vec2, vec2);
	um_ilnln(ept1, vec1, ept2, vec2, &num, npt);
	if(num == 0)
		{
		uu_uerror0(UA_DRAFTING,8);
		status = UU_FALSE;
		goto fexit; 
		}

	/* get entity information */
	s_status = um_retrieve_data_relnum(edrf->asso_blk[2].key, &rel_num);
	switch(rel_num)
		{
		case UM_CIRCLE_REL:
			uc_draft_arc(edrf->asso_blk[2].key, edrf->asso_blk[2].location,
						&radius, &dang, normal, c_spt, c_ept, &r_dummy);
			*ext_offset = edrf->grid_dist + radius;
			break;
		case UM_CONIC_REL:
			uc_draft_conic(edrf->asso_blk[2].key, edrf->asso_blk[2].location,
								c_spt , c_ept);
			*ext_offset = edrf->grid_dist;
			break;
		default:
			ua_regen_endpts(edrf, 2, tmp1_vec, tmp2_vec);
			um_vctovc(tmp1_vec, edrf->asso_blk[2].location);
			*ext_offset = edrf->grid_dist;
			break;
		}
	um_nptpln(edrf->asso_blk[2].location,cpln_origin,zaxis,entity_origin);
	um_vctovc(entity_origin, edrf->asso_blk[2].location);

fexit:
	uu_dexit;
	return(status);

	}

/*********************************************************************
**    E_FUNCTION     : ua_user_regen()
**		User select routine for dimension regeneration
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_user_regen()
	{
	int i, j, k, num1, relation;
	UU_KEY_ID curr_key;
	UU_LOGICAL status;
	UU_LOGICAL ua_gnxt();
	struct UA_generic_draft	edrf;

	uu_denter(UU_STRC,(us,"ua_user_regen()"));

	ud_lgeo(UU_TRUE, UD_dimensions);
	ua_select( 134, &num1);
	if( num1 == 0) goto fexit;

	for(i=0; i<num1; i++)
		{
		status = ua_gnxt(&curr_key);
		if(status)
			{
			if(um_retrieve_data_relnum(curr_key, &relation) == UU_SUCCESS)
				{
				if(relation == UA_LINEAR_DIM)
					{
					edrf.key = curr_key;
					uc_retrieve_data(&edrf,sizeof(struct UA_generic_draft	)) ;
					status = ua_regen_drafting(&edrf);
					}
				}
			}
		}
fexit:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_regen_save(edrf)
**		Save system defaults and replace with entity defaults
**    PARAMETERS   
**       INPUT  : 
**				edrf								drafting entity record
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_regen_save(edrf)
struct UA_generic_draft	*edrf;
	{
	int		i, j, k;

	uu_denter(UU_STRC,(us,"ua_edit_save"));

	save_dim_line_font = UA_dim_line_font;
	save_dim_line_color = UA_dim_line_color;
	save_ext_line_font = UA_ext_line_font;
	save_ext_line_color = UA_ext_line_color;
	save_arrow_symbol = UA_arrow_symbol;
	save_arrow_color = UA_arrow_color;
	save_dim_line_dens = UA_dim_line_dens;
	save_ext_line_dens = UA_ext_line_dens;
	save_arrow_dens = UA_arrow_dens;

	if(  edrf->line_blk_use > 0  )
		{
		for(i=0;i<edrf->line_blk_use;i++)
			{
			switch( edrf->line_blk[i].subtype )
				{
				case dim_line:
					{
					UA_dim_line_font = edrf->line_blk[i].line.line_font;
					UA_dim_line_color = edrf->line_blk[i].line.color;
					UA_dim_line_dens = edrf->line_blk[i].line.line_density;
					}
					break;
				case ext_line:
					{
					UA_ext_line_font = edrf->line_blk[i].line.line_font;
					UA_ext_line_color = edrf->line_blk[i].line.color;
					UA_ext_line_dens = edrf->line_blk[i].line.line_density;
					}
					break;
				}
			}
		}
	if( ( ( edrf->arc_blk_use!=0 )&&( edrf->line_blk_use==0) ) )
		{
		UA_dim_line_font = edrf->arc_blk[0].arc.line_font;
		UA_dim_line_color = edrf->arc_blk[0].arc.color;
		UA_dim_line_dens = edrf->arc_blk[0].arc.line_density;
		}
	if( ( edrf->arrow_blk_use != 0 ) )
		{
		UA_arrow_symbol = edrf->arrow_blk[0].arrow_type;
		UA_arrow_color = edrf->arrow_blk[0].arrow.color;
		UA_arrow_dens = edrf->arrow_blk[0].arrow.line_density;
		}
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : ua_regen_restore()
**		Restore system defaults
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_regen_restore()
	{
	uu_denter(UU_STRC,(us,"ua_regen_restore()"));

	UA_dim_line_font = save_dim_line_font;
	UA_dim_line_color = save_dim_line_color;
	UA_ext_line_font = save_ext_line_font;
	UA_ext_line_color = save_ext_line_color;
	UA_arrow_symbol = save_arrow_symbol;
	UA_arrow_color = save_arrow_color;
	UA_dim_line_dens = save_dim_line_dens;
	UA_ext_line_dens = save_ext_line_dens;
	UA_arrow_dens = save_arrow_dens;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ua_asso_check(edrf)
**		Check that drafting entity has correct associations
**    PARAMETERS   
**       INPUT  : 
**          edrf					drafting entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_asso_check( edrf)
	struct UA_generic_draft	*edrf;
	{
	int i, j, k, status, relation, s_status, mode, start_blk, num_blks;
	UU_KEY_ID  ent_key;

	uu_denter(UU_STRC,(us,"ua_asso_check()"));

	status = UU_SUCCESS;

	/* form positional tol may not have associated entities */
	if(edrf->etype == UA_FANDP_TOL) goto procexit;
	
	/* check if any association blocks */
	if(edrf->asso_blk_use == 0 )
		{
		status = UU_FAILURE;
		goto procexit;
		}

	/* switch on drafting entity sub-type */
	switch( edrf->etype )
		{
		case UA_LINEAR_DIM:  /* standard linear dimension  */
		case UA_CHAIN_DIM:    /* chained linear dimension */
		case UA_BASELN_DIM:    /* base-line linear dimension */
		case UA_ANGULAR_DIM:			  /* angular dimension */
		case UA_POLAR_DIM:    /* polar dimension */
			start_blk = 0;
			num_blks = 2;
			break;

		case UA_ARC_LEN_DIM:         /* arc length dimension */
		case UA_SECT_ARROW:
			start_blk = 0;
			num_blks = 1;
			break;

		case UA_RAD_CEN_DIM:			  /* radius dimension */

			switch( edrf->subtype )
				{
				case 4:
				case 5:
				case 6:
					start_blk = 0;
					num_blks = 1;
					break;
				case 10:
					start_blk = 0;
					num_blks = 1;
					break;
				}

		case UA_DIA_IN_DIM:         /* diameter dimension */

			switch( edrf->subtype )
				{
				case 3:
				case 2:
				case 1:
					start_blk = 0;
					num_blks = 1;
					break;
				case 7:
				case 8:
					start_blk = 0;
					num_blks = 2;
					break;
				case 9:
					start_blk = 0;
					num_blks = 1;
					break;
				}
			break;

		case UA_CENTERLINE:
			mode = edrf->subtype;
			num_blks = edrf->asso_blk_use;
			switch( mode )
				{
				case 1:
				case 2:
					start_blk = 0;
					break;
				case 3:
				case 4:
					start_blk = 1;
					break;
				}
			break;

		case UA_CROSSHATCH:
			start_blk = 0;
			num_blks = edrf->asso_blk_use - 1;
			break;

		case UA_LABEL_DIM:
			goto procexit;
			break;

		default:
			goto procexit;
		}

		/* check keys */
		for(i=start_blk;i<num_blks;i++)
			{
			if(edrf->asso_blk[i].key == 0)
				{
				status = UU_FAILURE;
				break;
				}
			else
				{
				s_status = um_retrieve_data_relnum(edrf->asso_blk[i].key,
														&relation);
				if(s_status == UU_FAILURE)
					{
					status = UU_FAILURE;
					break;
					}
				}
			}

procexit:
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : ua_regen_update_record(edrf)
**		Update and re-display regenerated entity
**    PARAMETERS   
**       INPUT  : 
**          edrf						updated entity record	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_regen_update_record(edrf)
	struct UA_generic_draft	*edrf;
	{
	int status;

	uu_denter(UU_STRC,(us,"ua_regen_update()"));

	/* update record in unibase */
	status = ua_update_entity(edrf->key,edrf);
	if(  status != 0  )
		{
		ua_create_entity(edrf,edrf->key);
		}

	/* now re-display regenerated entity */
	uc_display(edrf);
	uu_dexit;
	}
